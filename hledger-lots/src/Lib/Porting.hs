{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Porting where

import Hledger.Cli.Script hiding (Group)
import System.Console.ANSI (Color(..),ColorIntensity(..))
import Hledger.Data.Types ( MixedAmount(..))
import qualified Data.Text as T
import Data.Default (def)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Map.Strict as M
import Text.Tabular.AsciiWide hiding (render)
import Lens.Micro ((^.))

-- | Find postings matching a given query, within a given date span,
-- and also any similarly-matched postings before that date span.
-- Date restrictions and depth restrictions in the query are ignored.
-- A helper for the postings report.
matchedPostingsBeforeAndDuring' :: ReportSpec -> Journal -> DateSpan -> ([Posting],[Posting])
matchedPostingsBeforeAndDuring' rspec@ReportSpec{_rsReportOpts=ropts,_rsQuery=q} j reportspan =
    dbg5 "beforeps, duringps" $ span (beforestartq `matchesPosting`) beforeandduringps
  where
    beforestartq = dbg3 "beforestartq" $ dateqtype $ DateSpan Nothing (Exact <$> spanStart reportspan)
    beforeandduringps = 
        sortOn (postingDateOrDate2 (whichDate ropts))            -- sort postings by date or date2
      . (if invert_ ropts then map negatePostingAmount else id)  -- with --invert, invert amounts
      . journalPostings
      -- With most calls we will not require transaction prices past this point, and can get a big
      -- speed improvement by stripping them early. In some cases, such as in hledger-ui, we still
      -- want to keep prices around, so we can toggle between cost and no cost quickly. We can use
      -- the show_costs_ flag to be efficient when we can, and detailed when we have to.
      -- modify
      . (if show_costs_ ropts then id else journalMapPostingAmounts mixedAmountToUnit)
      $ journalValueAndFilterPostings rspec{_rsQuery=beforeandduringq} j

    -- filter postings by the query, with no start date or depth limit
    beforeandduringq = dbg4 "beforeandduringq" $ And [depthless $ dateless q, beforeendq]
      where
        depthless  = filterQuery (not . queryIsDepth)
        dateless   = filterQuery (not . queryIsDateOrDate2)
        beforeendq = dateqtype $ DateSpan Nothing (Exact <$> spanEnd reportspan)

    dateqtype = if queryIsDate2 dateq || (queryIsDate dateq && date2_ ropts) then Date2 else Date
      where
        dateq = dbg4 "dateq" $ filterQuery queryIsDateOrDate2 $ dbg4 "q" q  -- XXX confused by multiple date:/date2: ?


-- | Count cost
mixedAmountToUnit :: MixedAmount -> MixedAmount
mixedAmountToUnit (Mixed ma) =
  Mixed $
   M.foldlWithKey' toUnit noPrices withPrices
  where
    (noPrices, withPrices) = M.partition (isNothing . aprice) ma
    toUnit m k@(MixedAmountKeyNoPrice _) a = M.insert k a m
    toUnit m k@(MixedAmountKeyTotalPrice c1 c2) a
      | isNothing (aprice a) = M.insert k a m
      | otherwise = M.insert (MixedAmountKeyTotalPrice c1 c2) (a {aprice = unitPrice (aprice a)}) m
      where unitPrice o@(Just (UnitPrice _)) = o
            unitPrice (Just (TotalPrice tp)) =
              Just $ TotalPrice $ tp { aprice =
                    Just $ UnitPrice $ tp { aquantity = aquantity tp / aquantity a, aprice = Nothing}
                }
    toUnit m k@(MixedAmountKeyUnitPrice {}) a = M.insert k a m


-- | Select postings from the journal and add running balance and other
-- information to make a postings report. Used by eg hledger's register command.
postingsReport' :: ReportSpec -> Journal -> PostingsReport
postingsReport' rspec@ReportSpec{_rsReportOpts=ropts@ReportOpts{..}} j = items
    where
      (reportspan, colspans) = reportSpanBothDates j rspec
      whichdate   = whichDate ropts
      mdepth      = queryDepth $ _rsQuery rspec
      multiperiod = interval_ /= NoInterval

      -- postings to be included in the report, and similarly-matched postings before the report start date
      -- modify
      (precedingps, reportps) = matchedPostingsBeforeAndDuring' rspec j reportspan

      -- Postings, or summary postings with their subperiod's end date, to be displayed.
      displayps :: [(Posting, Maybe Period)]
        | multiperiod = [(p', Just period') | (p', period') <- summariseps reportps]
        | otherwise   = [(p', Nothing) | p' <- reportps]
        where
          summariseps = summarisePostingsByInterval whichdate mdepth showempty colspans
          showempty = empty_ || average_

      -- Posting report items ready for display.
      items =
        dbg4 "postingsReport items" $
        postingsReportItems displayps (nullposting,Nothing) whichdate mdepth startbal runningcalc startnum
        where
          -- In historical mode we'll need a starting balance, which we
          -- may be converting to value per hledger_options.m4.md "Effect
          -- of --value on reports".
          -- XXX balance report doesn't value starting balance.. should this ?
          historical = balanceaccum_ == Historical
          startbal | average_  = if historical then precedingavg else nullmixedamt
                   | otherwise = if historical then precedingsum else nullmixedamt
            where
              precedingsum = sumPostings precedingps
              precedingavg = divideMixedAmount (fromIntegral $ length precedingps) precedingsum

          runningcalc = registerRunningCalculationFn' ropts
          startnum = if historical then length precedingps + 1 else 1

-- | Based on the given report options, return a function that does the appropriate
-- running calculation for the register report, ie a running average or running total.
-- This function will take the item number, previous average/total, and new posting amount,
-- and return the new average/total.
registerRunningCalculationFn' :: ReportOpts -> (Int -> MixedAmount -> MixedAmount -> MixedAmount)
registerRunningCalculationFn' ropts
  | average_ ropts = \i avg amt -> avg `maPlus` divideMixedAmount (fromIntegral i) (amt `maMinus` avg)
  | otherwise      = \_ bal amt -> mapMixedAmount runningNewUnitFn $ (bal `maPlus` amt)

runningNewUnitFn :: Amount -> Amount
runningNewUnitFn a
   | q == 0 = a { aprice = noAvg (aprice a) }
   | t == 0 = a
   | otherwise = a { aprice = updateAvg (aprice a) }
  where q = aquantity a
        t = maybe 0 aquantity $ getTotal =<< ( aprice a)
        getTotal :: AmountPrice -> Maybe Amount
        getTotal (TotalPrice tp) = Just tp
        getTotal (UnitPrice _) = Nothing
        updateAvg :: Maybe AmountPrice -> Maybe AmountPrice
        updateAvg Nothing = Nothing
        updateAvg (Just (TotalPrice tp)) = 
          Just $ TotalPrice $ tp {
            aprice = Just $ UnitPrice $ tp { aquantity =   t / q, aprice = Nothing }
          }
        updateAvg old@(Just (UnitPrice _)) = old
        noAvg:: Maybe AmountPrice -> Maybe AmountPrice
        noAvg Nothing = Nothing
        noAvg (Just (TotalPrice tp)) = Just $ TotalPrice $ tp { aprice = Nothing }
        noAvg old@(Just (UnitPrice _)) = old

-- | Render a register report as plain text suitable for console output.
postingsReportAsText' :: CliOpts -> PostingsReport -> TL.Text
postingsReportAsText' opts = TB.toLazyText .
    postingsOrTransactionsReportAsText' alignAll opts (postingsReportItemAsText' opts) itemamt itembal
  where
    alignAll = boolopt "align-all" $ rawopts_ opts
    itemamt (_,_,_,Posting{pamount=a},_) = a
    itembal (_,_,_,_,a) = a

-- | Render one register report line item as plain text. Layout is like so:
-- @
-- <---------------- width (specified, terminal width, or 80) -------------------->
-- date (10)  description           account              amount (12)   balance (12)
-- DDDDDDDDDD dddddddddddddddddddd  aaaaaaaaaaaaaaaaaaa  AAAAAAAAAAAA  AAAAAAAAAAAA
-- @
-- If description's width is specified, account will use the remaining space.
-- Otherwise, description and account divide up the space equally.
--
-- With a report interval, the layout is like so:
-- @
-- <---------------- width (specified, terminal width, or 80) -------------------->
-- date (21)              account                        amount (12)   balance (12)
-- DDDDDDDDDDDDDDDDDDDDD  aaaaaaaaaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAAA  AAAAAAAAAAAA
-- @
--
-- date and description are shown for the first posting of a transaction only.
--
-- Returns a string which can be multi-line, eg if the running balance
-- has multiple commodities. Does not yet support formatting control
-- like balance reports.
--
-- Also returns the natural width (without padding) of the amount and balance
-- fields.
postingsReportItemAsText' :: CliOpts -> Int -> Int
                         -> (PostingsReportItem, [WideBuilder], [WideBuilder])
                         -> TB.Builder
postingsReportItemAsText' opts preferredamtwidth preferredbalwidth ((mdate, mperiod, mdesc, p, _), amt, bal) =
    table <> TB.singleton '\n'
  where
    table = renderRowB def{tableBorders=False, borderSpaces=False} . Group NoLine $ map Header
      [ textCell TopLeft $ fitText (Just datewidth) (Just datewidth) True True date
      , spacerCell
      , textCell TopLeft $ fitText (Just descwidth) (Just descwidth) True True desc
      , spacerCell2
      , textCell TopLeft $ fitText (Just acctwidth) (Just acctwidth) True True acct
      , spacerCell2
      , Cell TopRight $ map (pad amtwidth) amt
      , spacerCell2
      , Cell BottomRight $ map (pad balwidth) bal
      ]
    spacerCell  = Cell BottomLeft [WideBuilder (TB.singleton ' ') 1]
    spacerCell2 = Cell BottomLeft [WideBuilder (TB.fromString "  ") 2]
    pad fullwidth amt' = WideBuilder (TB.fromText $ T.replicate w " ") w <> amt'
      where w = fullwidth - wbWidth amt'
    -- calculate widths
    (totalwidth,mdescwidth) = registerWidthsFromOpts opts
    datewidth = maybe 10 periodTextWidth mperiod
    date = case mperiod of
             Just per -> if isJust mdate then showPeriod per else ""
             Nothing  -> maybe "" showDate mdate
    (amtwidth, balwidth)
      | shortfall <= 0 = (preferredamtwidth, preferredbalwidth)
      | otherwise      = (adjustedamtwidth, adjustedbalwidth)
      where
        mincolwidth = 2 -- columns always show at least an ellipsis
        maxamtswidth = max 0 (totalwidth - (datewidth + 1 + mincolwidth + 2 + mincolwidth + 2 + 2))
        shortfall = (preferredamtwidth + preferredbalwidth) - maxamtswidth
        amtwidthproportion = fromIntegral preferredamtwidth / fromIntegral (preferredamtwidth + preferredbalwidth)
        adjustedamtwidth = round $ amtwidthproportion * fromIntegral maxamtswidth
        adjustedbalwidth = maxamtswidth - adjustedamtwidth

    remaining = totalwidth - (datewidth + 1 + 2 + amtwidth + 2 + balwidth)
    (descwidth, acctwidth)
      | isJust mperiod = (0, remaining - 2)
      | otherwise      = (w, remaining - 2 - w)
      where
        w = fromMaybe ((remaining - 2) `div` 2) mdescwidth

    -- gather content
    desc = fromMaybe "" mdesc
    acct = parenthesise . elideAccountName awidth $ paccount p
      where
        (parenthesise, awidth) = case ptype p of
            BalancedVirtualPosting -> (wrap "[" "]", acctwidth-2)
            VirtualPosting         -> (wrap "(" ")", acctwidth-2)
            _                      -> (id,acctwidth)

-- | Render a 'PostingsReport' or 'AccountTransactionsReport' as Text,
-- determining the appropriate starting widths and increasing as necessary.
postingsOrTransactionsReportAsText'
    :: Show a => Bool -> CliOpts -> (Int -> Int -> (a, [WideBuilder], [WideBuilder]) -> TB.Builder)
    -> (a -> MixedAmount) -> (a -> MixedAmount) -> [a] -> TB.Builder
postingsOrTransactionsReportAsText' alignAll opts itemAsText itemamt itembal report =
    mconcat . snd $ mapAccumL renderItem (startWidth amt, startWidth bal) itemsWithAmounts
  where
    minWidth  = 12
    chunkSize = 1000

    renderItem (amtWidth, balWidth) item@(_, amt1, bal1) = ((amtWidth', balWidth'), itemBuilder)
      where
        itemBuilder = itemAsText amtWidth' balWidth' item
        amtWidth' = if alignAll then amtWidth else maximumStrict $ amtWidth : map wbWidth amt1
        balWidth' = if alignAll then balWidth else maximumStrict $ balWidth : map wbWidth bal1

    startWidth f = maximum $ minWidth : map wbWidth (concatMap f startAlign)
      where
        startAlign = (if alignAll then id else take chunkSize) itemsWithAmounts

    itemsWithAmounts = map (\x -> (x, showAmt $ itemamt x, showBal $ itembal x)) report
    -- modify
    showAmt = showMixedAmountLinesB' oneLine{displayColour=opts^.color__}
    showBal = showMixedAmountLinesB' oneLine{displayColour=opts^.color__, displayPrice=True}
    amt = second3
    bal = third3

-- | Helper for showMixedAmountB (and postingAsLines, ...) to show a list of Amounts on multiple lines.
-- This returns the list of WideBuilders: one for each Amount, and padded/elided to the appropriate width.
-- This does not honour displayOneLine; all amounts will be displayed as if displayOneLine were False.
showMixedAmountLinesB' :: AmountDisplayOpts -> MixedAmount -> [WideBuilder]
showMixedAmountLinesB' opts@AmountDisplayOpts{displayMaxWidth=mmax,displayMinWidth=mmin} ma =
    map (adBuilder . pad) elided
  where
    -- modify
    astrs = amtDisplayList (wbWidth sep) (showAmountB' opts) . orderedAmounts opts $ ma
    sep   = WideBuilder (TB.singleton '\n') 0
    width = maximum $ map (wbWidth . adBuilder) elided

    pad amt
      | Just mw <- mmin =
          let w = (max width mw) - wbWidth (adBuilder amt)
           in amt{ adBuilder = WideBuilder (TB.fromText $ T.replicate w " ") w <> adBuilder amt }
      | otherwise = amt

    elided = maybe id elideTo mmax astrs
    elideTo m xs = maybeAppend elisionStr short
      where
        elisionStr = elisionDisplay (Just m) (wbWidth sep) (length long) $ lastDef nullAmountDisplay short
        (short, long) = partition ((m>=) . wbWidth . adBuilder) xs

-- | General function to generate a WideBuilder for an Amount, according the
-- supplied AmountDisplayOpts. This is the main function to use for showing
-- Amounts, constructing a builder; it can then be converted to a Text with
-- wbToText, or to a String with wbUnpack.
-- Some special cases:
--
-- * The special "missing" amount is displayed as the empty string. 
--
-- * If an amount is showing digit group separators but no decimal places,
--   we force showing a decimal mark (with nothing after it) to make
--   it easier to parse correctly.
--
showAmountB' :: AmountDisplayOpts -> Amount -> WideBuilder
showAmountB' _ Amount{acommodity="AUTO"} = mempty
showAmountB'
  AmountDisplayOpts{displayPrice, displayColour, displayZeroCommodity, 
    displayThousandsSep, displayAddDecimalMark, displayOrder}
  a@Amount{astyle=style} =
    color $ case ascommodityside style of
      L -> showC (wbFromText comm) space <> quantity' <> price <> cost
      R -> quantity' <> showC space (wbFromText comm) <> price <> cost
  where
    color = if displayColour && isNegativeAmount a then colorB Dull Red else id
    quantity = showAmountQuantity displayAddDecimalMark $
      if displayThousandsSep then a else a{astyle=(astyle a){asdigitgroups=Nothing}}
    (quantity', comm)
      | amountLooksZero a && not displayZeroCommodity = (WideBuilder (TB.singleton '0') 1, "")
      | otherwise = (quantity, quoteCommoditySymbolIfNeeded $ acommodity a)
    space = if not (T.null comm) && ascommodityspaced style then WideBuilder (TB.singleton ' ') 1 else mempty
    -- concatenate these texts,
    -- or return the empty text if there's a commodity display order. XXX why ?
    showC l r = if isJust displayOrder then mempty else l <> r
    price = if displayPrice then showTotalPriceSymbol a else mempty
    -- modify
    cost =  showUnitPriceSymbol a

showTotalPriceSymbol :: Amount -> WideBuilder
showTotalPriceSymbol amt = case aprice amt of
    Nothing              -> mempty
    Just (UnitPrice  pa) -> showTotalPriceSymbol pa
    Just (TotalPrice pa) -> WideBuilder (TB.fromString " @@ ")  4 <> showAmountB' noColour{displayZeroCommodity=True} (sign pa { aprice = Nothing })
  where sign = if aquantity amt < 0 then negate else id

showUnitPriceSymbol :: Amount -> WideBuilder
showUnitPriceSymbol amt = case aprice amt of
    Nothing              -> mempty
    Just (UnitPrice  pa) -> WideBuilder (TB.fromString " @ ")  3 <> showAmountB' noColour{displayZeroCommodity=True} pa { aprice = Nothing }
    Just (TotalPrice pa) -> showUnitPriceSymbol pa
