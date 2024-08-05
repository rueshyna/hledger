{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Spec.TestCommoditiesTags
import Spec.TestMarketPrices

main :: IO ()
main = do
  hspec testCommoditiesTags
  hspec testMarketPrices
  hspec testDecodeTickerInfo
