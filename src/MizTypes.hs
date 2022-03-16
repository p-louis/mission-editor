{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module MizTypes 
    ( MizTag (..)
    , MizValue (..)
    , MizItem (..)
    )
where 

import Prelude (String, Integer, Int, Double, Bool (..), Show (..), Eq, (<>), ($), (.))
import Prelude as P
import Numeric
import Data.Text 

data MizTag 
    = MizStringTag String
    | MizIntTag Integer
    | MizMission
    deriving (Show, Eq)

data MizValue 
    = MizString String
    | MizInt Integer
    | MizNumber Double
    | MizBool Bool
    | MizList [MizItem]
    deriving (Show, Eq)

data MizItem
    = MizElem MizTag MizValue
    deriving (Eq)

instance Show MizItem where
    show = unpack . printItem "" ""

showBool :: Bool -> Text
showBool True = "true"
showBool False = "false"

showIndentedL :: Text -> Text -> [MizItem] -> Text
showIndentedL indent sep = P.foldr (\x s -> printItem indent sep x <> s) ""
    
showVal :: Text -> MizValue -> Text
showVal _ (MizString s) = pack $ show s
showVal _ (MizInt i) = pack $ show i
showVal _ (MizNumber d) = pack $ show d
showVal _ (MizBool b) = showBool b
showVal indent (MizList vs) = 
    "\n" <> 
        indent <>  "{\n" <> 
        showIndentedL (indent <> "    ") "," vs <> 
        indent <> "}" 

showTag :: Text -> MizTag -> Text
showTag indent (MizStringTag s) = indent <> "[\"" <> pack s <> "\"]"
showTag indent (MizIntTag i) = indent <> "[" <> (pack . show) i <> "]"
showTag _ MizMission = "mission"

printItem :: Text -> Text -> MizItem -> Text
printItem indent sep (MizElem tag (MizList l)) = showTag indent tag <> " = " <> showVal indent (MizList l) <> sep <> " -- end of " <> showTag "" tag <> "\n"
printItem indent sep (MizElem tag val) = showTag indent tag <> " = " <> showVal indent val <> sep <> "\n"
