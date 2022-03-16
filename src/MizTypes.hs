module MizTypes 
    ( MizTag (..)
    , MizValue (..)
    , MizItem (..)
    , endsWith
    )
where 

import Numeric

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
    show = printItem "" ""

showBool :: Bool -> String
showBool True = "true"
showBool False = "false"

dropLast :: Int -> [a] -> [a]
dropLast i xs = if length xs < i then [] else take (length xs - i) xs

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith m x 
    | length x < length m = False
    | length x == length m = x == m 
    | otherwise = endsWith m (drop 1 x)

showDouble :: Double -> String
showDouble d = 
    if endsWith ".0" disp then dropLast 2 disp else disp 
    where 
        disp = show d

showL :: Show a => String -> [a] -> String
showL sep [] = ""
showL sep (x:xs) = "\t" ++ show x ++ sep ++ showL sep xs

showIndentedL :: String -> String -> [MizItem] -> String
showIndentedL indent sep = foldr (\x s -> printItem indent sep x ++ s) ""
    
showVal :: String -> MizValue -> String
showVal _ (MizString s) = show s
showVal _ (MizInt i) = show i
showVal _ (MizNumber d) = showDouble d
showVal _ (MizBool b) = showBool b
showVal indent (MizList vs) = 
    "\n" ++ 
        indent ++  "{\n" ++ 
        showIndentedL (indent ++ "    ") "," vs ++ 
        indent ++ "}" 

showTag :: String -> MizTag -> String
showTag indent (MizStringTag s) = indent ++ "[\"" ++ s ++ "\"]"
showTag indent (MizIntTag i) = indent ++ "[" ++ show i ++ "]"
showTag _ MizMission = "mission"

printItem :: String -> String -> MizItem -> String
printItem indent sep (MizElem tag (MizList l)) = showTag indent tag ++ " = " ++ showVal indent (MizList l) ++ sep ++ " -- end of " ++ showTag "" tag ++ "\n"
printItem indent sep (MizElem tag val) = showTag indent tag ++ " = " ++ showVal indent val ++ sep ++ "\n"
