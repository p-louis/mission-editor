module MizTypes 
    ( MizTag (..)
    , MizValue (..)
    , MizItem (..)
    )
where 


data MizTag 
    = MizStringTag String
    | MizIntTag Integer
    deriving (Eq)

data MizValue 
    = MizString String
    | MizInt Integer
    | MizNumber Double
    | MizBool Bool
    | MizList [MizItem]
    deriving (Eq)

data MizItem
    = MizElem MizTag MizValue
    deriving (Eq)

showL :: Show a => String -> [a] -> String
showL sep [] = ""
showL sep (x:xs) = "\t" ++ show x ++ sep ++ showL sep xs

instance Show MizTag where
    show (MizStringTag s) = "\"" ++ s ++ "\""
    show (MizIntTag i) = show i

instance Show MizValue where
    show (MizString s) = "\"" ++ s ++ "\""
    show (MizInt i) = show i
    show (MizNumber d) = show d
    show (MizBool b) = show b
    show (MizList vs) = "\n{\n" ++ showL ",\n" vs ++ "}"

instance Show MizItem where
    show (MizElem tag val) = "[" ++ show tag ++ "] = " ++ show val 
    
