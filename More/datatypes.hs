-- Data type
data Question = Question Bool String deriving Show

-- Accessors
-- data Question = Question{ answered :: Bool
--                         , text     :: String } deriving Show
-- Question initializer
question :: String -> Question
question str = Question False (str ++ "?")

-- Initializer using accessors
-- question :: String -> Question
-- question str = Question{ answered = False
--                        , text     = str ++ "?"}

-- then you can work with your data using data constructor:

answer :: String -> Question -> Question
answer answ (Question False str) =
  Question True (str ++ " " ++ answ ++ ".")
answer _ old = old

-- or using the accessor functions (many combinations of the two approaches are possible):
-- answer :: String -> Question -> Question
-- answer answ qstn
--   | answered qstn = qstn
--   | otherwise     = qstn{ answered = True
--                         , text     = text qstn ++ " " ++ answ ++ "."}

-- example of calling the functions:
-- answer "Yes it is" (question "Is this answer already too long")