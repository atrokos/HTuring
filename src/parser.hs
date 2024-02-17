{-# LANGUAGE LambdaCase #-}

import GHC.Char
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

data Move = Left | Stay | Right
    deriving Show

data Command = State {subst::Char, move::Move, nextState::String}
    deriving Show

data Turing = Turing {currState::String, states::M.Map (String,Char) Command}
    deriving Show

data Tape = Tape {prevChars::[Char], curr::Char, nextChars::[Char]}
    deriving Show


removeComments :: [String] -> [String]
removeComments input = case input of
    [] -> []
    (line:lines) -> removeComment line : removeComments lines
                    where removeComment s = reverse $ dropWhile Char.isSpace $ reverse $ takeWhile (/= ';') s

clearInput :: [String] -> [String]
clearInput = filter (not . null)

parseLines :: [String] -> Maybe [((String, Char), Command)]
parseLines = traverse parseState

parseState :: String -> Maybe ((String, Char), Command)
parseState line = case stringToLower $ head comms of
    "state" -> strToState $ tail comms
    _       -> Nothing
    where comms = words line

strToState :: [String] -> Maybe ((String, Char), Command)
strToState inputs = case inputs of
    [currState, currCharStr, subCharStr, arrow, nextState] -> do
                            currChar <- stringToChar currCharStr
                            subChar <- stringToChar subCharStr
                            move <- case arrow of
                                "<-" -> Just Main.Left
                                "-"  -> Just Main.Stay
                                "->" -> Just Main.Right
                                _    -> Nothing
                            return ((currState, currChar), State subChar move nextState)
    _ -> Nothing

stringToChar :: String -> Maybe Char
stringToChar s = if length s == 1 then Just $ head s else Nothing

stringToLower :: String -> String
stringToLower = map Char.toLower

createTuring :: [String] -> Maybe Turing
createTuring lines = do
    states <- parseLines $ clearInput $ removeComments lines
    let map = M.fromList states
    return (Turing "Start" map)

tapeToLeft :: Tape -> Char  -> Maybe Tape
tapeToLeft (Tape prev curr next) newChar = case prev of
    [] -> Nothing
    _  -> Just $ Tape prev_ curr_ next_
        where
            prev_ = init prev
            curr_ = last prev
            next_ = case newChar of
                    '_' -> curr:next
                    _  -> newChar:next

tapeToRight :: Tape -> Char -> Maybe Tape
tapeToRight (Tape prev curr next) newChar = case next of
    [] -> Nothing
    _  -> Just $ Tape prev_ curr_ next_
        where
            prev_ = case newChar of
                '_' -> prev ++ [curr]
                _   -> prev ++ [newChar]
            curr_ = head next
            next_ = tail next

tapeStay :: Tape -> Char -> Tape
tapeStay (Tape prev curr next) sub = Tape prev sub next

moveTape :: Turing -> Tape -> Move -> Char -> Either (Turing, Tape) (String, Tape)
moveTape turing tape move sub = case move of
    Main.Left  -> case tapeToLeft tape sub of
                    Just result -> Prelude.Left (turing, result)
                    Nothing     -> Prelude.Right ("Tape ran out to the left!", tape)
    Main.Right -> case tapeToRight tape sub of
                    Just result -> Prelude.Left (turing, result)
                    Nothing     -> Prelude.Right ("Tape ran out to the right!", tape)
    Main.Stay  -> Prelude.Left (turing, tapeStay tape sub)

turingStep :: (Turing, Tape) -> Either (Turing, Tape) (String, Tape)
turingStep (turing@(Turing currState states), tape@(Tape prev curr next)) = case M.lookup (currState, curr) states of
    Just (State sub mov nex) -> moveTape turing tape mov sub
    Nothing -> Prelude.Right (currState, tape)

runTuring :: Turing -> Tape -> (String, Tape)
runTuring turing tape = runTuringHelper (turing, tape)
    where
        runTuringHelper both = case turingStep both of
            Prelude.Left nextStep -> runTuringHelper nextStep
            Prelude.Right lastStep -> lastStep

createTape :: String -> Tape
createTape (x:xs) = Tape "#" x (xs ++ "#")

showResults :: String -> Tape -> String
showResults endState (Tape prev curr next) =
    "The machine halted.\nEnd state: " ++
    endState ++ "\nModified tape:\n" ++ (prev ++ (curr:next))


main = do
    putStrLn "Hello! Please input the config folder for the Turing machine:"
    x <- getLine
    file <- readFile x
    let l = lines file
    let turing = case createTuring l of
            Just correct -> correct
            Nothing -> Turing "Fail" M.empty
    putStrLn "Turing loaded! Tape:"
    t <- getLine
    let tape = createTape t
    let results = uncurry showResults $ runTuring turing tape
    pure ()