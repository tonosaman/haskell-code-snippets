module Hassium.Args
    ( Option(..)
    , simplify
    ) where

import Data.List(intercalate, nub, concat, unfoldr)
import Data.Maybe(listToMaybe, maybeToList)

data Option 
   -- Main options
   = BuildOne | BuildAll | DumpInformationForThisModule | DumpInformationForAllModules
   | Logging Bool | Alert String | Overloading Bool | LvmPath String | Verbose | NoWarnings | MoreOptions
   | Information String
   -- More options
   | StopAfterParser | StopAfterStaticAnalysis | StopAfterTypeInferencing | StopAfterDesugar
   | DumpTokens | DumpUHA | DumpCore | DumpCoreToFile 
   | DebugLogger | Host String | Port Int 
   | DumpTypeDebug | AlgorithmW | AlgorithmM | DisableDirectives | NoRepairHeuristics
   -- Experimental options
   | ExperimentalOptions | KindInferencing | SignatureWarnings | RightToLeft | NoSpreading
   | TreeWalkTopDown | TreeWalkBottomUp | TreeWalkInorderTopFirstPre | TreeWalkInorderTopLastPre
   | TreeWalkInorderTopFirstPost | TreeWalkInorderTopLastPost | SolverSimple | SolverGreedy
   | SolverTypeGraph | SolverCombination | SolverChunks | UnifierHeuristics
   | SelectConstraintNumber Int
 deriving (Eq, Show)

simplify :: [Option] -> [Option]
simplify ops =
  let
    aboutPath = LvmPath . intercalate ":" . nub . concat $ [wordsWhen (==':') x | LvmPath x <- ops]
    aboutOverloading = maybeToList . listToMaybe $ [x | x@(Overloading _) <- reverse ops]
    aboutLogging =
      if not $ null [ True | Alert _ <- ops ] then [Logging True]
      else maybeToList . listToMaybe $ [x | x@(Logging _) <- reverse ops]
    aboutOtherOpts = nub . concat $ unfoldr f ops where
      f [] = Nothing
      f (LvmPath _ : ls) = Just([], ls)
      f (Logging _ : ls) = Just([], ls)
      f (Overloading _ : ls) = Just([], ls)
      f (op : ls) = Just([op], ls)
  in aboutPath : aboutOverloading ++ aboutLogging ++ aboutOtherOpts

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s'' where (w, s'') = break p s'
