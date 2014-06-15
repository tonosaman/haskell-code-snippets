module Hassium.Args
    ( Flag(..)
    , simplify
    , parseOpt
    ) where

import Data.List(intercalate, nub, unfoldr)
import Data.Maybe(listToMaybe, maybeToList)
import Control.Monad.Except(Except, throwError)
import System.Console.GetOpt(getOpt, OptDescr(Option), ArgDescr(NoArg, ReqArg), ArgOrder(Permute))

data Flag 
   -- Main options
   = BuildOne | BuildAll | DumpInformationForThisModule | DumpInformationForAllModules
   | Logging Bool | Alert String | Overloading Bool | LvmPath String | Verbose | NoWarnings | MoreOptions
   | Information String
   -- More options
   | StopAfterParser | StopAfterStaticAnalysis | StopAfterTypeInferencing | StopAfterDesugar
   | DumpTokens | DumpUHA | DumpCore | DumpCoreToFile | DebugLogger
   | DumpTypeDebug | AlgorithmW | AlgorithmM | DisableDirectives | NoRepairHeuristics
 deriving (Eq, Show)

options :: [OptDescr Flag]
options = mainOptions ++ moreOptions

mainOptions :: [OptDescr Flag]
mainOptions =
  [ Option ['b'] ["build"]                (NoArg BuildOne) "recompile module even if up to date"
  , Option ['B'] ["build-all"]            (NoArg BuildAll) "recompile all modules even if up to date"
  , Option ['i'] ["dump-information"]     (NoArg DumpInformationForThisModule) "show information about this module"
  , Option ['I'] ["dump-all-information"] (NoArg DumpInformationForAllModules) "show information about all imported modules"
  , Option []    ["enable-logging"]       (NoArg (Logging True)) "enable logging, overrides previous disable-logging"
  , Option []    ["disable-logging"]      (NoArg (Logging False)) "disable logging (default), overrides previous enable-logging flags"
  , Option ['a'] ["alert"]                (ReqArg Alert "MESSAGE") "compiles with alert flag in logging; MESSAGE specifies the reason for the alert."
  , Option []    ["overloading"]          (NoArg (Overloading True)) "turn overloading on (default), overrides all previous no-overloading flags"
  , Option []    ["no-overloading"]       (NoArg (Overloading False)) "turn overloading off, overrides all previous overloading flags"
  , Option ['P'] ["lvmpath"]              (ReqArg LvmPath "PATH") "use PATH as search path"
  , Option ['v'] ["verbose"]              (NoArg Verbose) "show the phase the compiler is in"
  , Option ['w'] ["no-warnings"]          (NoArg NoWarnings) "do notflag warnings"
  , Option ['X'] ["moreoptions"]          (NoArg MoreOptions) "show more compiler options"
  , Option []    ["info"]                 (ReqArg Information "NAME") "display information about NAME"
  ]

moreOptions :: [OptDescr Flag]
moreOptions =
  [ Option ['1'] ["stop-after-parsing"]          (NoArg StopAfterParser) "stop after parsing"
  , Option ['2'] ["stop-after-static-analysis"]  (NoArg StopAfterStaticAnalysis) "stop after static analysis"
  , Option ['3'] ["stop-after-type-inferencing"] (NoArg StopAfterTypeInferencing) "stop after type inferencing"
  , Option ['4'] ["stop-after-desugaring"]       (NoArg StopAfterDesugar) "stop after desugaring into Core"    
  , Option ['t'] ["dump-tokens"]                 (NoArg DumpTokens) "dump tokens to screen"
  , Option ['u'] ["dump-uha"]                    (NoArg DumpUHA) "pretty print abstract syntax tree"
  , Option ['c'] ["dump-core"]                   (NoArg DumpCore) "pretty print Core program"
  , Option ['C'] ["save-core"]                   (NoArg DumpCoreToFile) "write Core program to file"
  , Option []    ["debug-logger"]                (NoArg DebugLogger) "show logger debug information"
  , Option ['d'] ["type-debug"]                  (NoArg DumpTypeDebug) "debug constraint-based type inference"         
  , Option ['W'] ["algorithm-w"]                 (NoArg AlgorithmW) "use bottom-up type inference algorithm W"
  , Option ['M'] ["algorithm-m"]                 (NoArg AlgorithmM) "use folklore top-down type inference algorithm M"
  , Option []    ["no-directives"]               (NoArg DisableDirectives) "disable type inference directives"
  , Option []    ["no-repair-heuristics"]        (NoArg NoRepairHeuristics) "don't suggest program fixes"
  ]

simplify :: [Flag] -> [Flag]
simplify flags =
  let
    aboutPath = LvmPath . intercalate ":" . nub . concat $ [wordsWhen (==':') x | LvmPath x <- flags]
    aboutOverloading = maybeToList $ listToMaybe [x | x@(Overloading _) <- reverse flags]
    aboutLogging =
      if hasAlert flags then [Logging True]
      else maybeToList $ listToMaybe [x | x@(Logging _) <- reverse flags]
    aboutOther = nub . concat $ unfoldr f flags where
      f [] = Nothing
      f (LvmPath _ : ls) = Just([], ls)
      f (Logging _ : ls) = Just([], ls)
      f (Overloading _ : ls) = Just([], ls)
      f (op : ls) = Just([op], ls)
  in aboutPath : aboutOverloading ++ aboutLogging ++ aboutOther

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s'' where (w, s'') = break p s'

lvmPath :: [Flag] -> Maybe String
lvmPath flags = listToMaybe [ s | LvmPath s <- flags]

alertMessage :: [Flag] -> Maybe String
alertMessage flags = listToMaybe [ message | Alert message <- flags]

hasAlert :: [Flag] -> Bool
hasAlert flags = not $ null [ True | Alert _ <- flags ]

type CommandOptMonad = Either CommandOptError
data CommandOptError
 = InvalidFlag [String]
 | TooManyParams [String]
 | NoParams
 | CommandOptError String

instance Show CommandOptError where
  show args = case args of
    InvalidFlag errors ->
      show $ CommandOptError $ unlines
      $ "list of parameters is erroneous." : "Problem(s):" : map ("  " ++) errors
    TooManyParams params ->
      show $ CommandOptError $ unlines
      $ "only one non-option parameter expected, but found instead:" : map ("  " ++) params
    NoParams -> show $ CommandOptError "the name of the module to be compiled seems to be missing."
    CommandOptError s -> "Error in invocation: " ++ s

parseOpt :: [String] -> CommandOptMonad ([Flag], Maybe String)
parseOpt argstr
  | not $ null errors = throwError $ InvalidFlag errors
  | length params > 1 = throwError $ TooManyParams params
  | null params = throwError NoParams
  | otherwise = return (simplify $ defaults ++ flags, listToMaybe params)
  where
    (flags, params, errors) = getOpt Permute (options) argstr
    defaults = [Logging False, Overloading True]
