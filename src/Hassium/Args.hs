module Hassium.Args
    ( Option(..)
    , simplifyOptions
    , simplifyOptions'
    ) where

import Data.List(intercalate)

data Option 
   -- Main options
   = BuildOne | BuildAll | DumpInformationForThisModule | DumpInformationForAllModules
   | DisableLogging | EnableLogging | Alert String | Overloading | NoOverloading | LvmPath String | Verbose | NoWarnings | MoreOptions
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

-- Keep only the last of the overloading flags and the last of the logging enable flags.
-- The alert flag overrides logging turned off.
-- This function also collects all -P flags together and merges them into one. The order of the
-- directories is the order in which they were specified.
simplifyOptions :: [Option] -> [Option]
simplifyOptions ops = 
    let
      revdefops = reverse ops
      modops    = case alertMessageFromOptions revdefops of
                    (Just _)  ->  EnableLogging : revdefops -- Explicitly enable logging as well, just to be safe
                    Nothing   ->  revdefops
    in
      collectPaths (keepFirst [Overloading, NoOverloading] (keepFirst [EnableLogging, DisableLogging] modops)) [] []
          where
            -- Assumes the options are in reverse order, and also reverses them.
            -- Collects several LvmPath options into one
            collectPaths [] paths newops       = LvmPath (intercalate ":" paths) : newops              
            collectPaths (LvmPath path : rest) paths newops
                                               = collectPaths rest (path : paths) newops
            collectPaths (opt : rest) paths newops
                                               = collectPaths rest paths (opt : newops)                                   
            keepFirst fromList []              = []
            keepFirst fromList (opt : rest)    = if (opt `elem` fromList) then
                                                   opt : optionFilter fromList rest
                                                 else
                                                   opt : keepFirst fromList rest
            optionFilter fromList []           = []
            optionFilter fromList (opt : rest) = if (opt `elem` fromList) then
                                                   optionFilter fromList rest
                                                 else
                                                   opt : optionFilter fromList rest

-- Keep only the last of the overloading flags and the last of the logging enable flags.
-- The alert flag overrides logging turned off.
-- This function also collects all -P flags together and merges them into one. The order of the
-- directories is the order in which they were specified.
simplifyOptions' :: [Option] -> [Option]
simplifyOptions' ops = 
    let
      revdefops = reverse ops
      modops    = case alertMessageFromOptions revdefops of
                    (Just _)  ->  EnableLogging : revdefops -- Explicitly enable logging as well, just to be safe
                    Nothing   ->  revdefops
    in
      collectPaths (keepFirst [Overloading, NoOverloading] (keepFirst [EnableLogging, DisableLogging] modops)) [] []
          where
            -- Assumes the options are in reverse order, and also reverses them.
            -- Collects several LvmPath options into one
            collectPaths [] paths newops       = LvmPath (intercalate ":" paths) : newops              
            collectPaths (LvmPath path : rest) paths newops
                                               = collectPaths rest (path : paths) newops
            collectPaths (opt : rest) paths newops
                                               = collectPaths rest paths (opt : newops)                                   
            keepFirst fromList []              = []
            keepFirst fromList (opt : rest)    = if (opt `elem` fromList) then
                                                   opt : optionFilter fromList rest
                                                 else
                                                   opt : keepFirst fromList rest
            optionFilter fromList []           = []
            optionFilter fromList (opt : rest) = if (opt `elem` fromList) then
                                                   optionFilter fromList rest
                                                 else
                                                   opt : optionFilter fromList rest

-- Extracts the alert message. Returns Nothing if such is not present.
alertMessageFromOptions :: [Option] -> Maybe String
alertMessageFromOptions [] = Nothing
alertMessageFromOptions (Alert message: _) = Just message
alertMessageFromOptions (_ : rest) = alertMessageFromOptions rest
