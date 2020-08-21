module AnsiToPrompt (
      main
    ) where


import Control.Monad.State.Strict as S
import qualified Control.Monad.State.Strict (State)
import System.Environment (getArgs)


main :: IO ()
main = do
    args <- getArgs
    let pt = case args of
            ["--bash"] -> Bash
            ["--haskeline"] -> Haskeline
            _ -> error "Expecting --bash or --haskeline argument."
    interact $ runAnsiToPrompt pt


data PromptType
    = Bash
    | Haskeline


data Status
    = Normal
    | Escaping


runAnsiToPrompt :: PromptType -> String -> String
runAnsiToPrompt pt = flip S.evalState Normal . ansiToPrompt pt


ansiToPrompt :: PromptType -> String -> State Status String
ansiToPrompt pt str = case str of
    '\ESC' : rest -> do
        S.put Escaping
        let open = case pt of
              Bash -> "\\[\ESC"
              Haskeline -> "\ESC"
        liftM (open ++) $ ansiToPrompt pt rest
    'm' : rest -> do
        status <- S.get
        case status of
            Normal -> liftM ('m' :) $ ansiToPrompt pt rest
            Escaping -> do
                S.put Normal
                let close = case pt of
                        Bash -> "\\]"
                        Haskeline -> "\STX"
                liftM (('m' : close) ++) $ ansiToPrompt pt rest
    c : rest -> liftM (c :) $ ansiToPrompt pt rest
    "" -> return ""


