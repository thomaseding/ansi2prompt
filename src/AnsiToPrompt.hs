module AnsiToPrompt (
      main
    ) where


import Control.Monad.State.Strict


main :: IO ()
main = interact (flip evalState Normal . ansiToPrompt)


data Status = Normal | Escaping


ansiToPrompt :: String -> State Status String
ansiToPrompt str = case str of
    '\ESC' : rest -> do
        put Escaping
        liftM ("\\[\ESC" ++) $ ansiToPrompt rest
    'm' : rest -> do
        status <- get
        case status of
            Normal -> liftM ('m' :) $ ansiToPrompt rest
            Escaping -> do
                put Normal
                liftM ("m\\]" ++) $ ansiToPrompt rest
    c : rest -> liftM (c :) $ ansiToPrompt rest
    "" -> return ""


