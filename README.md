# MOVED TO [GITHUB](https://github.com/obsidiansystems/coquina)


# coquina
[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/coquina.svg)](https://hackage.haskell.org/package/coquina) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/coquina/badge)](https://matrix.hackage.haskell.org/#/package/coquina) [![Github CI](https://github.com/obsidiansystems/coquina/workflows/github-action/badge.svg)](https://github.com/obsidiansystems/coquina/actions) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/obsidiansystems/coquina/blob/master/LICENSE)

> *coquina*
> /kōˈkēnə/
> 1. a soft limestone of broken shells, used in road-making in the Caribbean and Florida.
> 2. an easy-to-use library for running shell commands from haskell.

```haskell
import Control.Monad
import Coquina
import System.Process (shell, proc)
import qualified Data.Text as T

main :: IO ()
main = do
  (exitCode, out, err) <- execShell $ do
    (contents, ()) <- readStdout $ run $ shell "ls"
    mapM_ (run . proc "file" . (:[])) $ take 10 $ lines $ T.unpack contents
  putStrLn $ unlines
    [ "Exit code: " ++ show exitCode
    , "stdout: " ++ T.unpack out
    , "stderr: " ++ T.unpack err
    ]
```
