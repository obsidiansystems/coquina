# coquina

> *coquina*
> /kōˈkēnə/
> 1. a soft limestone of broken shells, used in road-making in the Caribbean and Florida.
> 2. an easy-to-use library for running shell commands from haskell.

```haskell
import Control.Monad
import Coquina
import System.Process (shell, proc)

main :: IO ()
main = do
  (exitCode, out, err) <- execShell $ do
    (contents, ()) <- readStdout $ run $ shell "ls"
    mapM_ (run . proc "file" . (:[])) $ take 10 $ lines contents
  putStrLn $ unlines
    [ "Exit code: " ++ show exitCode
    , "stdout: " ++ out
    , "stderr: " ++ err
    ]
```
