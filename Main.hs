import PhoneDirectory
import System.Environment

main = do args <- getArgs
          case args of
               ["find", name] -> find
               ["delete", name] -> delete
               ["update"] -> update
               ["display"] -> display
               ["add"] -> add
               _ -> print syntaxError

syntaxError = putStrLn
 "Usage of commands [args]\n\
\\n\ 
\\find   Find a Person \n\
\delete Delete a Person\n\
\update Update person Record \n\
\display  Display User friendly \n\
\\add    Add person in phonebook \n"
