-- Anton Grahn
module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Skip |
    Read String |
    Begin [Statement]|
    Write Expr.T|
    Repeat Statement Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

iif = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildif
buildif ((v,e),s) = If v e s

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildwhi
buildwhi (v,e) = While v e

skip = accept "skip" -# require ";"  >-> buildsk
buildsk (s) = Skip 

read = accept "read" -# word #- require ";" >-> buildread
buildread (v) = Read v

write = accept "write" -# Expr.parse #- require ";" >-> buildwri
buildwri (e) = Write e

begin = accept "begin" -# iter parse #- require "end" >-> buildbeg
buildbeg (s) = Begin s

repeat = accept "repeat" -# parse #- require "until" # Expr.parse #- require ";" >-> buildrep
buildrep (s,e) = Repeat s e




exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (Assignment word cond: stmts) dict input = 
    exec stmts (Dictionary.insert (word,Expr.value cond dict) dict) input
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (While cond whilestmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (whilestmts: While cond whilestmts: stmts) dict input
    else exec stmts dict input

exec (Skip: stmts) dict input = exec stmts dict input
exec (Read word: stmts) dict (input:inputs) = 
    exec(stmts) (Dictionary.insert (word,input) dict) inputs
exec (Write cond: stmts) dict input =
    Expr.value cond dict: exec stmts dict input
exec (Begin stmt: stmts) dict input = exec(stmt ++ stmts) dict input

exec (Repeat stmt cond: stmts) dict input = 
    exec (stmt: if (Expr.value cond dict)>=0 then stmts  else Repeat stmt cond:stmts ) dict input  

exec _ _ _ = []

instance Parse Statement where
  parse = while ! iif ! begin ! Statement.read ! write ! begin ! skip ! assignment ! Statement.repeat
  toString = tostringhelp

tostringhelp :: Statement -> String
tostringhelp (Assignment string expr) = string ++" := " ++ Expr.toString expr ++ "; \n"
tostringhelp (If expr stmt stmt2) = "if " ++ Expr.toString expr ++ "\n" ++ tostringhelp stmt ++"else \n" ++ tostringhelp stmt2 
tostringhelp (While expr stmt) = "while "++ Expr.toString expr ++ "\n" ++ tostringhelp stmt 
tostringhelp (Skip) = "skip;\n"
tostringhelp (Read string) = "read " ++ string ++ ";\n"
tostringhelp (Begin stmts) = "begin\n"++ foldr(++) "" (map tostringhelp stmts) ++ "end;\n"
tostringhelp (Write expr) = "write " ++ Expr.toString expr ++ ";\n"
tostringhelp (Repeat stmt expr) = "Repeat \n" ++ tostringhelp(stmt) ++ "until " ++ Expr.toString expr ++ ";\n"