import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token

import Lambda (Term(..), normal')

def :: LanguageDef st
def = emptyDef{ identStart = letter
              , identLetter = alphaNum
              , reservedOpNames = ["\\", ".", "->", "="]
              , reservedNames = ["let", "in"]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , whiteSpace = m_whiteSpace } = makeTokenParser def

applicationList t ts = foldl (\t' t'' -> App t' t'') t ts

abstractionList vs t = foldr (\v' t' -> Abs v' t') t vs

lambdaParser :: Parser Term
lambdaParser = do{ m_reservedOp "\\"
                 ; vs <- many1 m_identifier
                 ; (m_reservedOp ".") <|> (m_reservedOp "->")
                 ; t <- termParser
                 ; return (abstractionList vs t)
                 }
              <|>
               do{ t <- notLambdaParser
                 ; ts <- many notLambdaParser
                 ; return (applicationList t ts)
                 }

notLambdaParser :: Parser Term
notLambdaParser = do{ v <- m_identifier
                    ; return (Var v)
                    }
                 <|>
                  m_parens lambdaParser

letParser :: Parser Term
letParser = do{ m_reserved "let"
              ; v <- m_identifier
              ; m_reservedOp "="
              ; t <- termParser
              ; m_reserved "in"
              ; t' <- termParser
              ; return (App (Abs v t') t)
              }

termParser :: Parser Term
termParser = letParser <|> lambdaParser

--main = parseTest termParser "let Y = \\f . (\\x . f (x x)) (\\x . f (x x)) in let zero = \\s -> \\z -> z in let succ = \\p s z -> s (p s z) in Y (succ zero)"

termToString :: Term -> String
termToString (Var v)    = v
termToString (App t t') = "(" ++ (termToString t) ++ ") (" ++ (termToString t') ++ ")"
termToString (Abs v t)  = "\\" ++ v ++ " . " ++ (termToString t)

main = do
    args <- getArgs
    fileContents <- readFile (head args)
    case parse termParser "" fileContents of
        Left error -> print error
        Right t    -> putStrLn (termToString t)
