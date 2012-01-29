import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token

import Lambda

def :: LanguageDef st
def = emptyDef{ identStart = letter
              , identLetter = alphaNum
              , reservedOpNames = ["\\", ".", "->"]
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

termParser :: Parser Term
termParser = lambdaParser

main = parseTest termParser "\\f . (\\x . f (x x)) (\\x . f (x x))"
--main = parseTest termParser "x x"
