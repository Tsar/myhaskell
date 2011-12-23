import Text.Parsec
import Text.Parsec.String

import Lambda

word :: Parser String
word = many1 letter

termParser :: Parser Term
termParser = do{ char '\\'
               ; v <- word
               ; string " . "
               ; t <- termParser
               ; return (Abs v t)
               }
            <|>
             do{ char '\\'
               ; v <- word
               ; string " -> "
               ; t <- termParser
               ; return (Abs v t)
               }
            <|>
             do{ char '('
               ; t <- termParser
               ; char ')'
               ; return t
               }
            <|>
             do{ t1 <- termParser
               ; char ' '
               ; t2 <- termParser
               ; return (App t1 t2)
               }
            <|>
             do{ v <- word
               ; return (Var v)
               }

--main = parseTest termParser "\\f . (\\x . f (x x)) (\\x . f (x x))"
main = parseTest termParser "x x"
