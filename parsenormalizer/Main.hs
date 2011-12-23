import Text.Parsec
import Text.Parsec.String

import Lambda

word :: Parser String
word = do{ c <- letter
         ; do{ cs <- word
             ; return (c:cs)
             }
           <|> return [c]
         }

termParser :: Parser Term
termParser = do{ char '\\'
               ; v <- word
               ; string " . "
               ; t <- termParser
               ; return (Abs v t)
               }
            <|>
             do{ t1 <- termParser
               ; t2 <- termParser
               ; return (App t1 t2)
               }
            <|>
             do{ char '('
               ; t <- termParser
               ; char ')'
               ; return t
               }
            <|>
             do{ v <- word
               ; return (Var v)
               }

--main = parseTest termParser "\\f . (\\x . f (x x)) (\\x . f (x x))"
main = parseTest termParser "a"
