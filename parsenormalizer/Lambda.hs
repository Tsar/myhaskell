module Lambda where

import Prelude hiding (iterate, (++), elem)

type Varible = String

-- Лямбда-терм
data Term = Var Varible
          | Abs Varible Term
          | App Term Term
          deriving (Show)

-- Тип [ a ] == Типу List a
-- значение [] == значению Nil
-- значение [ a ] == значению Cons a Nil
-- конструктор (:) == конструктору Cons

-- Конкатенация двух списков
(++) :: [a] -> [a] -> [a]
[] ++ b = b
(a:as) ++ b = a:(as ++ b)

-- Свободные переменные терма
free (Var v) = [v]
free (Abs v t) = filter (/= v) . free $ t -- /= это <<не равно>>
free (App t t') = (free t) ++ (free t')

-- Заменить все вхождения переменной var на what в терме term
subst var what term = case term of
    Var v    -> if v == var then what else term
    Abs v t  -> if v == var then term else Abs v (subst var what t)
    App t t' -> App (subst var what t) (subst var what t')

-- Содержит ли список элемент?
elem a [] = False
elem a (l:ls) = if a == l then True else elem a ls

-- Любопытная функция
iterate f x = (:) x $ iterate f (f x)

-- Генерирует список имён, производных от v, не входящих в fv
newname fv v = head . filter (\x -> not . elem x $ fv) . iterate ('_':) $ v

-- Обычная бета-редукция, хендлящая переименования переменных
betaRecuct :: Varible -> Term -> Term -> Term
betaRecuct var what term = case term of
    Var v    -> if v == var then what else term
    Abs v t  -> if v == var then term else Abs v (betaRecuct var (subst v (Var (newname (free what) v)) what) t)
    App t t' -> App (betaRecuct var what t) (betaRecuct var what t')

-- Нормализация нормальным порядком терма term
normal' :: Term -> Term
normal' term = case term of
    Var v            -> term
    App (Abs v t) t' -> normal' (betaRecuct v t' t)
    Abs v t          -> Abs v (normal' t)
    App t t'         -> App (normal' t) (normal' t')

-- Нормализация аппликативным порядком терма term
applicative' :: Term -> Term
applicative' term = case term of
    Var v            -> term
    App (Abs v t) t' -> applicative' (betaRecuct v (applicative' t') (applicative' t))
    Abs v t          -> term
    App t t'         -> App (applicative' t) (applicative' t')

-- Маркер конца ресурсов
data TooLoong = TooLoong deriving Show

-- (*) Нормализация нормальным порядком терма term за неболее чем n шагов.
-- Результат: Или числа итераций недостаточно, чтобы достичь нормальной
-- формы. Или (число нерастраченных итераций, терм в нормальной форме).
normal :: Int -> Term -> Either TooLoong (Int, Term)
normal n term = if n == -1 then Left TooLoong else
    case term of
        Var v            -> Right (n, term)
        App (Abs v t) t' -> normal (n - 1) (betaRecuct v t' t)
        Abs v t          -> let subRes = normal (n - 1) t in
                                case subRes of
                                    Left TooLoong  -> Left TooLoong
                                    Right (n', t') -> Right (n', (Abs v t'))
        App t t'         -> let subRes1 = normal (n - 1) t in
                                case subRes1 of
                                     Left TooLoong     -> Left TooLoong
                                     Right (n', tRes1) -> let subRes2 = normal (n' - 1) t' in
                                                              case subRes2 of
                                                                   Left TooLoong      -> Left TooLoong
                                                                   Right (n'', tRes2) -> Right (n'', (App tRes1 tRes2))

-- (*) Аналогичная нормализация аппликативным порядком.
applicative :: Int -> Term -> Either TooLoong (Int, Term)
applicative n term = if n == -1 then Left TooLoong else
    case term of
        Var v            -> Right (n, term)
        App (Abs v t) t' -> let subRes1 = applicative (n - 1) t' in
                                case subRes1 of
                                     Left TooLoong     -> Left TooLoong
                                     Right (n', tRes1) -> let subRes2 = applicative (n' - 1) t in
                                                              case subRes2 of
                                                                   Left TooLoong      -> Left TooLoong
                                                                   Right (n'', tRes2) -> applicative (n'' - 1) (betaRecuct v tRes1 tRes2)
        Abs v t          -> Right (n, term)
        App t t'         -> let subRes1 = applicative (n - 1) t in
                                case subRes1 of
                                     Left TooLoong     -> Left TooLoong
                                     Right (n', tRes1) -> let subRes2 = applicative (n' - 1) t' in
                                                              case subRes2 of
                                                                   Left TooLoong      -> Left TooLoong
                                                                   Right (n'', tRes2) -> Right (n'', (App tRes1 tRes2))

-- (***) Придумайте и реализуйте обобщённую функцию, выражающую некоторое
-- семейство стратегий редуцирования. В том смысле, что номальная, нормальная
-- головная, нормальная слабо-головная и аппликативная стратегии
-- при помощи этой функции будут выражаться некоторым элементарным образом.
-- Аргумент n можно отбросить, а можно оставить.
--
-- strategy = ?
--
-- normal = strategy ?
-- hnf = strategy ?
-- whnf = strategy ?
-- applicative = strategy ?
--
-- Какие ещё стратегии редуцирования вы знаете? Можно ли их выразить
-- при помощи этой стратегии? Если да, то как?
-- Если нет, то можно ли реализовать аналогичную функцию для _всех_
-- возможных стратегий редуцирования, а не только для такого семейства?
-- Если да, то как? Если нет, то почему?

--------------------------------------------------------

-- Область тестирования

loop' = Abs "x" $ App (Var "x") (Var "x")
loop = App loop' loop'

u = Abs "a" $ Abs "b" $ App (Var "a") $ App (Var "b") (Var "_b")
v = Abs "a" $ Abs "b" $ App (App (Var "a") (Var "b")) (Var "_b")
w = Abs "a" $ Abs "b" $ Abs "c" $ Abs "d" $ App (App (Var "a") (Var "b")) (App (Var "c") (Var "d"))

main = test 100
    [ ("no", normal)
    , ("ap", applicative) ]
    [ Var "a"
    , u
    , v
    , loop'
    , u `App` Var "a"
    , v `App` Var "a"
    , u `App` Var "b"
    , v `App` Var "b"
    , u `App` Var "_b"
    , v `App` Var "_b"
    , (u `App` Var "_b") `App` Var "_b"
    , (v `App` Var "_b") `App` Var "_b"
    , w
    , w `App` (Abs "a" (Var "a") `App` (Abs "b" $ Var "b"))
    , (w `App` Abs "a" (Var "b")) `App` loop
    , loop
    ]

-- Если вы не понимаете как это работает, то пока и не надо
pall n term  = mapM_ (\(desc, reduce) -> putStr (desc ++ ": ") >> print (reduce n term))

test :: Show a => Int -> [(String, Int -> Term -> a)] -> [Term] -> IO ()
test n funcs = mapM_ (\term -> print term >> pall n term funcs)
