-- Явно выпишем всё, что нам понадобится.
-- Мы об этих штуках ещё не говорили, потому
-- просто игнорируйте эти строчки.
-- В них мы из стандартной библиотеки хапаем
-- себе стандартные типы и функции. Целые числа
-- и операции их сравнения, например.
import Prelude ( Show(..)
               , Bool(..), Int(..), Double(..)
               , (+), (-), (*), (/), mod
               , (<), (==), (>), (<=), (>=))

---------------------------------------------
-- Синтаксис

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x + y
example2' x   = \y -> x + y
example2''    = \x -> \y -> x + y
example2'''   = \x y -> x + y
example2''''  = let z = \x y -> x + y in z
example2''''' = z where
    z x = \y -> x + y

-------------------------------------------
-- Примеры

otherwise = True

abs x | x < 0 = minusx
      | x == 0 = 0
      | otherwise = x
      where minusx = -x

-- Эквивалентны
gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b y
    where y = a `mod` b

gcd' a b = if b == 0
    then a
    else gcd' b (a `mod` b)
-- Обратите внимание, что gcd хвосторекурсивный.

-------------------------------------------
-- Операции над функциями

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   = gcd 121 (gcd 44 11)
example3'  = gcd 121 $ gcd 44 11
example3'' = ($) (gcd 121) (gcd 44 11)

infixr 9 .
(.) f g = \x -> f (g x)

example4 x = (gcd 121 (gcd 44 x))
example4'  = gcd 121 . gcd 44

-------------------------------------------
-- Неподвижные точки
eps = 0.0001

infix 1 ~=
a ~= b = abs (a - b) < eps

average x y = (x + y) / 2
averageDamp f = \ x -> average x (f x)

fixedPoint f g = if g ~= f g
    then g
    else fixedPoint f (f g)

sqrt' x = fixedPoint (\y -> x/y) 1 -- Обычно не сходится.
sqrt x = fixedPoint (averageDamp (\y -> x/y)) 1 -- А этот работает.

-- Взятие производной: нам дали функцию, а мы вернули тоже взяли да
-- и вернули функцию.
deriv :: (Double -> Double) -> (Double -> Double)
deriv f x = (f (x + eps) - f x) / eps

-- Метод Ньютона ищет ноль функции.
newton f x = x - f x / deriv f x

-- Результаты первых двух будут немного отличаться,
sqrt'' x = fixedPoint (newton (\y -> y*y - x)) 1
sqrt''' x = fixedPoint (averageDamp $ newton (\y -> y*y - x)) 1
-- а эта и предыдущая одинаковые.
sqrt'''' x = fixedPoint (averageDamp . newton $ \y -> y*y - x) 1

-------------------------------------------
-- Классические рекурсивные определения

-- Обратите внимание, что вычисление этой
-- функции сначала <<расширяется>>, а потом
-- <<скукоживается>>:
-- fac 10 = 10 * fac 9
--        = 10 * 9 * fac 8
--        ...
--        = 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1
--        = 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2
--        = 10 * 9 * 8 * 7 * 6 * 5 * 4 * 6
--        = 10 * 9 * 8 * 7 * 6 * 5 * 24
--        ...
--        = 3628800
fac 0 = 1
fac n = n * fac (n - 1)

-- а у этой версии ничего не разрастается:
-- fac' 10 = fac'' 10 1
--         = fac'' 9 10
--         = fac'' 9 90
--         ...
--         = fac'' 1 3628800
--         = fac'' 0 3628800
--         = 3628800
fac' n = fac'' n 1 where
    fac'' 0 x = x
    fac'' n a = fac'' (n - 1) (n*a)

-- Мне лень рисовать разрастание такого дерева.
-- Представьте сами.
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- И для этой представьте.
fib' n = fib'' n 1 1 where
    fib'' 0 p pp = pp
    fib'' n p pp = fib'' (n - 1) (pp + p) p

-------------------------------------------
-- Избавляемся от встроенных типов.
-- Мы бы начали с булевых, но мы их уже использовали (оператор if).

-- Однако самостоятельно определить себе булев тип очень просто:
data Bool' = True' | False'
    deriving Show

-- После этого, например, можно определить свой оператор
-- if:
if' True  a b = a
if' False a b = b

-------------------------------------------
-- Избавляемся от встроенных типов.
-- Начинаем с натуральных чисел (в математике -- жирная N)

data Nat = Zero | Succ Nat
    deriving Show          -- (1)
--instance Show Nat where  -- (2)
--    show = show . toInt  -- (3)
-- Чёрная магия: если закомментировать строку 1 и раскомментировать 2 и 3,
-- то 
-- > Zero
-- интерпретатор напечатает как 0,
-- а
-- > Succ (Succ Zero)
-- интерпретатор напечатает как 2.

fromInt 0 = Zero                   -- Этот конструктор (Zero) -- просто значение
                                   -- типа Nat.
fromInt a = Succ $ fromInt (a - 1) -- А этот конструктор сильно напоминает функцию.

toInt Zero = 0
toInt (Succ a) = 1 + toInt a

-- Сложение (эквивалентные определения)
plus :: Nat -> Nat -> Nat
plus Zero b     = b
plus (Succ a) b = plus a (Succ b)

plus' a b = case a of
    Zero -> b
    (Succ a) -> plus' a (Succ b)

-- Другое сложение (ведёт себя иначе, но
-- результат такой же)
plus'' Zero b     = Zero
plus'' (Succ a) b = Succ $ plus'' a b

-- Реализуйте:
-- * умножение (через сложение)
mul :: Nat -> Nat -> Nat
mul Zero b     = Zero
mul (Succ a) b = (a `mul` b) `plus` b

-- * вычитание (без использования предыдущих)
-- При этом, sub a b | (a - b), если a >= b
--                   | 0        иначе

sub :: Nat -> Nat -> Nat
sub Zero b            = Zero
sub a Zero            = a
sub (Succ a) (Succ b) = a `sub` b

greater :: Nat -> Nat -> Nat
greater Zero b = Zero
greater a Zero = Succ Zero
greater a b    = (a `sub` b) `greater` Zero

-- * деление (через вычитание, остаток можно выкинуть)
div :: Nat -> Nat -> Nat
div Zero b = Zero
div a b    = ((a `sub` b) `div` b) `plus` ((Succ a) `greater` b)

-------------------------------------------
-- Избавляемся от встроенных типов.
-- Продолжаем целыми числами (в математике -- жирная Z)

-- Придумайте тип для целых чисел, обладающий
-- тем свойством, что каждое целое число имеет в нём
-- уникальное представление.

data Nat' = One | Next Nat'
instance Show Nat' where show = show . toIntN

toIntN :: Nat' -> Int
toIntN One      = 1
toIntN (Next a) = 1 + toIntN a

fromIntN :: Int -> Nat'
fromIntN 1 = One
fromIntN a = Next (fromIntN (a - 1))

plusN One b     = Next b
plusN (Next a) b = plusN a (Next b)

data Integer = Zer | Pos Nat' | Neg Nat'
instance Show Integer where show = show . toIntI

toIntI :: Integer -> Int
toIntI Zer     = 0
toIntI (Pos a) = toIntN a
toIntI (Neg a) = -(toIntN a)

fromIntI :: Int -> Integer
fromIntI a | a == 0 = Zer
           | a > 0  = Pos (fromIntN a)
           | a < 0  = Neg (fromIntN (-a))

zunaryminus :: Integer -> Integer
zunaryminus Zer = Zer
zunaryminus (Pos a) = Neg a
zunaryminus (Neg a) = Pos a

-- Реализуйте:
-- * сложение
zplus :: Integer -> Integer -> Integer
zplus Zer b = b
zplus (Pos a) (Pos b) = Pos (plusN a b)
zplus (Neg a) (Neg b) = Neg (plusN a b)
zplus (Pos One) (Neg One) = Zer
zplus (Pos One) (Neg (Next b)) = Neg b
zplus (Pos (Next a)) (Neg b) = zplus (zplus (Pos a) (Neg b)) (Pos One)
zplus a b = zplus b a

-- * умножение
zmul :: Integer -> Integer -> Integer
zmul Zer b = Zer
zmul (Pos One) b = b
zmul (Pos (Next a)) b = zplus (zmul (Pos a) b) b
zmul (Neg a) b = zunaryminus (zmul (Pos a) b)

-- * вычитание
zsub :: Integer -> Integer -> Integer
zsub a b = zplus a (zunaryminus b)

-- Волков И.Р.: здесь я уже нагло заюзал булевый тип (ну Вы ведь верите, что я мог без него, как в div для Nat)
zgreaterthanzeroorequal (Pos a) = True
zgreaterthanzeroorequal Zer = True
zgreaterthanzeroorequal _ = False

-- * деление
zdiv :: Integer -> Integer -> Integer
zdiv Zer b = Zer
zdiv (Neg a) (Neg b) = zdiv (Pos a) (Pos b)
zdiv (Pos a) (Neg b) = zdiv (Neg a) (Pos b)
zdiv (Neg a) (Pos b) = zunaryminus (zdiv (Pos a) (Pos b))
zdiv a b = if (zgreaterthanzeroorequal (zsub a b) == True)
              then zplus (Pos One) (zdiv (zsub a b) b)
              else Zer

-------------------------------------------
-- Избавляемся от встроенных типов.
-- Продолжаем рациональными числами

-- Придумайте тип для рациональных чисел.
-- Уникальность представления каждого числа не обязательна.

-- Волков И.Р.: Рациональные представлю как p/q, где p и q принадлежат Z
data Rational = Constr Integer Integer
    deriving Show

-- Реализуйте:
-- * сложение
rplus :: Rational -> Rational -> Rational
rplus (Constr a b) (Constr c d) = Constr (zplus (zmul a d) (zmul b c)) (zmul b d)

-- * умножение
rmul :: Rational -> Rational -> Rational
rmul (Constr a b) (Constr c d) = Constr (zmul a c) (zmul b d)

-- * вычитание
rsub :: Rational -> Rational -> Rational
rsub (Constr a b) (Constr c d) = Constr (zsub (zmul a d) (zmul b c)) (zmul b d)

-- * деление
rdiv :: Rational -> Rational -> Rational
rdiv (Constr a b) (Constr c d) = Constr (zmul a d) (zmul b c)

-- Волков И.Р.: Теперь можно делать так: rplus (Constr (fromIntI 1) (fromIntI 2)) (Constr (fromIntI 1) (fromIntI 3)) и т.д.

-------------------------------------------
-- Конструируем типы.
-- Пары

-- Предположим, мы хотим уметь определять пары
-- элементов различных типов.
-- Мы могли бы делать это так:
data PairIntInt = PairII Int Int
data PairIntDouble = PairID Int Double
-- но это как-то грустно.

-- Магия Haskell позволяет нам сделать что-то типа
-- data Pair = \a b -> Pair#a#b a b
-- где #a и #b -- значения типовых переменных a и b,
-- или, даже лучше, просто
-- data Pair = \a b -> Pair a b
-- записывают это так:
data Pair a b = Pair a b
    deriving Show

-- Теперь
-- Pair Int Int
-- и
-- Pair Int Double
-- это разные типы, которые как бы генерируются компилятором.
--
-- Почти ничем не отличается от обычных функций (\x y -> y), функция Pair
-- имеет своим результатом тип с одним конструктором, который принимает два
-- аргумента соответствующих типов и возвращает упакованную из них пару.
-- Очень похоже на шаблонные параметры в C++.

-- Теперь мы можем писать функции, возвращающие несколько значений,
-- упакованных в пару.
example5 x = Pair x (Succ Zero)
-- Заметим, что это эквивалентно двум функциям:
example5'1 x = x
example5'2 x = Succ Zero

-- Следите за руками:
example6 x = let (Pair a b) = example5 x in a + (toInt b)
example6' x = let a = example5'1 x
                  b = example5'2 x in a + (toInt b)

-- Смотрите, я раньше сказал, что конструктор -- это почти функция.
-- Так вот он <<почти>> как раз потому, что его можно распаковывать
-- обратно.

-- Заметим также, что:
-- функция с типом
-- Pair a b -> c
-- имеет эквивалентного соседа с типом
-- a -> b -> c

example7 (Pair a b) = example7' a b
example7' a b = b

-- Это преобразование можно автоматизировать:
-- (напоминаю, что стрелка в типах правоассоциативна!)
curry :: (Pair a b -> c) -> a -> b -> c
curry f a b = f (Pair a b)

-- Реализуйте обратную:
uncurry :: (a -> b -> c) -> Pair a b -> c
uncurry f (Pair a b) = f a b

-- Просто какие-то примеры.
example8 (Pair Zero (Succ _)) = Succ Zero
example8 (Pair Zero Zero) = Zero
example8 (Pair (Succ a) _) = a

example9 (Pair (Pair a b) c) = Pair a (Pair b c)

-- Реализуйте функцию pmap с типом
pmap :: (a -> a') -> (b -> b') -> Pair a b -> Pair a' b'
pmap f g (Pair a b) = Pair (f a) (g b)
-- делающую что-то разумное.

-------------------------------------------
-- Конструируем типы.
-- Списки, деревья

data List a = Cons a (List a) -- Элемент и хвост
            | Nil             -- Конец списка
    deriving Show

example10 = Cons (Succ Zero) $ Cons Zero $ Nil

length Nil = 0
length (Cons _ b) = 1 + length b

-- Реализуйте функцию map с типом
map :: (a -> b) -> List a -> List b
map f Nil        = Nil
map f (Cons a l) = Cons (f a) (map f l)
-- делающую что-то разумное и такую, что length l == length (map f l)

data Tree a = Node a (Tree a) (Tree a) -- Узел
            | Leaf                     -- Лист
    deriving Show

max a b = if a >= b then a else b

height Leaf = 0
height (Node _ a b) = 1 + max (height a) (height b)

-- Реализуйте функцию
tmap :: (a -> b) -> Tree a -> Tree b
tmap f Leaf         = Leaf
tmap f (Node x y z) = Node (f x) (tmap f y) (tmap f z)
-- делающую что-то разумное и такую, что height t == height (tmap f t)

-- Реализуйте функцию
list2tree :: List a -> Tree a
list2tree Nil        = Leaf
list2tree (Cons a l) = Node a (list2tree l) Leaf
-- делающую что-то разумное и такую, что length l == height (list2tree l)

-------------------------------------------
-- Конструируем типы.
-- Логическое или

data Maybe a = Just a
             | Nothing
    deriving Show

-- Реализуйте функцию
find :: (a -> Bool) -> List a -> Maybe a
find p Nil = Nothing
find p (Cons a l) = if (p a) == True
    then Just a
    else find p l
-- которая ищет в списке t элемент, удовлетворяющий предикату p (если такой есть).

-- Реализуйте функцию
filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (Cons a l) = if (p a) == True
    then (Cons a (filter p l))
    else filter p l
-- которая генерирует список из элементов t, удовлетворяющих предикату f.

isJust Nothing  = False
isJust (Just _) = True

-- При помощи filter, isJust и map реализуйте разумную функцию с типом
fromjust :: Maybe a -> a
fromjust (Just a) = a

maybefilter :: List (Maybe a) -> List a
maybefilter l = map fromjust (filter isJust l)
--
-- подсказка:
-- map (\(Just a) -> a)
-- Что в этой функции (и подсказке) плохо?
-- Волков И.Р.: Если записать, как я написал fromjust, то вроде бы всё хорошо...

-- Реализуйте разумную функцию
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter f l = maybefilter (map f l)

-- При помощи неё реализуйте maybefilter':
-- maybefilter' :: List (Maybe a) -> List a
-- maybefilter' l = ?
-- не обладающую предыдущим недостатком.
-- Волков И.Р.: Не вижу там недостатка. И к тому же у меня gfilter сделан через maybefilter.

data Either a b = Left a
                | Right b
    deriving Show

-- Пустое множество
data Empty = Empty

-- Реализуйте
maybe2either :: Maybe a -> Either Empty a
maybe2either Nothing = Left Empty
maybe2either (Just a) = Right a
-- Волков И.Р.: Я вообще не понял, какая семантика предполагается у этой функции (но может хоть угадал?)

-- Реализуйте
emap :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
emap f g (Left x) = Left $ f x
emap f g (Right x) = Right $ g x
-- Волков И.Р.: Тут тоже пишу наугад
