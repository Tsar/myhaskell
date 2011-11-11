{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs, TypeSynonymInstances #-}

module OtherPrelude where
import Prelude( Show(..), Bool(..), Integer(..), Rational(..), Num(..), (+), (-), (*), (/), (<), (==), (>), (<=), (>=), not, (&&), error, ($), (.) )

-- Склеить два списка за O(length a)
(++) :: [a] -> [a] -> [a]
[] ++ b     = b
(a:as) ++ b = a:(as ++ b)

-- Список без первого элемента
tail :: [a] -> [a]
tail (a:as) = as

-- Список без последнего элемента
init :: [a] -> [a]
init (a:[]) = []
init (a:as) = a:(init as)

-- Первый элемент
head :: [a] -> a
head (a:as) = a

-- Последний элемент
last :: [a] -> a
last (a:[]) = a
last (a:as) = last as

-- n первых элементов списка
take :: Integer -> [a] -> [a]
take 0 a      = []
take n (a:as) = a:(take (n - 1) as)

-- Список без n первых элементов
drop :: Integer -> [a] -> [a]
drop 0 a      = a
drop n (a:as) = drop (n - 1) as

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p []     = []
takeWhile p (a:as) = if (p a) then a:(takeWhile p as) else []

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p []     = []
dropWhile p (a:as) = if (p a) then (dropWhile p as) else (a:as)

-- Разбить список в пару (наибольший префикс удовлетворяющий p, всё остальное)
span :: (a -> Bool) -> [a] -> ([a], [a])
span p a = ((takeWhile p a), (dropWhile p a))

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
break :: (a -> Bool) -> [a] -> ([a], [a])
break p []     = ([], [])
break p (a:as) = if (p a) then ((a:subgood), subbad) else ([], a:as) where (subgood, subbad) = break p as

-- n-ый элемент списка (считая с нуля)
(!!) :: [a] -> Integer -> a
[]     !! n = error "!!: empty list"
(a:as) !! 0 = a
(a:as) !! n = as !! (n - 1)

-- Список задом на перёд
reverse :: [a] -> [a]
reverse []     = []
reverse (a:as) = (reverse as) ++ (a:[])

-- Добавить элемент ко всем спискам в списке (вспомогательная функция)
appendElementToAllLists :: a -> [[a]] -> [[a]]
appendElementToAllLists x []     = []
appendElementToAllLists x (l:ls) = (x:l):(appendElementToAllLists x ls)

-- (*) Все подсписки данного списка
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = sub_xs ++ (appendElementToAllLists x sub_xs) where sub_xs = subsequences xs

-- Вставка в список на определенную позицию (вспомогательная функция)
insertIntoListAtPos :: a -> Integer -> [a] -> [a]
insertIntoListAtPos a n l = (take n l) ++ (a:[]) ++ (drop n l)

-- Вставка во все списки на определенную позицию (вспомогательная функция)
insertIntoAllListsAtPos :: a -> Integer -> [[a]] -> [[a]]
insertIntoAllListsAtPos a n []     = []
insertIntoAllListsAtPos a n (l:ls) = (insertIntoListAtPos a n l):(insertIntoAllListsAtPos a n ls)

-- Вставка во все списки на все позиции <= n (вспомогательная функция)
insertIntoAllListsAtAllPoses :: a -> Integer -> [[a]] -> [[a]]
insertIntoAllListsAtAllPoses a 0 l = insertIntoAllListsAtPos a 0 l
insertIntoAllListsAtAllPoses a n l = (insertIntoAllListsAtPos a n l) ++ (insertIntoAllListsAtAllPoses a (n - 1) l)

-- Длина списка (вспомогательная функция)
lengthOfList :: [a] -> Integer
lengthOfList []     = 0
lengthOfList (a:as) = 1 + (lengthOfList as)

-- (*) Все перестановки элементов данного списка
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = insertIntoAllListsAtAllPoses x (lengthOfList xs) (permutations xs)

-- Повторяет элемент бесконечное число раз
repeat :: a -> [a]
repeat a = a:(repeat a)


-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z (a:[]) = f z a
foldl f z l      = f (foldl f z (init l)) (last l)

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f z [] = []
scanl f z l  = (scanl f z (init l)) ++ ((foldl f z l):[])

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z (a:[]) = f a z
foldr f z (a:as) = f a (foldr f z as)

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f z []     = []
scanr f z (a:as) = (foldr f z (a:as)):(scanr f z as)

finiteTimeTest = take 10 $ foldr (:) [] $ repeat 1

-- map f l = из первой лабораторной
map :: (a -> b) -> [a] -> [b]
map f []    = []
map f (a:l) = (f a):(map f l)

-- Склеивает список списков в список
concat :: [[a]] -> [a]
concat []     = []
concat (a:as) = a ++ concat as

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f []     = []
concatMap f (a:as) = (f a) ++ (concatMap f as)

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: [a] -> [b] -> [(a, b)]
zip [] b          = []
zip a []          = []
zip (a:as) (b:bs) = (a, b):(zip as bs)

-- Аналогично, но плющить при помощи функции, а не конструктором (,)
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] b          = []
zipWith f a []          = []
zipWith f (a:as) (b:bs) = (f a b):(zipWith f as bs)

-- Интересные классы типов
class Monoid a where
    mzero :: a
    mappend :: a -> a -> a

instance Monoid [a] where
    mzero = []
    mappend = (++)

instance Monoid Integer where
    mzero = 0
    mappend = (+)

data MulInteger = Mult Integer
data MulRational = RMult Rational

-- Реализуйте инстансы Monoid для Rational и MulRational
instance Monoid Rational where
    mzero = 0
    mappend = (+)

instance Monoid MulRational where
    mzero = RMult 1
    (RMult a) `mappend` (RMult b) = RMult $ a * b

instance Monoid MulInteger where
    mzero = Mult 1
    (Mult a) `mappend` (Mult b) = Mult $ a * b

-- Фолдабл
class MFoldable t where
    mfold :: Monoid a => t a -> a

-- Альтернативный фолдабл
class Monoid a => AMFoldable t a where
    amfold :: t a -> a
-- Изучите раздницу между этими двумя определениями.

-- Смотрите какой чит. Можно построить дерево только из элементов моноида.
data MTree a = Monoid a => MLeaf | MNode a (MTree a) (MTree a)

-- Выпишите тип этого выражения.
-- Окай, выписал.
-- Фигурирует ли в нём Monoid?
-- Да.
-- Почему?
-- Предположительно, потому что если мы будем юзать MTree Type, у нас может
-- не оказаться инстанса Monoid Type и это проблема.
mtfold :: Monoid a => MTree a -> a
mtfold MLeaf = mzero -- А то, что a - моноид нам будет даровано самой природой
mtfold (MNode a l r) = a `mappend` (mtfold l) `mappend` (mtfold r)

-- Напишите терм с типом
-- (...) => MTree a -> x
-- где x -- тип на ваш вкус, (...) - какие-то констреинты (возможно пустые),
-- при этом этот терм внутри должен использовать то, что a -- моноид, но в
-- констреинтах Monoid a быть не должно.
-- Для широты фантазии в терме можно использовать классы типов, определённые в любом
-- месте этого файла.
--mterm = ?

-- (**) Разберитесь чем отличаются эти определения.
-- "Скомпилируйте" их в наш гипотетический язык программирования с
-- типом Dict.
instance MFoldable MTree where
    mfold = mtfold

instance Monoid a => AMFoldable MTree a where
    amfold = mtfold

--------- Тут переделаем немного
-- Группа
--class Group a where
--    gzero :: a
--    ginv  :: a -> a
--    gmult :: a -> a -> a
--
--class Group Integer where
--    gzero = 0
--    ginv a = -a
--    gmult = (+)
--
--class Group MulInteger where
--    ? это я погорячился, да

-- Хаскель слабоват для нормального определения всех этих штук.
-- Кольцо вообще непонятно как определить, потому что группы и полугруппы
-- должны быть по паре (тип, операция).
class Monoid a => Group a where
    ginv :: a -> a

-- Определите
--instance Group для Integer, Rational, MulRational
instance Group Integer where
    ginv = (0 -)

instance Group Rational where
    ginv = (0 -)

instance Group MulRational where
    ginv (RMult a) = RMult (1 / a)

-- Группу и Абелеву группу в Хаскеле тоже не различить :(
class Group a => Ring a where
    -- mappend из моноида это сложение
    rmul :: a -> a -> a -- а это умножение

-- Определите
--instance Ring для Integer, Rational
instance Ring Integer where
    rmul = (*)

instance Ring Rational where
    rmul = (*)

-- На самом деле коммутативное кольцо, но что поделать
class Ring a => Field a where
    rinv :: a -> a

-- Определите
--instance Field для Rational
instance Field Rational where
    rinv = (1 /)

-- Реализуйте тип для матриц (через списки) и операции над ними
data Matrix a = Ring a => Matrix [[a]]
-- Чем должно быть a? Моноидом? Группой? Ещё чем-нибудь?
-- Можно кольцом: "Определение. Матрица — математический объект, записываемый в виде прямоугольной таблицы элементов кольца или поля <...>"

-- Вспомогательная функция (чтобы отображать матрицу вот в таком виде: [[1, 2], [3, 4]])
getAsListOfLists :: Matrix a -> [[a]]
getAsListOfLists (Matrix x) = x

-- Разбивает список на списки заданного размера и возвращает список этих списков (вспомогательная функция)
anticoncat :: Integer -> [a] -> [[a]]
anticoncat n [] = []
anticoncat n l  = (take n l):(anticoncat n (drop n l))

matsum :: Matrix a -> Matrix a -> Matrix a
matsum (Matrix x) (Matrix y) = Matrix (anticoncat (lengthOfList x) (zipWith mappend (concat x) (concat y)))

matscalarmul :: Matrix a -> Matrix a -> Matrix a
matscalarmul (Matrix x) (Matrix y) = Matrix (anticoncat (lengthOfList x) (zipWith rmul (concat x) (concat y)))

getRow :: Integer -> Matrix a -> [a]
getRow n (Matrix a) = a !! n

getColumn :: Integer -> Matrix a -> [a]
getColumn n (Matrix a) = map (!! n) a

sumList :: Ring a => [a] -> a
sumList []     = mzero
sumList (a:al) = a `mappend` (sumList al)

getMulRowCol :: Ring a => [a] -> [a] -> a
getMulRowCol r c = sumList (zipWith rmul r c)

matmulGetRow' :: Ring a => Matrix a -> Matrix a -> Integer -> Integer -> [a]
matmulGetRow' x y n (-1) = []
matmulGetRow' x y n m    = (matmulGetRow' x y n (m - 1)) ++ ((getMulRowCol (getRow n x) (getColumn m y)):[])

matmulGetRow :: Matrix a -> Matrix a -> Integer -> [a]
matmulGetRow x (Matrix (y:ys)) n = matmulGetRow' x (Matrix (y:ys)) n ((lengthOfList y) - 1)

matmul' :: Ring a => Matrix a -> Matrix a -> Integer -> [[a]]
matmul' x y (-1) = []
matmul' x y n    = (matmul' x y (n - 1)) ++ ((matmulGetRow x y n):[])

matmul :: Matrix a -> Matrix a -> Matrix a
matmul (Matrix x) y = Matrix (matmul' (Matrix x) y ((lengthOfList x) - 1))

-- (**) Реализуйте классы типов для векторных и скалярных полей.
-- Перепишите в этих терминах что-нибудь из написанного выше.
-- Реализуйте оператор дифференцирования, взятия градиента.
--class ? ScalarField ? where
--    ?

--class ? VectorField ? where
--    ?
