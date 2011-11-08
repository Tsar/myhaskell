module OtherPrelude where
import Prelude

-- Склеить два списка за O(length a)
(++) :: [a] -> [a] -> [a]
a ++ b = ?

-- Список без первого элемента
tail :: [a] -> [a]
tail = ?

-- Список без последнего элемента
init :: [a] -> [a]
init = ?

-- Первый элемент
head :: [a] -> a
head = ?

-- Последний элемент
last :: [a] -> a
last = ?

-- n первых элементов списка
take :: Integer -> [a] -> [a]
take = ?

-- Список без n первых элементов
drop :: Integer -> [a] -> [a]
drop = ?

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = ?

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile = ?

-- Разбить список в пару (найбольший префикс удовлетворяющий p, всё остальное)
span :: (a -> Bool) -> [a] -> ([a], [a])
span p = ?

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
break :: (a -> Bool) -> [a] -> ([a], [a])
break = ?

-- n-ый элемент списка (считая с нуля)
(!!) :: [a] -> Integer -> a
[] !! n = error "!!: empty list"
l  !! n = ?

-- Список задом на перёд
reverse :: [a] -> [a]
reverse = ?

-- (*) Все подсписки данного списка
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = ?

-- (*) Все перестановки элементов данного списка
permutations :: [a] -> [[a]]
permutations = ?

-- Повторяет элемент бесконечное число раз
repeat :: a -> [a]
repeat = ?


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
foldl f z l = ?

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl = ?

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
foldr f z l = ?

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr = ?

finiteTimeTest = take 10 $ foldr (:) [] $ repeat 1

-- map f l = из первой лабораторной

-- Склеивает список списков в список
concat :: [[a]] -> [a]
concat = ?

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap = ?

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: [a] -> [b] -> [(a, b)]
zip a b = ?

-- Аналогично, но плющить при помощи функции, а не конструктором (,)
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = ?

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

instange Monoid MulInteger where
    mzero = 1
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

-- Выпишите тип этого выражения. Фигурирует ли в нём Monoid? Почему?
mtfold MLeaf = mzero -- А то, что a - моноид нам будет даровано самой природой
mtfold (MNode a l r) = a `mappend` (mtfold l) `mappend` (mtfold r)

-- Напишите терм с типом
-- (...) => MTree a -> x
-- где x -- тип на ваш вкус, (...) - какие-то констреинты (возможно пустые),
-- при этом этот терм внутри должен использовать то, что a -- моноид, но в
-- констреинтах Monoid a быть не должно.
-- Для широты фантазии в терме можно использовать классы типов, определённые в любом
-- месте этого файла.
mterm = ?

-- (**) Разберитесь чем отличаются эти определения.
-- "Скомпилируйте" их в наш гипотетический язык программирования с
-- типом Dict.
instance MFoldable MTree where
    mfold = mtfold

instance Monoid a => AMFoldable MTree a where
    amfold = mtfold

-- Группа
class Group a where
    gzero :: a
    ginv  :: a -> a
    gmult :: a -> a -> a

class Group Integer where
    gzero = 0
    ginv a = -a
    gmult = (+)

class Group MulInteger where
    ?

-- Реализуйте тип для матриц (через списки) и операции над ними
data Matrix a = ?
-- Чем должно быть a? Моноидом? Группой? Ещё чем-нибудь?

matsum = ?

matscalarmul = ?

matmul = ?

-- (**) Реализуйте классы типов для векторных и скалярных полей.
-- Перепишите в этих терминах что-нибудь из написанного выше.
-- Реализуйте оператор дифференцирования, взятия градиента.
class ? ScalarField ? where
    ?

class ? VectorField ? where
    ?
