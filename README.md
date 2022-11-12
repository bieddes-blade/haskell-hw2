# Домашнее задание №2

Мягкий дедлайн: 25 марта, 23:59 MSK.
Жесткий дедлайн: 28 марта, 23:59 MSK.

Сдавать в https://classroom.github.com/a/EmDnGRXO коммитами в master.

## Оценка

За каждую пронумерованную задачу — 1 балл.

На десятку достаточно решить 25 задач (набрать 25 баллов).

## Требования

Алгоритмы должны быть асимптотически оптимальными по времени.

Нельзя пользоваться частичными функциями (head, tail, NonEmpty.fromList).
<!-- без необходимости. -->

## Тесты

Ко всем функциям строго желательно писать юнит-тесты (кейсы и проперти).

Тесты свойств (property) — это тесты со случайно выбираемыми параметрами.

В качестве кейс-тестов можно брать проперти без параметров.

```hs
-- кейс-тесты де факто, потому что единственный случай тестируется
prop_example1 :: Property
prop_example1 = 2 + 2 === 4

prop_example2 :: Bool
prop_example2 = null emptyTree

-- настоящие проперти — со случайно выбираемыми параметрами
prop_example3 :: NonEmpty A -> Property
prop_example3 xs = head (sort xs) === minimum xs

prop_example4 :: NonEmpty A -> Bool
prop_example4 xs = not $ null xs
```

Тесты для инстансов можно писать на основе законов классов:

```hs
prop_Functor_Identity :: f A -> Property
prop_Functor_Identity x =
  fmap id x === x

prop_Functor_Composition :: Fun B C -> Fun A B -> f A -> Property
prop_Functor_Composition (Fun _ f) (Fun _ g) x =
  fmap (f . g) x === (fmap f . fmap g) x

prop_Applicative_Identity :: f A -> Property
prop_Applicative_Identity v =
  (pure id <*> v) === v

prop_Applicative_Composition :: f (Fun B C) -> f (Fun A B) -> f A -> Property
prop_Applicative_Composition u' v' w =
  (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))
  where
    u = applyFun <$> u'
    v = applyFun <$> v'

prop_Applicative_Homomorphism :: Fun A B -> A -> Property
prop_Applicative_Homomorphism (Fun _ f) x =
  (pure f <*> pure x) === (pure (f x) :: f B)

prop_Applicative_Interchange :: f (Fun A B) -> A -> Property
prop_Applicative_Interchange u' y =
  (u <*> pure y) === (pure ($ y) <*> u)
  where
    u = applyFun <$> u'

prop_Monad_LeftIdentity :: A -> Fun A (m B) -> Property
prop_Monad_LeftIdentity a (Fun _ k) =
  (return a >>= k) === k a

prop_Monad_RightIdentity :: m B -> Property
prop_Monad_RightIdentity m =
  (m >>= return) === m

prop_Monad_Associativity :: f A -> Fun A (f B) -> Fun B (f C) -> Property
prop_Monad_Associativity m (Fun _ k) (Fun _ h) =
  (m >>= (\x -> k x >>= h)) === ((m >>= k) >>= h)

prop_traverse_Identity :: t A -> Property
prop_traverse_Identity x =
  traverse Identity x === Identity x

prop_traverse_Composition :: Fun A (F B) -> Fun B (G C) -> t A -> Property
prop_traverse_Composition (Fun _ f) (Fun _ g) x =
  traverse (Compose . fmap g . f) x
    === (Compose . fmap (traverse g) . traverse f) x

prop_sequenceA_Identity :: t A -> Property
prop_sequenceA_Identity x =
  (sequenceA . fmap Identity) x === Identity x

prop_sequenceA_Composition :: t (F (G A)) -> Property
prop_sequenceA_Composition x =
  (sequenceA . fmap Compose) x === (Compose . fmap sequenceA . sequenceA) x

type F = Maybe

type G = Either String
```

## Functor и его друзья

### 1. Сумма чисел в строке

```hs
stringSum :: String -> Maybe Int
```

Числа в строке разделены одним или несколькими пробельными символами. Если хотя бы один элемент строки нельзя сконвертировать в целое число, то необходиомо вернуть `Nothing`.

Функция должна использовать инстанс Traversable для списка.

### 2. Тесты

Написать несколько простых проперти тестов на _stringSum_.

### 3—7. Инстансы NonEmpty

```hs
data NonEmpty a = a :| [a]
```

Вручную без deriving.

3. Functor
4. Applicative
5. Monad
6. Foldable
7. Traversable

## Монады и монадические вычисления

### 8. Арифметическое выражение

Арифметическое выражение (именно выражение, не результат его вычисления) можно представить рекурсивным алгебраическим типом данных. Реализуйте этот тип данных, чтобы с его помощью можно было задавать следующие операции:

* Целочисленные константы
* Сложение двух выражений
* Вычитание выражений
* Произведение выражений
* Деление выражений
* Возведение в степень выражений

После этого напишите функцию, которая принимает выражение и вычисляет его. Обратите внимание на то, что выражение может не получиться вычислить по разным причинам.

```hs
eval :: Expr -> Either ArithmeticError Int
```

То есть Вы должны создать свой тип данных, который обозначает арифметическую ошибку и возвращать `Either` — либо ошибка, которая возникла, либо результат. Если выражение содержит несколько ошибок, то можно вернуть любую.

Достаточно проверять только на следующие арифметические ошибки:

1. Деление на 0.
2. Возведение в отрицательную степень.

**Подсказка:** если реализовать функцию с `Either` сразу тяжело, то попробуйте `eval :: Expr -> Maybe Int`, после чего замените `Maybe` на `Either String`, а затем `String` можно будет заменить за свой тип данных.

### 9. Simple Moving Average

Реализуйте [Simple Moving Average](https://en.wikipedia.org/wiki/Moving_average) алгоритм, используя State. Надо придумать, как реализовать алгоритм с изменяемым значением, определив, какая часть данных должна изменяться и передаваться между итерациями.

```hs
ghci> moving 4 [1, 5, 3, 8, 7, 9, 6]
[1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]

ghci> moving 2 [1, 5, 3, 8, 7, 9, 6]
[1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
```

## Парсер-комбинаторы

Это блок самый важный в этом домашнем задании. Реализация всех упражнений из этого блока поможет понять, как устроены парсер-комбинаторы, а это важно, потому что они крайне полезны на практике. Перед решением заданий убедитесь, что вы осознали материал семинара и можете прорешать базовые упражнения по следующим ссылкам:

* [Parser Combinators: Basics](http://www.seas.upenn.edu/~cis194/spring13/hw/10-applicative.pdf)
* [Parser Combinators: Implementing simple parser](http://www.seas.upenn.edu/~cis194/spring13/hw/11-applicative2.pdf)

### 10—13

Имеется тип простого парсера:

```hs
newtype Parser s a = Parser { runParser :: [s] -> Either String (a, [s]) }
```

Он может работать не только со строкой (s ~ Char), но и с любым потоком данных.
В Left должно быть человеческое описание ошибки.

Реализуйте вручную инстансы:

10. Functor
11. Applicative
12. Monad
13. Alternative

### 14—17. Базовые парсеры

14. ok — Парсер никогда не падает и не поглощает инпут.
15. eof — Проверяет, что парсер дошёл до конца потока данных (иначе падает).
16. satisfy — Парсер принимает предикат на элемент потока и возвращает элемент, поглащая его из потока, если предикат на элемент равен `True`, иначе падает.
17. element и stream — Парсят один или несколько элементов потока (как `char` и `string`).

### 18—19. Простые парсеры

Используя существующие комбинаторы (реализовав по необходимости остальные), напишите следующие парсеры строковых потоков:

18. Парсер правильных скобочных последовательностей (падает, если последовательность неправильная, и не падает, если правильная).
19. Парсер целого числа, перед которым может быть знак `+` или `-`. Нельзя пользоваться функциями, связанными с классом Read.
