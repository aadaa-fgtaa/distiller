main = concatMap two ( zipWith f (map g (filter h input1)) ( filter j input2 ) )

map f l = case l of
  Null        -> Null
  Cons[x, xs] -> Cons[f x, map f xs]

filter f l = case l of
  Null        -> Null
  Cons[x, xs] -> case f x of
    True  -> Cons[x, filter f xs]
    False -> filter f xs

zipWith f l r = case l of
  Null        -> Null
  Cons[x, xs] -> case r of
    Null        -> Null
    Cons[y, ys] -> Cons[f x y, zipWith f xs ys]

append l r = case l of
  Null        -> r
  Cons[x, xs] -> Cons[x, append xs r]

concatMap f l = case l of
  Null        -> Null
  Cons[x, xs] -> append (f x) (concatMap f xs)

two x = Cons[x, Cons[x, Null]]
