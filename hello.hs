myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []                   
myMap f (x:xs) = f x : myMap f xs    

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
    | p x       = x : myFilter p xs
    | otherwise = myFilter p xs


myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc []     = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs


myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc []     = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

mySum [] = 0
mySum (x:xs) = x + mySum xs

myProduct [] = 1
myProduct (x:xs) = x * myProduct xs


myCount _ [] = 0
myCount a (x:xs)  
    |  a == x = 1 +  myCount a xs
    |  otherwise = myCount a xs

myCompose :: (b -> c) -> (a -> b) -> a -> c
myCompose f g x = f (g x)

