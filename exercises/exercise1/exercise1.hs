threedifferent1 ::  Int -> Int -> Int -> Bool
threedifferent1 x y z 
    |x/=y && x /= z && y/=z = True
    |otherwise  = False

threedifferent2 ::  Int -> Int -> Int -> Bool
threedifferent2 x y z = x/=y && x /= z && y/=z

twoequal :: Int -> Int -> Bool
twoequal x y = x==y

threeequal :: Int -> Int -> Int -> Bool
threeequal x y z = twoequal x y && twoequal x z

fourequal :: Int -> Int -> Int ->Int -> Bool
fourequal x y z w = threeequal x y z && threeequal y z w 

smallerroot, largerroot :: Float -> Float -> Float ->Float
smallerroot a b c = -b - sqrt(b^2-4*a*c) / 2*a
largerroot a b c = -b + sqrt(b^2-4*a*c) / 2*a

addition :: Int -> Int -> Int
addition x y = x+y

multiplication :: (Int, Int) -> Int
multiplication (x, y) 
    |x == 1 = y
    |otherwise = y + multiplication(x-1, y) 

vsshelp :: (Int, Int) -> Int
vsshelp (x ,y)
    | y*y > x = y-1
    | otherwise = vsshelp(x, y+1)

verysimplesqrt :: (Int) -> Int
verysimplesqrt (x) = vsshelp(x, 1)