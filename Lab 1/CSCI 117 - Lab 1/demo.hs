doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
                    then x
                    else x*2

data Color = Red | Orange | Yellow | Green | Blue | Violet
     deriving (Show, Eq, Ord, Enum)

data Name = Henry | Adam | Yelena | Gabe | Ian | Sam
    deriving (Show, Eq, Ord, Enum)