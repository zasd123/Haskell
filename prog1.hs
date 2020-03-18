{-
###############################################################################################################################
  prog1.hs
  Zachary Hansen, February 1st, 2019
#############################################################################################################################
-}


{-
Function returns true if the number is non-negative.
-}
isNegative :: Float -> Bool
isNegative x 
  |x < 0 = True
  |x > 0 = False
  |x == 0 = error "Zero is neither positive nor negative" 
  |otherwise = error "Invalid input"


{-
Function returns True if both if x / y results in a whole number. 
-}
hasRemainder :: Integer -> Integer -> Bool
hasRemainder x y  	
  |mod x y > 0  = True
  |mod x y == 0 = False
  |otherwise = error "Something went wrong"


{-
Function returns the middle of inputs  x, y, and z. 
-}
middle :: Integer -> Integer -> Integer -> Integer
middle x y z
  |z == x = x
  |x == y = y
  |y == z = z
  |z > x && x > y = x
  |x > z && z > y = z
  |y > z && z > x = z
  |z > y && y > x = y
  |x > y && y > z = y
  |y > x && x > z = x
  |otherwise = error "Something went wrong"


{-
Function returns true when x and y are both true , or both false.
-}		
nor :: Bool -> Bool -> Bool
nor x y
  |x == True && y == True = True
  |x == False && y == False = True
  |otherwise  = False


{-
Function returns the result of (1/2) * x * y
-}
triangleArea :: Integer -> Integer -> Float
triangleArea x y
  |x > 0 && y > 0 = (fromIntegral((x)*(y))*(1/2 :: Float))
  |otherwise = error "Something went wrong"


{-
Function returns the result of (x*3) only if  x is less than or equal to 100.
-}    
tripleNumber :: Integer -> Integer
tripleNumber x
  |x <= 100 = (x * 3)
  |otherwise = error "Number is less than 100"


{-
Function returns true when the parameter x contains a vowel. 
-}
isVowel :: Char -> Bool
isVowel x 
  | x == 'a' || x =='e' || x == 'i' || x == 'o' || x == 'u'  = True
  |otherwise = False


{-
Function returns the grade string associated with the value of input x.  
-}
letterGrade :: Integer -> String
letterGrade x
  | x > 93  = "A "
  | x <= 93 && x > 90 = "A-"
  | x <= 90 && x > 87 = "B+"
  | x <= 87 && x > 83 = "B"
  | x <= 83 && x > 80 = "B-"  
  | x <= 80 && x > 77 = "C+"
  | x <= 77 && x > 73 = "C"
  | x <= 73 && x > 70 = "C-"
  | x <= 70 && x > 67 = "D+"
  | x <= 67 && x > 63 = "D"
  | x <= 63 && x > 60 = "D- "
  | x < 60 = "F "
  | otherwise =  error "You definitley failed"


{-
Function returns the average of the inputs x, y, and z.
-}
averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z
  | x > 0 && y > 0 && z > 0 = (((fromIntegral x) + (fromIntegral  y) + (fromIntegral z))/(3 :: Float)) 
  |otherwise = error "Invalid Input"


{-
Function calculates the average of three inputs and returns the quatity of inputs that are below the average.  
-}
howManyBelowAverage :: Integer -> Integer -> Integer -> Integer
howManyBelowAverage x y z
  |fromIntegral x < averageThree x y z && fromIntegral y < averageThree x y z   = 2
  |fromIntegral x < averageThree x y z && fromIntegral z < averageThree x y z   = 2 	 
  |fromIntegral z < averageThree x y z && fromIntegral y < averageThree x y z   = 2  
  |fromIntegral x < averageThree x y z  = 1
  |fromIntegral y < averageThree x y z  = 1
  |fromIntegral z < averageThree x y z  = 1

