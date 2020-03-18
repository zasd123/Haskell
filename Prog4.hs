{-
#######################################################################################################
  Prog4.hs
  Zachary Hansen, March 19th,2019
#######################################################################################################
-}

module Prog4 where

import Data.List

--Write a function older that takes two dates and returns whichever one is older.
--(MM/DD/YYYY)
older :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
older (x,y,z) (x1,y1,z1)
  |z1 > z = (x,y,z)
  |z > z1 = (x1,y1,z1)
  |x1 > x = (x,y,z)
  |x > x1 = (x1,y1,z)
  |y1 > y = (x,y,z)
  |y > y1 = (x1,y1,z1) 


--Write a function numInMonth that takes a month and a list of dates and returns how many dates in the list match the given month.
numInMonth :: Int -> [(Int, Int, Int)] -> Int
numInMonth a [] = 0  
numInMonth a ((x,y,z):xs) 
  |a == x = (1+ (numInMonth a xs))
  |a /= x = (0+ (numInMonth a xs))
  |otherwise = error "There was an issue"

  

--Write a function datesInMonth that takes a month and a list of dates and returns a list of dates that match the given month.
datesInMonth :: Int -> [(Int, Int, Int)] -> [(Int,Int,Int)]
datesInMonth a [] = []
datesInMonth a b  = [ (x,y,z) | (x,y,z)<- b, x == a]



--Write a function date2Str that takes a date and returns its string equivalent in the form "February 23, 2018". (Hint: what is the operator for concatenating strings? Hint: look up how to convert an Int to a String.)
date2Str :: (Int, Int, Int) -> String
date2Str (x,y,z)
  |x == 1 = ("January "++ show y ++ ", " ++ show z)
  |x == 1 = ("February "++ show y ++ ", " ++ show z)  
  |x == 3 = ("March "++ show y ++ ", " ++ show z)  
  |x == 4 = ("April "++ show y ++ ", " ++ show z)  
  |x == 5 = ("May "++ show y ++ ", " ++ show z)  
  |x == 6 = ("June "++ show y ++ ", " ++ show z)  
  |x == 7 = ("July "++ show y ++ ", " ++ show z)  
  |x == 8 = ("August "++ show y ++ ", " ++ show z)  
  |x == 9 = ("September "++ show y ++ ", " ++ show z)  
  |x == 10 = ("October "++ show y ++ ", " ++ show z)  
  |x == 11 = ("November "++ show y ++ ", " ++ show z)  
  |x == 12 = ("December "++ show y ++ ", " ++ show z)  


--Same as above, but do not use 12 conditionals. Instead, use a list holding 12 strings (the months) as well as the !! operator to index this list.
date2Str' :: (Int, Int, Int) -> String
date2Str' (x,y,z) = ["January", "February", "March", "April", "May", "June", "July" , "August", "September", "October", "November", "December"] !! (x-1) ++" "++ show y ++ ", " ++ show z 
   

--What a function monthLookup that takes a numeric day in the calendar year (between 1 and 365) and returns what month that day is in (excluding leap years).
monthLookup :: Int -> Int
monthLookup x 
  |x>0 && x <= 31 = 1
  |x>31 && x <= 59 = 2
  |x>59 && x <= 90 = 3
  |x>90 && x <= 120 = 4
  |x>120 && x <= 151 = 5
  |x > 151 && x <= 181 = 6
  |x > 181 && x <= 212 = 7
  |x > 212 && x <= 243 = 8
  |x > 243 && x <= 273 = 9
  |x > 273 && x <= 304 = 10
  |x > 273 && x <= 334 = 11
  |x > 334 && x <= 365 = 12
 
--Write a function monthRange that takes two numeric days (from previous problem) and returns an integer list of the months between those dates (inclusive), e.g.: monthRange 23 101 should return [1,2,3,4]. If the second argument is earlier than the first argument, return the empty list.
monthRange :: Int -> Int -> [Int]
monthRange  x y  
  |x < y = nub(monthLookup x:(monthRange (x+1) y))
  |x == y = [monthLookup y]  


--Write a function validDate that takes a date and returns whether it is valid (e.g. November 31 is not valid). Do not be concerned about leap years.
validDate :: (Int, Int, Int) -> Bool
validDate (x,y,_) 
  |x==1  && y>0  && y<=31  =True
  |x==2  && y>0  && y<=28  =True
  |x==3  && y>0  && y<=31  =True
  |x==4  && y>0  && y<=30  =True
  |x==5  && y>0  && y<=31  =True
  |x==6  && y>0  && y<=30  =True
  |x==7  && y>0  && y<=31  =True
  |x==8  && y>0  && y<=31  =True
  |x==9  && y>0  && y<=30  =True
  |x==10 && y>0  && y<=31  =True
  |x==11 && y>0  && y<=30  =True
  |x==12 && y>0  && y<=31  =True
  |otherwise = False;
 
--Write a function validLeapDate that takes a date and returns whether it is a leap date, that is exactly February 29th on a leap year. (Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100.) 
validLeapDate :: (Int, Int, Int) -> Bool
validLeapDate (x,y,z)
  |x==2 && y==29 && z `mod` 400 == 0 || x==2 && y==29 && z `mod` 4 == 0 && z `mod` 100 /= 0 = True
  |otherwise = False

--Write a function season that takes a date and returns the season that the date is in. 
season :: (Int, Int, Int) -> String
season (x,y,z)
  |x==3 && y >= 20 && y <= 31 = "Spring"
  |x==4 = "Spring" 
  |x==5 = "Spring"
  |x==6 && y <= 21 = "Spring"
  |x==6 && y > 21 = "Summer"
  |x==7 = "Summer"
  |x==8 = "Summer"
  |x==9 && y<=23 ="Summer"
  |x==9 && y > 23 = "Fall"
  |x==10 = "Fall"
  |x==11 = "Fall"
  |x==12 && y <= 21 = "Fall"
  |x==12 && y >21 = "Winter"
  |x==1 = "Winter"
  |x==2 = "Winter"
  |x==3 && y <= 20 = "Winter" 
