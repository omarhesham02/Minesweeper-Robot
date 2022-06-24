type Cell = (Int, Int) 
data MyState = S Cell [Cell] deriving (Show, Eq)


manhattan :: Cell -> Cell -> Int
manhattan (x1,y1) (x2,y2) = abs(x1 - x2) + abs(y1 - y2)

get1stHalf :: [Cell] -> [Cell]
get1stHalf l = take (div (length l) 2) l

get2ndHalf :: [Cell] -> [Cell]
get2ndHalf l = take (div (length l) 2 + (mod (length l) 2)) (reverse l)

merge :: Cell -> [Cell] -> [Cell] -> [Cell]
merge curr [] x = x
merge curr x [] = x
merge curr (h1:t1) (h2:t2)	| (manhattan curr h1) < (manhattan curr h2) = h1 : merge curr t1 (h2:t2)
							| otherwise = h2 : merge curr (h1:t1) t2
						
mergeSort :: Cell -> [Cell] -> [Cell]
mergeSort _ [] = []			
mergeSort _ [x] = [x]
mergeSort curr list = merge curr (mergeSort curr (get1stHalf list)) (mergeSort curr (get2ndHalf list))

sortMines :: Cell -> [Cell] -> [Cell]
sortMines curr mines = mergeSort curr  mines

collect :: MyState -> [String]
collect (S _ []) = []
collect (S curr (h:t)) = (getMyWay curr h) ++ (collect (S h(sortMines h t)))

getMyWay :: Cell -> Cell -> [String]
getMyWay (x1,y1) (x2,y2) | x1 > x2 = "up":getMyWay (x1 - 1, y1) (x2,y2)
						 | x1 < x2 = "down":getMyWay (x1 + 1, y1) (x2,y2)
						 | y1 < y2 = "right":getMyWay (x1, y1 + 1) (x2,y2)
						 | y1 > y2 = "left":getMyWay (x1, y1 - 1) (x2,y2)
						 | otherwise = ["collect"]

solve :: Cell -> [Cell] -> [String]
solve curr mines = collect (S curr (sortMines curr mines))

