type Cell = (Int, Int) 
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)




delete :: Cell -> [Cell] -> [Cell]

delete x [] = []
delete x (h:t) | x == h = t 
			   | otherwise = (h : delete x t)


removeNulls :: [MyState] -> [MyState]

removeNulls [] = []
removeNulls (h:t) | h == Null = removeNulls t
				 | otherwise = (h: removeNulls t)




up :: MyState -> MyState

up (S (x, y) mines action prevState) | x == 0 = Null
					 				 | otherwise = (S ((x - 1), y) mines "up" (S (x, y)  mines action prevState))


down :: MyState -> MyState

down (S (x, y) mines action prevState) | x == 3 = Null
					   				   | otherwise = (S ((x + 1), y) mines "down" (S (x, y)  mines action prevState))



left :: MyState -> MyState

left (S (x, y) mines action prevState) | y == 0 = Null
					  				   | otherwise = (S (x, (y - 1)) mines "left" (S (x, y)  mines action prevState))


right :: MyState -> MyState

right (S (x, y) mines action prevState) | y == 3 = Null
					   					| otherwise = (S (x, (y + 1)) mines "right" (S (x, y)  mines action prevState))


collect :: MyState -> MyState

collect (S (x, y) mines action prevState) | not (elem (x, y) mines) = Null
										  | otherwise = (S (x, y) (delete (x, y) mines) "collect" (S (x, y) mines action prevState))


nextMyStates :: MyState -> [MyState]

nextMyStates state = removeNulls [up state, down state, left state, right state, collect state]


isGoal :: MyState -> Bool

isGoal (S (x, y) mines action prevState) = length mines == 0


search :: [MyState] -> MyState

search (h:t) | isGoal h = h
			 | otherwise = search (t ++ nextMyStates h)


constructSolution :: MyState -> [String]

constructSolution (S (x, y) mines action Null) = []
constructSolution (S (x, y) mines action prevState) = constructSolution prevState ++ [action]


solve :: Cell -> [Cell] -> [String]

solve cell listCells = constructSolution (search [(S cell listCells "" Null)] )

