type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState  deriving (Show,Eq)

up :: MyState -> MyState 
up (S (0, y) list string ms) = Null
up (S (x, y) list string ms) = (S (x - 1, y) list "up" (S (x, y) list string ms))

down :: MyState -> MyState 
down (S (3, y) list string ms) = Null
down (S (x, y) list string ms) = (S (x + 1, y) list "down" (S (x, y) list string ms))


left :: MyState -> MyState
left (S(x,0)list string ms)=Null
left (S(x,y)list string ms) = (S(x,y-1)list "left" (S (x,y)list string ms))

right :: MyState -> MyState
right (S(x,3)list string ms)=Null
right (S(x,y)list string ms) = (S(x,y+1)list "right" (S (x,y)list string ms))

collectHelper2 :: Cell -> Cell -> Bool
collectHelper2 (w, x) (y, z) = if w == y then if x == z then True else False else False

collectHelper :: Cell -> [Cell] -> Bool
collectHelper x [] = False
collectHelper x (h:t) = if collectHelper2 x h then True else (collectHelper x t)

removeCellFromList :: Cell -> [Cell] -> [Cell]
removeCellFromList x [] = []
removeCellFromList x (h:t) = if collectHelper2 x h then t else [h] ++ removeCellFromList x t

collect :: MyState -> MyState
collect (S cell list string ms) = if not (collectHelper cell list) then Null
                    else (S cell (removeCellFromList cell list) "collect" (S cell list string ms))


filtertheNulls :: [MyState] -> [MyState]
filtertheNulls [] = []
filtertheNulls (h : t) = if h == Null then filtertheNulls t else [h] ++ filtertheNulls t

nextMyStates::MyState->[MyState]
nextMyStates (S(x,y)list string ms) =filtertheNulls[up (S(x,y)list string ms),down (S(x,y)list string ms),left (S(x,y)list string ms),right (S(x,y)list string ms),collect(S(x,y)list string ms)]

isGoal::MyState->Bool
isGoal (S(x,y)list string ms) = if length list==0 then True
else False

search :: [MyState] -> MyState
search (h:t) = if (isGoal h) == True then h 
else search (t ++ (nextMyStates h))

reverse_list :: [String] -> [String]
reverse_list = \list ->
    case list of
        [] -> []
        x:xs -> reverse_list xs ++ [x]
		
solutionhelper :: [String] -> [String]
solutionhelper (h:t) = if h == "" then [""]
else [h] ++ solutionhelper t

solutionhelper2 :: [String] -> [String]
solutionhelper2 (h:t) = if h== "" then solutionhelper2 t
else [h] ++ solutionhelper2 t

filternull :: [String] ->[String]
filternull []=[]
filternull (h : t) = if h == "" then filternull t 
else [h] ++ filternull t

constructSolution:: MyState -> [String]
constructSolution Null = []
constructSolution (S(x,y)list string ms) = filternull(constructSolution ms ++[string])


solvehelper :: Cell -> [Cell] -> MyState
solvehelper (x,y)list = (S(x,y)list "" Null)

solvehelper2 :: MyState -> MyState
solvehelper2 (S(x,y)list string ms) = search[(solvehelper(x,y)list)]	

solvehelper3  :: MyState -> [String]
solvehelper3 Null = [""]
solvehelper3 (S(x,y)list string ms) = constructSolution(solvehelper2 (S(x,y)list string ms))

solve :: Cell -> [Cell] -> [String]
solve (x,y) [] = solvehelper3 (S(x,y)[] "" Null)
solve (x,y)list =  solvehelper3 (S(x,y)list "" Null)