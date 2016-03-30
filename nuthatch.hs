-- Very small subset of Nuthatch DSL
data Walk  = Walk String [Stmt]
data Stmt  = Print [Expr] 
           | Println [Expr] 
           | If Exprb [Stmt] [Stmt] 
           | WalkTo Int
data Expr  = Str String 
           | Boolean Exprb 
           | Value
data Exprb = Eq Expr Expr 
           | Leaf 
           | Down 
           | Up

-- Simple tree data structure
data Tree = Node String [Tree]

-- Walk execution context: (Ctx value isLeaf isDown isUp)
data Ctx = Ctx String Bool Bool Bool

-- ------------------------------------------------------------
-- ---------------------- Main functions ----------------------
-- ------------------------------------------------------------

-- Runs a Walk on a given tree and prints the output
-- directly to the console
runWalk :: Walk -> Tree -> IO ()
-- `base tree` actually returns a list of strings with each string
-- representing a output line: print it with some foldl and map magic
runWalk w tree = putStr (foldr (++) "\n" (base tree))
	where
		-- The "base" function which basically prints the root node
		-- of the tree where we know that we have the join point "down"
		base x = (render x True 0) ++ internal x

		-- This function is really only an alias which unfolds the
		-- arguments for "children" from an ordinary root node of a
		-- subtree
		internal (Node v t) = (children (Node v t) t 0)

		-- Performs the main tree traversal considering also the
		-- "return" values from the "walk to" statements
		children parent t i = 
			-- Take a look at the i-th child (if any)
			if (i < length t) then 
				-- Now: get the "walk to" value from the walk and then
				-- walk to the requested node
				if (test (t !! i) True (i-1)) < 0 then 
					-- If normal walk through is requested
					(render (t !! i) True (i-1)) ++ -- 1.) Render the current node
					(internal (t !! i)) ++          -- 2.) Work on all child nodes
					(render parent False (i+1)) ++  -- 3.) Go back up to the parent node
					children parent t (i+1)         -- 4.) Go to next parent node

				else if (test (t !! i) True (i-1)) == 0 then
					-- The "0" value represents a "walk up" e.g.
					-- skip the subtree of the current node
					children parent t (i+1)         -- 1.) Go to next parent node

				else
					-- Otherwise a value between 1 and #-of-children
					-- is given -> walk directly to this node
					-- 1.) Render the REQUESTED (!) node
					render (t !! (getindex  (t !! i) True (i-1))) True (getindex (t !! i) True (i-1)) ++
					-- 2.) Render all children of the REQUESTED (!) node
					internal (t !! (getindex  (t !! i) True (i-1)))

			else 
				-- If there is no i-th child nothing new can be added
				[]

		-- Function to "render" the current node. This means that the
		-- walk is evaluated in the context of the current node
		render (Node v t) d u = [fst (eval (makectx v t d u) w)]

		-- Function to evaluate the walk for the current context and
		-- consume the result from the "walk to" statement (same as "render")
		test (Node v t) d u = snd (eval (makectx v t d u) w)

		-- Helper function to perform "test" a subtract one (array index
		-- alignment)
		getindex p a b = (test p a b) - 1

		-- Helper function to create a Ctx
		-- makectx :: String -> [Tree] -> Bool -> Int -> Ctx
		makectx v c isDown isUp = (Ctx v (length c == 0) isDown (isUp == (length c) || (length c == 0)))


-- Evaluates a Walk to a tuple consisting of the following 
-- components:
--    * All printed strings from the walk for the current context
--    * The id of the next node to visit according to Nuthatch
--      specification 
eval :: Ctx -> Walk -> (String, Int)
eval c (Walk _ stmts) = retmap (reduce (execs c stmts))
	where
		-- Executes a list of statements on the grounds of a context
		execs :: Ctx -> [Stmt] -> [(String, Maybe Int)]
		execs c [] = []
		execs c (stmt:stmts) = [(evals c stmt)] ++ execs c stmts

		-- Evaluates a single statement to a tuple of all 
		-- outputted strings and a Maybe containing the id
		-- of the next node to visit
		evals :: Ctx -> Stmt -> (String, Maybe Int)
		evals _ (WalkTo i)     = ("", Just i)
		evals c (Print ex1)    = (foldl (++) "" (map (evale c) ex1), Nothing)
		evals c (Println ex1)  = (foldl (++) "" (map (evale c) ex1) ++ "\n", Nothing)
		evals c (If b st1 st2) = if evalb c b
									then reduce (execs c st1)
								    else reduce (execs c st2)

		-- Maps the resulting list of tuples of [(String, Maybe Int)] 
		-- to tuples of type [(String, Int)]
		retmap :: (String, (Maybe Int)) -> (String, Int)
		retmap (a, Nothing)  = (a, -1) -- "-1" is default value for walk next
		retmap (a, (Just b)) = (a, b)

		-- Reduces the list of statement "outputs" to a single tuple
		reduce x = foldl (\(a,_)(c,d) -> (a++c,d)) ("",Nothing) x

		-- Evaluates an expression based on a context
		evale :: Ctx -> Expr -> String
		evale _ (Str s)             = s
		evale (Ctx v _ _ _) (Value) = v
		evale e (Boolean ex1)       = if evalb e ex1 then "True" else "False"

		-- Evaluates a boolean expression based on a context
		evalb :: Ctx -> Exprb -> Bool
		evalb (Ctx _ x _ _) (Leaf) = x
		evalb (Ctx _ _ x _) (Down) = x
		evalb (Ctx _ _ _ x) (Up)   = x
		evalb e (Eq ex1 ex2)       = evale e ex1 == evale e ex2

-- Dumps a walk in a more Nuthatch DSL like manner to the console
pp :: Walk -> IO ()
pp (Walk name stmts) = putStr ("walk " ++ name ++ " " ++ (dumpw stmts))
	where
		-- Dumps the entire walk
		dumpw stmts = "{\n" ++ (apply (dumps "  ") stmts) ++ "}\n"

		-- Dumps the statements
		dumps s (Print ex)     = s ++ "print " ++ (impl (map dumpe ex) " + ") ++ ";"
		dumps s (Println ex)   = s ++ "println " ++ (impl (map dumpe ex) " + ") ++ ";"
		dumps s (WalkTo i)     = s ++ "walk to " ++ (show i) ++ ";"
		dumps s (If b st1 st2) = s ++ "if (" ++ dumpeb b ++ ") {\n" ++
									apply (dumps (s ++ "  ")) st1 ++
								 s ++ "} else {\n" ++
									apply (dumps (s ++ "  ")) st2 ++	
								 s ++ "}"

		-- Dumps all expressions
		dumpe (Str s)       = "\"" ++ s ++ "\""
		dumpe (Boolean exb) = dumpeb exb
		dumpe (Value)       = "value"

		-- Dumps all boolean expressions
		dumpeb (Eq ex1 ex2) = (dumpe ex1) ++ " == " ++ (dumpe ex2)
		dumpeb (Leaf)       = "leaf"
		dumpeb (Down)       = "down"
		dumpeb (Up)         = "up"

		-- Helper to convert a list of elements of type a by a
		-- function to a string and return an imploded string
		apply :: (a -> String) -> [a] -> String
		apply f vs = unlines (map (\x -> (f x)) vs)

		-- Helper function to join an array of strings
		impl :: [String] -> String -> String
		impl [] _ = ""
		impl [x] _ = x
		impl (x:xs) c = x ++ c ++ impl xs c

-- ------------------------------------------------------------
-- ------------------------- Examples -------------------------
-- ------------------------------------------------------------

-- Walks to the leftmost node of the tree
goLeftWalk :: Walk
goLeftWalk = 
	Walk "goLeft" [
		(Println [(Str "Node: "), Value]),
		(WalkTo 1)
	]

-- Traverses through a tree along the "default" walk
-- until a plus sign is reached; then the walk is aborted
-- (go back to parent = walk to 0)
skipPlusWalk :: Walk
skipPlusWalk = 
	Walk "skipPlus" [
		(Println [(Str "Node: "), Value]),
		(If (Eq Value (Str "+")) [WalkTo 0] [])
	]

-- Dumps all join points and their values for a given
-- tree
dumpJoinPointsWalk :: Walk
dumpJoinPointsWalk = 
	Walk "dumpJoinPoints" [
		(Println [
			(Str "v="), Value, 
			(Str "\tisLeaf="), (Boolean Leaf), 
			(Str "\tisDown="), (Boolean Down), 
			(Str "\tisUp="), (Boolean Up)
		])
	]

-- An example walk to print the example tree the same
-- way as presented in the Nuthatch paper (tree as NPN)
toStringWalk :: Walk
toStringWalk = 
	Walk "toString" 
	[ (If Leaf
		[ (Print [Value]) ]
		[ (If Down 
			[ (Print [Value, (Str "(")]) ] 
			[ (If Up 
				[ (Print [(Str ")")]) ] 
				[ (Print [(Str ", ")]) ]
			  )
			]
		  )
		]
	  )
	]

-- An excercise tree
atree :: Tree
atree = 
	Node "-" [
		(Node "132" []), 
		(Node "*" [
			(Node "+" [(Node "6" []), (Node "4" [])]),
			(Node "9" [])
		])
	]

-- ------------------------------------------------------------
-- A main function to run the most interesting examples

main = 
	do
		-- Run the demo "toString" walk
		putStrLn "---\n(1) Example: flatten the demo tree to a string:" 
		runWalk toStringWalk atree

		-- Show details about the join points
		putStrLn "\n---\n(2) Go through the demo tree and print all nodes"
		putStrLn "visited and the join point conditions:"
		runWalk dumpJoinPointsWalk atree

		-- Skip a subtree with a '+' sign as root node
		putStrLn "\n---\n(3) Skip all subtrees with a '+' at their root"
		putStrLn "(using the >>walk to 0;<< statement)"
		runWalk skipPlusWalk atree

		-- Pretty print a walk
		putStrLn "\n---\n(4) Pretty print the toString walk using the pp function"
		pp toStringWalk
