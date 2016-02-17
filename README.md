# Tree-walk put in a Nutshell

*This project is an implementation of assignment 3 of the [SLE lecture 2015/16](http://softlang.wikidot.com/course:sle1516) held by Prof. Lämmel. On the grounds of the paper [2] from the [SLE conference 2015](http://dblp.uni-trier.de/db/conf/sle/sle2015.html) an introductory guide to the topic of tree walks was created. This project wraps a showcase implementation of a subset of Nuthatch DSL from the paper [1] in Haskell.*

*Be sure to checkout my blog article [here](http://maxstrauch.github.io/2016/02/01/nuthatch.html).*

## Foreknowledge

A keynote which explains the scheme and paradigm of tree walks as presented in [1, 2] can be found [here](https://github.com/maxstrauch/sle-tree-walk/raw/master/nuthatch-j-keynote.pdf). It is highly recommended to take a look into this resources if one does not have any foreknowledge about Nuthatch or the proposed idea of tree walks.

Furthermore there is a full-featured Java based implementation of the Nuthatch paradigm from [1, 2] called __Nuthatch/J__. The source code is available as a GitHub Project [here](https://github.com/nuthatchery/nuthatch). There is also a project page of Nuthatch with further material available under [http://nuthatchery.org/](http://nuthatchery.org/).

## Intent

In [1] a DSL called Nuthatch was proposed to formalize tree walks. The intent of this project is therefore an implementation of a __simple__ and __self-contained__ solution in __Haskell__ which implements a small subset of the Nuthatch DSL. Thus this project shows the functional principle of Nuthatch by the simplicity of implementation.

The "official" intentional information for this project can be found [in the repo in file `intent.txt`](https://raw.githubusercontent.com/maxstrauch/sle-tree-walk/master/intent.txt).

# The project solution

The file `nuthatch.hs` is the outcome of this project: a simple and self-contained implementation of a subset of the Nuthatch DSL. Beside the implementation there is a keynote to present the project and its outcome. This presentation can be found [here](https://github.com/maxstrauch/sle-tree-walk/raw/master/keynote.pdf) (file `keynote.pdf` in repo). Furthermore this document serves as a textual documentation to amend the keynote.

The source file `nuthatch.hs` is also well documented and can be read quite easily.


## Example

In order to run a little example and see that the project works one can simply start the GHCi with `ghci nuthatch.hs` and use the predefined demo tree `atree` along with a predefined walk, e.g. `toStringWalk`. The result (should) look like:

	$ ghci nuthatch.hs
	GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
	Loading package ghc-prim ... linking ... done.
	Loading package integer-gmp ... linking ... done.
	Loading package base ... linking ... done.
	[1 of 1] Compiling Main             ( nuthatch.hs, interpreted )
	Ok, modules loaded: Main.
	*Main> runWalk toStringWalk atree
	-(132, *(+(6, 4), 9))
	*Main> 

Forthermore the function `main` can be used to print multiple examples showing the different features of this implementation.

The following example walks are available:

 - `toStringWalk` flattens a tree to a string using infix notation where it is assumed that the tree is a formula tree containing operators in nodes and numbers in it's leafs. For the demo tree `atree` the result looks like `-(132, *(+(6, 4), 9))` (= 42).
 - `skipPlusWalk` demonstrates the feature of skipping an entire subtree using the `walk to 0;` return statement indicating that the next visited node should be the parent node.
 - `goLeftWalk` a simple walk that uses the `walk to 1;` statement to always walk to the far right of a tree by only going to the first child node.
 - `dumpJoinPointsWalk` is a kind of debug walk which dumps all nodes of the tree (visited using the default walk) and all join points. 

The output of `runWalk dumpJoinPointsWalk atree` might look like:

	*Main> runWalk dumpJoinPointsWalk atree
	v=-     isLeaf=False    isDown=True     isUp=False
	v=132   isLeaf=True     isDown=True     isUp=True
	v=-     isLeaf=False    isDown=False    isUp=False
	v=*     isLeaf=False    isDown=True     isUp=False
	v=+     isLeaf=False    isDown=True     isUp=False
	v=6     isLeaf=True     isDown=True     isUp=True
	v=+     isLeaf=False    isDown=False    isUp=False
	v=4     isLeaf=True     isDown=True     isUp=True
	v=+     isLeaf=False    isDown=False    isUp=True
	v=*     isLeaf=False    isDown=False    isUp=False
	v=9     isLeaf=True     isDown=True     isUp=True
	v=*     isLeaf=False    isDown=False    isUp=True
	v=-     isLeaf=False    isDown=False    isUp=True

# Theory of Operation

## Implementation of Nuthatch DSL

The subset of the Nuthatch DSL is implemented in Haskell using custom data types. The definition is rather short since only a very small subset is implemented to keep the project simple. It looks like:

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

A `Walk` consists of a title and a list of statements. As statements only a `If` statement, a `Print` statement and the `WalkTo` statement is available allowing only to implement some demo walks. Furthermore there are two kinds of expressions: normal expressions, `Expr`, which are only used in the `Print` statements to output information about the tree. The second type of expressions are boolean expressions, `Exprb`, which contain an equal test expression (for `skipPlusWalk`) and the join points.

Using this subset the following walk can be defined:

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

The `pp` function of this project has the following signature: `pp :: Walk -> IO ()`. The function takes a walk and pretty prints it in a nice way. The result of `pp toStringWalk` looks like:

	*Main> pp toStringWalk
	walk toString {
	  if (leaf) {
	    print value;
	  } else {
	    if (down) {
	      print value + "(";
	    } else {
	      if (up) {
	        print ")";
	      } else {
	        print ", ";
	      }
	    }
	  }
	}

## Tree structure

Trees which can be feed into the `run` function to evaluate a walk on a tree are defined using a custom data structure with this definition:

	-- Simple tree data structure
	data Tree = Node String [Tree]

It is basically a node with a string value and as many child nodes as possible (listed in an array). The demo tree `atree` therfore looks like the following in this notation:

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

## The main function `runWalk`

The function `runWalk` with the signature `runWalk :: Walk -> Tree -> IO ()` is the main function which takes a tree and a walk and executes this walk over the tree. The results are printed directly to the console. `runWalk` has to walk through the tree and evaluate for every visited node the given `Walk`. For that the `eval` function is used. How this function works is explained in the next section. For now it is sufficient to know that the the `eval` function returns a tuple `(String, Int)`. The first string contains the output generated by the print statements and the second value is the "return" value of the walk created by the statement `WalkTo i` where 

 - -1 indicates that the run function should continue with the default walk (__Attention:__ the original papers [1, 2] use the value -1 as an alias for the last node which is not used in this way here)
 - 0 indicates that the next node is the parent node (skip the subtree)
 - 1 .. #-of-child-nodes the next child node to walk to

The `runWalk` function contains multiple wrapper or alias functions which are simply used as different entry points since the parameters of this functions are slightly different. In general a call of `runWalk` is forwarded through `base` and `internal` to the main inner function `children` which performs the main part of the computation. `base` is used to append the root node to the output and `internal` is then later called since when `base` would be called instead the root node would be appended multiple times and this is not a desired behaviour.

The core of `runWalk` looks like:

	children parent t i = 
		if (i < length t) then 
			if (test (t !! i) True (i-1)) < 0 then 
				(render (t !! i) True (i-1)) ++ -- 1.1.) Render the current node
				(internal (t !! i)) ++          -- 1.2.) Work an all child nodes
				(render parent False (i+1)) ++  -- 1.3.) Go back up to the parent node
				children parent t (i+1)         -- 1.4.) Go to next parent node

			else if (test (t !! i) True (i-1)) == 0 then
				-- The "0" value represents a "walk up" e.g.
				-- skip the subtree of the current node
				children parent t (i+1)         -- 2.1.) Go to next parent node

			else
				-- Otherwise a value between 1 and #-of-children
				-- is given -> walk directly to this node
				-- 3.1.) Render the REQUESTED (!) node
				render (t !! (getindex  (t !! i) True (i-1))) True (getindex (t !! i) True (i-1)) ++
				-- 3.2.) Render all children of the REQUESTED (!) node
				internal (t !! (getindex  (t !! i) True (i-1)))

		else 
			-- If there is no i-th child nothing new can be added
			[]

The outer if statement ensures that the child node counter does not get out of its bound. More noteworthy are the inner `if` statements which are responsible for working on the _correct_ the next node. The first case is executed when the default walk is taken (return value of `eval` is -1). Comments 1.1.) to 1.4.) describe the steps performed. The second case is performed when the return value is 0 and the subtree of the current node should be skipped. Statement 2.1.) is responsible for iterating over all other child nodes of the parent node. The third case is then performed when a specific node is requested as next node. Be aware of the fact that no error checking is done at this stage (for simplicity) and errors could arise by misusing this. Points 3.1.) and 3.2.) describe the control flow.

The helper function `render` looks like 

	render (Node v t) d u = [fst (eval (makectx v t d u) w)]

and "renders" a node by evaluating the walk on the context (`makectx` creates a context for the current node with the current join point conditions). Since the eval function returns a tuple and at this stage only the string output is needed the first value is only used.

The helper function `test` does the same thing as `render` does with the difference that this function takes the second value of the tuple which contains the number to decide which node should be visited next.

## The function `eval`

This function evaluates a walk for a given context and has the signature `eval :: Ctx -> Walk -> (String, Int)`. A context looks like:

	-- Walk execution context: (Ctx value isLeaf isDown isUp)
	data Ctx = Ctx String Bool Bool Bool

and wraps the join point condition (booleans denoted with a starting "is") and the the value of the node. The `eval` function is fairly simple and looks like:

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

The helper function `execs` is used to evaluate every statement of a list of statements by calling the `evals` function which evaluates a statement.

`evals` returns always a `(String, Maybe Int)`. The type `Maybe` comes in handy here, because not every statement sets a value for the second value of the tuple which is the number of the next node to visit. Therefore in this cases the value `Nothing` can be used. The statement `WalkTo i` sets the next node to visit and therefore `Just i` can be used. This way it can be simply distinguished between this cases. The main function call looks like

	eval c (Walk _ stmts) = retmap (reduce (execs c stmts))

Here several steps are prerformed:

 - Firstly `execs c stmts` returns a list of tuples (`[(String, Maybe Int)]`) which might look like `[("output1", Nothing), ("output2", Just 1), ...]`. Using `reduce` this list is reduced or compressed to a single tuple using the `foldl` function. All strings are concatenated and only the last "walk to" statement result of type `Maybe Int` is used. Therefore placing a `WalkTo` before an other statement will result in ignoring this `WalkTo` statement. But for simplicity reasons this behaviour is considered as okay and the convention that the `WalkTo` statement should be the last statement of a walk might be enfored by the user.
 - Secondly `retmap` simply converts the `Maybe` to a scalar value which than can be used by the `run` function.

In order to get some insight into the evaluation of a statement the evaluation of `Print` is explained in the following. The responsible function is:

	evals c (Print ex1) = (foldl (++) "" (map (evale c) ex1), Nothing)

A `Print` statement consists of a list of expressions `Expr`. The part `(map (evale c) ex1)` maps a list of this expressions (`ex1`) to a list of evaluated expressions which are strings. The `foldl` function is used along with the string concatenation operator `++` to flatten this array of strings to a single string and this string is as the first value of the tuple returned along with Nothing on the second place since no `WalkTo` value can be extracted from this statement.

The remaining functions `evale` and `evalb` are pretty simple and use the context `Ctx` to provide data about the current node and the join point conditions.

# Conclusion

The result of this project is a well working simple and self-contained implementation of the Nuthatch DSL. As shown in section "[The main function `runWalk`](#the-main-function-runwalk)" the functional principle of Nuthatch can be seen very good. The examples also support showing the functional principle and point out the mechanism of the `walk to ...;` statement really well which is an unique feature of Nuthatch. 

Besides that the implementation is very simple. For example not state variables are supported. This feature was omitted since it is not crucial for the core functionality of Nuthatch. Such feature could be added very simple to this project result but would note help to explain the core of Nuthatch. Therefore this minimal set is sufficient (at least for the author).

# References

 - [1] A. H. Bagge, R. Lämmel: Walk Your Tree Any Way You Want. ICMT 2013. [http://softlang.uni-koblenz.de/nuthatch/paper.pdf](http://softlang.uni-koblenz.de/nuthatch/paper.pdf)
 - [2] A. H. Bagge: Analysis and transformation with the nuthatch tree-walking library. SLE Conference 2015. [http://dl.acm.org/citation.cfm?doid=2814251.2814264](http://dl.acm.org/citation.cfm?doid=2814251.2814264)
 - [3] R. Lämmel: Language interpreters. Software Languages Team, CS Faculty, University of Koblenz-Landau. `<No URL available>`

# License

Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0). Click here for details. The license applies to the entire source code.

`This program is distributed WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.`

