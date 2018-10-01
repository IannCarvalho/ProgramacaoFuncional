--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.
data Triple a b c =  Triple a b c deriving (Eq,Show)

tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c

--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
data Quadruple a b = Quadruple a a b b

firstTwo (Quadruple a b c d) = (a,b) 
secondTwo (Quadruple a b c d) = (c,d)

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d deriving (Eq,Show)

tuple1 (Tuple1 a) = Just a 
tuple1 (Tuple2 a b) = Just a 
tuple1 (Tuple3 a b c) = Just a 
tuple1 (Tuple4 a b c d) = Just a 

tuple2 (Tuple2 a b) = Just b 
tuple2 (Tuple3 a b c) = Just b 
tuple2 (Tuple4 a b c d) = Just b 
tuple2 _ = Nothing 

tuple3 (Tuple3 a b c) = Just c 
tuple3 (Tuple4 a b c d) = Just c 
tuple3 _ = Nothing 

tuple4 (Tuple4 a b c d) = Just d 
tuple4 _ = Nothing

data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = error "Empty list"
listHead (Cons x xs) = x

listTail Nil = Nil
listTail (Cons x xs) = xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)


listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 

--Escreva as funcoes sobre a estrutura de dados binary tree
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a) deriving (Eq,Show)

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

getValue NIL = Nothing
getValue (Node a _ _ ) = Just a

getLeft NIL = NIL
getLeft (Node _ left _) = left

getRight NIL = NIL
getRight (Node _ _ right) = right

getParent _ NIL = NIL
getParent x (Node a left right) | (Just x == getValue left) || (Just x == getValue right) = (Node a left right)
                                | (x < a) = getParent x left
                                | otherwise = getParent x right

--verifica se uma BT é uma BST
isBST NIL = True
isBST (Node a left right) | a < getValue left = False
						   | a > getValue right = False
						   | otherwise = (isBST left) && (isBST right)

--insere uma nova chave na BST retornando a BST modificada
insert value NIL = Node value NIL NIL
insert value (Node a left right) | value < a = Node a (insert x left) right
								 | otherwise = Node a left (insert x right)

--retorna o Node da BST contendo o dado procurado ou entao NIL
search value NIL = NIL
search value (Node a left right) | value == a = Node a left right
								  | otherwise = (search left) && (search right)

--retorna o elmento maximo da BST
maximum NIL = NIL
maximum (Node a left right) | right == NIL = (Node a left right)
							| otherwise = maximum right

--retorna o elemento minimo da BST
minimum NIL = NIL
minimum (Node a left right) | (left == NIL) = (Node a left right)
							| otherwise = minimum right

--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
predecessor value NIL = NIL
predecessor value (Node a left right) | search value bst == NIL = NIL
									   | (left == NIL) = firstParentRight node bst
									   | otherwise = maximum left
										where
											result = search value bst
											bst = (Node a left right)

firstParentRight (Node x left right) (Node a b c) | (parent /= NIL) && (getLeft parent == son) = firstParentRight parent bst
                                                   | otherwise = parent
													where
														parent = getParent x bst
														son = (Node x left right)
														bst = (Node a b c)


--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
sucessor value NIL = NIL
successor value (Node a left right) | (node == NIL) = NIL
									| (getRight node == NIL) = firstParentLeft node bst
									| otherwise = myMinimum (getRight node) 
										where
											node = search value bst
											bst = (Node a left right) 

firstParentLeft (Node x left right) (Node a b c) | (parent /= NIL) && (getRight parent == son) = firstParentLeft parent bst
                                                  | otherwise = parent
													where
														parent = getParent x bst
														son = (Node x left right)
														bst = (Node a b c) 

--remove ume lemento da BST
{-remove value NIL = NIL
remove value (Node x left right) | searched == NIL = bst
								  | otherwise = remove searched
									where
										searched = search value bst
										bst = (Node x left right)

recursiveRemove value (Node x left right) | left == NIL && right == NIL = NIL
										   | right == NIL = (Node (getValue left) (getLeft left) right)
										   | left == NIL = (Node (getValue right) left (getRight right))
										   | otherwise = (Node getValue left right) &&
											where
												smaller = minimum(right)
												aux = getValue smaller
-}

--retorna uma lista com os dados da BST nos diversos tipos de caminhamento
preOrder NIL = []
preOrder (Node x left right) = [x] ++ (preOrder left) ++ (preOrder right) 


order NIL = []
order (Node x left right) = (order left) ++ [x] ++ (order right) 

postOrder NIL = []
postOrder (Node x left right) = (postOrder left) ++ (postOrder right) ++ [x] 
