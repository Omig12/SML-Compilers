# Israel O Dilán-Pantojas
#### 2 / abril / 2018
#### 801-11-2035
#### Asignación #3 
#### First & Follow

## Indices
1. Practice #1
2. Practice #2
3. Assingment


## Practice #1

1. E -> T X
2. T -> (E)
3. T -> int Y
4. X -> + E
5. X -> 
6. Y -> * T
7. Y -> 

Var |  rule  |    first    | follow
----|--------|-------------|-------
 E  | {1}    | { int, ( }   | { ), follow(X), $}
 T  | {2,3}  | { int, ( }   | { +, ), $, follow(Y)}
 X  | {4,5}  | { +,  }      | { ), $}
 Y  | {6,7}  | { *,  }      | { +, ), $}

======================================================
------------------------------------------------------

## Practice #2

1. E' -> E
2. E  -> E + T
3. E  -> T
4. T  -> (E)
5. T  -> id

S  |   First    | Follow
---|------------|--------
E' | { id, ( }  | { $ }
E  | { id, ( }  | { $ , + , ) }
T  | { id, ( }  | { $ , + , ) }

======================================================
------------------------------------------------------

## Assignment

 1. Prog  -> { Stms }
 2. Stms  -> Stm Stms
 3. Stms  -> " "
 4. Stm   -> id =  Expr;
 5. Stm   -> if ( Expr ) Stm
 6. Expr  -> id Etail
 7. Etail -> + Expr
 8. Etail -> - Expr
 9. Etail -> " "

  S      |     First    | Follow 
---------|--------------|--------
 Prog    | {            | $
 Stms    | id, if, " "  | }
 Stm     | id, if       | id,  if, } 
 Expr    | id           | ; , ) 
 Etail   | +, -, " "    | ; , )
