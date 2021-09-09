# Supercompilation

A simple positive supercompiler. 

### Usage:  
  `make [tests | repl | sc | clean]`  
- `repl` --- run Haskell interpreter  
- `sc` --- build executable
- `tests` --- compile and run tests

### Prerequirements:
- ghc  
- make
- Text.PrettyPrint module
- Text.Parsec.* modules

### [Language:](https://github.com/ZhekehZ/Supercompilation/blob/main/lib/Lang.hs)
```haskell
  Term := V                                          -- variable
        | N                                          -- number
        | func(Term, ..., Term)                      -- builtin function
        | pred(Term, ..., Term)                      -- builtin predicate
        | Term Term                                  -- application
        | \V -> Term                                 -- lambda abstraction
        | Constructor(Term, ..., Term)               -- constructor
        | Function                                   -- function
        | let V = Term in Term                       -- let expresstion
        | case Term of { Con V ... V => Term; ... }  -- case expresstion

  Definition := V = Term                             -- function definition (name = body)
  Program := (V, Definition, ..., Definition)        -- program (entry point and functions)
```

### Repl features
- Program1, Program2 --- program examples (sum of squares and KMP test)
- IntProgram:
    - `compile :: PROGRAM -> [(Name, TERM)] -> PROGRAM`   
       compile the program with the given arguments
    - `eval :: PROGRAM -> [(Name, TERM)] -> TERM`  
       evaluate the program with the given arguments
    - `tree :: PROGRAM -> [(Name, TERM)] -> Tree (...)`  
      build **process tree** for the program and arguments
    - `treeN :: PROGRAM -> [(Name, TERM)] -> Int -> Tree (...)`  
      same as *`tree`*, but performs only the specified number of computation steps

<details>
<summary> <b>Process tree</b> example </summary>
<pre><code>
    ┌ Node
   0│ TERM:
    │    let v1 = 1 in let v = 0 in sum (squares (upto v1 arg0)) v
    └ META = Let
      ┌ Node
     1│ TERM:
      │    1
      └ META = Regular
      ┌ Node
     1│ TERM:
      │    0
      └ META = Regular
      ┌ Node
     1│ TERM:
      │    sum (squares (upto v1 arg0)) v
      └ META = Function with args: ["v1","arg0","v"]
        ┌ Node
       2│ TERM:
        │    case squares (upto v1 arg0) of {
        │      Nil => v;
        │      Cons v2 v1 => sum v1 (plus v2 v)
        │    }
        └ META = Regular
          ┌ Node
         3│ TERM:
          │    case (\xs -> case xs of {
          │                   Nil => Nil;
          │                   Cons x xs => Cons(mul x x, squares xs)
          │                 }) (upto v1 arg0) of {
          │      Nil => v;
          │      Cons v2 v1 => sum v1 (plus v2 v)
          │    }
          └ META = Regular
            ┌ Node
           4│ TERM:
            │    case case upto v1 arg0 of {
            │           Nil => Nil;
            │           Cons x xs => Cons(mul x x, squares xs)
            │         } of {
            │      Nil => v;
            │      Cons v2 v1 => sum v1 (plus v2 v)
            │    }
            └ META = Regular
              ┌ Node
             5│ TERM:
              │    case case (\m -> \n -> case gt m n of {
              │                             True => Nil;
              │                             False => Cons(m, upto (plus m 1) n)
              │                           }) v1 arg0 of {
              │           Nil => Nil;
              │           Cons x xs => Cons(mul x x, squares xs)
              │         } of {
              │      Nil => v;
              │      Cons v2 v1 => sum v1 (plus v2 v)
              │    }
              └ META = Regular
                ┌ Node
               6│ TERM:
                │    case case (\n -> case gt v1 n of {
                │                       True => Nil;
                │                       False => Cons(v1, upto (plus v1 1) n)
                │                     }) arg0 of {
                │           Nil => Nil;
                │           Cons x xs => Cons(mul x x, squares xs)
                │         } of {
                │      Nil => v;
                │      Cons v2 v1 => sum v1 (plus v2 v)
                │    }
                └ META = Regular
                  ┌ Node
                 7│ TERM:
                  │    case case case gt v1 arg0 of {
                  │                True => Nil;
                  │                False => Cons(v1, upto (plus v1 1) arg0)
                  │              } of {
                  │           Nil => Nil;
                  │           Cons x xs => Cons(mul x x, squares xs)
                  │         } of {
                  │      Nil => v;
                  │      Cons v2 v1 => sum v1 (plus v2 v)
                  │    }
                  └ META = Split `case case case gt v1 arg0 of {   True =>...`, cases: ["True","False"]
                    ┌ Node
                   8│ TERM:
                    │    gt v1 arg0
                    └ META = Regular
                    ┌ Node
                   8│ TERM:
                    │    v
                    └ META = Regular
                    ┌ Node
                   8│ TERM:
                    │    case case Cons(v1, upto (plus v1 1) arg0) of {
                    │           Nil => Nil;
                    │           Cons x xs => Cons(mul x x, squares xs)
                    │         } of {
                    │      Nil => v;
                    │      Cons v2 v1 => sum v1 (plus v2 v)
                    │    }
                    └ META = Regular
                      ┌ Node
                     9│ TERM:
                      │    case Cons(mul v1 v1, squares (upto (plus v1 1) arg0)) of {
                      │      Nil => v;
                      │      Cons v2 v1 => sum v1 (plus v2 v)
                      │    }
                      └ META = Regular
                        ┌ Node
                      10│ TERM:
                        │    let v3 = plus v1 1 in let v2 = plus (mul v1 v1) v in sum (squares (upto v3 arg0)) v2
                        └ META = Let
                          ┌ Node
                        11│ TERM:
                          │    plus v1 1
                          └ META = Regular
                          ┌ Node
                        11│ TERM:
                          │    plus (mul v1 v1) v
                          └ META = Regular
                          ┌ Node
                        11│ TERM:
                          │    sum (squares (upto v3 arg0)) v2
                          └ META = Fold (10 up): [v1 := v3,v := v2]
  </code></pre>
</details>

### Application
Build and run example:
```bash
$ make sc
$ ./sc examples/program2.prog output.prog p A
$ cat output.prog 
fun1 s
where
def fun1 s = case s of {
               Nil => False;
               Cons v1 v3 => case eql v1 A of {
                               True => True;
                               False => fun1 v3
                             }
             }
```
> see [Parser.hs](https://github.com/ZhekehZ/Supercompilation/blob/main/app/Parser.hs) for syntax defails

### Performance
Performance of string searching (in the number of reduction steps): `superCompile p` vs `eval p`  

string\pattern |A        |AA       |AAA      |AB       |ABB      |AAB      |ABC      |ABAC     |AAAB     |ABABAB   |ABCAABB  |AAABAAA  |AAABACA  |AAABACBB 
-----------------------|--------- | ---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------
BAC                    |8/36     |13/89    |13/89    |15/89    |15/89    |13/89    |15/89    |15/89    |13/89    |15/89    |15/89    |13/89    |13/89    |15/89    
AAAAAAA                |4/13     |6/32     |8/51     |31/272   |31/272   |29/344   |31/272   |31/272   |27/378   |31/272   |31/272   |27/378   |29/378   |31/378   
AAAABBBBBBCCCCC        |4/13     |6/32     |8/51     |20/158   |22/177   |18/173   |61/441   |63/441   |16/150   |61/441   |63/441   |57/536   |59/536   |61/536   
BABABABABACBABABABAB   |8/36     |65/632   |65/632   |12/55    |67/738   |65/632   |67/738   |36/402   |65/632   |20/131   |83/738   |65/632   |65/632   |83/632   
CCCCCCAAAAAAAACCCCCCAAA|28/151   |30/170   |32/189   |95/716   |95/716   |91/845   |95/716   |95/716   |87/917   |95/716   |95/716   |87/917   |91/917   |95/917   
BABABABABABCAABBBABABAB|8/36     |44/403   |75/739   |12/55    |52/559   |48/422   |38/410   |83/959   |75/739   |20/131   |58/486   |75/739   |77/739   |95/739   
CCCCCAAAAABAAAABACACCC |24/128   |26/147   |28/166   |44/315   |87/754   |42/349   |87/754   |68/582   |40/345   |83/792   |91/754   |46/402   |64/726   |81/1039  
BACBBACAAABACBBAC      |8/36     |30/231   |32/250   |44/315   |69/544   |38/311   |69/544   |50/353   |36/269   |67/563   |71/544   |57/620   |57/639   |52/345   
