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
```