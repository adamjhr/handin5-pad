
## Exercise 5.1

dotnet fsi -r FsLexYacc.Runtime.dll Absyn.fs FunPar.fs FunLex.fs Parse.fs

The fsharp solution can be found in `exercise5_1.fs`

```fsharp
let merge l1 l2 =
    let rec aux l1 l2 acc =
        match l1, l2 with
        | [], l2 -> List.rev l2 @ acc
        | l1, [] -> List.rev l1 @ acc
        | e1::r1, e2::r2 -> if e2 < e1 then aux (e1::r1) r2 (e2::acc) else aux r1 (e2::r2) (e1::acc)
    aux l1 l2 [] |> List.rev;;
```

The C# solution can be found in `exercise5_1.java`

```csharp
static int[] merge (int[] xs, int[] ys) {
        int[] merged = new int[ys.Length + xs.Length];
        int xi = 0;
        int yi = 0;
        while (xi < xs.Length || yi < ys.Length) {
            if (xi == xs.Length || xs.Length == 0) {
                merged[xi+yi] = ys[yi];
                yi++;
            } else if (yi == ys.Length  || ys.Length == 0) {
                merged[xi+yi] = xs[xi];
                xi++;
            } else if (xs[xi] > ys[yi]) {
                merged[xi+yi] = ys[yi];
                yi++;
            } else {
                merged[xi+yi] = xs[xi];
                xi++;
            }
        }
        return merged;
    }
```

## Exercise 5.7

????????

## Exercise 6.1

```fsharp
"let add x = let f y = x+y in f end in add 2 5 end" =
val it = Letfun
    ("add", "x", Letfun ("f", "y", Prim ("+", Var "x", Var "y"), Var "f"),
     Call (Call (Var "add", CstI 2), CstI 5))
run it;; -> val it: HigherFun.value = Int 7

"let add x = let f y = x+y in f end in let addtwo = add 2 in addtwo 5 end end" =
val it = Letfun
    ("add", "x", Letfun ("f", "y", Prim ("+", Var "x", Var "y"), Var "f"),
     Let ("addtwo", Call (Var "add", CstI 2), Call (Var "addtwo", CstI 5)))
run it;; -> val it: HigherFun.value = Int 7

"let add x = let f y = x+y in f end in let addtwo = add 2 in let x = 77 in addtwo 5 end end end" =
val it = Letfun
    ("add", "x", Letfun ("f", "y", Prim ("+", Var "x", Var "y"), Var "f"),
     Let
       ("addtwo", Call (Var "add", CstI 2),
        Let ("x", CstI 77, Call (Var "addtwo", CstI 5))))
run it;; -> val it: HigherFun.value = Int 7

"let add x = let f y = x+y in f end in add 2 end" = 
val it: HigherFun.value =
  Closure
    ("f", "y", Prim ("+", Var "x", Var "y"),
     [("x", Int 2);
      ("add",
       Closure
         ("add", "x", Letfun ("f", "y", Prim ("+", Var "x", Var "y"), Var "f"),
          []))])
run it;; -> val it: HigherFun.value =
  Closure
    ("f", "y", Prim ("+", Var "x", Var "y"),
     [("x", Int 2);
      ("add",
       Closure
         ("add", "x", Letfun ("f", "y", Prim ("+", Var "x", Var "y"), Var "f"),
          []))])
```

The result of running the 3rd program is 7, as we expected, since the binding (x = 77) happens in a different scope than the function addtwo, it has no effect on the function, addtwo still has x bound to 2, not 77.

The 4th program returns a closure for the function add, where the variable x is bound to 2 (This can be seen as the function addtwo, in previous programs).
