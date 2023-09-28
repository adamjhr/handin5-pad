
## Exercise 5.1

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