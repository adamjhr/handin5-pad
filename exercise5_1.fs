let merge l1 l2 =
    let rec aux l1 l2 acc =
        match l1, l2 with
        | [], l2 -> List.rev l2 @ acc
        | l1, [] -> List.rev l1 @ acc
        | e1::r1, e2::r2 -> if e2 < e1 then aux (e1::r1) r2 (e2::acc) else aux r1 (e2::r2) (e1::acc)
    aux l1 l2 [] |> List.rev;;

merge [1; 3; 4; 4;] [2;];;
merge [12; 13;] [1; 14;];;
merge [2; 3; 5] [];;
merge [14; 15; 16;] [10; 11; 12;];;