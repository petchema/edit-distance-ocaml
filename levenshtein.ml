let min3 a b c =
  if a < b then
    if a < c then a else c
  else if b < c then b else c

let levenshtein s1 s2 =
  let insert_cost = 1 in
  let delete_cost = 1 in
  let replace_cost = 1 in
  let rec aux i1 i2 =
    match i1, i2 with
    | i1, 0 -> i1 * delete_cost
    | 0, i2 -> i2 * insert_cost
    | i1, i2 ->
       if s1.[i1 - 1] = s2.[i2 - 1] then
         aux (i1 - 1) (i2 - 1)
       else
         min3
           (aux (i1 - 1) i2 + delete_cost)
           (aux i1 (i2 - 1) + insert_cost)
           (aux (i1 - 1) (i2 - 1) + replace_cost) in
  aux (String.length s1) (String.length s2)

let levenshtein2 s1 s2 =
  let insert_cost = 1 in
  let delete_cost = 1 in
  let replace_cost = 1 in
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  let rec aux j row future_row =
    if j = l1 then
      row.(l2)
    else begin
        future_row.(0) <- row.(0) + insert_cost;
        for i = 1 to l2 do
          future_row.(i) <-
            if s1.[j] = s2.[i - 1] then
              row.(i - 1)
            else
              min3
                (future_row.(i - 1) + delete_cost)
                (row.(i) + insert_cost)
                (row.(i - 1) + replace_cost)
        done;
        aux (j + 1) future_row row
    end in
  let start_row = Array.init (l2 + 1) (fun i -> i * insert_cost) in
  let temp_row = Array.make (l2 + 1) 0 in
  aux 0 start_row temp_row
