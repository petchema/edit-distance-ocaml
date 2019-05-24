let min3 a b c =
  if a < b then
    if a < c then a else c
  else if b < c then b else c

(* naive implementation *)
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

(* real stuff *)
let levenshtein2 insert_cost delete_cost replace_cost seek_cost trim_cost =
  fun s1 s2 ->
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  let rec aux i row future_row =
    if i = l1 then
      if l2 > l1 then
        row |>
          Array.mapi (fun i cost -> cost + trim_cost s2 i) |>
          Array.fold_left min max_int
      else
        row.(l2)
    else begin
        future_row.(0) <- row.(0) + delete_cost s1.[i];
        for j = 1 to l2 do
          future_row.(j) <-
            if s1.[i] = s2.[j - 1] then
              row.(j - 1)
            else
              min3
                (future_row.(j - 1) + insert_cost s2.[j - 1])
                (row.(j) + delete_cost s1.[i])
                (row.(j - 1) + replace_cost s1.[i] s2.[j - 1])
        done;
        (* swap rows *)
        aux (i + 1) future_row row
    end in
  let start_row = Array.init (l2 + 1) (fun j -> seek_cost s2 j) in
  (* just allocate a temp row, content is not used *)
  let temp_row = Array.make (l2 + 1) 0 in
  aux 0 start_row temp_row
