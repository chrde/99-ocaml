(* 31 *)
let siege m n =
  let rec range acc min max =
    if min > max then acc else range (max :: acc) min (max - 1)
  in
  let rec siege_rec l =
    match l with
    | [] -> []
    | h :: t -> h :: siege_rec (List.filter (fun x -> x mod h <> 0) t)
  in
  siege_rec (range [] m n)
;;

let is_prime n = if n = 1 then true else Lists.last (siege 2 n) = Some n

let%test _ = siege 2 3 = [ 2; 3 ]
let%test _ = siege 2 50 = [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47 ]
let%test _ = is_prime 1 = true
let%test _ = is_prime 2 = true
let%test _ = is_prime 4 = false
let%test _ = is_prime 41 = true
let%test _ = is_prime 42 = false

(* 32 *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let%test _ = gcd 13 27 = 1
let%test _ = gcd 20536 7826 = 2

(* 33 *)
let coprime a b = gcd a b = 1

let%test _ = coprime 13 27 = true
let%test _ = coprime 20536 7826 = false

(* 34 *)
let phi x =
  let rec phi_rec acc y =
    if x = y
    then acc
    else (
      let coprime = if coprime x y then 1 else 0 in
      phi_rec (acc + coprime) (y + 1))
  in
  if x = 1 then 1 else phi_rec 0 1
;;

let%test _ = phi 10 = 4
let%test _ = phi 13 = 12

(* 35 *)
let factors m =
  let rec factors_rec m acc primes =
    match primes with
    | [] -> List.rev acc
    | x :: y ->
      if m mod x = 0 then factors_rec (m / x) (x :: acc) (x :: y) else factors_rec m acc y
  in
  factors_rec m [] (siege 2 m)
;;

let%test _ = factors 1 = []
let%test _ = factors 2 = [ 2 ]
let%test _ = factors 12 = [ 2; 2; 3 ]
let%test _ = factors 315 = [ 3; 3; 5; 7 ]

(* 36 *)
let factors1 m = Lists.encode (factors m) |> List.map (fun (x, y) -> y, x)

let%test _ = factors1 315 = [ 3, 2; 5, 1; 7, 1 ]

(* 37 *)
let rec pow a n =
  match n with
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * if n mod 2 = 0 then 1 else a
;;

let phi_improved m =
  let calc prime count = (prime - 1) * pow prime (count - 1) in
  let rec phi_rec acc factors =
    match factors with
    | [] -> acc
    | (prime, count) :: y -> phi_rec (calc prime count * acc) y
  in
  phi_rec 1 (factors1 m)
;;

let%test _ = phi_improved 10 = 4
let%test _ = phi_improved 13 = 12
let%test _ = phi_improved 132 = 40

(* 39 *)
let all_primes m n = siege m n

let%test _ = List.length (all_primes 2 7920) = 1000

(* 40 *)
let goldbach n =
  let rec matching_prime m primes =
    match primes with
    | [] -> None
    | x :: y -> if x + m = n then Some x else matching_prime m y
  in
  let rec find_primes primes =
    match primes with
    | [] -> raise Not_found
    | x :: y ->
      (match matching_prime x y with
      | Some m -> x, m
      | None -> find_primes y)
  in
  find_primes (siege 2 n)
;;

let%test _ = goldbach 28 = (5, 23)

(* 41 *)
let goldbach_list m n =
  let rec goldbach_rec acc m =
    if m > n
    then List.rev acc
    else (
      let x, y = goldbach m in
      goldbach_rec ((m, (x, y)) :: acc) (m + 2))
  in
  goldbach_rec [] (if m mod 2 = 0 then m else m+1)
;;

let%test _ =
  goldbach_list 9 20
  = [ 10, (3, 7); 12, (5, 7); 14, (3, 11); 16, (3, 13); 18, (5, 13); 20, (3, 17) ]
;;
