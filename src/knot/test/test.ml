open Knot__Util
open Printf

let test_bracket =
  let out = ref("") in
  let loop = bracket k in
  let loop_expected = {Knot.Laurent.coeffs = [0; 0; 0; 0; -1]; least = -1} in
  let trefoil = bracket k31 in
  let trefoil_expected = {Knot.Laurent.coeffs = [-1; 0; 0; 0; 0; 0; 0; 0; -1; 0; 0; 0; 1]; least = -5} in
  let () =
    match loop with
    | p when p = loop_expected -> out := !out ^ "✅ Bracket : (loop) Ok\n";
    | p -> out := !out ^ (sprintf "❌ Bracket : (loop) supposed to be %s : %s\n" (Knot__Laurent.string_of_laurent_pretty loop_expected) (Knot__Laurent.string_of_laurent_pretty p));
  in
  let () =
    match trefoil with
    | p when p = trefoil_expected -> out := !out ^ "✅ Bracket : (trefoil) Ok\n";
    | p -> out := !out ^ (sprintf "❌ Bracket : (trefoil) supposed to be %s : %s\n" (Knot__Laurent.string_of_laurent_pretty trefoil_expected) (Knot__Laurent.string_of_laurent_pretty p));
  in
  !out

let test_writhe =
  let out = ref("") in
  let () =
    match writhe k with
    | 1 -> out := "✅ Writhe : (loop) Ok\n" ;
    | (-1) -> out := "❌ Writhe : (loop) -1 : might be inverted\n" ;
    | n -> out := (sprintf "❌ Writhe : (loop) supposed to be 1 : is %i\n"  n);
  in
  let () =
    match writhe k31 with
    | (-3) -> out := !out ^ "✅ Writhe : (trefoil) Ok\n";
    | 3 -> out := !out ^ "❌ Writhe : (trefoil) -3 : might be inverted !\n";
    | n -> out := !out ^ (sprintf "❌ Writhe : (trefoil) supposed to be -3 : is %i\n" n);
  in
  let () = 
    match writhe k41 with
    | 0 -> out := !out ^ "✅ Writhe : (figure eight) Ok\n";
    | n -> out := !out ^ (sprintf "❌ Writhe : (figure eight) supposed to be 0 : is %i\n" n);
  in
  !out

let test_jones =
  let out = ref("") in
  let miller = jones k62 in
  let miller_expected = {Knot.Laurent.coeffs = [1; -2; 2; -2; 2; -1; 1]; least = -5} in
  let () =
    match jones k with
    | {Knot.Laurent.coeffs = [1]; least = 0} -> out := "✅ Jones : (loop) Ok\n";
    | p -> out := !out ^ (sprintf "❌ Jones : (loop) supposed to be A^0 : is %s\n" (Knot__Laurent.string_of_laurent_pretty p));
  in
  let () =
    match miller with
    | p when p = miller_expected -> out := !out ^ "✅ Jones : (miller institute) Ok\n";
    | p -> out := !out ^ (sprintf "❌ Jones : (miller institute) supposed to be %s : is %s\n" (Knot__Laurent.string_of_laurent_pretty miller_expected) (Knot__Laurent.string_of_laurent_pretty p));
  in
  !out

let tests = [test_bracket; test_writhe; test_jones]
let () = List.iter print_string tests