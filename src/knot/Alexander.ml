let produit_polynome p [|x;y|]=
	let m = Array.length p in
	let t = Array.make (m+1) 0 in
	for i = 1 to m do
			t.(i)<-p.(i-1)*y + p.(i)*x
	done;
	t.(0)<-p.(0)*x;
	t.(m)<-p.(m-1)*y;
	t;;

let diff p q =
	let n = Array.length p in
	let m = Array.length q in
	let k = max n m in
	let l = m + n - k in
	let t = Array.make k 0 in
	for i = 0 to l-1 do
		t.(i)<-p.(i) - q.(i)
		done;
	for i = l to k-1 do
		if n>m then t.(i)<-p.(i)
		else t.(i)<- - q.(i)
	done;
	t;;

let det m =
	let n = Array.length m in
	for i = 1 to n - 1 do
		let p = m.(i-1).(i-1) in
		let q = m.(i).(i-1) in
		for j = i + 1 to n-1 do
			m.(i).(j)<- diff (produit_polynome p m.(i).(j)) (produit_polynome m.(i-1).(j) q)
		done;done;
	m.(n-1).(n-1);;
	
let rec taille liste =
	match liste with
	|[]-> 0
	|t::q -> 1+taille q;;

let matrice noeud =
	let n = taille noeud in	
	let signe (a,b,c,d) =
		if b = (d - 1) mod n then -1
		else 1
	in
	let m = Array.make_matrix n n [|0;0|] in
	let croisement = Array.make (2*n) (0,0,0,0) in
	let n_arc = Array.make (2*n) 0 in
	let rec premier_tour l =
		match l with
		|[] -> ()
		|(a,b,c,d)::q-> croisement.(a) <- (a,b,c,d); croisement.(b) <- (a,b,c,d)
	in premier_tour noeud ;
	let num_arc = ref 0 in
	for i = 0 to n-1 do
		let (a,b,c,d)=croisement.(i) in
		n_arc.(i) <- !num_arc;
		if i = b then num_arc:= !num_arc + 1 ;
		done;
	let rec deuxieme_tour l i =
		match l with
		|[]->()
		|(a,b,c,d)::q-> m.(i).(n_arc.(a))<-[|1;-1|] ;
						if signe (a,b,c,d) = 1
							 then (m.(i).(n_arc.(b))<-[|-1;0|];
							 		m.(i).(n_arc.(d))<-[|0;1|])
							 else (m.(i).(n_arc.(d))<-[|-1;0|];
							 		m.(i).(n_arc.(b))<-[|0;1|]);
						deuxieme_tour q (i+1)
	in deuxieme_tour noeud 0;
	m;;

let alexander noeud =
	det (matrice noeud);;	
		
	
		
	 