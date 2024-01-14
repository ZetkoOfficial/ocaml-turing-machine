open Turing
open Tipi

(* V napakah je še ime problematične definicije *)
exception MissingDefinitions  of string
exception MultipleDefinitions of string 

exception MissingTranstion of int * int

(* Napaka vsebuje neznan simbol *)
exception UnknownSymbol of string

exception ExpectedDeterministic
exception InvalidListSize of int

(** Izračuna a^b. *)
let rec pow a b = 
  match b with
  | 0 -> 1 | 1 -> a
  | b -> 
    let b' = pow a (b/2) in
    if b mod 2 = 0 then b' * b' else b'*b'*a
;;

(** Vrne napako v primeru da je tokenov ki zadoščajo [f] več ali manj od [1]. 
    Drugače najde token ki zadišča predikatu [f]. *)
let find_sym ?(label="") f tokeni = 
  match List.filter f tokeni with
  | [t] -> t
  | []  -> raise (MissingDefinitions label)
  | _   -> raise (MultipleDefinitions label)
;;

let is_zacetek t = match t with Parser.Zacetek _ -> true | _ -> false
let is_podatki t = match t with Parser.Podatki _ -> true | _ -> false
let is_tranzicija t = match t with Parser.Tranzicija _ -> true | _ -> false

let pridobi_stanja tranzicije = 
  tranzicije |> 
  List.fold_left ( fun acc ((s,_), (s',_)) -> s::s'::acc ) [] |>
  List.sort_uniq (compare)
;;

let pridobi_podatke tranzicije = 
  tranzicije |> 
  List.fold_left ( fun acc ((_,list), (_,list')) -> 
    let add1 = List.fold_left (fun acc p -> p::acc) in
    let add2 = List.fold_left (fun acc (p,_) -> p::acc) in
    add2 (add1 acc list) list'
  ) [] |>
  List.sort_uniq (compare)
;;

let prevedi_podatek p = 
  if p = "_" then Prazno
  else Podatek p
;;

let preveri_navodilo (p,smer) =
  match smer with
  | Parser.Levo   -> prevedi_podatek p, Levo
  | Parser.Desno  -> prevedi_podatek p, Desno
  | Parser.Ni     -> prevedi_podatek p, N
;;

(** 
  Funkcija prevede seznam tokenov v turingovo napravo.

  Opcija [~force] ignorira nekritične napake(kot so manjajoče tranzicije in definicije simbolov).
  Opcija [~deterministic_only] preveri da je turingova naprava deterministična. 
  
  @raise MissingDefinitions če manjka kaka definicija
  @raise MultipleDefinitions če je kake definicije preveč
  @raise MissingTranstion(najdeno,pričakovano) če niso podane vse tranzicije
  @raise UnknownSymbol če se pojavi nedefiniran simbol v podatkih
*)
let prevedi_tokene ?(force=false) ?(deterministic_only=false) tokeni = 
  (* odstranimo morebitne popolne duplikate *)
  let tokeni = List.sort_uniq (compare) tokeni in

  (* Prodobimo nujna tokena *)
  let (zacetno_stanje, st_tirov) = 
    match find_sym ~label:"@ (začetno_stanje) (število_tirov)" (is_zacetek) tokeni with
    | Parser.Zacetek (u,v) -> u,v
    | _ -> failwith "nepričakovana napaka" in

  let veljavni_podatki = 
    match find_sym ~label:"$ [veljavni_podatki]" (is_podatki) tokeni with
    | Parser.Podatki list -> list
    | _ -> failwith "nepričakovana napaka" in

  let tranzicije = tokeni |> List.filter (is_tranzicija) |> List.map (fun t ->
    match t with
    | Parser.Tranzicija (i,t) -> (i,t)
    | _ -> failwith "nepričakovana napaka"
  ) in

  (* preverimo če so seznami pravilno dolgi *)
  let dolzine_ok = List.fold_left (fun acc (i,((_, list), (_, list'))) -> 
    if acc = None then 
      if (List.length list = st_tirov && List.length list' = st_tirov) then None
      else Some i
    else acc
  ) None tranzicije in

  match dolzine_ok with
  | Some i -> raise (InvalidListSize i)
  | _ -> ();

  let tranzicije = List.map (fun (_,t) -> t) tranzicije in
  let vsa_stanja, vsi_podatki = pridobi_stanja tranzicije, pridobi_podatke tranzicije in

  (* preverimo če so vsi_podatki podmnožica veljavnih *)
  let manjkajoc_simbol = 
    List.fold_left (fun acc simbol -> 
      if List.mem simbol veljavni_podatki then acc else Some simbol
    ) None vsi_podatki in 
    begin 
      match manjkajoc_simbol with
      | None -> ()
      | Some sym -> if not force then raise (UnknownSymbol sym) 
    end;

  (* preverimo če se vse kombinacije stanj in podatkov pojavijo v tranzicijah *)
  let comb_pricakovno = 
    (List.length @@ List.filter (fun s -> not @@ String.starts_with ~prefix:"!" s) vsa_stanja) * 
    (pow (List.length vsi_podatki) st_tirov) in
  
  let comb_najdeno = tranzicije |> 
    List.map (fun (t_in,_) -> t_in) |>
    List.sort_uniq (compare) |> List.length in

  if comb_najdeno <> comb_pricakovno && not force
    then raise (MissingTranstion (comb_najdeno, comb_pricakovno));

  (* preverimo če naprava ni deterministična *)
  if not @@ List.fold_left (fun acc (t_in,_) -> 
    if not acc then acc
    else List.length @@ (List.find_all (fun (t_in', _) -> t_in = t_in') tranzicije) = 1
  ) true tranzicije then
    if deterministic_only then raise ExpectedDeterministic;
  
  (* nazadnje ustvarimo turingovo napravo *)

  let zacetno_stanje = Stanje zacetno_stanje in
  let koncna_stanja = vsa_stanja |> 
    List.filter (String.starts_with ~prefix:"!") |> 
    List.map (fun s -> Stanje s) in

  let tranzicije: (string, string) turing_tranzicija list = tranzicije |> 
    List.map (fun ((s,p), (s',n)) -> 
      (Stanje s, List.map (prevedi_podatek) p), (Stanje s', List.map (preveri_navodilo) n)  
    ) in

   NDVT.ustvari st_tirov zacetno_stanje koncna_stanja tranzicije
;;