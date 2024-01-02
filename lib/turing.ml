type 'p podatek = Prazno | Podatek of 'p
type 's stanje  = Stanje of 's
type smer       = Levo | Desno | N       

(* 
   Tir predstavimo kot kazalec na trenutni element na traku,
   kjer trak predstavimo z Map-om da se izognemo problemom pri premikanju
   čez trak.
*)
module Trak = Map.Make(Int)
type 'p tir = {
  trak:     'p podatek Trak.t;
  glava:    int
}

(* Tipa predstavlja trenutno stanje neke turingove naprave z večimi tiri in obliko njegove tranzicicje *)
type ('s, 'p) turing_data = {
  tiri:     'p tir list;
  stanje:   's stanje
}
type ('s, 'p) turing_tranzicija = ( 's stanje * ('p podatek list) ) * ( 's stanje * ('p podatek * smer) list )

(* 
    Turingovo napravo z večimi tirom predstavimo z trenutnim stanjem naprave,
    seznamom tranzicij in seznamom sprejemnih stanj. 
*)
type ('s, 'p) turing = {
  data:               ('s, 'p) turing_data;
  sprejemna_stanja:   ('s stanje) list;
  tranzicije:         ('s, 'p) turing_tranzicija list
}

(** Pomožna funkcija ki ustvari seznam [n] tirov. *)
let ustvari_tire n = 
  let rec aux n acc = 
    if n = 0 then acc
    else aux (n-1) ({trak=Trak.empty; glava=0}::acc) in
  aux n []
;;

(** Iz začetnega stanja, seznama sprejemnih stanj in tranzicij ustvari turingovo napravo z [n] tiri.  *)
let ustvari_turing n zacetno_stanje sprejemna_stanja tranzicije: ('s, 'p) turing = 
  let data = {
    tiri = ustvari_tire n;
    stanje = zacetno_stanje
  } in { data; sprejemna_stanja; tranzicije }
;;

type ('a,'b) rezultat = Konec of 'a | Naprej of 'a | Napaka of 'a

(* Pomagalni funckiji za branje tirov in iskanje ujemajočih tranzicij. *)
let preberi_tire tiri = 
  List.map (fun tir -> 
    match Trak.find_opt tir.glava tir.trak with
    | Some p -> p | None -> Prazno
  ) tiri
;;
let ujemajoca_tranzicija t_in (t_in',_) = (t_in = t_in');;  

let posodobi_tire tiri pisanje = 
  List.map2 (fun tir (p,smer) ->
    let glava = match smer with
      | Levo  -> tir.glava-1
      | Desno -> tir.glava+1
      | N     -> tir.glava in

    { trak = Trak.add tir.glava p tir.trak; glava }
  ) tiri pisanje
;;

(** Funkcija vrne turingove stroje po enem koraku nedeterminističnega turingovega stroja. *)
let korak turing = 
  let data = turing.data in

  if List.mem data.stanje turing.sprejemna_stanja then [ Konec turing ]
  else
    let s,p = data.stanje, preberi_tire data.tiri in
    let tranzicije = List.find_all (ujemajoca_tranzicija (s,p)) turing.tranzicije in
    if tranzicije = [] then [ Napaka turing ]
    else 
      List.map (fun (_, (s', pisanje)) -> 
        let data = {
          tiri = posodobi_tire data.tiri pisanje;
          stanje = s' 
        } in Naprej { turing with data }
      ) tranzicije
;;

(** Od [i] naprej nastavlja vrednosti [prvega] traku na vrednosti v seznamu [input].  *)
let rec nastavi_input i input tiri = 
  match input with
  | [] -> tiri
  | h::t -> 
    let h',t' = List.hd tiri, List.tl tiri in
    nastavi_input (i+1) t ( {h' with trak = Trak.add i h h'.trak} :: t' )
;;

(** Požene deteministični turingov stroj z podanim [input]-om. *)
let pozeni turing input = 
  let rec aux turing = 
    match korak turing with
    | [Konec   turing'] -> Konec turing'
    | [Naprej  turing'] -> aux turing'
    | [error] -> error
    | _ -> print_endline "Stroj ni determinističen!"; Napaka turing in

  let data = { turing.data with
    tiri = nastavi_input 0 input turing.data.tiri
  } in aux {turing with data}
;;