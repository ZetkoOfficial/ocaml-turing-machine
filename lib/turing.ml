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

(* Tipa predstavlja trenutno stanje neke turingove naprave z enim tirom in obliko njegove tranzicicje *)
type ('s, 'p) turing1_data = {
  tir:      'p tir;
  stanje:   's stanje
}
type ('s, 'p) turing1_tranzicija = ('s stanje * 'p podatek) * ('s stanje * 'p podatek * smer)

(* 
    Turingovo napravo z enim tirom predstavimo z trenutnim stanjem naprave,
    seznamom tranzicij in seznamom sprejemnih stanj. 
*)
type ('s, 'p) turing1 = {
  data:               ('s, 'p) turing1_data;
  sprejemna_stanja:   ('s stanje) list;
  tranzicije:         ('s, 'p) turing1_tranzicija list
}

(** Iz začetnega stanja, seznama sprejemnih stanj in tranzicij ustvari turingovo napravo z enim tirom.  *)
let ustvari_turing1 zacetno_stanje sprejemna_stanja tranzicije: ('s, 'p) turing1 = 
  let data = {
    tir = { trak = Trak.empty; glava = 0 };
    stanje = zacetno_stanje
  } in { data; sprejemna_stanja; tranzicije }
;;

type ('a,'b) rezultat = Konec of 'a | Naprej of 'a | Napaka of 'a

(* Pomagalni funckiji za branje tirov in iskanje ujemajočih tranzicij. *)
let preberi_tir tir = 
  match Trak.find_opt tir.glava tir.trak with
  | Some p -> p | None -> Prazno
;;
let ujemajoca_tranzicija t_in (t_in',_) = (t_in = t_in');;  

(** Funkcija vrne turingove stroje po enem koraku nedeterminističnega turingovega stroja. *)
let korak turing = 
  let data = turing.data in

  if List.mem data.stanje turing.sprejemna_stanja then [ Konec turing ]
  else
    let s,p = data.stanje, preberi_tir data.tir  in
    let tranzicije = List.find_all (ujemajoca_tranzicija (s,p)) turing.tranzicije in
    if tranzicije = [] then [ Napaka turing ]
    else 
      List.map (fun (_, (s',p',smer)) -> 
        let glava = match smer with
        | Levo  -> data.tir.glava-1
        | Desno -> data.tir.glava+1
        | N     -> data.tir.glava in

        let data = {
          tir = {
            trak = Trak.add data.tir.glava p' data.tir.trak; 
            glava 
          };
          stanje = s' 
        } in Naprej { turing with data }
      ) tranzicije
;;

(** Od [i] naprej nastavlja vrednosti traku na vrednosti v seznamu [input].  *)
let rec nastavi_trak i input trak = 
  match input with
  | [] -> trak
  | h::t -> nastavi_trak (i+1) t (Trak.add i h trak)
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
    tir = { turing.data.tir with trak = nastavi_trak 0 input turing.data.tir.trak };
  } in aux {turing with data}
;;