(** 
  Splošne definicije tipov potrebnih za ustvarjanje in pogon instanc 
  večtirnih turingovih strojev.
*)
module Tipi = struct
  type 'p podatek = Prazno | Podatek of 'p
  type 's stanje  = Stanje of 's
  type smer       = Levo | Desno | N
  module Trak = Map.Make(Int)

  (* 
    Tir predstavimo kot kazalec na trenutni element na traku ter trak,
    kjer trak predstavimo z Map-om da se izognemo problemom pri premikanju
    čez trak.
  *)
  type 'p tir = {
    trak:     ('p podatek) Trak.t;
    glava:    int
  }

  (* Tranzicijo predstavimo kot par (stanja in trenutnimi podatki) in (novega stanja in seznama navodil tirom) *)
  type ('s, 'p) turing_tranzicija = ( 's stanje * ('p podatek list) ) * ( 's stanje * ('p podatek * smer) list )

  (* Tip predstavlja rezultat po koraku turingove naprave. *)
  type 'a rezultat = Konec of 'a | Naprej of 'a | Napaka of 'a

  (* 
    Tip predstavlja trenutno stanje neke turingove naprave z večimi tiri.
  *)
  type ('s, 'p) turing_data = {
    tiri:     'p tir list;
    stanje:   's stanje
  }

  (* 
    Turingovo napravo z večimi tiri predstavimo s trenutnim stanjem naprave,
    seznamom tranzicij in seznamom sprejemnih stanj. 
  *)
  type ('s, 'p) turing = {
    data:               ('s, 'p) turing_data;
    sprejemna_stanja:   ('s stanje) list;
    tranzicije:         ('s, 'p) turing_tranzicija list
  }
end

(** Implementacija nedeterministične večtirne turingove naprave. *)
module NDVT = struct 
  open Tipi

  (** Podmodul, ki vsebuje pomožne funkcije ki jih potrebuje implemenacija. *)
  module Helper = struct 
    (** Pomožna funkcija ki ustvari seznam [n] praznih tirov. *)
    let ustvari_tire n = 
      let rec aux n acc = 
        if n = 0 then acc
        else aux (n-1) ({trak=Trak.empty; glava=0}::acc) in
      aux n []
    ;;

    (** Vrne seznam trentunih podatkov v tirih. *)
    let preberi_tire tiri = 
      List.map (fun tir -> 
        match Trak.find_opt tir.glava tir.trak with
        | Some p -> p | None -> Prazno
      ) tiri
    ;;

    (** Preveri če se [(stanje,podatek)] ujema z neko tranzicijo. *)
    let ujemajoca_tranzicija t_in (t_in',_) = (t_in = t_in');;  

    (** 
      Vsakemu tiru napiše podatek in ga ustrezno premakne, 
      glede na navodila v seznamu [pisanje].    
    *)
    let posodobi_tire tiri pisanje = 
      List.map2 (fun tir (p,smer) ->
        let glava = match smer with
          | Levo  -> tir.glava-1
          | Desno -> tir.glava+1
          | N     -> tir.glava in

        { trak = Trak.add tir.glava p tir.trak; glava }
      ) tiri pisanje
    ;;

    (** Funkcija Od [i] naprej nastavlja vrednosti [prvega] traku na vrednosti v seznamu [input].  *)
    let rec nastavi_input i input tiri = 
      match input with
      | [] -> tiri
      | h::t -> 
        let h',t' = List.hd tiri, List.tl tiri in
        nastavi_input (i+1) t ( {h' with trak = Trak.add i h h'.trak} :: t' )
    ;;
  end

  (** Iz začetnega stanja, seznama sprejemnih stanj in tranzicij ustvari turingovo napravo z [n] tiri.  *)
  let ustvari n zacetno_stanje sprejemna_stanja tranzicije: ('s, 'p) turing = 
    let data = {
      tiri = Helper.ustvari_tire n;
      stanje = zacetno_stanje
    } in { data; sprejemna_stanja; tranzicije }
  ;;

  (* Pomožni funckiji za branje tirov in iskanje ujemajočih tranzicij. *)

  (** Funkcija vrne rezultate po enem koraku nedeterminističnega turingovega stroja. *)
  let korak turing = 
    let data = turing.data in

    if List.mem data.stanje turing.sprejemna_stanja then [ Konec turing ]
    else
      let s,p = data.stanje, Helper.preberi_tire data.tiri in
      let tranzicije = List.find_all (Helper.ujemajoca_tranzicija (s,p)) turing.tranzicije in
      if tranzicije = [] then [ Napaka turing ]
      else 
        List.map (fun (_, (s', pisanje)) -> 
          let data = {
            tiri = Helper.posodobi_tire data.tiri pisanje;
            stanje = s' 
          } in Naprej { turing with data }
        ) tranzicije
  ;;

  (** Požene nedeterministični večtirni turingov stroj s podanim [input]-om. *)
  let pozeni turing input = 
    let rec aux turing = 
      match korak turing with
      | [Konec   turing'] -> Konec turing'
      | [Naprej  turing'] -> aux turing'
      | [error] -> error
      | _ -> print_endline "Stroj ni determinističen!"; Napaka turing in

    let data = { turing.data with
      tiri = Helper.nastavi_input 0 input turing.data.tiri
    } in aux {turing with data}
  ;;

end