(* Knjižnica funkcij za parsiranje datotek pred compilanjem. *)

exception InvalidBrackets
exception MissingDefinitions
exception MultipleDefinitions

let trim vrstice = List.map(fun vrstica -> String.trim vrstica) vrstice

let odstrani_komentarje vrstice = 
  List.filter (fun vrstica -> 
    not @@ String.starts_with ~prefix:"#" vrstica && vrstica <> ""
  ) vrstice
;;

(** 
    Trim-ano vrstico spremeni v seznam tokenov ločenih po presledkih.
    obnašanje seznamov tu ni validrano znotraj tokenov.
    
    @raise InvalidBrackets
*)
let v_tokene vrstica = 
  let split = Str.split (Str.regexp " +") vrstica in
  let rec aux parts in_array acc = 
    match parts, in_array with
    | [],true -> raise InvalidBrackets
    | [],false -> List.rev acc
    | h::t,true ->
        let delta = if String.starts_with ~prefix:"[" h then 1 else 0 in
        let delta = if String.ends_with ~suffix:"]" h then delta-1 else delta in

        if delta > 0 then raise InvalidBrackets
        else 
          let status = if delta = 0 then true else false in
          aux t status ( ((List.hd acc)^h) :: (List.tl acc) )
    | h::t,false ->
        let delta = if String.starts_with ~prefix:"[" h then 1 else 0 in
        let delta = if String.ends_with ~suffix:"]" h then delta-1 else delta in

        if delta < 0 then raise InvalidBrackets
        else 
          let status = if delta = 0 then false else true in
          aux t status (h::acc) in

  aux split false []
;;

(** Pretvori token seznama v seznam *)
let v_seznam seznam_token = 
  let seznam_token = Str.global_replace (Str.regexp "\\[\\|\\]") "" seznam_token in
  Str.split (Str.regexp ", *") seznam_token
;;

(** 
    Najde vrstico, ki se začne z tokenom [sym].
    @raise MissingDefinitions če je te vrstice ni 
    @raise MultipleDefinitions če je takih vrstic več
*)
let najdi_sym sym vrstice = 
  match List.filter (fun vrstica -> List.hd vrstica = sym) vrstice with
  | [vrstica] -> vrstica
  | [] -> raise MissingDefinitions
  | _ ->  raise MultipleDefinitions
;;