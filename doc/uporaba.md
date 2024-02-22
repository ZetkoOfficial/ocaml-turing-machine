# Uporaba programa `otm`
V tem odseku je razložena uporaba programa `otm` za prevajanje in pogon turingovih strojev kot opisanih v odseku [Splošno](splosno.md). Pred prvo uporabo je potrebno poklicati ukaz `make` da se program `./otm` zgradi v zunanji direktoriji. V tem odseku bomo označevali mesta kjer uporabnik napiše veljavno vrednost brez presledkov z oznako `( ... )`. Vredno je omembe tudi da so vse vrstice ki se začnejo z znakom `#` ignorirane (so komentarji).

## Obvezni sintaktični elementi 
Vsak veljaven file, ki ga želimo prevesti v turingovo napravo potrebuje opis množice podatkov ki jih lahko pričakuje in opis začetnega stanja in števila tirov. Za ta namen uporabimo naslednji sintaksi:

### Začetni pogoji
`@ (začetno_stanje) (število_tirov)` definira začetno stanje in število tirov za turingovo napravo.

Primer: `@ start 1` definira `1`-tirno turingovo napravo z začetnim stanjem `start`. 

### Veljavni simboli
`$ [ (simbol_1), (simbol_2), ... ]` definira veljavne simbole ki se lahko pojavijo v vhodnih podatkih.

Primer: `$ [_,0,1,2,3,4,5,6,7,8,9]` definira veljaven prazni simbol, ter števila od 0 do 9.

## Tranzicije
Prav tako potrebujemo nekako definirati tranzicije, za kar najprej definirajmo kako podamo pogoj, spremembo in posledico
### Pogoj tranzicije
`(stanje) [ (simbol_1), (simbol_2), ... ]` definira pogoj ki je izpoljnjen 
ko je trenutno stanje enako `(stanje)` in je pod prvim tirom simbol `(simbol_1)`, pod drugim tirom `(simbol_2)`, itd... Alternativno lahko v primeru, da je turingov stroj 1-tiren oglate oklepaje pri seznamu simbolov spustimo. Stanje v pogoju ne sme biti končno (se ne začne z klicajem).
### Sprememba
`(nov_simbol) - ( (l)evo/(d)esno/(n)i )` definira zapis novega simbola na trenutno mesto in premik v eno izmed smeri. 
### Posledica tranzicije
`(novo_stanje) ([ spremembe ])` definira posledico ki spremeni stanje na `(novo_stanje)` in izvede prvo spremebo na prvem traku, drugo na drugem itd... Novo stanje je lahko tudi končno, v katerem primeru ga označimo z klicajem spredaj torej recimo `!konec`.
### Tranzicija
Zdaj lahko celotno tranzicijo iz teh delov popolnoma podamo na dva načina:
#### Enovrstično
`(pogoj) -> (posledica)`, kadar želimo podati tranzicijo z eno samo posledico (ponavadi tako definiramo za deterministične stroje)

Primer: `A _ ->  B [1 - desno]` definira tranzicjo ki če smo v stanju `A` in je pod nami prazen simbol potem spremenimo stanje na `B` in napišemo na trenutno mesto 1, ter se premaknemo desno.  
#### Večvrstično 
```
(pogoj) -> 
| (posledica_1)
| (posledica_2)
| ...
```
kadar želimo istemu pogoju definirati več posledic, ki se nanj nanašajo.

Primer: 
```
A _ ->
| B [_-n]
| C [_-n]
```
definira tranzicijo ki če smo v stanju `A` in je pod nami prazen simbol potem naredi eno izmed dveh posledic (nedeterministično).

## Prevajanje in pogon datotek
Ko smo napisali datoteko, ki opisuje program, jo lahko poskusimo prevesti in pognati.
Za seznam opcij lahko pokličete kar ukaz `./otm` brez argumentov. Ko smo dodali morebitne opcijske argumente lahko zdaj prevedemo in poženemo example program z:
```
./otm lib/example_files/primer1
```
To po datoteko sprva poskušalo parsirati in bo v primeru sintaktičnih napak vrglo dokaj opisni error in vrstico v katerem se pojavi. Potem ga bo poskušal program prevesti, kar tudi morda vrne kako napako z morebitnim opisom kako jo popraviti. Nazadnje, če so vsi prejšni koraki uspešni bo uporabnika prostilo za začetne podatke, ki bodo populirani v prvi tir (glava bo postavljena na prvo mesto v inputu). 

Če se program uspešno izvede bo natistnjeno končno stanje prvega tira. Če je stroj nedeterminističen bo natisnjen tir tiste veje, ki se prva konča.