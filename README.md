# Opomba 
Trenutno projekt še ni popolnoma dokončan, stvari, ki jih načrtujem do končnega roka oddaje pa so med drugim:
- Izboljšana napaka za manjkajoče tranzicije (napisalo bo tudi kateri pogoj je manjkajoč ne le število manjkajočih) 
- Debugiranje(za deterministične) po korakih, da lahko uporabnik počasi opazuje kaj se dogaja s turingovo napravo na vsakem koraku po vseh tirih. 
- Polepšana dokumentacija

Vseeno pa prosim za vaše mnenje/vprašanja o trenutnem stanju projekta in stvari, ki bi jih želeli da popravim oziroma dodam. 

V tem projektu sem se bolj osredotočil na izdelavo compilerja, ki naj bi omogočil lažje pisanje veljavnih turingovih programov (zato ima dokaj lep izgled v terminalu, vrže opisne sintaktične/semantične napake, itd...), kot na izdelavo nekega grafičnega vmesnika ki bi deloval v brskalniku. Upam, da je to za ta projekt spejemljivo.

# Specifikacija in navodila uporabe
O definiciji (več)tirnega (ne)determističnega turingovega stroja, ki jo uporavljam v tem projektu si lahko preberete odsek [Splošno](doc/splosno.md).

Pred začetkom uporabe pa je močno priporočjivo da si preberete odsek [Uporaba](doc/uporaba.md).
# Primeri programov
Primere nekaterih programov za turingove stroje lahko poženemo z:
```
./otm lib/example_files/primer1
./otm lib/example_files/primer2
./otm lib/example_files/primer3
./otm lib/example_files/primer4
```
Nahajajo se v mapi `lib/example_files`.

