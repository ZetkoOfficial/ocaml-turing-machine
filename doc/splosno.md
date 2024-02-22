# Splošno o turingovih napravah
Turingovo napravo bomo v tem projektu definirali podobno kot je definirana na [Wikipediji](https://en.wikipedia.org/wiki/Turing_machine), le z morda malce drugačno terminologijo, ki bolje representira kako jih bomo implementirali v kodi.
## Definicija
Najprej definirajmo množici *podatkov* $P$ in *stanj* $S$, kjer je
- $\\_ \in P$ *prazni podatek*
- vsak element $S$ oblike $!s$ imenujemo *končno stanje*.

Definirajmo še neskončen *trak* katerega elementi so elementi množice podatkov in 
*glavo* ki kaže na nek element traku.
Zdaj lahko definiramo *tir* kot zbirko traku in glave, ki kaže na njega.

*Tranzcijo n-tirne turingove naprave* definiramo dvodelno iz:
- *pogoja*, ki je element množice $S \times P^n$ in predstavlja kombinacijo stanja in podatkov pod glavami tirov, ki jo tranzicija pričakuje
- *posledice*, ki je element množice $S \times (P \times \{levo,desno,nič\})^n$ ki predstavlja spremembo stanja po tranziciji, spremebe podatkov pod glavami in premike glav v neko smer.

*Turingov stroj z n-tiri* je potem zbirka $P$,$S$, množice tranzicij, katerih pogoji opisujejo vse možne kombinacije(kjer se omejimo na nekončna stanja), stanja in $n$ tirov. Vredno je omembe, da so tiri in stanje edini deli stroja, ki se spreminjajo med delovanjem.

Prav tako je vredno omembe, da v tej definiciji dovolimo več tranzicij z istim pogojem, in v tem primeru se njihove posledice izvedejo paralelno. Takim napravam
pravimo *nedeterministične*, medtem ko napravam kjer je vsak pogoj definiran netanko enkrat pravimo *deterministične*.

## Delovanje
Turingovem stroju lahko najprej nastavimo neke začetne podatke tirov, čemur rečemo *vhodni podatki*. Sprva vse glave kažejo na začetni element trakov in je stanje turnigovega stroja nastavljeno na začetno stanje.

Med delovanjem turingovega stroja se ponavlja nasledji koraki:
1. **preverimo če je stanje končno**, in v primeru da je vse stroje ustavimo
2. **najdemo tranzicije z veljavnim pogojem**
3. **vzporedno posodobimo/ustvarimo nove stroje z uveljavljenimi poslediciami** za vsako tako tranzicijo (če je determinističen potem bo v vsakem trenutnku stroj le en) 

Tak turingov stroj se lahko *ustavi* ali pa *ne ustavi* in določanje tega kaj se bo s strojem zgodilo je nemogoče določiti z algoritom vnaprej v splošnem (glej [Halting Problem](https://en.wikipedia.org/wiki/Halting_problem)).

## Opombe
Iz definicije je očitno, da potrebujemo za veljavni turingov stroj definirati najmanj 
$|S'||P|^n$ tranzicij, kjer je $S'$ množica nekočnih stanj. Ker to število lahko kar hitro naraste je ključno da poskusimo najti uravnoteženo razmerje med številom trirov, številom stanj in podatkov da definicije tranzicij niso ne prezahtevne ne preštevilčne. 
