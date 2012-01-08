
import Data.List

-- INFO pozoro na to kdy jsou velikonoce,  jestli je prestupnej rok a kterym
--      dnem zacina rok

velDen      = 9
prestupny   = True
zacatekRoku = 6    -- 0 .. 6 je Ponedli .. Nedele

type Datum  = (Int, Int, String) -- den mesic svatek
type Datumy = [Datum]

main = putStrLn $ kalendar (minRok ++ datumy ++ dalsiRok) 0 ++ seznamSvatku
    where
        minRok = reverse $ take zacatekRoku (reverse datumy)
        dalsiRok = take presahDalsi datumy

presahDalsi = (-365 - (if prestupny then 1 else 0) - zacatekRoku) `mod` 14

kalendar :: Datumy -> Int -> String
kalendar [] _ = ""
kalendar ds i = stranka i (take 14 ds) ++ "\n" ++ kalendar (drop 14 ds) (i+1)

stranka :: Int -> Datumy -> String
stranka i ds = begStranka ++ begTyden (mesicTydne prvniTyden) (2*i+1)
            ++ tyden prvniTyden
            ++ endTyden
            ++ "&\n" ++ obrazek i ++"\n&\n"
            ++ begTyden (mesicTydne druhyTyden) (2*i+2)
            ++ tyden druhyTyden
            ++ endTyden ++ endStranka
    where
        prvniTyden = take 7 ds
        druhyTyden = drop 7 ds

        tyden :: Datumy -> String
        tyden = concat . map den . zip3 [0..] (repeat i)

mesicTydne :: Datumy -> String
mesicTydne ds
        | prvni == posledni = mesice !! prvni
        | otherwise = mesice !! prvni ++"/"++ mesice !! posledni
    where
        prvni    = proj (ds !! 0)
        posledni = proj (ds !! 6)
        proj (_,m,_) = m

den :: (Int,Int,Datum) -> String
den (d,i,(den, mesic, jmeno)) =
        "\\den{" ++ den' ++
        "}{" ++ dny !! d ++
        "}{" ++ jmenoASvatek ++
        "}{" ++ zvyr ++ "}\n"
    where
        jmenoASvatek = jmeno ++ if svText /= ""
                                    then " (" ++ svText ++ ")"
                                    else ""

        den'   = visual den ++ ".~" ++ visual (mesic+1) ++ "."
        mesic' = mesice !! mesic
        svatek = jeSvatek (den,mesic)
        zvyr | i == 0 && den > 14 || i > 3 && mesic == 0 = "SeaGreen"
             | d == 6 || d == 5 || not (null svatek) = "red"
             | otherwise   = "black"

        svText | null svatek = ""
               | otherwise   = "státní svátek" -- TODO rozbalit svatek

visual :: Int -> String
visual i = ['~' | i < 10 ] ++ show i

jeSvatek :: (Int, Int) -> [String]
jeSvatek (den,mesic) =
        map proj $ filter (\(d,m,_) -> d == den && m == mesic+1) svatky
    where proj (_,_,a) = a

obrazek i = "\\obrazek{" ++ show i ++ "}"


begStranka = "\\begin{stranka}\n"
begTyden m t = "\\begin{tyden}{"++ m ++"}{"++ show t ++". týden}\n"
endTyden   = "\\end{tyden}\n"
endStranka = "\\end{stranka}\n\n"


mesice :: [String]
mesice = [ "Leden", "Únor", "Březen", "Duben"
         , "Květen", "Červen", "Červenec", "Srpen"
         , "Září", "Říjen", "Listopad", "Prosinec"]

dny :: [String]
dny = [ "Pondělí", "Úterý", "Středa", "Čtvrtek"
      , "Pátek", "Sobota", "Neděle" ]

svatky :: [(Int,Int,String)] -- seznam den,mesic
svatky =
    [ (1, 1, "Den obnovy samostatného českého státu")
    , (velDen, 4, "Velikonoční pondělí")
    , (1, 5, "Svátek práce")
    , (8, 5, "Den vítězství")
    , (5, 7, "Den slovanských věrozvěstů Cyrila a Metoděje")
    , (6, 7, "Den upálení mistra Jana Husa")
    , (28, 9, "Den české státnosti")
    , (28, 10, "Den vzniku samostatného československého státu")
    , (17, 11, "Den boje za svobodu a demokracii")
    , (24, 12, "Štědrý den")
    , (25, 12, "1. svátek vánoční")
    , (26, 12, "2. svátek vánoční")
    ]

seznamSvatku :: String
seznamSvatku = "\\begin{svatky}\n" ++ concat (map toList svatky) ++ "\\end{svatky}\n"
    where
        -- TODO prasarna
        toList (den,mesic,nazev) = "\\item[~~~~~~~~~~~~~~~~~~~~~~~~~~~~" ++ show den ++ ". " ++ show mesic ++ ".] " ++ nazev ++ "\n"

mkMesic :: Int -> [(Int,String)] -> Datumy
mkMesic i = map (\(a,b) -> (a,i,b))

datumy :: Datumy
datumy = concat $ zipWith mkMesic [0..]
    -- Leden
    [   [ (1, "Nový rok")
        , (2, "Karina")
        , (3, "Radmila")
        , (4, "Diana")
        , (5, "Dalimil")
        , (6, "Tři králové")
        , (7, "Vilma")
        , (8, "Čestmír")
        , (9, "Vladan")
        , (10, "Břetislav")
        , (11, "Bohdana")
        , (12, "Pravoslav")
        , (13, "Edita")
        , (14, "Radovan")
        , (15, "Alice")
        , (16, "Ctirad")
        , (17, "Drahoslav")
        , (18, "Vladislav")
        , (19, "Doubravka")
        , (20, "Ilona")
        , (21, "Běla")
        , (22, "Slavomír")
        , (23, "Zdeněk")
        , (24, "Milena")
        , (25, "Miloš")
        , (26, "Zora")
        , (27, "Ingrid")
        , (28, "Otýlie")
        , (29, "Zdislava")
        , (30, "Robin")
        , (31, "Marika")
        ]

    -- Unor
    ,   [ (1, "Hynek")
        , (2, "Nela (Hromnice)")
        , (3, "Blažej")
        , (4, "Jarmila")
        , (5, "Dobromila")
        , (6, "Vanda")
        , (7, "Veronika")
        , (8, "Milada")
        , (9, "Apolena")
        , (10, "Mojmír")
        , (11, "Božena")
        , (12, "Slavěna")
        , (13, "Věnceslav")
        , (14, "Valentýn")
        , (15, "Jiřina")
        , (16, "Ljuba")
        , (17, "Miloslav")
        , (18, "Gizela")
        , (19, "Patrik")
        , (20, "Oldřich")
        , (21, "Lenka")
        , (22, "Petr")
        , (23, "Svatopluk")
        , (24, "Matěj")
        , (25, "Liliana")
        , (26, "Dorota")
        , (27, "Alexandr")
        , (28, "Lumír")
        ] ++ [(29, "Horymír a Rufin") | prestupny ]

    -- Brezen
    ,   [ (1, "Bedřich")
        , (2, "Anežka")
        , (3, "Kamil")
        , (4, "Stela")
        , (5, "Kazimír")
        , (6, "Miroslav")
        , (7, "Tomáš")
        , (8, "Gabriela")
        , (9, "Františka")
        , (10, "Viktorie")
        , (11, "Anděla")
        , (12, "Řehoř")
        , (13, "Růžena")
        , (14, "Rút, Matylda")
        , (15, "Ida")
        , (16, "Elena, Herbert")
        , (17, "Vlastimil")
        , (18, "Eduard")
        , (19, "Josef")
        , (20, "Světlana")
        , (21, "Radek")
        , (22, "Leona")
        , (23, "Ivona")
        , (24, "Gabriel")
        , (25, "Marián")
        , (26, "Emanuel")
        , (27, "Dita")
        , (28, "Soňa")
        , (29, "Taťána")
        , (30, "Arnošt")
        , (31, "Kvido")
        ]

    -- Duben
    ,   [ (1, "Hugo")
        , (2, "Erika")
        , (3, "Richard")
        , (4, "Ivana")
        , (5, "Miroslava")
        , (6, "Vendula")
        , (7, "Heřman, Heřmína")
        , (8, "Ema")
        , (9, "Dušan")
        , (10, "Darja")
        , (11, "Izabela")
        , (12, "Julius")
        , (13, "Aleš")
        , (14, "Vincenc")
        , (15, "Anastázie")
        , (16, "Irena")
        , (17, "Rudolf")
        , (18, "Valérie")
        , (19, "Rostislava")
        , (20, "Marcela")
        , (21, "Alexandra")
        , (22, "Evžénie")
        , (23, "Vojtěch")
        , (24, "Jiří")
        , (25, "Marek")
        , (26, "Oto")
        , (27, "Jaroslav")
        , (28, "Vlastislav")
        , (29, "Robert")
        , (30, "Blahoslav")
        ]

    -- Kveten
    ,   [ (1, "Svátek práce")
        , (2, "Zikmund")
        , (3, "Alexej")
        , (4, "Květoslav")
        , (5, "Klaudie")
        , (6, "Radoslav")
        , (7, "Stanislav")
        , (8, "Státní svátek")
        , (9, "Ctibor")
        , (10, "Blažena")
        , (11, "Svatava")
        , (12, "Pankrác")
        , (13, "Servác (Den Matek)")
        , (14, "Bonifác")
        , (15, "Žofie")
        , (16, "Přemysl")
        , (17, "Aneta")
        , (18, "Nataša")
        , (19, "Ivo")
        , (20, "Zbyšek")
        , (21, "Monika")
        , (22, "Emil")
        , (23, "Vladimír")
        , (24, "Jana")
        , (25, "Viola")
        , (26, "Filip")
        , (27, "Valdemar")
        , (28, "Vilém")
        , (29, "Maxmilián")
        , (30, "Ferdinand")
        , (31, "Kamila")
        ]

    -- Cerven
    ,   [ (1, "Laura")
        , (2, "Jarmil")
        , (3, "Tamara")
        , (4, "Dalibor")
        , (5, "Dobroslav")
        , (6, "Norbert")
        , (7, "Iveta")
        , (8, "Medard")
        , (9, "Stanislava")
        , (10, "Otta")
        , (11, "Bruno")
        , (12, "Antonie")
        , (13, "Antonín")
        , (14, "Roland")
        , (15, "Vít")
        , (16, "Zbyněk")
        , (17, "Adolf")
        , (18, "Milan")
        , (19, "Leoš")
        , (20, "Květa")
        , (21, "Alois")
        , (22, "Pavla")
        , (23, "Zdeňka")
        , (24, "Jan")
        , (25, "Ivan")
        , (26, "Adriana")
        , (27, "Ladislav")
        , (28, "Lubomír")
        , (29, "Petr a Pavel")
        , (30, "Šárka")
        ]

    -- Cervenec
    ,   [ (1, "Jaroslava")
        , (2, "Patricie")
        , (3, "Radomír")
        , (4, "Prokop")
        , (5, "Cyril a Metoděj")
        , (6, "Mistr Jan Hus")
        , (7, "Bohuslava")
        , (8, "Nora")
        , (9, "Drahoslava")
        , (10, "Libuše")
        , (11, "Olga")
        , (12, "Bořek")
        , (13, "Markéta")
        , (14, "Karolína")
        , (15, "Jindřich")
        , (16, "Luboš")
        , (17, "Martina")
        , (18, "Drahomíra")
        , (19, "Čeněk")
        , (20, "Ilja")
        , (21, "Vítězslav")
        , (22, "Magdaléna")
        , (23, "Libor")
        , (24, "Kristýna")
        , (25, "Jakub")
        , (26, "Anna")
        , (27, "Věroslav")
        , (28, "Viktor")
        , (29, "Marta")
        , (30, "Bořivoj")
        , (31, "Ignác")
        ]

    -- Srpen
    ,   [ (1, "Oskar")
        , (2, "Gustav")
        , (3, "Miluše")
        , (4, "Dominik")
        , (5, "Kristián a Milivoj")
        , (6, "Oldřiška")
        , (7, "Lada")
        , (8, "Soběslav")
        , (9, "Roman")
        , (10, "Vavřinec")
        , (11, "Zuzana")
        , (12, "Klára")
        , (13, "Alena")
        , (14, "Alan a Sylva")
        , (15, "Hana")
        , (16, "Jáchym")
        , (17, "Petra")
        , (18, "Helena")
        , (19, "Ludvík")
        , (20, "Bernard")
        , (21, "Johana")
        , (22, "Bohuslav")
        , (23, "Sandra")
        , (24, "Bartoloměj")
        , (25, "Radim")
        , (26, "Luděk")
        , (27, "Otakar")
        , (28, "Augustýn")
        , (29, "Evelína")
        , (30, "Vladěna")
        , (31, "Pavlína")
        ]

    -- Zari
    ,   [ (1, "Linda, Samuel")
        , (2, "Adéla")
        , (3, "Bronislav")
        , (4, "Jindřiška")
        , (5, "Boris")
        , (6, "Boleslav")
        , (7, "Regina")
        , (8, "Mariana")
        , (9, "Daniela")
        , (10, "Irma")
        , (11, "Denisa")
        , (12, "Marie")
        , (13, "Lubor")
        , (14, "Radka")
        , (15, "Jolana")
        , (16, "Ludmila")
        , (17, "Naděžda")
        , (18, "Kryštof")
        , (19, "Zita")
        , (20, "Oleg")
        , (21, "Matouš")
        , (22, "Darina")
        , (23, "Berta")
        , (24, "Jaromír")
        , (25, "Zlata")
        , (26, "Andrea")
        , (27, "Jonáš")
        , (28, "Václav")
        , (29, "Michal")
        , (30, "Jeroným")
        ]

    -- Rijen
    ,   [ (1, "Igor")
        , (2, "Olívie, Oliver")
        , (3, "Bohumil")
        , (4, "František")
        , (5, "Eliška")
        , (6, "Hanuš")
        , (7, "Justýna")
        , (8, "Věra")
        , (9, "Štefan, Sára")
        , (10, "Marina")
        , (11, "Andrej")
        , (12, "Marcel")
        , (13, "Renáta")
        , (14, "Agáta")
        , (15, "Tereza")
        , (16, "Havel")
        , (17, "Hedvika")
        , (18, "Lukáš")
        , (19, "Michaela")
        , (20, "Vendelín")
        , (21, "Brigita")
        , (22, "Sabina")
        , (23, "Teodor")
        , (24, "Nina")
        , (25, "Beáta")
        , (26, "Erik")
        , (27, "Šarlota")
        , (28, "")
        , (29, "Silvie")
        , (30, "Tadeáš")
        , (31, "Štěpánka")
        ]

    -- Listopad
    ,   [ (1, "Felix")
        , (2, "Památka zesnulých")
        , (3, "Hubert")
        , (4, "Karel")
        , (5, "Miriam")
        , (6, "Liběna")
        , (7, "Saskie")
        , (8, "Bohumír")
        , (9, "Bohdan")
        , (10, "Evžen")
        , (11, "Martin")
        , (12, "Benedikt")
        , (13, "Tibor")
        , (14, "Sáva")
        , (15, "Leopold")
        , (16, "Otmar")
        , (17, "Mahulena")
        , (18, "Romana")
        , (19, "Alžběta")
        , (20, "Nikola")
        , (21, "Albert")
        , (22, "Cecílie")
        , (23, "Klement")
        , (24, "Emílie")
        , (25, "Kateřina")
        , (26, "Artur")
        , (27, "Xenie")
        , (28, "René")
        , (29, "Zina")
        , (30, "Ondřej")
        ]

    -- Prosinec
    ,   [ (1, "Iva")
        , (2, "Blanka")
        , (3, "Svatoslav")
        , (4, "Barbora")
        , (5, "Jitka")
        , (6, "Mikuláš")
        , (7, "Ambrož")
        , (8, "Květoslava")
        , (9, "Vratislav")
        , (10, "Julie")
        , (11, "Dana")
        , (12, "Simona")
        , (13, "Lucie")
        , (14, "Lýdie")
        , (15, "Radana")
        , (16, "Albína")
        , (17, "Daniel")
        , (18, "Miloslav")
        , (19, "Ester")
        , (20, "Dagmar")
        , (21, "Natálie")
        , (22, "Šimon")
        , (23, "Vlasta")
        , (24, "Adam a Eva")
        , (25, "Boží hod")
        , (26, "Štěpán")
        , (27, "Žaneta")
        , (28, "Bohumila")
        , (29, "Judita")
        , (30, "David")
        , (31, "Silvestr")
        ]
    ]
