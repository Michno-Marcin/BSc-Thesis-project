# ZAINSTALOWANIE, ORAZ WCZYTANIE NIEZBĘDNYCH BIBLIOTEK DLA PROJEKTU
if (!("readr" %in% rownames(installed.packages()))) install.packages("readr")
library("readr") # Wczytanie danych formatu csv (read_csv())
if (!("readxl" %in% rownames(installed.packages()))) install.packages("readxl")
library("readxl") # Wczytanie danych formatu: xlsx, xls (read_excel())
if (!("stringr" %in% rownames(installed.packages()))) install.packages("stringr")
library(stringr) # Rozdzielenie kolumn ramek danych z datą na lata i miesiące (str_split())
if (!("dplyr" %in% rownames(installed.packages()))) install.packages("dplyr")
library("dplyr") # Filtrowanie / wybieranie danych z ramki (na podst. daty przy użyciu between())
if (!("psych" %in% rownames(installed.packages()))) install.packages("psych")
library("psych") # Wyznaczenie średniej geometrycznej i harmonicznej
if (!("stats" %in% rownames(installed.packages()))) install.packages("stats")
library("stats") # Funkcje statystyczne (np. wariancja), testy korelacji, wykresy gęstości
if (!("ie2misc" %in% rownames(installed.packages()))) install.packages("ie2misc")
library("ie2misc") # Wyznaczenie współczynnika zmienności (V_Q)
if (!("moments" %in% rownames(installed.packages()))) install.packages("moments")
library("moments") # Wyznaczenie współczynnika skośności, kurtozy (skewness(), curtosis())
if (!("writexl" %in% rownames(installed.packages()))) install.packages("writexl")
library("writexl") # Zapisanie tabeli w formie pliku z formatem xlsx
if (!("ggpubr" %in% rownames(installed.packages()))) install.packages("ggpubr")
library("ggpubr") # Wykresy rozrzutu (np. ggscatter())
if (!("gridExtra" %in% rownames(installed.packages()))) install.packages("gridExtra")
library("gridExtra") # Wyświetlenie wielu wykresów typu ggplot (ggscattter) jednocześnie


# WCZYTANIE I OSZACOWANIE/OBLICZENIE DANYCH, KOREKTY NAZW KOLUMN
## DO PONIŻSZEJ ZMIENNEJ NALEŻY PRZYPISAĆ PEŁNĄ ŚCIEŻKĘ ROZPAKOWANYCH ZAŁĄCZNIKÓW !!!
## NP.: sciezka_zalacznikow <- "C:/Inzynierski projekt dyplomowy/załączniki".
## ZNAK SEPARATORA MIĘDZY PODFOLDERAMI W JĘZYKU R TO "/" !!!
sciezka_zalacznikow <- ""

## Pomocnicza funkcja do korekty wartości liczbowych w danych
korekta_wartosci_liczbowych_w_ramce <- function(nazwa_ramki_danych) {
  q <- get(nazwa_ramki_danych)
  if (dim(q)[1] > 3) {
    liczby <- str_split(string = q[, dim(q)[2]], pattern = "", simplify = T)
  } else {
    liczby <- str_split(string = q[1, ], pattern = "", simplify = T)
  }
  liczby_lista <- c()
  for (nr_liczby in 1:ifelse(test = dim(q)[1] > 3, yes = dim(q)[1], no = dim(q)[2])) {
    suppressWarnings({
      # Wyeliminowanie liter z wartości liczbowej, zamiana przecinka na kropkę
      liczba <- gsub(pattern = ",", ".", liczby[nr_liczby, ])[liczby[nr_liczby, ] != ""]
      liczba <- liczba[as.numeric(liczba) >= 0 | liczba == "."]
      liczba <- paste(liczba[!is.na(liczba)], collapse = "")
    })
    liczby_lista <- c(liczby_lista, liczba)
  }
  if (dim(q)[1] > 3) {
    q[, dim(q)[2]] <- liczby_lista
  } else {
    q[1, ] <- liczby_lista
  }
  return(q)
}

## Pomocnicza funkcja do poprawy danych z rzymskimi oznaczeniami dat
korekta_ramek_rzymskich <- function(ramka_danych) {
  q <- ramka_danych
  if (dim(q)[2] > 3) {
    daty_ramka <- str_split_fixed(names(q), " ", 2)
    daty_ramka[, 1] <- as.numeric(as.roman(daty_ramka[, 1]))
    nowa_ramka <- cbind(as.numeric(daty_ramka[, 2]), as.numeric(daty_ramka[, 1]), as.numeric(q[1, ]))
    names(nowa_ramka) <- c("Rok", "Miesiac_lub_Kwartal", "Wartosc")
    return(nowa_ramka)
  } else {
    q[, 2] <- as.numeric(as.roman(q[, 2]))
    return(q)
  }
}


## Dane z folderu "Gospodarka"
setwd(paste(sciezka_zalacznikow, "/Dane użyte w projekcie/Gospodarka", sep = ""))
stopy_renty_międzybankowe <- as.data.frame(read_csv(
  "miesięcznie - 3-miesięczne lub 90-dniowe stopy i renty Stopy międzybankowe dla Polski.csv"
))
dlug_publiczny_kwart <- as.data.frame(
  read_excel("dług publiczny kwartalnie.xlsx", range = "A7:B56", col_names = F)
)
names(dlug_publiczny_kwart) <- c("Rok, kwartal", "Wartosc dlugu")
handel_detaliczny <- as.data.frame(
  read_csv("Handel detaliczny ogółem miesięcznie z korektą sezonową.csv")
)
kurs_dolara_mies <- as.data.frame(read_csv("kurs dolara - miesięcznie.csv"))
kurs_dolara_rocznie <- as.data.frame(read_csv("kurs dolara - rocznie.csv"))
Produkcja_przemyslu_mies <- as.data.frame(
  read_csv("Produkcja przemysłu ogółem, korekta sezonowa, mies.csv")
)
produkt_krajowy_kwart <- as.data.frame(
  read_csv("Realny produkt krajowy brutto dla Polski (co 3 mies).csv")
)
efektywny_kurs_walutowy_mies <- as.data.frame(read_csv(
  "Realny szeroki efektywny kurs walutowy miesięcznie bez korekty sezonowej.csv"
))
eksport_towarow <- as.data.frame(read_csv("Wartość eksportu towarów.csv"))
import_mies_bez_korekty <- as.data.frame(read_csv("Wartość importu - miesięcznie bez korekty.csv"))
import_mies_z_korekta <- as.data.frame(
  read_csv("Wartość importu - miesięcznie z korektą sezonową.csv")
)
import_rocznie_z_korekta <- as.data.frame(
  read_csv("Wartość importu - rocznie z korektą sezonową.csv")
)

### Dane z podfolderu "Ceny"
setwd(paste(sciezka_zalacznikow, "/Dane użyte w projekcie/Gospodarka/Ceny", sep = ""))
bulka_pszenna <- as.data.frame(read_excel("bulka_pszenna.xlsx",
  sheet = "DANE",
  range = "D1:E23", col_names = T
))
chleb_pszenno_zytni <- as.data.frame(read_excel(
  "chleb_pszenno-zytni.xlsx",
  sheet = "DANE", col_names = F, range = "D4:E23"
))
names(chleb_pszenno_zytni) <- c("Rok", "Cena")
Ceny_akcji <- as.data.frame(read_csv("Łączne ceny akcji dla Polski.csv"))
maka_pszenna <- as.data.frame(read_excel(
  "maka_pszenna.xlsx",
  sheet = "DANE", col_names = F, range = "D2:E23"
))
names(maka_pszenna) <- c("Rok", "Cena")
Cena_m2_dzialki <- as.data.frame(read_excel(
  "Cena 1m2 powierzchni użytkowej budynku mieszkalnego oddanego do użytkowania.xlsx",
  sheet = 2, range = "C1:E90"
)[c(2, 1, 3)])
Cena_m2_dzialki <- korekta_ramek_rzymskich(Cena_m2_dzialki)
cena_produktu_krajowego_brutto <- as.data.frame(read_csv(
  "Cena bieżąca produktu krajowego brutto (co 3 mies).csv"
))

### Dane z podfolderu "Wskaźniki, indeksy"
setwd(paste(sciezka_zalacznikow, "/Dane użyte w projekcie/Gospodarka/Wskaźniki, indeksy", sep = ""))
M2_mies <- as.data.frame(read_csv("M2 wskaźnik miesięcznie.csv"))
M3_bez_korekty_mies <- as.data.frame(read_csv("M3 wskaźnik miesięcznie bez korekty.csv"))
M3_z_korekta_mies <- as.data.frame(read_csv("M3 wskaźnik miesięcznie z korektą sezonową.csv"))
M3_bez_korekty_rocznie <- as.data.frame(read_csv("M3 wskaźnik rocznie bez korekty.csv"))
inflacja <- as.data.frame(read_excel(
  "miesieczne_wskazniki_cen_towarow_i_uslug_konsumpcyjnych_od_1982_roku.xlsx",
  range = "C2:F1945", col_names = F
))
inflacja <- inflacja[inflacja[, 1] == "Poprzedni miesiąc = 100", 2:4]
names(inflacja) <- c("Rok", "Miesiac", "Wartosc")
inflacja <- inflacja[dim(inflacja)[1]:1, ] # Zamiana wierszy w celu porządku chronologicznego

Indeks_cen_konsump_bez_zywn_energ <- as.data.frame(read_csv(
  "Indeks cen konsumpcyjnych Wszystkie pozycje z wyłączeniem żywności i energii .csv"
))
Indeks_cen_konsump_ogolem <- as.data.frame(read_csv(
  "Indeks cen producentów Dobra konsumpcyjne ogółem - miesięcznie.csv"
))
Indeks_cen_producja_przem_mies <- as.data.frame(read_csv(
  "Indeks cen producentów krajowych Produkcja przemysłowa - miesięcznie.csv"
))
Indeks_cen_producja_przem_rocznie <- as.data.frame(read_csv(
  "Indeks cen producentów krajowych Produkcja przemysłowa - rocznie.csv"
))
Indeks_cen_czynsz_napr_utrz_mies <- as.data.frame(read_csv(
  "Wskaźnik cen konsumpcyjnych Czynsze, naprawy i utrzymanie - miesięcznie bez korekty sezonowej.csv"
))
Indeks_cen_energia <- as.data.frame(read_excel(
  "Wskaźnik cen towarów i usług konsumpcyjnych Energia dla Polski.xls",
  range = "A11:B344"
))
Indeks_cen_all_mies <- as.data.frame(read_csv(
  "Wskaźnik cen towarów i usług konsumpcyjnych Wszystkie pozycje dla Polski - miesięcznie.csv"
))
Indeks_cen_all_rocznie <- as.data.frame(read_csv(
  "Wskaźnik cen towarów i usług konsumpcyjnych Wszystkie pozycje dla Polski - rocznie.csv"
))
Indeks_cen_zywnosc <- as.data.frame(read_csv(
  "Wskaźnik cen towarów i usług konsumpcyjnych Żywność.csv"
))
Indeks_cen_administrowane_mies <- as.data.frame(read_csv(
  "Zharmonizowany wskaźnik cen konsumpcyjnych Ceny Całkowicie Administrowane - miesięcznie.csv"
))
Indeks_cen_gaz <- as.data.frame(read_csv(
  "Zharmonizowany wskaźnik cen konsumpcyjnych Gaz dla Polski.csv"
))
Indeks_cen_konserwacja <- as.data.frame(read_csv(
  "Zharmonizowany wskaźnik cen konsumpcyjnych Konserwacja i naprawa mieszkań.csv"
))
Indeks_cen_mieszkanie <- as.data.frame(read_csv(
  "Zharmonizowany wskaźnik cen konsumpcyjnych Mieszkanie, woda, energia elektryczna, gaz i inne paliwa.csv"
))
Indeks_cen_oleje <- as.data.frame(read_csv(
  "Zharmonizowany wskaźnik cen konsumpcyjnych Oleje i tłuszcze .csv"
))
Indeks_cen_wakacyjne <- as.data.frame(read_excel(
  "Zharmonizowany wskaźnik cen konsumpcyjnych Pakiety wakacyjne dla Polski.xls",
  range = "A11:B333"
))
Indeks_cen_restauracje_hotele <- as.data.frame(read_csv(
  "Zharmonizowany wskaźnik cen konsumpcyjnych Restauracje i hotele.csv"
))
Indeks_cen_transport <- as.data.frame(read_csv(
  "Zharmonizowany wskaźnik cen konsumpcyjnych Transport .csv"
))
Indeks_cen_rekreacja_opieka_z_wyl <- as.data.frame(read_csv(
  "Zharmonizowany wskaźnik cen konsumpcyjnych usługi związane z rekreacją i opieką osobistą, z wył.csv"
))
Indeks_cen_wod_napoje_bezalkoholowe_soki <- as.data.frame(read_csv(
  "Zharmonizowany wskaźnik cen konsumpcyjnych Wody mineralne, napoje bezalkoholowe oraz soki owocowe i warzywne.csv"
))
Indeks_cen_zywnosc_napoje_bezalkoholowe <- as.data.frame(read_csv(
  "Zharmonizowany wskaźnik cen konsumpcyjnych Żywność i napoje bezalkoholowe.csv"
))

### Dane z podfolderu "Praca, dochody, wydatki"
setwd(paste(sciezka_zalacznikow, "/Dane użyte w projekcie/Gospodarka/Praca, dochody, wydatki", sep = ""))
#### Wczytanie danych dot. rejestracji bezrobotnych
#### Przypisanie im odpowiednich oznaczeń dat
bezrobotni_rejestracje_2010_2022_mies <- as.data.frame(read_excel(
  "Bezrobotni zarejestrowani, bezrobotni nowo zarejestrowani, bezrobotni wyrejestrowani - miesięcznie od 2010.xlsx",
  range = "b1:ev5"
))
bezrobotni_zarejestr_2010_2022_mies <- bezrobotni_rejestracje_2010_2022_mies[1, ]
bezrobotni_nowo_zarejestr_2010_2022_mies <- bezrobotni_rejestracje_2010_2022_mies[2, ]
bezrobotni_wyrejestr_2010_2022_mies <- bezrobotni_rejestracje_2010_2022_mies[3, ]
bezrobotni_wyrejestr_podjecie_pracy_2010_2022_mies <- bezrobotni_rejestracje_2010_2022_mies[4, ]
dane_bezrobotni_rejestracje <- c(
  "bezrobotni_zarejestr_2010_2022_mies",
  "bezrobotni_nowo_zarejestr_2010_2022_mies",
  "bezrobotni_wyrejestr_2010_2022_mies",
  "bezrobotni_wyrejestr_podjecie_pracy_2010_2022_mies"
)
for (ramka_danych in dane_bezrobotni_rejestracje) {
  q <- korekta_ramek_rzymskich(korekta_wartosci_liczbowych_w_ramce(ramka_danych))
  # Oszacowanie brakującej wartości jako średniej z dwóch sąsiednich
  # Brakująca Wartość występuje w sierpniu 2020 r. (pomiędzy rekordami nr: 127, 128)
  assign(
    x = ramka_danych,
    value = rbind(q[1:127, ], c(2020, 8, mean(c(q[127, 3], q[128, 3]))), q[128:nrow(q), ])
  )
}

stopa_bezrobocia_mies_od_2010 <- as.data.frame(read_excel(
  "Stopa bezrobocia miesięcznie od 2010 .xlsx",
  range = "c1:ew2"
))
stopa_bezrobocia_mies_od_2010 <- korekta_ramek_rzymskich(stopa_bezrobocia_mies_od_2010)
q <- stopa_bezrobocia_mies_od_2010
q <- rbind(q[1:127, ], c(2020, 8, mean(c(q[127, 3], q[128, 3]))), q[128:nrow(q), ])
stopa_bezrobocia_mies_od_2010 <- q

nieobsadzone_miejsca_pracy_2010_2022_mies_bez_kor <- as.data.frame(read_excel(
  "Całkowita liczba nieobsadzonych miejsc pracy - miesięcznie bez korekty.xls",
  range = "a12:b404", col_names = F
))
nieobsadzone_miejsca_pracy_2010_2022_mies_z_kor <- as.data.frame(read_csv(
  "Całkowita liczba nieobsadzonych miejsc pracy - miesięcznie z korektą sezonową.csv",
  skip = 1, col_names = F
))
nieobsadzone_miejsca_pracy_2010_2022_rocz_bez_kor <- as.data.frame(read_excel(
  "Całkowita liczba nieobsadzonych miejsc pracy - rocznie bez korekty.xls",
  range = "a12:b43", col_names = F
))
nieobsadzone_miejsca_pracy_2010_2022_rocz_bez_kor <- as.data.frame(read_excel(
  "Całkowita liczba nieobsadzonych miejsc pracy - rocznie bez korekty.xls",
  range = "a12:b43", col_names = F
))

kwoty_bazowe_1999_2022_rocz_bez_kor <- as.data.frame(read_excel(
  "Kwoty bazowe (emerytury, renty obowiązujące od 1 marca kolejnych lat).xlsx",
  range = "a1:x2"
))

nieobsadzone_miejsca_pracy_2010_2022_mies_z_kor <- as.data.frame(read_csv(
  "Stopa bezrobocia rejestrowanego dla Polski (mies. 1990-2022).csv"
))

#### Wczytanie danych dot. bezrobotnych rocznie, przypisanie im odpowiednich oznaczeń dat
dane_bezrobotni <- c(
  "bezrobotni_zarejestr_1998_2021", "bezrobotni_poprzednio_pracujacy_1999_2021",
  "bezrobotni_dotychczas_niepracujacy_1999_2021", "bezrobotni_mezczyzni_1998_2021",
  "bezrobotni_mezczyzni_poprzednio_pracujacy_1999_2021",
  "bezrobotni_mezczyzni_dotychczas_niepracujacy_1999_2021", "bezrobotni_kobiety_1998_2021",
  "bezrobotni_kobiety_poprzednio_pracujace_1999_2021",
  "bezrobotni_kobiety_dotychczas_niepracujace_1999_2021"
)
zakresy_danych_bezrobotni <- c(
  "c5:z5", "ab5:ax5", "az5:bv5", "bw5:ct5", "cv5:dr5",
  "dt5:ep5", "eq5:fn5", "fp5:gl5", "gn5:hj5"
)
for (nr_danych in 1:length(dane_bezrobotni)) {
  assign(x = dane_bezrobotni[nr_danych], value = as.data.frame(read_excel(
    "Bezrobotni zarejestrowani wg płci i typu.xlsx",
    sheet = 2,
    range = zakresy_danych_bezrobotni[nr_danych], col_names = F
  )))
  q <- get(dane_bezrobotni[nr_danych])
  eval(parse(text = 'names(q) <- read_excel(
    "Bezrobotni zarejestrowani wg płci i typu.xlsx", sheet = 2,
    range=gsub(pattern="5",replacement="3",x=zakresy_danych_bezrobotni[nr_danych]), col_name =F)'))
  assign(x = dane_bezrobotni[nr_danych], value = q)
}
dane_bezrobotni
##### W celu wywołania zmiennej z listy użyj jej bezpośredniej nazwy, lub  funkcji get, np.:
get(dane_bezrobotni[1])

#### Wczytanie danych dot. dochodów i przypisanie im odpowiednich oznaczeń dat
dane_dochody <- c(
  "Dochod_na_os_w_gospodarstw_ogolem",
  "Dochod_na_os_w_gospodarstw_z_pracy_na_wlasny_rachunek",
  "Dochod_na_os_w_gospodarstw_do_dyspozycji"
)
zakresy_danych_dochody <- c("C4:Y4", "Z4:AV4", "AW4:BS4")
for (nr_danych in 1:length(dane_dochody)) {
  assign(x = dane_dochody[nr_danych], value = as.data.frame(read_excel(
    "Dochód rozporządzalny na 1 os w gospodarstwie domowym (ogółem, do dyspozycji, na własny rachunek).xlsx",
    sheet = 2, range = zakresy_danych_dochody[nr_danych], col_names = F
  )))
  q <- get(dane_dochody[nr_danych])
  eval(parse(text = 'names(q) <- read_excel(
    "Dochód rozporządzalny na 1 os w gospodarstwie domowym (ogółem, do dyspozycji, na własny rachunek).xlsx",
    sheet = 2, range = gsub(pattern = "4",replacement = "2",
             x = zakresy_danych_dochody[nr_danych]), col_names = F)'))
  assign(x = dane_dochody[nr_danych], value = q)
}
dane_dochody

#### Wczytanie danych dot. przeciętnej liczby osób w gospodarstwie domowym
#### Przypisanie im odpowiednich oznaczeń dat
dane_przecietna_liczba_os_w_gosp <- c(
  "przecietna_liczba_os_w_gospodarstw_ogolnie", "przecietna_liczba_os_w_gospodarstw_pracujacy",
  "przecietna_liczba_os_w_gospodarstw_pobierajacy_swiadczenia",
  "przecietna_liczba_os_w_gospodarstw_na_utrzymaniu"
)
zakresy_danych_przecietna_liczba_os_w_gosp <- c("C4:Y4", "Z4:AV4", "AW4:BS4", "CQ4:DM4")
for (nr_danych in 1:length(dane_przecietna_liczba_os_w_gosp)) {
  assign(x = dane_przecietna_liczba_os_w_gosp[nr_danych], value = as.data.frame(
    read_excel("Przeciętna liczba osób w gospodarstwie ze wzgl. na pracę, świadczenia.xlsx",
      sheet = 2, range = zakresy_danych_przecietna_liczba_os_w_gosp[nr_danych], col_names = F
    )
  ))
  q <- get(dane_przecietna_liczba_os_w_gosp[nr_danych])
  eval(parse(text = 'names(q) <- read_excel(
    "Przeciętna liczba osób w gospodarstwie ze wzgl. na pracę, świadczenia.xlsx",sheet = 2,
    range = gsub(pattern = "4",replacement = "2",x = zakresy_danych_przecietna_liczba_os_w_gosp[
             nr_danych]), col_names = F)'))
  assign(x = dane_przecietna_liczba_os_w_gosp[nr_danych], value = q)
}
dane_przecietna_liczba_os_w_gosp

przecietne_mies_wynagrodzenie_brutto <- as.data.frame(read_excel(
  "Przeciętne miesięczne wynagrodzenia brutto.xlsx",
  range = "C4:V4", col_names = F, sheet = 2
))
names(przecietne_mies_wynagrodzenie_brutto) <- read_excel(
  "Przeciętne miesięczne wynagrodzenia brutto.xlsx",
  sheet = 2, range = "C2:V2", col_names = F
)

#### Wczytanie danych dot. przeciętnych wydatków wg grup,
#### Przypisanie im odpowiednich oznaczeń dat
dane_przecietne_wydatki_wg_grup <- c(
  "przecietne_wydatki_ogolem", "przecietne_wydatki_towary_uslugi_konsump",
  "przecietne_wydatki_zywnosc_napoje_bezlakohol", "przecietne_wydatki_alkoholowe_tytoniowe",
  "przecietne_wydatki_odziez_obuwie", "przecietne_wydatki_uzytkowanie_nosniki_energii",
  "przecietne_wydatki_wyposanie_prowadz_gospodarstw_dom", "przecietne_wydatki_zdrowie",
  "przecietne_wydatki_transport", "przecietne_wydatki_lacznosc",
  "przecietne_wydatki_rekreacja_kultura", "przecietne_wydatki_edukacja",
  "przecietne_wydatki_restauracje_hotele"
)
zakresy_danych_przecietne_wydatki_wg_grup <- c(
  "C4:Z4", "AA4:AX4", "AY4:BV4", "BX4:CT4", "CV4:DR4",
  "DT4:EP4", "ER4:FN4", "FP4:GL4", "GN4:HJ4", "HL4:IH4",
  "IJ4:JF4", "JH4:KD4", "KF4:LB4"
)
for (nr_danych in 1:length(dane_przecietne_wydatki_wg_grup)) {
  assign(x = dane_przecietne_wydatki_wg_grup[nr_danych], value = as.data.frame(
    read_excel("Przeciętne wydatki wg grup na 1 os.xlsx",
      sheet = 2,
      range = zakresy_danych_przecietne_wydatki_wg_grup[nr_danych], col_names = F
    )
  ))
  q <- get(dane_przecietne_wydatki_wg_grup[nr_danych])
  eval(parse(text = 'names(q) <- read_excel("Przeciętne wydatki wg grup na 1 os.xlsx", sheet = 2,
    range = gsub(pattern = "4",replacement = "2",
      x = zakresy_danych_przecietne_wydatki_wg_grup[nr_danych]), col_names = F)'))
  assign(x = dane_przecietne_wydatki_wg_grup[nr_danych], value = q)
}
dane_przecietne_wydatki_wg_grup

zarobki_godzinowe_produkcja <- as.data.frame(read_csv("Zarobki godzinowe produkcja.csv"))
zarobki_godzinowe_sektor_prywatny <- as.data.frame(read_csv(
  "Zarobki miesięczne Sektor prywatny.csv"
))

## Dane z folderu "Komunikacja"
setwd(paste(sciezka_zalacznikow, "/Dane użyte w projekcie/Komunikacja", sep = ""))
rejestracje_sam <- as.data.frame(read_csv(
  "Rejestracje samochodów osobowych - miesięcznie z korektą sezonową.csv"
))
wypadki_drogowe <- as.data.frame(read_excel(
  "Wypadki drogowe, ranni, martwi.xlsx",
  sheet = 2, range = "C4:Y4", col_names = F
))
names(wypadki_drogowe) <- read_excel(
  "Wypadki drogowe, ranni, martwi.xlsx",
  sheet = 2, range = "C2:Y2", col_names = F
)
wypadki_drogowe_martwi <- as.data.frame(read_excel(
  "Wypadki drogowe, ranni, martwi.xlsx",
  sheet = 2, range = "Z4:AV4", col_names = F
))
names(wypadki_drogowe_martwi) <- read_excel(
  "Wypadki drogowe, ranni, martwi.xlsx",
  sheet = 2, range = "Z2:AV2", col_names = F
)
wypadki_drogowe_ranni <- as.data.frame(read_excel(
  "Wypadki drogowe, ranni, martwi.xlsx",
  sheet = 2, range = "AW4:BS4", col_names = F
))
names(wypadki_drogowe_ranni) <- read_excel(
  "Wypadki drogowe, ranni, martwi.xlsx",
  sheet = 2, range = "AW2:BS2", col_names = F
)

## Dane z folderu "Leczenie, choroby"
setwd(paste(sciezka_zalacznikow, "/Dane użyte w projekcie/Leczenie, choroby", sep = ""))
leczenie_alkoholowe <- as.data.frame(read_excel(
  "leczenie-alkoholowe-wykaz-alk-w-latach-2000-2020 rocznie.xlsx",
  range = "A10:B30", col_names = F
))
names(leczenie_alkoholowe) <- c("ROK", "Ilosc")

wydatki_sluzba_zdrowia <- as.data.frame(read_csv("poland-healthcare-spending.csv", skip = 16))[, 1:3]
wskaznik_cen_konsump_zdrowie <- as.data.frame(read_csv(
  "Zharmonizowany wskaźnik cen konsumpcyjnych Zdrowie dla Polski 1996 - 2022.csv"
))

COVID <- as.data.frame(read_excel(
  "COVID dziennie przypadki, zgony, wyleczenia.xlsx",
  sheet = 2, range = "B6:E743", col_names = F
))
COVID[, 1:6] <- cbind(str_split_fixed(COVID[, 1], "/", n = 3)[, c(3, 1, 2)], COVID[, 2:4])
names(COVID) <- c("Rok", "Miesiac", "Dzien", "Potwierdzenia", "Zgony", "Wyzdrowienia")

gruzlica <- as.data.frame(read_excel("gruźlica.xlsx", sheet = 2, range = "C5:Y5", col_names = F))
names(gruzlica) <- as.data.frame(read_excel("gruźlica.xlsx", sheet = 2, range = "C3:Y3", col_names = F))

AIDS <- as.data.frame(read_excel("aids.xlsx", sheet = 2, range = "C4:Y4", col_names = F))
names(AIDS) <- as.data.frame(read_excel("aids.xlsx", sheet = 2, range = "C2:Y2", col_names = F))

COVID_szczepienia <- as.data.frame(read_excel(
  "Liczba szczepień covid dziennie.xlsx",
  sheet = 2, range = "B6:C657", col_names = F
))
COVID_szczepienia[, 1:4] <- cbind(str_split_fixed(
  COVID_szczepienia[, 1],
  pattern = "[/]", n = 3
)[, c(3, 1, 2)], COVID_szczepienia[, 2])
names(COVID_szczepienia) <- c("Rok", "Miesiac", "Dzien", "Ilosc szczepien")

zachorowania_weneryczne <- as.data.frame(read_excel("Nowe zachorowania na choroby weneryczne.xlsx",
  col_names = F, range = "C4:Y4", sheet = 2
))
names(zachorowania_weneryczne) <- as.data.frame(read_excel(
  "Nowe zachorowania na choroby weneryczne.xlsx",
  range = "C2:Y2", sheet = 2, col_names = F
))

co2_emisje <- as.data.frame(read_csv("poland-carbon-co2-emissions.csv", skip = 16))[, 1:3]
gazy_cieplarniane_emisje <- as.data.frame(
  read_csv("poland-ghg-greenhouse-gas-emissions.csv", skip = 16)
)[, 1:3]

## Dane z folderu "Ludność"
setwd(paste(sciezka_zalacznikow, "/Dane użyte w projekcie/Ludność", sep = ""))
ludnosc_OGOLEM_2011_22_mies <- as.data.frame(read_excel(
  "Ludność 2011_2022 miesiecznie.xlsx",
  range = "a24:b159", col_names = F
))
ludnosc_OGOLEM_2011_22_mies[, 1:3] <- cbind(
  str_split_fixed(ludnosc_OGOLEM_2011_22_mies$...1, " M", 2), ludnosc_OGOLEM_2011_22_mies[, 2]
)
names(ludnosc_OGOLEM_2011_22_mies) <- c("Rok", "Miesiac", "Ludnosc")
ludnosc_OGOLEM_1950_2020 <- as.data.frame(read_excel(
  "Ludność, ruch naturalny i migracje w latach 1946-2020 (GUS).xls",
  range = "a8:b78", col_names = c("Rok", "Ludnosc w tys."), sheet = 1
))

### Wczytanie danych dot. ludności i przypisanie im odpowiednich oznaczeń dat
dane_ludnosc <- c(
  "malzenstwa_1950_2020", "rozwody_1950_2020", "urodzenia_zywe_1950_2020",
  "zgony_smiertelnosc_1950_2020", "urodzenia_martwe_1950_2020", "imigracja_zagraniczna_1950_2020",
  "emigracja_zagraniczna_1950_2020"
)
zakresy_danych_ludnosc <- c("c8:c78", "d8:d78", "e8:e78", "f8:f78", "g8:g78", "l8:l78", "m8:m78")
zakresy_danych_ludnosc_2 <- c("p8:p78", "q8:q78", "r8:r78", "s8:s78")
lata_z_pliku <- as.data.frame(read_excel(
  "Ludność, ruch naturalny i migracje w latach 1946-2020 (GUS).xls",
  range = "a8:a78", col_names = F
))
i <- 1
for (nr_danych in 1:length(dane_ludnosc)) {
  # Wczytanie zbioru danych
  assign(dane_ludnosc[nr_danych], cbind(lata_z_pliku, as.data.frame(read_excel(
    "Ludność, ruch naturalny i migracje w latach 1946-2020 (GUS).xls",
    range = zakresy_danych_ludnosc[nr_danych], col_names =
      gsub("_1950_2020", " w tys.", dane_ludnosc[nr_danych]), sheet = i
  ))))
  # Poprawka nazw kolumn w niektórych zbiorach
  if (nr_danych < 5) {
    assign(
      x = dane_ludnosc[nr_danych],
      value = cbind(get(dane_ludnosc[nr_danych]), as.data.frame(read_excel(
        "Ludność, ruch naturalny i migracje w latach 1946-2020 (GUS).xls",
        range = zakresy_danych_ludnosc_2[nr_danych], sheet = i,
        col_names = gsub(
          pattern = "w tys.", replacement = "na 1000 ludności",
          x = names(get(dane_ludnosc[nr_danych]))[2]
        )
      )))
    )
  }
}
for (ramka_danych in dane_ludnosc) {
  assign(x = ramka_danych, value = korekta_wartosci_liczbowych_w_ramce(ramka_danych))
}

uchodzcy_1991_2021 <- as.data.frame(read_csv("poland-refugee-statistics.csv", skip = 16))
uchodzcy_1991_2021 <- cbind(as.numeric(
  str_split_fixed(uchodzcy_1991_2021[, 1], "-", 2)[, 1]
), uchodzcy_1991_2021[, 2])

dane_lata <- as.data.frame(read_excel("Rozwój i zmiany w strukturze ludności w latach 1950-2020.xls",
  range = "a8:a78", col_names = F
))
wspolcz_feminizacji_1950_2020 <- cbind(dane_lata, as.data.frame(
  read_excel("Rozwój i zmiany w strukturze ludności w latach 1950-2020.xls",
    range = "c8:c78", col_names = "Wspolczynnik feminizacji"
  )
))
ludnosc_mezczyzni_1950_2020 <-
  round(ludnosc_OGOLEM_1950_2020 / (wspolcz_feminizacji_1950_2020[, 2] + 100) * 100)
ludnosc_kobiety_1950_2020 <- round(ludnosc_OGOLEM_1950_2020 / (
  wspolcz_feminizacji_1950_2020[, 2] + 100) * wspolcz_feminizacji_1950_2020[, 2])
names(ludnosc_mezczyzni_1950_2020) <- "Mezczyzni w tys."
names(ludnosc_kobiety_1950_2020) <- "Kobiety w tys."
ludnosc_mezczyzni_1950_2020 <- cbind(dane_lata, ludnosc_mezczyzni_1950_2020[, 2])
ludnosc_kobiety_1950_2020 <- cbind(dane_lata, ludnosc_kobiety_1950_2020[, 2])
w_wieku_produkcyjnym_1950_2020 <- cbind(dane_lata, as.data.frame(read_excel(
  "Rozwój i zmiany w strukturze ludności w latach 1950-2020.xls",
  range = "g8:g78",
  col_names = "W wieku produkcyjnym"
)) * ludnosc_OGOLEM_1950_2020[, 2])
wspol_lud_w_wieku_nieprod_na_100_w_prod_1950_2020 <- cbind(dane_lata, as.data.frame(read_excel(
  "Rozwój i zmiany w strukturze ludności w latach 1950-2020.xls",
  range = "i8:i78", col_names = "W wieku przedprodukcyjnym"
)))
w_wieku_przedprodukcyjnym_1950_2020 <- cbind(
  w_wieku_produkcyjnym_1950_2020[, 1],
  w_wieku_produkcyjnym_1950_2020[, 2] * wspol_lud_w_wieku_nieprod_na_100_w_prod_1950_2020[, 2] / 100
)

adopcje_2000_2020 <- as.data.frame(read_excel(
  "adopcje---przysposobienie-w-latach-2000-2020.xlsx",
  range = "a7:c27"
))[, c(1, 3)]
names(adopcje_2000_2020) <- c("Rok", "Ilosc")
maloletni_w_rodz_zast_1989_2021 <- as.data.frame(read_excel(
  "umieszczenie-maloletniego-w-rodzinie-zastepczej-stan-kart-opm-w-latach-1989-2021.xlsx",
  range = "a9:b41", col_names = c("Rok", "Ilosc")
))

wladza_rodz_pozbawienie_itp <- as.data.frame(read_excel(
  "wladza-rodzicielska-pozbawienie-zawieszenie-ograniczenie-w-latach-2000-2021 (na podstawie .doc).xlsx"
))[-1, c(1, 3)]
wladza_rodz_pozbawienie_itp <- korekta_wartosci_liczbowych_w_ramce("wladza_rodz_pozbawienie_itp")

urodzenia_pozamalzenskie_2000_2020 <- cbind(data.frame(2000:2020), as.data.frame(
  read_excel("Urodzenia w latach 1970-2020.xls",
    range = "d20:d40",
    col_names = "Urodzenia pozamalzenskie"
  )
))
names(urodzenia_pozamalzenskie_2000_2020)[1] <- "Rok"

urodzenia_pierwsze_dzieci_1970_2020 <- as.data.frame(read_excel(
  "Urodzenia żywe według kolejności urodzenia dziecka u matki w latach 1960-2020.xls",
  range = "a10:d60"
))[, c(1, 3, 4)]
urodzenia_pierwsze_dzieci_1970_2017 <- urodzenia_pierwsze_dzieci_1970_2020[
  1:(dim(urodzenia_pierwsze_dzieci_1970_2020)[1] - 3),
]
names(urodzenia_pierwsze_dzieci_1970_2017) <-
  c("Rok", "Pierwsze dzieci", "Odsetek pierwszych dzieci")

zgony_niemowlat_1_4_plec_1955_2019 <- as.data.frame(
  read_csv("Śmiertelność dzieci z podziałem na płcie 1955-2019.csv")
)
zgony_niemowlat_1_4_plec_1955_2019 <- zgony_niemowlat_1_4_plec_1955_2019[
  zgony_niemowlat_1_4_plec_1955_2019$Country == "Poland",
][, -c(1, 2)]
plcie_niemowl <- unique(zgony_niemowlat_1_4_plec_1955_2019$Gender)
for (plec in plcie_niemowl) {
  assign(paste("zgony_niemowlat_1_4_1955_2019_", plec, sep = ""), zgony_niemowlat_1_4_plec_1955_2019[
    zgony_niemowlat_1_4_plec_1955_2019$Gender == plec,
  ])
} # Zapisanie w osobnych zmiennych danych dot. jedynie danej płci
for (plec in plcie_niemowl) {
  assign(
    x = paste(paste("zgony_niemowlat_1_4_1955_2019_", plec, sep = ""), "ZG", sep = "_"),
    value = get(paste("zgony_niemowlat_1_4_1955_2019_", plec, sep = ""))[, c(1, 3)]
  )
} # Zapisanie danych dot. zgonów dla płci
dane_zgony_niemowlat <- paste(paste("zgony_niemowlat_1_4_1955_2019_",
  plcie_niemowl,
  sep = ""
), "ZG", sep = "_")

#### Przygotowanie danych o zgonach miesięcznie w Polsce w latach 2000-2022
##### Pomocnicze oszacowanie zgonów dziennie
Zgony_osz_dzienne_2000_2022_bez_poprawki <- Zgony_osz_dzienne_2000_2022_bez_poprawki_M <-
  Zgony_osz_dzienne_2000_2022_bez_poprawki_K <- c()
ramki_osz_mies_zgony_bez_poprawki <- c(
  "Zgony_osz_dzienne_2000_2022_bez_poprawki", "Zgony_osz_dzienne_2000_2022_bez_poprawki_M",
  "Zgony_osz_dzienne_2000_2022_bez_poprawki_K"
)
zrodlo <- "zgony_wg_tygodni_grup_wiekowych_plci/Zgony według tygodni w Polsce_"
i <- 0
for (ramka in ramki_osz_mies_zgony_bez_poprawki) {
  i <- i + 1
  for (rok in 2000:2022) {
    # Wczytanie danych zgonów tygodniowych dla danego roku
    if (rok >= 2020) {
      if (rok == 2020 | rok == 2021) {
        Zgony_tygodniowe_roku <- as.data.frame(read_excel(
          paste(zrodlo, paste(rok, ".xlsx", sep = ""), sep = ""),
          sheet = i, range = "D9:BD9", col_names = FALSE
        ))
      } else {
        Zgony_tygodniowe_roku <- as.data.frame(read_excel(
          paste(zrodlo, paste(rok, ".xlsx", sep = ""), sep = ""),
          sheet = i, range = "D9:AX9", col_names = F
        ))
      }
    } else {
      Zgony_tygodniowe_roku <- as.data.frame(read_excel(
        paste(zrodlo, paste(rok, ".xlsx", sep = ""), sep = ""),
        sheet = i, range = "D10:BD10", col_names = F
      ))
    }
    # Pomocnicze oszacowanie dzienne
    if (rok != 2022) {
      if (is.na(Zgony_tygodniowe_roku[53])) Zgony_tygodniowe_roku <- Zgony_tygodniowe_roku[1:52]
    }
    Oszacowanie_dzienne_zgonow <- c(rep(Zgony_tygodniowe_roku / 7, each = 7))
    assign(x = ramka, value = c(get(ramka), Oszacowanie_dzienne_zgonow))
  }
  # Poprawka względem normy ISO8601, tj. dodanie założonych danych z 1,2 stycznia 2000 r. oraz
  # usunięcie danych z dwóch ostatnich dni 2022 r.
  assign(
    x = gsub("_bez_poprawki", "", ramka),
    value = c(rep(get(ramka)[1], 2), get(ramka)[1:(length(get(ramka)))])
  )
}
##### Oszacowanie zgonów miesięcznie
dni_w_miesiacach <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
dni_w_mies_przestepny_rok <- c(31, 29, dni_w_miesiacach[3:12])
Zgony_osz_mies_2000_2022 <- Zgony_osz_mies_2000_2022_K <- Zgony_osz_mies_2000_2022_M <-
  data.frame(rep(2000:2022, each = 12), rep(1:12, times = 23), rep(0, 12 * 23))
names(Zgony_osz_mies_2000_2022) <- names(Zgony_osz_mies_2000_2022_M) <-
  names(Zgony_osz_mies_2000_2022_K) <- c("Rok", "Miesiac", "Zgony")
i <- 0
ramki_osz_mies_zgony <- gsub(
  "dzienne_2000_2022_bez_poprawki", "mies_2000_2022",
  ramki_osz_mies_zgony_bez_poprawki
)
for (ramka in ramki_osz_mies_zgony) {
  i <- i + 1
  nr_dnia <- 1
  nr_wiersza_do_uzupelnienia <- 1
  for (rok in 2000:2022) {
    # Zsumowanie danych dziennych dla kolejnych miesiący
    for (miesiac in 1:12) {
      # (Ostatni rekord danych z listopada 2022 r.)
      if (rok < 2022 | miesiac < 11) {
        eval(parse(text = paste(ramka, "[nr_wiersza_do_uzupelnienia,3] <- round(sum(unlist(
        eval(parse(text=paste(gsub('mies_2000_2022','dzienne_2000_2022', ramka),''))))[
                              nr_dnia:(nr_dnia+dni_w_miesiacach[miesiac]-1)]))")))
        ifelse(rok %% 4 == 0, nr_dnia <- nr_dnia + dni_w_mies_przestepny_rok[miesiac],
          nr_dnia <- nr_dnia + dni_w_miesiacach[miesiac]
        )
        nr_wiersza_do_uzupelnienia <- nr_wiersza_do_uzupelnienia + 1
      }
    }
  }
}
###### Usunięcie 01.2000 r. i 11-12 mies. 2022 r. z powodu niekompletności danych
for (ramka in ramki_osz_mies_zgony) {
  assign(x = ramka, value = get(ramka)[2:(dim(get(ramka))[1] - 2), ])
}
ramki_osz_mies_zgony

###### Obliczenie Śmiertelności miesięcznie w możliwym przedziale
###### na podst. dost?pnych danych (2011.06 - 2021.12)
smiertelnosc_2011_6_2022_9_mies <- Zgony_osz_mies_2000_2022[between(
  Zgony_osz_mies_2000_2022$Rok, 2011, 2022
) &
  (Zgony_osz_mies_2000_2022$Miesiac >= 6 | Zgony_osz_mies_2000_2022$Rok != 2011) &
  (Zgony_osz_mies_2000_2022$Rok != 2022 | Zgony_osz_mies_2000_2022$Miesiac < 10), c(1, 2, 3)]
smiertelnosc_2011_6_2022_9_mies$Zgony <- as.numeric(ludnosc_OGOLEM_2011_22_mies[, 3]) /
  (smiertelnosc_2011_6_2022_9_mies$Zgony * 1000)
names(smiertelnosc_2011_6_2022_9_mies)[3] <- "Smiertelnosc ogolnie"

dane_zgony_wg_przyczyn_lata <- as.numeric(
  read_excel("zGONY WG PRZYCZYN.xlsx", sheet = 2, range = "c2:tk2", col_names = F)
)
dane_zgony_wg_przyczyn_dane <- as.numeric(
  read_excel("zGONY WG PRZYCZYN.xlsx", sheet = 2, range = "c4:tk4", col_names = F)
)
dane_zgony_wg_przyczyn_nazwy <- as.character(
  read_excel("zGONY WG PRZYCZYN.xlsx", sheet = 2, range = "c1:tk1", col_names = F)
)
dane_zgony_wg_przyczyn_nazwy <-
  dane_zgony_wg_przyczyn_nazwy[dane_zgony_wg_przyczyn_nazwy != "NA"]
dane_zgony_przyczynowe <-
  cbind(
    dane_zgony_wg_przyczyn_lata, dane_zgony_wg_przyczyn_dane,
    rep(dane_zgony_wg_przyczyn_nazwy,
      each =
        length(dane_zgony_wg_przyczyn_dane) / length(dane_zgony_wg_przyczyn_nazwy)
    )
  )

zgony_chor_zakazne_pasozyt_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] == "niektóre choroby zakaźne i pasożytnicze ogółem", c(1, 2)
]
zgony_chor_zakazne_pasozyt_1999_2021 <- cbind(
  as.numeric(zgony_chor_zakazne_pasozyt_1999_2021[, 1]),
  as.numeric(zgony_chor_zakazne_pasozyt_1999_2021[, 2])
)

zgony_nowotwory_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] == "nowotwory ogółem", c(1, 2)
]
zgony_nowotwory_1999_2021 <- cbind(
  as.numeric(zgony_nowotwory_1999_2021[, 1]), as.numeric(zgony_nowotwory_1999_2021[, 2])
)

zgony_nowotwory_zlosliwe_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] == "nowotwory złośliwe ogółem", c(1, 2)
]
zgony_nowotwory_zlosliwe_1999_2021 <- cbind(
  as.numeric(zgony_nowotwory_zlosliwe_1999_2021[, 1]),
  as.numeric(zgony_nowotwory_zlosliwe_1999_2021[, 2])
)

zgony_chor_krwi_narzadow_krwio_itp_1999_2021 <- dane_zgony_przyczynowe[dane_zgony_przyczynowe[, 3] ==
  "choroby krwi i narządów krwiotwórczych oraz niektóre choroby przebiegające  z udziałem mechanizmów autoimmunologicznych", c(1, 2)]
zgony_chor_krwi_narzadow_krwio_itp_1999_2021 <- cbind(
  as.numeric(zgony_chor_krwi_narzadow_krwio_itp_1999_2021[, 1]),
  as.numeric(zgony_chor_krwi_narzadow_krwio_itp_1999_2021[, 2])
)

zgony_zaburz_wydz_odzyw_itp_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] ==
    "zaburzenia wydzielania wewnętrznego, stanu odżywiania i przemiany metabolicznej ogółem",
  c(1, 2)
]
zgony_zaburz_wydz_odzyw_itp_1999_2021 <- cbind(
  as.numeric(zgony_zaburz_wydz_odzyw_itp_1999_2021[, 1]),
  as.numeric(zgony_zaburz_wydz_odzyw_itp_1999_2021[, 2])
)

zgony_psyc_zach_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] == "zaburzenia psychiczne i zaburzenia zachowania", c(1, 2)
]
zgony_psyc_zach_1999_2021 <- cbind(
  as.numeric(zgony_psyc_zach_1999_2021[, 1]), as.numeric(zgony_psyc_zach_1999_2021[, 2])
)

zgony_chor_ukl_nerw_narz_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] == "choroby układu nerwowego i narządów zmysłów", c(1, 2)
]
zgony_chor_ukl_nerw_narz_1999_2021 <- cbind(
  as.numeric(zgony_chor_ukl_nerw_narz_1999_2021[, 1]),
  as.numeric(zgony_chor_ukl_nerw_narz_1999_2021[, 2])
)

zgony_chor_ukl_kraz_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] == "choroby układu krążenia ogółem", c(1, 2)
]
zgony_chor_ukl_kraz_1999_2021 <- cbind(
  as.numeric(zgony_chor_ukl_kraz_1999_2021[, 1]), as.numeric(zgony_chor_ukl_kraz_1999_2021[, 2])
)

zgony_ukl_odd_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] == "choroby układu oddechowego ogółem", c(1, 2)
]
zgony_ukl_odd_1999_2021 <- cbind(
  as.numeric(zgony_ukl_odd_1999_2021[, 1]), as.numeric(zgony_ukl_odd_1999_2021[, 2])
)

zgony_ukl_traw_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] == "choroby układu trawiennego ogółem", c(1, 2)
]
zgony_ukl_traw_1999_2021 <- cbind(
  as.numeric(zgony_ukl_traw_1999_2021[, 1]), as.numeric(zgony_ukl_traw_1999_2021[, 2])
)

zgony_chor_skor_tkanki_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] == "choroby skóry i tkanki podskórnej", c(1, 2)
]
zgony_chor_skor_tkanki_1999_2021 <- cbind(
  as.numeric(zgony_chor_skor_tkanki_1999_2021[, 1]), as.numeric(zgony_chor_skor_tkanki_1999_2021[, 2])
)

zgony_chor_ukl_kostn_miesn_tkanki_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] == "choroby układu kostnostawowego, mięśniowego i tkanki łącznej", c(1, 2)
]
zgony_chor_ukl_kostn_miesn_tkanki_1999_2021 <-
  cbind(
    as.numeric(zgony_chor_ukl_kostn_miesn_tkanki_1999_2021[, 1]),
    as.numeric(zgony_chor_ukl_kostn_miesn_tkanki_1999_2021[, 2])
  )

zgony_chor_ukl_mocz_plcio_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] == "choroby układu moczowo-płciowego", c(1, 2)
]
zgony_chor_ukl_mocz_plcio_1999_2021 <-
  cbind(
    as.numeric(zgony_chor_ukl_mocz_plcio_1999_2021[, 1]),
    as.numeric(zgony_chor_ukl_mocz_plcio_1999_2021[, 2])
  )

zgony_ciaza_porod_polog_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] == "ciąża, poród i połóg", c(1, 2)
]
zgony_ciaza_porod_polog_1999_2021 <-
  cbind(
    as.numeric(zgony_ciaza_porod_polog_1999_2021[, 1]),
    as.numeric(zgony_ciaza_porod_polog_1999_2021[, 2])
  )

zgony_stany_okolopor_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] == "niektóre stany rozpoczynające się w okresie okołoporodowym", c(1, 2)
]
zgony_stany_okolopor_1999_2021 <-
  cbind(
    as.numeric(zgony_stany_okolopor_1999_2021[, 1]),
    as.numeric(zgony_stany_okolopor_1999_2021[, 2])
  )

zgony_wady_wrodz_znieksz_aberracje_chrom_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] ==
    "wady rozwojowe wrodzone, zniekształcenia i aberracje chromosomowe", c(1, 2)
]
zgony_wady_wrodz_znieksz_aberracje_chrom_1999_2021 <-
  cbind(
    as.numeric(zgony_wady_wrodz_znieksz_aberracje_chrom_1999_2021[, 1]),
    as.numeric(zgony_wady_wrodz_znieksz_aberracje_chrom_1999_2021[, 2])
  )

zgony_przyczyny_zewnetrz_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] == "zewnętrzne przyczyny zachorowania i zgonu - ogółem", c(1, 2)
]
zgony_przyczyny_zewnetrz_1999_2021 <-
  cbind(
    as.numeric(zgony_przyczyny_zewnetrz_1999_2021[, 1]),
    as.numeric(zgony_przyczyny_zewnetrz_1999_2021[, 2])
  )

zgony_wypadki_komun_nastepstwa_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] ==
    "zewnętrzne przyczyny zachorowania i zgonu - wypadki i nieszczęśliwe następstwa wypadków - wypadki komunikacyjne ogółem",
  c(1, 2)
]
zgony_wypadki_komun_nastepstwa_1999_2021 <-
  cbind(
    as.numeric(zgony_wypadki_komun_nastepstwa_1999_2021[, 1]),
    as.numeric(zgony_wypadki_komun_nastepstwa_1999_2021[, 2])
  )

zgony_samobojstwa_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] ==
    "zewnętrzne przyczyny zachorowania i zgonu - samobójstwo", c(1, 2)
]
zgony_samobojstwa_1999_2021 <- cbind(as.numeric(
  zgony_samobojstwa_1999_2021[, 1]
), as.numeric(zgony_samobojstwa_1999_2021[, 2]))

zgony_zabojstwa_1999_2021 <- dane_zgony_przyczynowe[
  dane_zgony_przyczynowe[, 3] == "zewnętrzne przyczyny zachorowania i zgonu - zabójstwo", c(1, 2)
]
zgony_zabojstwa_1999_2021 <- cbind(as.numeric(
  zgony_zabojstwa_1999_2021[, 1]
), as.numeric(zgony_zabojstwa_1999_2021[, 2]))

## Dane z folderu "Przestępstwa"
setwd(paste(sciezka_zalacznikow, "/Dane użyte w projekcie/Przestępstwa", sep = ""))
pozbaw_woln_rocznie <- as.data.frame(read_excel(
  "dozywotnie-i-25-lat-pozbawienia-wolnosci-w-latach-1946-2020 (rocznie, na podst. doc).xlsx"
))
pozbaw_woln_rocznie_doz_w1inst_1996_2020 <- pozbaw_woln_rocznie[53:dim(pozbaw_woln_rocznie)[1], 1:2]
pozbaw_woln_rocznie_doz_prawomocnie_1996_2018 <-
  pozbaw_woln_rocznie[55:dim(pozbaw_woln_rocznie)[1] - 2, c(1, 3)]
pozbaw_woln_rocznie_25_w1inst_1970_2020 <- pozbaw_woln_rocznie[27:dim(pozbaw_woln_rocznie)[1], c(1, 4)]
pozbaw_woln_rocznie_25_prawomocnie_1970_2018 <-
  pozbaw_woln_rocznie[27:dim(pozbaw_woln_rocznie)[1] - 2, c(1, 5)]

### Wczytanie danych dot. nieletnich, przypisanie im odpowiednich oznaczeń dat
lata_dane_nieletni <- read_excel(
  "nieletni-prawomocne-orzeczenia-w-latach-2000-2019.xlsx",
  range = "b5:u5", col_names = F
)
dane_nieletni <- c(
  "nieletni_orzeczenia_czyny_karalne_2000_2019", "nieletni_orzeczenia_czyny_karalne_2000_2019_M",
  "nieletni_orzeczenia_czyny_karalne_2000_2019_K", "nieletni_orzeczenia_karalne_demoralizacja",
  "nieletni_orzeczenia_karalne_demoralizacja_M", "nieletni_orzeczenia_karalne_demoralizacja_K",
  "nieletni_orzeczenia_karalne_przeciwko_zdrowiu_zyciu"
)
zakresy_danych_nieletni <- c("b11:u11", "b12:u12", "b13:u13", "b16:u16", "b17:u17", "b18:u18", "b82:u82")
for (nr_danych in 1:length(dane_nieletni)) {
  # Wczytanie zbioru danych
  assign(x = dane_nieletni[nr_danych], value = as.data.frame(read_excel(
    "nieletni-prawomocne-orzeczenia-w-latach-2000-2019.xlsx",
    col_names = F,
    range = zakresy_danych_nieletni[nr_danych]
  )))
  # Poprawka nazw kolumn i zapis wczytanego zbioru danych do zmiennej o okre?lonej nazwie
  ramka_z_nazw <- get(dane_nieletni[nr_danych])
  names(ramka_z_nazw) <- lata_dane_nieletni
  assign(x = dane_nieletni[nr_danych], value = ramka_z_nazw)
}
dane_nieletni

wspolcz_samobojstw_rocznie_1990_2020 <- as.data.frame(read_csv(
  "poland-suicide-rate.csv",
  skip = 16
))[, c(1, 2)]

prawomocnie_skazani_1946_2017 <- as.data.frame(read_excel(
  "prawomocnie-skazani-w-latach-1946-2017--w-tym-kara-smierci-1946-1987 (na podst. .doc).xlsx"
))
prawomocnie_skazani_ogolem_1946_2018 <- prawomocnie_skazani_1946_2017[, c(1, 2)]
prawomocnie_skazani_na_smierc_1946_1987 <- prawomocnie_skazani_1946_2017[
  !prawomocnie_skazani_1946_2017[3] == "x", c(1, 3)
]


# POPRAWKA NAZW KOLUMN Z datą DLA ZBIORów DANYCH POBRANYCH ZE STRONY: FRED.STLOUISFED.ORG
fred_data_edit_list <- c(
  "stopy_renty_międzybankowe", "cena_produktu_krajowego_brutto", "handel_detaliczny",
  "kurs_dolara_mies", "kurs_dolara_rocznie", "M2_mies", "M3_bez_korekty_mies", "M3_z_korekta_mies",
  "M3_bez_korekty_rocznie", "Produkcja_przemyslu_mies", "produkt_krajowy_kwart",
  "efektywny_kurs_walutowy_mies", "Indeks_cen_konsump_ogolem", "Indeks_cen_producja_przem_mies",
  "Indeks_cen_producja_przem_rocznie", "Indeks_cen_czynsz_napr_utrz_mies", "Indeks_cen_energia",
  "Indeks_cen_all_mies", "Indeks_cen_all_rocznie", "Indeks_cen_zywnosc",
  "Indeks_cen_administrowane_mies", "Indeks_cen_gaz", "Indeks_cen_konserwacja",
  "Indeks_cen_mieszkanie", "Indeks_cen_oleje", "Indeks_cen_wakacyjne", "Indeks_cen_restauracje_hotele",
  "Indeks_cen_transport", "Indeks_cen_rekreacja_opieka_z_wyl",
  "Indeks_cen_wod_napoje_bezalkoholowe_soki", "Indeks_cen_zywnosc_napoje_bezalkoholowe",
  "zarobki_godzinowe_produkcja", "zarobki_godzinowe_sektor_prywatny", "rejestracje_sam",
  "wskaznik_cen_konsump_zdrowie", "eksport_towarow", "import_mies_bez_korekty",
  "import_mies_z_korekta", "import_rocznie_z_korekta", "Ceny_akcji",
  "Indeks_cen_konsump_bez_zywn_energ", "nieobsadzone_miejsca_pracy_2010_2022_mies_z_kor",
  "nieobsadzone_miejsca_pracy_2010_2022_rocz_bez_kor",
  "nieobsadzone_miejsca_pracy_2010_2022_mies_bez_kor", "wspolcz_samobojstw_rocznie_1990_2020",
  "co2_emisje", "wydatki_sluzba_zdrowia", "gazy_cieplarniane_emisje"
)
for (i in fred_data_edit_list) {
  q <- get(i)
  q <- as.data.frame(cbind(
    as.data.frame(str_split_fixed(q[, 1], "-", n = 3)[, 1]),
    as.data.frame(str_split_fixed(q[, 1], "-", n = 3)[, 2]),
    as.data.frame(q[, 2:dim(q)[2]])
  ))
  names(q) <- c("Rok", "Miesiac", names(get(i))[2:dim(get(i))[2]])
  assign(i, q)
}


# DOPASOWANIE OKRESÓW I CZĘSTOTLIWOŚCI DANYCH DO DANYCH DOT. ZGONów / ŚmiertELnoścI
# ORAZ PODZIAŁ W ZWIĄZKU Z: COVID - 03.2021R., TRANSFORMACJĄ SYSTEMOWĄ - 1989 R.
## Dane -  zgony skutkowe
### Miesięcznie (w krótszym okresie czasu, z podziałem na płcie)
dane_skutkowe_zgony_mies <- ramki_osz_mies_zgony
for (ramka in ramki_osz_mies_zgony) {
  q <- get(ramka)
  assign(x = paste(ramka, "_przed_cov", sep = ""), value = q[q[, 1] < 2020 | (q[, 1] == 2020 & q[, 2] < 3), ])
  assign(x = paste(ramka, "_po_cov", sep = ""), value = q[q[, 1] >= 2021 | (q[, 1] == 2020 & q[, 2] >= 3), ])
  dane_skutkowe_zgony_mies <- c(
    dane_skutkowe_zgony_mies,
    paste(ramka, "_przed_cov", sep = ""),
    paste(ramka, "_po_cov", sep = "")
  )
}

### Rocznie (w dłuższym okresie czasu)
zgony_ogolem_1950_2020 <- zgony_smiertelnosc_1950_2020[, c(1, 2)]
zgony_ogolem_1950_1988 <- zgony_ogolem_1950_2020[zgony_ogolem_1950_2020[, 1] < 1989, ]
zgony_ogolem_1990_2020 <- zgony_ogolem_1950_2020[zgony_ogolem_1950_2020[, 1] > 1989, ]

dane_skutkowe_zgony_rocznie <- c(
  "zgony_ogolem_1950_2020", "zgony_ogolem_1950_1988", "zgony_ogolem_1990_2020"
)

## Dane -  ŚmiertELNOŚĆ skutkowa
### Tygodniowo
#### Wczytanie danych dot. zgonów tygodniowo w latach 2020 - 2022.
setwd(paste(sciezka_zalacznikow, "/Dane użyte w projekcie/Ludność", sep = ""))
zrodlo <- "zgony_wg_tygodni_grup_wiekowych_plci/Zgony według tygodni w Polsce_"
i <- 1
plcie_zgony <- c("_ogolem", "_mez", "_kob")
Zgony_tygodniowe_roku_2020_2022_ogolem_ram <- data.frame()
for (plec in plcie_zgony) {
  assign(paste("Zgony_tygodniowe_roku_2020_2022_ram", plec, sep = ""), data.frame())
  w <- get(paste("Zgony_tygodniowe_roku_2020_2022_ram", plec, sep = ""))
  w20 <- paste("Zgony_tygodniowe_roku_2020", plec, sep = "")
  w21 <- paste("Zgony_tygodniowe_roku_2021", plec, sep = "")
  w22 <- paste("Zgony_tygodniowe_roku_2022", plec, sep = "")
  assign(x = w20, value = as.data.frame(read_excel(
    paste(zrodlo, "2020.xlsx", sep = ""),
    range = "D9:BD9", col_names = F, sheet = i
  )))
  il_tyg_20 <- length(get(w20))
  w[1:il_tyg_20, c(1:3)] <- c(rep("2020", il_tyg_20), 1:il_tyg_20, as.numeric(get(w20)))
  assign(x = w21, value = as.data.frame(read_excel(
    paste(zrodlo, "2021.xlsx", sep = ""),
    range = "D9:BC9", col_names = F, sheet = i
  )))
  il_tyg_21 <- length(get(w21))
  w[(1 + il_tyg_20):(il_tyg_20 + il_tyg_21), c(1:3)] <- c(
    rep("2021", il_tyg_21),
    1:il_tyg_21, as.numeric(get(w21))
  )
  assign(x = w22, value = as.data.frame(read_excel(
    paste(zrodlo, "2022.xlsx", sep = ""),
    range = "D9:AX9", col_names = F, sheet = i
  )))
  il_tyg_22 <- length(get(w22))
  w[(1 + il_tyg_20 + il_tyg_21):(il_tyg_20 + il_tyg_21 + il_tyg_22), c(1:3)] <- c(
    rep("2022", il_tyg_22), 1:il_tyg_22, as.numeric(get(w22))
  )
  names(w) <- c("Rok", "Tydzien", "Zgony")
  assign(x = paste("Zgony_tygodniowe_roku_2020_2022_ram", plec, sep = ""), value = w)
  i <- i + 1
}
Zgony_tygodniowe_roku_2020_2022_ramki <-
  paste("Zgony_tygodniowe_roku_2020_2022_ram", plcie_zgony, sep = "")

dane_skutkowe_zgony_tygod <- Zgony_tygodniowe_roku_2020_2022_ramki

### Miesięcznie (w krótszym okresie czasu)
smiertelnosc_2011_6_2022_9_mies
smiertelnosc_2011_przed_covid_mies <- smiertelnosc_2011_6_2022_9_mies[
  smiertelnosc_2011_6_2022_9_mies[, 1] < 2020 |
    (smiertelnosc_2011_6_2022_9_mies[, 1] == 2020 & smiertelnosc_2011_6_2022_9_mies[, 2] < 3),
]
smiertelnosc_po_covid_mies <- smiertelnosc_2011_6_2022_9_mies[
  (smiertelnosc_2011_6_2022_9_mies[, 2] > 2 & smiertelnosc_2011_6_2022_9_mies[, 1] == 2020) |
    smiertelnosc_2011_6_2022_9_mies[, 1] >= 2021,
]
dane_skutkowe_smiert_mies <- c(
  "smiertelnosc_2011_6_2022_9_mies",
  "smiertelnosc_2011_przed_covid_mies", "smiertelnosc_po_covid_mies"
)

### Kwartalnie (od 3 kwartału 2011 r. do 3 kwartału 2022 r.)
lata_kwartalnie <- rep(unique(smiertelnosc_2011_6_2022_9_mies[, 1]), each = 4)
miesiace_kwartalnie <- rep(1:4, times = 22 - 10)
smiertelnosc_kwart <- data.frame(cbind(lata_kwartalnie, miesiace_kwartalnie))
for (nr_wiersza in 1:dim(smiertelnosc_kwart)[1]) {
  smiertelnosc_kwart[nr_wiersza, 3] <- mean(smiertelnosc_2011_6_2022_9_mies[
    between(
      x = smiertelnosc_2011_6_2022_9_mies$Miesiac,
      left = 1 + (smiertelnosc_kwart[nr_wiersza, 2] - 1) * 3,
      right = 3 + (smiertelnosc_kwart[nr_wiersza, 2] - 1) * 3
    ) &
      smiertelnosc_2011_6_2022_9_mies$Rok == smiertelnosc_kwart[nr_wiersza, 1], 3
  ])
}
names(smiertelnosc_kwart) <- c("Rok", "Kwartal", "Smiertelnosc")
smiertelnosc_kwart <- smiertelnosc_kwart[
  between(smiertelnosc_kwart$Rok, 2012, 2021) |
    (smiertelnosc_kwart$Rok == 2011 & smiertelnosc_kwart$Kwartal >= 3) |
    (smiertelnosc_kwart$Rok == 2022 & smiertelnosc_kwart$Kwartal <= 3),
]
smiertelnosc_kwart_po_COV <- smiertelnosc_kwart[
  smiertelnosc_kwart[, 1] > 2020 | (smiertelnosc_kwart[, 1] == 2020 & smiertelnosc_kwart[, 2] > 1),
]
smiertelnosc_kwart_przed_COV <- smiertelnosc_kwart[
  smiertelnosc_kwart[, 1] < 2020 | (smiertelnosc_kwart[, 1] == 2020 & smiertelnosc_kwart[, 2] == 1),
]

dane_skutkowe_smiert_kwart <- c(
  "smiertelnosc_kwart", "smiertelnosc_kwart_po_COV",
  "smiertelnosc_kwart_przed_COV"
)

### Rocznie (w dłuższym okresie czasu)
smiertelnosc_ogolem_1950_2020 <- zgony_smiertelnosc_1950_2020[, c(1, 3)]
smiertelnosc_ogolem_1950_1988 <-
  smiertelnosc_ogolem_1950_2020[smiertelnosc_ogolem_1950_2020[, 1] < 1989, ]
smiertelnosc_ogolem_1990_2020 <-
  smiertelnosc_ogolem_1950_2020[smiertelnosc_ogolem_1950_2020[, 1] > 1989, ]

dane_skutkowe_smiert_rocznie <- c(
  "smiertelnosc_ogolem_1950_2020", "smiertelnosc_ogolem_1950_1988",
  "smiertelnosc_ogolem_1990_2020"
)

## Dane przyczynowe zgonów zostaną przycięte do odpowiednich okresów:
##  tygodniowe (wg. formatu ISO-8601) od tygodnia nr 1 2000 r. do tygodnia nr 47 2022 r.,
##  miesięczne od lutego 2000 r. do października 2022 r., roczne do lat: 1950 - 2020.
## Dodatkowo zostaną podzielone w przypadku uwzględnienia okresu COVID, lub 1989 r.

### Przycięcie danych przyczynowych zgonów rocznie (w tym z podziałem wg 1989 r.)
dane_przyczynowe_zgony_rocznie <- c()
co2_emisje_zgonowe <- co2_emisje[, -4]
gazy_cieplarniane_emisje_zgonowe <- gazy_cieplarniane_emisje[, -4]
urodzenia_pierwsze_dzieci_1970_2017_zgonowe <- urodzenia_pierwsze_dzieci_1970_2017[, -3]
dane_zgony_rocznie <- c(
  "M3_bez_korekty_rocznie", "import_rocznie_z_korekta", "leczenie_alkoholowe",
  "co2_emisje_zgonowe", "gazy_cieplarniane_emisje_zgonowe", "uchodzcy_1991_2021",
  "w_wieku_produkcyjnym_1950_2020", "w_wieku_przedprodukcyjnym_1950_2020", "adopcje_2000_2020",
  "maloletni_w_rodz_zast_1989_2021", "wladza_rodz_pozbawienie_itp",
  "urodzenia_pozamalzenskie_2000_2020", dane_zgony_niemowlat,
  "urodzenia_pierwsze_dzieci_1970_2017_zgonowe", "zgony_chor_zakazne_pasozyt_1999_2021",
  "zgony_nowotwory_1999_2021", "zgony_nowotwory_zlosliwe_1999_2021",
  "zgony_chor_krwi_narzadow_krwio_itp_1999_2021", "zgony_zaburz_wydz_odzyw_itp_1999_2021",
  "zgony_psyc_zach_1999_2021", "zgony_chor_ukl_nerw_narz_1999_2021", "zgony_chor_ukl_kraz_1999_2021",
  "zgony_ukl_odd_1999_2021", "zgony_ukl_traw_1999_2021", "zgony_chor_skor_tkanki_1999_2021",
  "zgony_chor_ukl_kostn_miesn_tkanki_1999_2021", "zgony_chor_ukl_mocz_plcio_1999_2021",
  "zgony_ciaza_porod_polog_1999_2021", "zgony_stany_okolopor_1999_2021",
  "zgony_wady_wrodz_znieksz_aberracje_chrom_1999_2021", "zgony_przyczyny_zewnetrz_1999_2021",
  "zgony_wypadki_komun_nastepstwa_1999_2021", "zgony_samobojstwa_1999_2021",
  "zgony_zabojstwa_1999_2021", "nieobsadzone_miejsca_pracy_2010_2022_rocz_bez_kor",
  "pozbaw_woln_rocznie_doz_w1inst_1996_2020", "pozbaw_woln_rocznie_doz_prawomocnie_1996_2018",
  "pozbaw_woln_rocznie_25_w1inst_1970_2020", "prawomocnie_skazani_ogolem_1946_2018",
  "prawomocnie_skazani_na_smierc_1946_1987"
)
#### PODZIAŁ danych z ramek o dwóch rodzajach wymiarów na okresy względem roku 1989 oraz COVID
for (ramka_danych_kolumnowe in dane_zgony_rocznie) {
  q <- get(ramka_danych_kolumnowe)
  q <- q[between(as.numeric(q[, 1]), 1950, 2020), ]
  assign(x = ramka_danych_kolumnowe, value = q)
  dane_przyczynowe_zgony_rocznie <- c(dane_przyczynowe_zgony_rocznie, ramka_danych_kolumnowe)
  if (dim(q[between(as.numeric(q[, 1]), 1950, 1988), ])[1] > 0 & dim(q[between(as.numeric(q[, 1]), 1990, 2020), ])[1] > 0) {
    assign(paste(ramka_danych_kolumnowe, "_przed_1989", sep = ""), q[between(as.numeric(q[, 1]), 1950, 1988), ])
    assign(paste(ramka_danych_kolumnowe, "_po_1989", sep = ""), q[between(as.numeric(q[, 1]), 1990, 2020), ])
    dane_przyczynowe_zgony_rocznie <- c(
      dane_przyczynowe_zgony_rocznie,
      paste(ramka_danych_kolumnowe, "_przed_1989", sep = ""),
      paste(ramka_danych_kolumnowe, "_po_1989", sep = "")
    )
  }
}
dane_przyczynowe_listy_zgony_rocznie <- c(
  "wypadki_drogowe", "wypadki_drogowe_martwi", "gruzlica",
  "AIDS", "zachorowania_weneryczne", dane_bezrobotni,
  dane_nieletni
)
for (ramka_danych_wierszowe in dane_przyczynowe_listy_zgony_rocznie) {
  q <- get(ramka_danych_wierszowe)
  q <- q[, q != "-"]
  q <- q[, between(as.numeric(names(q)), 1950, 2020)]
  assign(x = ramka_danych_wierszowe, value = q)
  dane_przyczynowe_zgony_rocznie <- c(dane_przyczynowe_zgony_rocznie, ramka_danych_wierszowe)
  if (length(q[, names(q) < 1989]) > 0 & length(q[, names(q) > 1989]) > 0) {
    assign(paste(ramka_danych_wierszowe, "_przed_1989", sep = ""), q[, between(names(q), 1950, 1989)])
    assign(paste(ramka_danych_wierszowe, "_po_1989", sep = ""), q[, between(names(q), 1990, 2020)])
    dane_przyczynowe_zgony_rocznie <- c(
      dane_przyczynowe_zgony_rocznie,
      paste(ramka_danych_wierszowe, "_przed_1989", sep = ""),
      paste(ramka_danych_wierszowe, "_po_1989", sep = "")
    )
  }
}
dane_przyczynowe_zgony_rocznie

### Przycięcie danych przyczynowych zgonów miesięcznie (w tym z podziałem COVID)
M2_mies_GOT <- M2_mies[M2_mies[, 1] >= 2000, ][-1, ]
M3_bez_korekty_mies_GOT <- M3_bez_korekty_mies[
  between(as.numeric(M3_bez_korekty_mies[, 1]), 2000, 2022),
][-1, ]
M3_bez_korekty_mies_przed_COV <- M3_bez_korekty_mies[
  (M3_bez_korekty_mies[, 1] < 2020 & M3_bez_korekty_mies[, 1] >= 2000) |
    (M3_bez_korekty_mies[, 1] == 2020 & as.numeric(M3_bez_korekty_mies[, 2]) < 3),
][-1, ]
M3_bez_korekty_mies_po_COV <- M3_bez_korekty_mies[
  M3_bez_korekty_mies[, 1] > 2020 | (M3_bez_korekty_mies[, 1] == 2020 &
    as.numeric(M3_bez_korekty_mies[, 2]) >= 3),
]
M3_z_korekta_mies_GOT <- M3_z_korekta_mies[M3_z_korekta_mies[, 1] >= 2000, ][-1, ]
eksport_towarow_GOT <- eksport_towarow[eksport_towarow[, 1] <= 2021, ]
eksport_towarow_przed_COV <- eksport_towarow_GOT[
  eksport_towarow_GOT[, 1] < 2020 | (eksport_towarow_GOT[, 1] == 2020 &
    as.numeric(eksport_towarow_GOT[, 2]) < 3),
]
eksport_towarow_po_COV <- eksport_towarow_GOT[eksport_towarow_GOT[, 1] > 2020 |
  (eksport_towarow_GOT[, 1] == 2020 & as.numeric(eksport_towarow_GOT[, 2]) >= 3), ]
import_mies_bez_korekty_GOT <- import_mies_bez_korekty[
  between(as.numeric(import_mies_bez_korekty[, 1]), 2000, 2021),
][-1, ]
import_mies_bez_korekty_przed_COV <- import_mies_bez_korekty_GOT[
  import_mies_bez_korekty_GOT[, 1] < 2020 |
    (import_mies_bez_korekty_GOT[, 1] == 2020 & as.numeric(import_mies_bez_korekty_GOT[, 2]) < 3),
]
import_mies_bez_korekty_po_COV <- import_mies_bez_korekty_GOT[
  import_mies_bez_korekty_GOT[, 1] > 2020 |
    (import_mies_bez_korekty_GOT[, 1] == 2020 & as.numeric(import_mies_bez_korekty_GOT[, 2]) >= 3),
]
import_mies_z_korekta_GOT <- import_mies_z_korekta[between(
  as.numeric(import_mies_z_korekta[, 1]), 2000, 2021
), ][-1, ]
import_mies_z_korekta_przed_COV <- import_mies_z_korekta_GOT[
  import_mies_z_korekta_GOT[, 1] < 2020 | (import_mies_z_korekta_GOT[, 1] == 2020 &
    as.numeric(import_mies_z_korekta_GOT[, 2]) < 3),
]
import_mies_z_korekta_po_COV <- import_mies_z_korekta_GOT[
  import_mies_z_korekta_GOT[, 1] > 2020 | (import_mies_z_korekta_GOT[, 1] == 2020 &
    as.numeric(import_mies_z_korekta_GOT[, 2]) >= 3),
]

#### Obliczenie danych dot. COVID miesięcznie na podst. danych dziennych
COVID_mies <- data.frame()
nr_wiersza <- 1
for (rok in unique(COVID$Rok)) {
  for (miesiac in sort(as.numeric(unique(COVID$Miesiac)))) {
    if (!((rok == 2020 & miesiac < 3) | (rok == 2022 & miesiac > 2))) {
      COVID_mies[nr_wiersza, c(1, 2)] <- c(rok, miesiac)
      COVID_mies[nr_wiersza, 3] <- sum(COVID[COVID[, 1] == rok & COVID[, 2] == miesiac, 4])
      COVID_mies[nr_wiersza, 4] <- sum(COVID[COVID[, 1] == rok & COVID[, 2] == miesiac, 5])
      COVID_mies[nr_wiersza, 5] <- sum(COVID[COVID[, 1] == rok & COVID[, 2] == miesiac, 6])
      nr_wiersza <- nr_wiersza + 1
    }
  }
}
names(COVID_mies) <- names(COVID)[-3]
COVID_mies # Od 2020.03 do 2022.02
##### Podzial zbioru do osobnych zmiennych ze względu na cechy
COVID_mies_potw <- COVID_mies[, 1:3]
COVID_mies_zgony <- COVID_mies[, c(1, 2, 4)]
COVID_mies_wyzdr <- COVID_mies[, c(1, 2, 5)]

#### PODZIAŁ danych dot. rejestracji bezrobotnych na okresy względem COVID
dane_bezrobotni_rejestracje_z_COV <- c()
for (ramka in dane_bezrobotni_rejestracje) {
  r <- get(ramka)
  assign(gsub("2010_2022", "przed_COV", ramka), r[r[, 1] < 2020 | (r[, 1] == 2020 & r[, 2] < 3), ])
  assign(gsub("2010_2022", "po_COV", ramka), r[(r[, 1] == 2020 & r[, 2] > 2) | r[, 1] > 2020, ])
  dane_bezrobotni_rejestracje_z_COV <- c(
    dane_bezrobotni_rejestracje_z_COV, ramka,
    gsub(pattern = "2010_2022", replacement = "przed_COV", x = ramka),
    gsub(pattern = "2010_2022", replacement = "po_COV", x = ramka)
  )
}
dane_bezrobotni_rejestracje_z_COV

nieobsadzone_miejsca_pracy_przed_COV_mies_bez_kor <-
  nieobsadzone_miejsca_pracy_2010_2022_mies_bez_kor[
    (nieobsadzone_miejsca_pracy_2010_2022_mies_bez_kor[, 1] < 2020 &
      nieobsadzone_miejsca_pracy_2010_2022_mies_bez_kor[, 1] > 2000) |
      (nieobsadzone_miejsca_pracy_2010_2022_mies_bez_kor[, 1] == 2020 &
        as.numeric(nieobsadzone_miejsca_pracy_2010_2022_mies_bez_kor[, 2]) < 3),
  ][-1, ]
nieobsadzone_miejsca_pracy_po_COV_mies_bez_kor <-
  nieobsadzone_miejsca_pracy_2010_2022_mies_bez_kor[
    nieobsadzone_miejsca_pracy_2010_2022_mies_bez_kor[, 1] > 2020 |
      (nieobsadzone_miejsca_pracy_2010_2022_mies_bez_kor[, 1] == 2020 &
        as.numeric(nieobsadzone_miejsca_pracy_2010_2022_mies_bez_kor[, 2]) >= 3),
  ]
nieobsadzone_miejsca_pracy_przed_COV_mies_z_kor <-
  nieobsadzone_miejsca_pracy_2010_2022_mies_z_kor[
    (nieobsadzone_miejsca_pracy_2010_2022_mies_z_kor[, 1] < 2020 &
      nieobsadzone_miejsca_pracy_2010_2022_mies_z_kor[, 1] > 2000) |
      (nieobsadzone_miejsca_pracy_2010_2022_mies_z_kor[, 1] == 2020 &
        as.numeric(nieobsadzone_miejsca_pracy_2010_2022_mies_z_kor[, 2]) < 3),
  ][-1, ]
nieobsadzone_miejsca_pracy_po_COV_mies_z_kor <-
  nieobsadzone_miejsca_pracy_2010_2022_mies_z_kor[
    nieobsadzone_miejsca_pracy_2010_2022_mies_z_kor[, 1] > 2020 |
      (nieobsadzone_miejsca_pracy_2010_2022_mies_z_kor[, 1] == 2020 &
        as.numeric(nieobsadzone_miejsca_pracy_2010_2022_mies_z_kor[, 2]) >= 3),
  ]

dane_przyczynowe_zgony_mies <- c(
  "M2_mies_GOT", "M3_bez_korekty_mies_GOT", "M3_bez_korekty_mies_przed_COV",
  "M3_bez_korekty_mies_po_COV", "M3_z_korekta_mies_GOT", "eksport_towarow_GOT",
  "eksport_towarow_przed_COV", "eksport_towarow_po_COV", "import_mies_bez_korekty_GOT",
  "import_mies_bez_korekty_przed_COV", "import_mies_bez_korekty_po_COV", "import_mies_z_korekta_GOT",
  "import_mies_z_korekta_przed_COV", "import_mies_z_korekta_po_COV", "COVID_mies_potw",
  "COVID_mies_wyzdr", "COVID_mies_zgony", dane_bezrobotni_rejestracje_z_COV,
  "nieobsadzone_miejsca_pracy_przed_COV_mies_bez_kor",
  "nieobsadzone_miejsca_pracy_po_COV_mies_bez_kor",
  "nieobsadzone_miejsca_pracy_przed_COV_mies_z_kor", "nieobsadzone_miejsca_pracy_po_COV_mies_z_kor"
)

### Przycięcie danych przyczynowych zgonów tygodniowo
#### Wczytanie danych dot. COVID tygodniowo (wg ISO-8601) w latach 2020 - 2022
##### Dane: od 09.03.2020 r. do 06.03.2022 r.
COVID_ISO_2020 <- COVID[6:306, ]
COVID_ISO_2021 <- COVID[307:670, ]
COVID_ISO_2022 <- COVID[671:733, ]
COVID_tyg_2020 <- COVID_tyg_2021 <- COVID_tyg_2022 <- data.frame()
nr_wiersza <- 1
for (tydzien in 11:53) { # Tygodnie ISO-8601 roku 2020 uwzględnione w zbiorze danych
  COVID_tyg_2020[nr_wiersza, c(1, 2)] <- c(2020, tydzien)
  for (nr_kol in 3:5) {
    COVID_tyg_2020[nr_wiersza, nr_kol] <-
      sum(COVID_ISO_2020[(1 + (tydzien - 11) * 7):(7 + (tydzien - 11) * 7), nr_kol + 1])
  }
  nr_wiersza <- nr_wiersza + 1
}
nr_wiersza <- 1
for (tydzien in 1:52) { # Tygodnie ISO-8601 roku 2021 uwzględnione w zbiorze danych
  COVID_tyg_2021[nr_wiersza, c(1, 2)] <- c(2021, tydzien)
  for (nr_kol in 3:5) {
    COVID_tyg_2021[nr_wiersza, nr_kol] <-
      sum(COVID_ISO_2021[(1 + (tydzien - 1) * 7):(7 + (tydzien - 1) * 7), nr_kol + 1])
  }
  nr_wiersza <- nr_wiersza + 1
}
nr_wiersza <- 1
for (tydzien in 1:9) { # Do 9 tygodnia ISO-8601 roku 2022 uwzględnione w zbiorze danych
  COVID_tyg_2022[nr_wiersza, c(1, 2)] <- c(2022, tydzien)
  for (nr_kol in 3:5) {
    COVID_tyg_2022[nr_wiersza, nr_kol] <-
      sum(COVID_ISO_2022[(1 + (tydzien - 1) * 7):(7 + (tydzien - 1) * 7), nr_kol + 1])
  }
  nr_wiersza <- nr_wiersza + 1
}
COVID_tyg <- rbind(COVID_tyg_2020, COVID_tyg_2021, COVID_tyg_2022)
names(COVID_tyg) <- c("Rok", "Tydzien", names(COVID)[4:6])
##### PODZIAŁ zbioru ze względu na cecy do osobnych zmiennych
COVID_potw_tyg <- COVID_tyg[, c(1:3)]
COVID_zgony_tyg <- COVID_tyg[, c(1, 2, 4)]
COVID_wyzdr_tyg <- COVID_tyg[, c(1, 2, 5)]

#### Wczytanie danych dot. szczepień tygodniowo od ostatniego z 2020 r. do końca roku 2022 r.
##### Wczytanie danych dziennych
COVID_szczepienia_ISO_2022 <- rbind(c(2021, 1, 3), COVID_ISO_2021[, 1:3])
COVID_szczepienia_ISO_2022[, 1] <- 2022
COVID_szczepienia_ISO_2022[
  (nrow(COVID_szczepienia_ISO_2022) - 1), 1
] <- 2023
COVID_szczepienia_ISO_2022 <- COVID_szczepienia_ISO_2022[1:(nrow(COVID_szczepienia_ISO_2022) - 1), ]
COVID_szczepienia_ISO_2020_2022 <-
  rbind(COVID_ISO_2020[295:301, 1:3], COVID_ISO_2021[, 1:3], COVID_szczepienia_ISO_2022)
COVID_szczepienia_ISO_2020_2022$Szczepienia <- rep(0, nrow(COVID_szczepienia_ISO_2020_2022))
for (i in 1:dim(COVID_szczepienia)[1]) {
  COVID_szczepienia_ISO_2020_2022[
    COVID_szczepienia_ISO_2020_2022$Rok == COVID_szczepienia[i, 1] &
      COVID_szczepienia_ISO_2020_2022$Miesiac == as.numeric(COVID_szczepienia[i, 2]) &
      COVID_szczepienia_ISO_2020_2022$Dzien == as.numeric(COVID_szczepienia[i, 3]), 4
  ] <-
    COVID_szczepienia[i, 4]
}
##### Zastąpienie brakuj?cych wartości średnimi z sąsiednich
szczepienia_ilosc <- as.numeric(COVID_szczepienia_ISO_2020_2022[, 4])
for (nr_probki in 1:length(szczepienia_ilosc)) {
  if (szczepienia_ilosc[nr_probki] == 0) {
    szczepienia_ilosc[nr_probki] <- round(
      mean(c(szczepienia_ilosc[nr_probki - 1], szczepienia_ilosc[nr_probki + 1]))
    )
  }
}
COVID_szczepienia_ISO_2020_2022[, 4] <- szczepienia_ilosc
COVID_szczepienia_ISO_2020 <- COVID_szczepienia_ISO_2020_2022[1:7, ]
COVID_szczepienia_ISO_2021 <- COVID_szczepienia_ISO_2020_2022[8:371, ]
COVID_szczepienia_ISO_2022 <- COVID_szczepienia_ISO_2020_2022[
  372:dim(COVID_szczepienia_ISO_2020_2022)[1],
]

##### Obliczenie danych tygodniowych szczepień na COVID wg formatu ISO-8601
COVID_szcz_tyg_2020 <- COVID_szcz_tyg_2021 <- COVID_szcz_tyg_2022 <- data.frame()
COVID_szcz_tyg_2020[1, 1:3] <- c(2020, 53, sum(COVID_szczepienia_ISO_2020[1:7, 4]))
nr_wiersza <- 1
for (tydzien in 1:52) { # Tygodnie ISO-8601 roku 2021
  COVID_szcz_tyg_2021[nr_wiersza, c(1, 2)] <- c(2021, tydzien)
  COVID_szcz_tyg_2021[nr_wiersza, 3] <-
    sum(COVID_szczepienia_ISO_2021[(1 + (tydzien - 1) * 7):(7 + (tydzien - 1) * 7), 4])
  nr_wiersza <- nr_wiersza + 1
}
nr_wiersza <- 1
for (tydzien in 1:52) { # Do 52 tygodnia ISO-8601 roku 2022
  COVID_szcz_tyg_2022[nr_wiersza, c(1, 2)] <- c(2022, tydzien)
  COVID_szcz_tyg_2022[nr_wiersza, 3] <-
    sum(COVID_szczepienia_ISO_2022[(1 + (tydzien - 1) * 7):(7 + (tydzien - 1) * 7), 4])
  nr_wiersza <- nr_wiersza + 1
}
COVID_szcz_tyg <- rbind(COVID_szcz_tyg_2020, COVID_szcz_tyg_2021, COVID_szcz_tyg_2022)
names(COVID_szcz_tyg) <- c("Rok", "Tydzien", "Ilosc szczepien")

dane_przyczynowe_zgony_tygod <- c(
  "COVID_potw_tyg", "COVID_zgony_tyg",
  "COVID_wyzdr_tyg", "COVID_szcz_tyg"
)

## Dane przyczynowe Śmiertelności zostaną przycięte do odpowiednich okresów:
##  miesięczne od 2011-06 do 2021-12 oraz z podziałem na okresy przed/po COVID,
##  roczne do lat: od 1950 do 2020 oraz z podziałem przed/po roku 1989,
##  kwartalne do okresu: od 3 kwartału 2011 r. do 3 kwartału 2022 r.

### Przycięcie danych przyczynowych Śmiertelności miesięcznie (w tym przed/po COVID)
### do okresu: od 2011-06 do 2021-12
dane_przyczynowe_smiert_mies <- c()
stopa_bezrobocia_mies_od_2010_GOT <- stopa_bezrobocia_mies_od_2010[
  between(stopa_bezrobocia_mies_od_2010[, 1], 2012, 2021) | (stopa_bezrobocia_mies_od_2010[, 1] == 2011 &
    stopa_bezrobocia_mies_od_2010[, 2] > 5),
]
dane_przyczynowe_smiert_mies_petlowe <- c(
  "stopy_renty_międzybankowe", "efektywny_kurs_walutowy_mies", "Produkcja_przemyslu_mies",
  "handel_detaliczny", "inflacja", "Indeks_cen_konsump_bez_zywn_energ", "Indeks_cen_konsump_ogolem",
  "Indeks_cen_producja_przem_mies", "Indeks_cen_czynsz_napr_utrz_mies", "Indeks_cen_energia",
  "Indeks_cen_all_mies", "Indeks_cen_zywnosc", "Indeks_cen_administrowane_mies", "Indeks_cen_gaz",
  "Indeks_cen_konserwacja", "Indeks_cen_mieszkanie", "Indeks_cen_oleje", "Indeks_cen_wakacyjne",
  "Indeks_cen_restauracje_hotele", "Indeks_cen_transport", "Indeks_cen_rekreacja_opieka_z_wyl",
  "Indeks_cen_wod_napoje_bezalkoholowe_soki", "Indeks_cen_zywnosc_napoje_bezalkoholowe",
  "zarobki_godzinowe_produkcja", "zarobki_godzinowe_sektor_prywatny", "wskaznik_cen_konsump_zdrowie",
  "rejestracje_sam", "stopa_bezrobocia_mies_od_2010_GOT", "kurs_dolara_mies"
)
for (ramka_danych in dane_przyczynowe_smiert_mies_petlowe) {
  q <- get(ramka_danych)
  q <- q[between(as.numeric(q[, 1]), 2012, 2020) | (q[, 1] == 2011 & as.numeric(q[, 2]) > 5) |
    (q[, 1] == 2021 & as.numeric(q[, 2]) < 12), ]
  assign(x = ramka_danych, value = q)
  dane_przyczynowe_smiert_mies <- c(dane_przyczynowe_smiert_mies, ramka_danych)
  if (dim(q[between(as.numeric(q[, 1]), 2012, 2019) | (q[, 1] == 2011 & as.numeric(q[, 2]) > 5) |
    (q[, 1] == 2020 & as.numeric(q[, 2]) < 3), ])[1] > 0 &
    dim(q[(q[, 1] == 2020 & as.numeric(q[, 2]) > 2) | q[, 1] == 2021, ])[1] > 0) {
    assign(
      x = paste(ramka_danych, "_przed_COV", sep = ""),
      value = q[between(as.numeric(q[, 1]), 2012, 2019) | (q[, 1] == 2011 & as.numeric(q[, 2]) > 5) |
        (q[, 1] == 2020 & as.numeric(q[, 2]) < 3), ]
    )
    assign(paste(ramka_danych, "_po_COV", sep = ""), q[(q[, 1] == 2020 & as.numeric(q[, 2]) > 2) | q[, 1] == 2021, ])
    dane_przyczynowe_smiert_mies <- c(
      dane_przyczynowe_smiert_mies,
      paste(ramka_danych, "_przed_COV", sep = ""), paste(ramka_danych, "_po_COV", sep = "")
    )
  }
}

### Przycięcie danych przyczynowych Śmiertelności rocznie (w tym przed/po 1989 r.)
dane_przyczynowe_smiert_rocznie <- c()
urodzenia_pierwsze_dzieci_1970_2017_smiert <- urodzenia_pierwsze_dzieci_1970_2017[, -2]
co2_emisje_smiert <- co2_emisje[, c(1, 2, 4)]
dane_ludnosc_smiert <- dane_ludnosc[c(1, 2, 3)]
#### Pogrupowanie danych dot. zgonów niemowląt wg płci
for (plec in plcie_niemowl) {
  assign(
    x = paste(paste("zgony_niemowlat_1_4_1955_2019_", plec, sep = ""), "SM", sep = "_"),
    value = get(paste("zgony_niemowlat_1_4_1955_2019_", plec, sep = ""))[, c(1, 3)]
  )
}
dane_smiert_niemowlat <- paste(paste("zgony_niemowlat_1_4_1955_2019_",
  plcie_niemowl,
  sep = ""
), "SM", sep = "_")
#### PODZIAŁ danych na okresy względem roku 1989
dane_smiert_rocznie_GOT <- c(
  "bulka_pszenna", "chleb_pszenno_zytni", "maka_pszenna", "Indeks_cen_producja_przem_rocznie",
  "Indeks_cen_all_rocznie", "co2_emisje_smiert", dane_ludnosc_smiert, dane_smiert_niemowlat,
  "wspolcz_samobojstw_rocznie_1990_2020", "kurs_dolara_rocznie"
)
for (ramka_danych_kolumnowe in dane_smiert_rocznie_GOT) {
  q <- get(ramka_danych_kolumnowe)
  q <- q[between(as.numeric(q[, 1]), 1950, 2020), ]
  assign(x = ramka_danych_kolumnowe, value = q)
  dane_przyczynowe_smiert_rocznie <- c(dane_przyczynowe_smiert_rocznie, ramka_danych_kolumnowe)
  if (dim(q[between(as.numeric(q[, 1]), 1950, 1988), ])[1] > 0 &
    dim(q[between(as.numeric(q[, 1]), 1990, 2020), ])[1] > 0) {
    assign(
      paste(ramka_danych_kolumnowe, "_przed_1989", sep = ""),
      q[between(as.numeric(q[, 1]), 1950, 1988), ]
    )
    assign(
      paste(ramka_danych_kolumnowe, "_po_1989_2020", sep = ""),
      q[between(as.numeric(q[, 1]), 1990, 2020), ]
    )
    dane_przyczynowe_smiert_rocznie <- c(
      dane_przyczynowe_smiert_rocznie,
      paste(ramka_danych_kolumnowe, "_przed_1989", sep = ""),
      paste(ramka_danych_kolumnowe, "_po_1989_2020", sep = "")
    )
  }
}
dane_przyczynowe_ramki_wierszowe_smiert_rocznie <- c(
  "przecietne_mies_wynagrodzenie_brutto", "przecietne_wydatki_ogolem", dane_dochody,
  "przecietna_liczba_os_w_gospodarstw_pracujacy", "kwoty_bazowe_1999_2022_rocz_bez_kor"
)
for (ramka_danych_wierszowe in dane_przyczynowe_ramki_wierszowe_smiert_rocznie) {
  q <- get(ramka_danych_wierszowe)[, ramka_danych_wierszowe != "-"]
  q <- q[, between(as.numeric(names(q)), 1950, 2020)]
  assign(x = ramka_danych_wierszowe, value = q)
  dane_przyczynowe_smiert_rocznie <- c(dane_przyczynowe_smiert_rocznie, ramka_danych_wierszowe)
  if (length(q[, names(q) < 1989]) > 0 & length(q[, names(q) > 1989]) > 0) {
    assign(
      paste(ramka_danych_wierszowe, "_przed_1989", sep = ""),
      q[, between(as.numeric(names(q)), 1950, 1989)]
    )
    assign(
      paste(ramka_danych_wierszowe, "_po_1989_2020", sep = ""),
      q[, between(as.numeric(names(q)), 1990, 2020)]
    )
    dane_przyczynowe_smiert_rocznie <- c(
      dane_przyczynowe_smiert_rocznie,
      paste("_przed_1989", sep = ""),
      paste(ramka_danych_wierszowe, "_po_1989_2020", sep = "")
    )
  }
}

dane_przyczynowe_smiert_rocznie

### Przycięcie danych przyczynowych Śmiertelności kwartalnie
### (od 3 kwartału 2011 r. do 3 kwartału 2022 r.), porównanie: smiertelnosc_kwart
Cena_m2_dzialki_GOT <- Cena_m2_dzialki[between(Cena_m2_dzialki$Rok, 2011, 2022), ]
Cena_m2_dzialki_GOT <- Cena_m2_dzialki_GOT[(dim(Cena_m2_dzialki_GOT)[1] - 2):1, ]
Cena_m2_dzialki_GOT_po_cov <- Cena_m2_dzialki_GOT[
  Cena_m2_dzialki_GOT[, 1] == 2020 & Cena_m2_dzialki_GOT[, 2] > 1,
]
Cena_m2_dzialki_GOT_przed_cov <- Cena_m2_dzialki_GOT[
  Cena_m2_dzialki_GOT[, 1] < 2020 | Cena_m2_dzialki_GOT[, 2] == 1,
]
#### Poprawka oznaczeń dat, tj. miesiący na kwartały
for (ramki_kwartal_do_poprawy in c(
  "produkt_krajowy_kwart",
  "cena_produktu_krajowego_brutto"
)) {
  q <- get(ramki_kwartal_do_poprawy)
  kwartaly <- as.numeric(q[, 2])
  for (nr in 1:length(kwartaly)) {
    kwartaly[nr] <- (kwartaly[nr] - 1) / 3 + 1
  }
  q[, 2] <- kwartaly
  names(q)[1:2] <- c("Rok", "Kwartal")
  assign(paste(ramki_kwartal_do_poprawy, "_GOT", sep = ""), q[between(as.numeric(q$Rok), 2011, 2022), ][-c(1, 2), ])
}

dane_przyczynowe_smiert_kwart <- c(
  "Cena_m2_dzialki_GOT", "Cena_m2_dzialki_GOT_przed_cov",
  "Cena_m2_dzialki_GOT_po_cov", "produkt_krajowy_kwart_GOT",
  "cena_produktu_krajowego_brutto_GOT"
)


# MIARY OPISOWE
## Przygotowanie ramek z podziałem na zgony/Śmiertelność, odpowiednie okresy
## Nazwy zbiorów danych znajdują się w pierwszej kolumnie.
okresy_rodzaje_zgony <- c("_mies", "_rocznie", "_tygod")
okresy_rodzaje_smiert <- c("_mies", "_rocznie", "_kwart")
skutki <- c("zgony", "smiert")
for (skutek in skutki) {
  for (okres in get(paste("okresy_rodzaje", skutek, sep = "_"))) {
    assign(x = paste(paste("statystyki", skutek, sep = "_"), okres, sep = ""), value = data.frame())
    s_z <- get(paste(paste("statystyki", skutek, sep = "_"), okres, sep = ""))
    D_s <- get(paste(paste("dane_skutkowe", skutek, sep = "_"), okres, sep = ""))
    D_p <- get(paste(paste("dane_przyczynowe", skutek, sep = "_"), okres, sep = ""))
    s_z[1:length(D_s), 1] <- D_s
    s_z[(length(D_s) + 1):(length(D_s) + length(D_p)), 1] <- D_p
    assign(x = paste(paste("statystyki", skutek, sep = "_"), okres, sep = ""), value = s_z)
  }
}
stat_zgony <- paste("statystyki_zgony", okresy_rodzaje_zgony, sep = "")
stat_smiert <- paste("statystyki_smiert", okresy_rodzaje_smiert, sep = "")

## Wyznaczenie podstawowych parametrów opisowych
### Pomocnicza funkcja wyznaczająca rozstęp
R <- function(wartosci) {
  return(max(wartosci) - min(wartosci))
}
### Pomocnicza funkcja wyznaczająca współczynnik zmienności (V_S)
V_S <- function(wartosci) {
  return(sd(wartosci) / mean(wartosci))
}
### Pomocnicza funkcja wyznaczająca współczynnik zmienności (V_Q)
V_Q <- function(wartosci) {
  return((IQR(wartosci, na.rm = T) / 2) / median(wartosci))
}

for (skutek in skutki) {
  for (okresy_statystyk in 1:(length(get(paste("stat", skutek, sep = "_"))))) {
    statystyki_i <- get(get(paste("stat", skutek, sep = "_"))[okresy_statystyk])
    # Wyznaczenie miar opisowych, zapisanie w ramkach danych
    for (nr_danych in 1:dim(statystyki_i)[1]) {
      dane_i <- get(statystyki_i[nr_danych, 1])
      # Przypisanie parametrów opisowych do kolejnych kolumn ramki danych
      if (dim(dane_i)[1] > 2) {
        # Ewentualne usunięcie zbędnych kolumn
        if (okresy_statystyk == 2 & dim(dane_i)[2] > 2) {
          dane_i <- dane_i[, c(1, dim(dane_i)[2])]
        }
        # Minimum, maksimum, kwartyle 1-3 rz?du (w tym mediana), średnia arytm.
        statystyki_i[nr_danych, 2:7] <- round(summary(as.numeric(
          dane_i[, ifelse(okresy_statystyk == 2, 2, 3)]
        ))[c(1, 6, 2, 3, 5, 4)], digits = 6)
        # średnia geometryczna
        suppressWarnings({
          statystyki_i[nr_danych, 8] <- round(geometric.mean(
            as.numeric(dane_i[, ifelse(okresy_statystyk == 2, 2, 3)])
          ), 6)
        })
        # średnia harmoniczna
        statystyki_i[nr_danych, 9] <- round(harmonic.mean(
          as.numeric(dane_i[, ifelse(okresy_statystyk == 2, 2, 3)])
        ), 6)
        # Wariancja
        statystyki_i[nr_danych, 10] <- round(var(
          as.numeric(dane_i[, ifelse(okresy_statystyk == 2, 2, 3)])
        ), 6)
        # Odchylenie standardowe
        statystyki_i[nr_danych, 11] <- round(sd(
          as.numeric(dane_i[, ifelse(okresy_statystyk == 2, 2, 3)])
        ), 6)
        # Odchylenie przeciętne
        statystyki_i[nr_danych, 12] <- round(madstat(
          as.numeric(dane_i[, ifelse(okresy_statystyk == 2, 2, 3)])
        ), 6)
        # współczynnik zmienności (V_S)
        statystyki_i[nr_danych, 13] <- round(V_S(
          as.numeric(dane_i[, ifelse(okresy_statystyk == 2, 2, 3)])
        ), 6)
        # rozstęp
        statystyki_i[nr_danych, 14] <- round(R(as.numeric(
          dane_i[, ifelse(okresy_statystyk == 2, 2, 3)]
        )), digits = 6)
        # rozstęp ćwiartkowy
        statystyki_i[nr_danych, 15] <- round(IQR(
          as.numeric(dane_i[, ifelse(okresy_statystyk == 2, 2, 3)]),
          na.rm = T
        ), 6)
        # współczynnik zmienności (V_Q)
        statystyki_i[nr_danych, 16] <- round(V_Q(
          as.numeric(dane_i[, ifelse(okresy_statystyk == 2, 2, 3)])
        ), 6)
        # współczynnik asymetrii
        statystyki_i[nr_danych, 17] <- round(skewness(
          as.numeric(dane_i[, ifelse(okresy_statystyk == 2, 2, 3)])
        ), 6)
        # Kurtoza
        statystyki_i[nr_danych, 18] <- round(kurtosis(
          as.numeric(dane_i[, ifelse(okresy_statystyk == 2, 2, 3)])
        ), 6)
      } else {
        # Minimum, maksimum, kwartyle 1-3 rzędu (w tym mediana), średnia arytm.
        statystyki_i[nr_danych, 2:7] <- round(summary(as.numeric(dane_i[1, ]))[c(1, 6, 2, 3, 5, 4)], 6)
        # średnia geometryczna
        statystyki_i[nr_danych, 8] <- round(geometric.mean(as.numeric(dane_i[1, ])), 6)
        # średnia harmoniczna
        statystyki_i[nr_danych, 9] <- round(harmonic.mean(as.numeric(dane_i[1, ])), 6)
        # Wariancja
        statystyki_i[nr_danych, 10] <- round(var(as.numeric(dane_i[1, ])), 6)
        # Odchylenie standardowe
        statystyki_i[nr_danych, 11] <- round(sd(as.numeric(dane_i[1, ])), 6)
        # Odchylenie przeciętne
        statystyki_i[nr_danych, 12] <- round(madstat(as.numeric(dane_i[1, ])), 6)
        # współczynnik zmienności (V_S)
        statystyki_i[nr_danych, 13] <- round(V_S(as.numeric(dane_i[1, ])), 6)
        # rozstęp
        statystyki_i[nr_danych, 14] <- round(R(as.numeric(dane_i[1, ])), 6)
        # rozstęp kwartylny
        statystyki_i[nr_danych, 15] <- round(IQR(as.numeric(dane_i[1, ])), 6)
        # współczynnik zmienności (V_Q)
        statystyki_i[nr_danych, 16] <- round(V_Q(as.numeric(dane_i[1, ])), 6)
        # współczynnik asymetrii
        statystyki_i[nr_danych, 17] <- round(skewness(as.numeric(dane_i[1, ])), 6)
        # Kurtoza
        statystyki_i[nr_danych, 18] <- round(kurtosis(as.numeric(dane_i[1, ])), 6)
      }
    }


    names(statystyki_i) <- c(
      "Skrót nazwy danych dla statystyk",
      "X_min", "X_max", "Q1", "Q2(Me)", "Q3", "?r.", "G", "H", "Var", "S", "d", "V_S", "R", "IQR", "V_Q", "A", "K"
    )
    assign(x = get(paste("stat", skutek, sep = "_"))[okresy_statystyk], value = statystyki_i)
    # Zapisanie otrzymanych statystyk dot. zgonów i Śmiertelności dla poszczególnych okresów w pliku
    statystyki_i_plik <- cbind(statystyki_i, statystyki_i[, 1])
    names(statystyki_i_plik)[length(names(statystyki_i_plik))] <- names(statystyki_i_plik)[1]
    setwd(sciezka_zalacznikow)
    write_xlsx(
      statystyki_i_plik,
      paste(paste(get(paste("stat", skutek, sep = "_"))[okresy_statystyk]), ".xlsx", sep = "")
    )
  }
}
stat_zgony
stat_smiert


# Graficzna reprezentacja danych:
# Przedstawienie wizualne rozkładu wartości wybranych zbiorów danych (po ich skróceniu w celu
# zgodności okresów z danymi dot. zgonów i Śmiertelności) na różnych wykresach
## Wybrane wykresy typu plot - ilość zgonów miesięcznie i tygodniowo
par(mfrow = c(2, 2))
plot(Zgony_osz_mies_2000_2022[, 3],
  main = "Wykres ilości zgonów w Polsce w latach 2000 - 2022",
  col = "green", xlab = "Nr miesiąca po 02.2000 r.", ylab = "ilość zgonów", ylim = c(10000, 60000)
)
lines(Zgony_osz_mies_2000_2022_K[, 3], col = "red")
lines(Zgony_osz_mies_2000_2022_M[, 3], col = "darkblue")
plot(Zgony_osz_mies_2000_2022_przed_cov[, 3],
  col = "green", xlab = "Nr miesiąca po 02.2000 r.",
  ylab = "ilość zgonów", main = "Wykres ilości zgonów w Polsce w latach 2000 - 2020 przed COVID",
  ylim = c(10000, 45000)
)
lines(Zgony_osz_mies_2000_2022_K_przed_cov[, 3], col = "red")
lines(Zgony_osz_mies_2000_2022_M_przed_cov[, 3], col = "darkblue")
plot(Zgony_osz_mies_2000_2022_po_cov[, 3],
  col = "green", xlab = "Nr miesiąca po 03.2020 r.",
  ylab = "ilość zgonów", main = "Wykres ilości zgonów w Polsce w latach 2020 - 2022 po COVID",
  ylim = c(10000, 60000)
)
lines(Zgony_osz_mies_2000_2022_K_po_cov[, 3], col = "red")
lines(Zgony_osz_mies_2000_2022_M_po_cov[, 3], col = "darkblue")
plot(as.numeric(Zgony_tygodniowe_roku_2020_2022_ram_ogolem[, 3]),
  col = "green", ylim = c(3000, 17000),
  xlab = "Nr tygodnia od początku 2020 r.", ylab = "ilość zgonów",
  main = "Wykres ilości zgonów w Polsce tygodniowo w latach 2020 - 2022"
)
lines(as.numeric(Zgony_tygodniowe_roku_2020_2022_ram_kob[, 3]), col = "red")
lines(as.numeric(Zgony_tygodniowe_roku_2020_2022_ram_mez[, 3]), col = "darkblue")

## Wybrane histogramy
par(mfrow = c(2, 2))
hist(M2_mies_GOT[, 3],
  main = "Histogram agregatu M2 (podażu pieniądza) w okresie 02.2000 - 04.2017",
  breaks = 25, col = "blue", xlab = "Wartość agregatu", ylab = "Częstotliwość"
)
hist(M3_bez_korekty_mies_GOT[, 3],
  col = "green", xlab = "Wartość agregatu", ylab = "Częstotliwość",
  main = "Histogram agregatu M3 (podażu pieniądza) w okresie 02.2000 - 09.2022", breaks = 25
)
hist(M3_bez_korekty_mies_przed_COV[, 3],
  col = "red", xlab = "Wartość agregatu", ylab = "Częstotliwość",
  main = "Histogram agregatu M3 (podażu pieniądza) w okresie 02.2000 - 02.2022", breaks = 25
)
hist(M3_bez_korekty_mies_po_COV[, 3],
  main = "Histogram agregatu M3 (podażu pieniądza) w okresie 03.2020 - 09.2022",
  breaks = 5, col = "violet", xlab = "Wartość agregatu", ylab = "Częstotliwość"
)

## Wybrane wykresy gęstości rozkładu danych, porównanie rozkładów
par(mfrow = c(2, 2))
plot(density(eksport_towarow_GOT[, 3], na.rm = TRUE),
  main = "Wykres gęstości rozkładu wartości eksportu towarów dla Polski w latach 2006 - 2021",
  xlab = "Wartość eksportu [z?]", ylab = "Gęstość prawdopodobieństwa wartosci cechy",
  col = "blue", xlim = c(5 * 10^9, 3.6 * 10^10), ylim = c(0, 1.3 * 10^(-10))
)
lines(density(eksport_towarow_przed_COV[, 3], na.rm = TRUE), col = "green")
lines(density(eksport_towarow_po_COV[, 3], na.rm = TRUE), col = "red")
abline(v = mean(eksport_towarow_GOT[, 3], na.rm = TRUE), col = "blue") # średnie w postaci linii pionowych
abline(v = mean(eksport_towarow_przed_COV[, 3], na.rm = TRUE), col = "green")
abline(v = mean(eksport_towarow_po_COV[, 3], na.rm = TRUE), col = "red")
plot(density(import_mies_bez_korekty_GOT[, 3], na.rm = TRUE),
  main = "Wykres gęstości rozkładu wartości importu towarów dla Polski w latach 2000 - 2021",
  xlab = "Wartość importu [z?]", ylab = "Gęstość prawdopodobieństwa wartosci cechy",
  col = "blue", xlim = c(3 * 10^9, 15 * 10^10), ylim = c(0, 2 * 10^(-11))
)
lines(density(import_mies_bez_korekty_przed_COV[, 3], na.rm = TRUE), col = "green")
lines(density(import_mies_bez_korekty_po_COV[, 3], na.rm = TRUE), col = "red")
abline(v = mean(import_mies_bez_korekty_GOT[, 3], na.rm = TRUE), col = "blue")
abline(v = mean(import_mies_bez_korekty_przed_COV[, 3], na.rm = TRUE), col = "green")
abline(v = mean(import_mies_bez_korekty_po_COV[, 3], na.rm = TRUE), col = "red")
plot(density(COVID_potw_tyg[, 3], na.rm = TRUE),
  col = "blue",
  main = "Wykres gęstości rozkładu ilości przypadków COVID dla Polski do 9 tyg. 2022 r.",
  xlab = "Liczebność danej cechy", ylab = "Gęstość prawdopodobieństwa wartosci cechy",
  xlim = c(-1 * 10^(7), 4.5 * 10^7), ylim = c(0, 4.2 * 10^(-8))
)
lines(density(COVID_wyzdr_tyg[, 3], na.rm = TRUE), col = "green")
abline(v = mean(COVID_potw_tyg[, 3], na.rm = TRUE), col = "blue")
abline(v = mean(COVID_wyzdr_tyg[, 3], na.rm = TRUE), col = "green")
plot(density(COVID_zgony_tyg[, 3], na.rm = TRUE),
  col = "red", xlab = "ilość zgonów",
  ylab = "Gęstość prawdopodobieństwa wartosci cechy",
  main = "Wykres gęstości rozkładu ilości zgonów na COVID dla Polski do 9 tyg. 2022 r."
)
abline(v = mean(COVID_zgony_tyg[, 3], na.rm = TRUE), col = "red")

## Wybrane wykresy pudełkowe
par(mfrow = c(2, 2))
zgony_niemowlat_boxplot_dane <- data.frame(
  zgony_niemowlat_1_4_1955_2019_Total_ZG[, 2],
  zgony_niemowlat_1_4_1955_2019_Male_ZG[, 2],
  zgony_niemowlat_1_4_1955_2019_Female_ZG[, 2]
)
names(zgony_niemowlat_boxplot_dane) <- c("ogółem", "Chłopcy", "Dziewczynki")
boxplot(zgony_niemowlat_boxplot_dane,
  col = "blue", xlab = "Płcie", ylab = "Ilość zgonów", border = "black",
  main = "Wykres pudełkowy zgonów niemowląt w latach 1955 - 2019 w Polsce"
)
zgony_niemowlat_boxplot_dane_przed_1989 <- data.frame(
  zgony_niemowlat_1_4_1955_2019_Total_ZG_przed_1989[, 2],
  zgony_niemowlat_1_4_1955_2019_Male_ZG_przed_1989[, 2],
  zgony_niemowlat_1_4_1955_2019_Female_ZG_przed_1989[, 2]
)
names(zgony_niemowlat_boxplot_dane_przed_1989) <- c("ogółem", "Chłopcy", "Dziewczynki")
boxplot(zgony_niemowlat_boxplot_dane_przed_1989,
  col = "blue", xlab = "Płcie", ylab = "Ilość zgonów",
  border = "black", main = "Wykres pudełkowy zgonów niemowląt w latach 1955 - 1988 w Polsce"
)
zgony_niemowlat_boxplot_dane_po_1989 <- data.frame(
  zgony_niemowlat_1_4_1955_2019_Total_ZG_po_1989[, 2],
  zgony_niemowlat_1_4_1955_2019_Male_ZG_po_1989[, 2],
  zgony_niemowlat_1_4_1955_2019_Female_ZG_po_1989[, 2]
)
names(zgony_niemowlat_boxplot_dane_po_1989) <- c("ogółem", "Chłopcy", "Dziewczynki")
boxplot(zgony_niemowlat_boxplot_dane_po_1989,
  col = "blue", xlab = "Płcie", ylab = "Ilość zgonów",
  border = "black", main = "Wykres pudełkowy zgonów niemowląt w latach 1990 - 2019 w Polsce"
)
smiertelnosc_niemowlat_boxplot_dane <- data.frame(
  zgony_niemowlat_1_4_1955_2019_Total_SM[, 2],
  zgony_niemowlat_1_4_1955_2019_Male_SM[, 2],
  zgony_niemowlat_1_4_1955_2019_Female_SM[, 2]
)
names(smiertelnosc_niemowlat_boxplot_dane) <- c("ogółem", "Chłopcy", "Dziewczynki")
boxplot(smiertelnosc_niemowlat_boxplot_dane,
  xlab = "Płcie", ylab = "Śmiertelność", border = "black",
  main = "Wykres pudełkowy Śmiertelności niemowląt w latach 1955 - 2019 w Polsce", col = "blue"
)

## Wykresy dystrybuanty empirycznej
par(mfrow = c(2, 2))
plot(
  ecdf(as.numeric(nieletni_orzeczenia_karalne_demoralizacja)),
  col = "blue", xlim = c(2000, 20000), xlab = "ilość przestępstw", ylab = "Wartość dystrybuanty",
  main = "Wykres dystrybuanty emp. ilości orzeczeń kar. z powodu demoralizacji w Polsce"
)
lines(ecdf(as.numeric(nieletni_orzeczenia_karalne_demoralizacja_M)), col = "green")
lines(ecdf(as.numeric(nieletni_orzeczenia_karalne_demoralizacja_K)), col = "red")
plot(
  x = ecdf(as.numeric(nieletni_orzeczenia_czyny_karalne_2000_2019)),
  col = "blue", xlim = c(5000, 47000), xlab = "ilość przestępstw", ylab = "Wartość dystrybuanty",
  main = "Wykres dystrybuanty emp. ilości orzeczeń kar. z powodu czynów kar. w Polsce"
)
lines(ecdf(as.numeric(nieletni_orzeczenia_czyny_karalne_2000_2019_M)), col = "green")
lines(ecdf(as.numeric(nieletni_orzeczenia_czyny_karalne_2000_2019_K)), col = "red")
plot(
  x = ecdf(prawomocnie_skazani_ogolem_1946_2018[, 2]),
  col = "blue", xlab = "ilość przestępstw", ylab = "Wartość dystrybuanty",
  main = "Wykres dystrybuanty emp. ilości prawomocnie skazanych w Polsce"
)
lines(ecdf(prawomocnie_skazani_ogolem_1946_2018_przed_1989[, 2]), col = "green")
lines(ecdf(prawomocnie_skazani_ogolem_1946_2018_po_1989[, 2]), col = "red")
plot(
  x = ecdf(as.numeric(pozbaw_woln_rocznie_25_w1inst_1970_2020[, 2])), xlim = c(30, 210),
  col = "blue", xlab = "ilość przestępstw", ylab = "Wartość dystrybuanty",
  main = "Wykres dystrybuanty emp. ilości pozbawień woln. na 25 lat w Polsce"
)
lines(ecdf(as.numeric(pozbaw_woln_rocznie_25_w1inst_1970_2020_przed_1989[, 2])), col = "green")
lines(ecdf(as.numeric(pozbaw_woln_rocznie_25_w1inst_1970_2020_po_1989[, 2])), col = "red")


# Weryfikacja hipotez - test rozkładu Shapiro-Wilka, testy korelacji Pearsona, Spearmana, wykresy
## Przygotowanie ramek z podziałem na zgony/Śmiertelność, odpowiednie okresy
## Nazwy danych znajdują się w pierwszej kolumnie (jak w przypadku statystyk)
kor_zgony <- gsub(pattern = "statystyki", replacement = "korelacje", stat_zgony)
kor_smiert <- gsub(pattern = "statystyki", replacement = "korelacje", stat_smiert)
kor_skutki_zgony <- gsub("statystyki", "korelacje_skutki", stat_zgony)
kor_skutki_smiert <- gsub("statystyki", "korelacje_skutki", stat_smiert)
for (skutek in skutki) {
  for (okres in get(paste("okresy_rodzaje", skutek, sep = "_"))) {
    assign(paste(paste("korelacje", skutek, sep = "_"), okres, sep = ""), data.frame())
    k <- get(paste(paste("korelacje", skutek, sep = "_"), okres, sep = ""))
    assign(x = paste(paste("korelacje_skutki", skutek, sep = "_"), okres, sep = ""), value = c())
    kp <- get(paste(paste("korelacje_skutki", skutek, sep = "_"), okres, sep = ""))
    D_s <- get(paste(paste("dane_skutkowe", skutek, sep = "_"), okres, sep = ""))
    D_p <- get(paste(paste("dane_przyczynowe", skutek, sep = "_"), okres, sep = ""))
    k[1:length(D_p), 1] <- D_p
    names(k) <- "Skrót nazwy danych dla korelacji"
    kp[1:length(D_s)] <- D_s
    assign(paste(paste("korelacje", skutek, sep = "_"), okres, sep = ""), k)
    assign(paste(paste("korelacje_skutki", skutek, sep = "_"), okres, sep = ""), kp)
  }
}

## Przycięcie danych skutkowych (oraz w razie potrzeby przyczynowych), sprawdzenie normalności
## rozkładów badanych wartości, wykonanie testów korelacji Pearsona/Spearmana, wykresy korelacji
### Pomocnicza funkcja tworząca wykres pozwalający ocenić liniowość korelacji dwóch prób
linear_plot <- function(wartosci_x, wartosci_y, dane_x, dane_y, cor.method = "pearson", add = "reg.line") {
  w <- ggscatter(data.frame(wartosci_x, wartosci_y),
    x = "wartosci_x", y = "wartosci_y",
    xlab = dane_x, ylab = dane_y, add = add, conf.int = T, cor.coef = T,
    cor.method = cor.method, add.params = list(color = "blue", fill = "green")
  )
  ggpar(w, main = ifelse(test = cor.method == "pearson", yes = "Wykres liniowej korelacji danych",
    no = "Wykres nieliniowej korelacji danych"
  ))
}
wykresy_kor <- c()
for (skutek in skutki) {
  for (okresy_korelacji in 1:(length(get(paste("kor", skutek, sep = "_"))))) {
    # Pobranie listy zbiorów danych potencjalnie przyczynowych
    korelacje_i <- get(get(paste("kor", skutek, sep = "_"))[okresy_korelacji])
    korelacje_skutki_i <- get(
      get(paste("kor_skutki", skutek, sep = "_"))[okresy_korelacji]
    )
    for (nr_danych in 1:dim(korelacje_i)[1]) {
      # Pobranie zbioru danych potencjalnie przyczynowych
      dane_przycz_i <- get(korelacje_i[nr_danych, 1])
      # Ewentualne usunięcie zbędnych kolumn z ramki danych
      if (okresy_korelacji == 2 & dim(dane_przycz_i)[2] > 2 & dim(dane_przycz_i)[1] != 1) {
        dane_przycz_i <- dane_przycz_i[, c(1, dim(dane_przycz_i)[2])]
      }
      # Ustalenie okresu danych przyczynowych dla korelacji
      ## Lata okresu badanego
      min_rok <- ifelse(dim(dane_przycz_i)[1] > 2,
        min(as.numeric(dane_przycz_i[, 1])), min(as.numeric(names(dane_przycz_i)))
      )
      max_rok <- ifelse(dim(dane_przycz_i)[1] > 2,
        max(as.numeric(dane_przycz_i[, 1])), max(as.numeric(names(dane_przycz_i)))
      )
      if (dim(dane_przycz_i)[1] > 2) {
        ## Ewentualne miesiące/kwartały/Tygodnie okresu badanego
        if (okresy_korelacji != 2) {
          min_inne <- as.numeric(dane_przycz_i[1, 2])
          max_inne <- as.numeric(dane_przycz_i[nrow(dane_przycz_i), 2])
          dane_przycz_wartosci_i <- as.numeric(dane_przycz_i[, 3])
        } else {
          dane_przycz_wartosci_i <- as.numeric(dane_przycz_i[, 2])
        }
      } else {
        dane_przycz_wartosci_i <- as.numeric(dane_przycz_i)
      }
      # Przycięcie okresu danych skutkowych do odpowiedniego okresu  dla testu korelacji
      for (nr_danych_skutki in 1:length(korelacje_skutki_i)) {
        # Pobranie zbioru danych skutkowych (zgonów lub Śmiertelności)
        dane_skutki_i <- get(korelacje_skutki_i[nr_danych_skutki])
        if (okresy_korelacji != 2) {
          dane_skutki_i <- dane_skutki_i[between(as.numeric(dane_skutki_i[, 1]), min_rok, max_rok) & (as.numeric(
            dane_skutki_i[, 1]
          ) > min_rok | as.numeric(dane_skutki_i[, 2]) >= min_inne) &
            (as.numeric(dane_skutki_i[, 1]) < max_rok | as.numeric(dane_skutki_i[, 2]) <= max_inne), ]
          dane_skutki_wartosci_i <- as.numeric(dane_skutki_i[, 3])
        } else {
          dane_skutki_i <- dane_skutki_i[between(as.numeric(dane_skutki_i[, 1]), min_rok, max_rok), ]
          dane_skutki_wartosci_i <- as.numeric(dane_skutki_i[, 2])
        }
        # Gdy dane nie mają wspólnych okresów, zostanie przypisana do ramki informacja jako "X".
        if (length(dane_skutki_wartosci_i) != 0) {
          # Jeśli dane skutkowe są krótsze, skrócone zostaną dane przyczynowe.
          if (length(dane_skutki_wartosci_i) != length(dane_przycz_wartosci_i)) {
            # Ustalenie okresu danych skutkowych dla korelacji
            min_rok_skutk <- min(as.numeric(dane_skutki_i[, 1]))
            max_rok_skutk <- max(as.numeric(dane_skutki_i[, 1]))
            if (okresy_korelacji != 2) {
              min_inne_skutk <- as.numeric(dane_skutki_i[1, 2])
              max_inne_skutk <- as.numeric(dane_skutki_i[nrow(dane_skutki_i), 2])
            }
            # Przycięcie danych przyczynowych do odpowiedniego okresu dla testu korelacji
            if (okresy_korelacji != 2) {
              dane_przycz_i_j <- dane_przycz_i[
                between(as.numeric(dane_przycz_i[, 1]), min_rok_skutk, max_rok_skutk) &
                  (dane_przycz_i[, 1] != min_rok_skutk | as.numeric(dane_przycz_i[, 2]) >= min_inne_skutk) &
                  (dane_przycz_i[, 1] != max_rok_skutk |
                    as.numeric(dane_przycz_i[, 2]) <= max_inne_skutk),
              ]
              dane_przycz_wartosci_i_j <- as.numeric(dane_przycz_i_j[, 3])
            } else {
              if (dim(dane_przycz_i)[1] != 1) {
                dane_przycz_i_j <- dane_przycz_i[between(
                  as.numeric(dane_przycz_i[, 1]),
                  min_rok_skutk, max_rok_skutk
                ), ]
                dane_przycz_wartosci_i_j <- as.numeric(dane_przycz_i_j[, 2])
              } else {
                dane_przycz_i_j <- dane_przycz_i[between(
                  as.numeric(names(dane_przycz_i)),
                  min_rok_skutk, max_rok_skutk
                )]
                dane_przycz_wartosci_i_j <- as.numeric(dane_przycz_i_j)
              }
            }
          } else {
            dane_przycz_i_j <- dane_przycz_i
            dane_przycz_wartosci_i_j <- dane_przycz_wartosci_i
          }
          # W przypadku małej ilości danych (poniżej 20 rekordów dla grup danych nie będących
          # skutkiem podziału względem COVID, lub 1989r, lub poniżej 3 rekordów testy zostaną
          # pominięte, a informacja o tym zapisana jako "N".
          if (sum(unique(length(dane_skutki_wartosci_i) >= 20 |
            (str_count(
              toupper(paste(
                korelacje_skutki_i[nr_danych_skutki],
                korelacje_i[nr_danych, 1]
              )),
              c(toupper(paste("COV", collapse = " ")), "88", "89", "90")
            ) > 0) &
              length(dane_skutki_wartosci_i) >= 3))) {
            # Sprawdzone zostanie założenie normalności rozkładu wartości zbiorów danych przy
            # pomocy testu Shapiro-Wilka. Jeśli można założyć normalność obu rozkładów (p>=0.05)
            # przeprowadzony zostanie test korelacji Pearsona, w innym wypadku test Spearmana.
            if (shapiro.test(dane_skutki_wartosci_i)[2] >= 0.05 & shapiro.test(
              dane_przycz_wartosci_i_j
            )[2] >= 0.05) {
              # Wykonanie testu korelacji Pearsona i zapisanie wyniku w ramce
              kor_test <- cor.test(dane_przycz_wartosci_i_j, dane_skutki_wartosci_i)
              # Zapisanie wyników testu korelacji Persona (wartości p-value oraz korelacji - cor)
              korelacje_i[nr_danych, (1 + nr_danych_skutki)] <- paste(
                round(kor_test$p.value, 4), round(kor_test$estimate, 4),
                sep = " p|cor "
              )
              # Zapisanie wykresu pokazującego zależność liniową skorelowanych zmiennych
              # jeśli dane nie są skutkiem podziału wzgl. COVID, lub roku 1989
              if (sum(str_count(
                toupper(paste(korelacje_skutki_i[nr_danych_skutki], korelacje_i[nr_danych, 1])),
                c("COV", "88", "89", "90")
              )) == 0) {
                nazwa_wykresu <- paste("linear_plot_", skutek, get(
                  paste("okresy_rodzaje_", skutek, sep = "")
                )[okresy_korelacji], "_", nr_danych, "_",
                nr_danych_skutki,
                sep = ""
                )
                assign(nazwa_wykresu, linear_plot(
                  dane_przycz_wartosci_i_j, dane_skutki_wartosci_i,
                  korelacje_skutki_i[nr_danych_skutki],
                  korelacje_i[nr_danych, 1]
                ))
                wykresy_kor <- c(wykresy_kor, nazwa_wykresu)
                nazwa_wykresu <- paste("linear_plot_S_", skutek, get(
                  paste("okresy_rodzaje_", skutek, sep = "")
                )[okresy_korelacji], "_", nr_danych, "_",
                nr_danych_skutki,
                sep = ""
                )
                assign(nazwa_wykresu, linear_plot(
                  dane_przycz_wartosci_i_j, dane_skutki_wartosci_i,
                  korelacje_skutki_i[nr_danych_skutki],
                  korelacje_i[nr_danych, 1], "spearman", "loess"
                ))
                wykresy_kor <- c(wykresy_kor, nazwa_wykresu)
              }
            } else # Wykonanie testu korelacji Spearmana
            {
              kor_test <- cor.test(dane_przycz_wartosci_i_j, dane_skutki_wartosci_i,
                method = "spearman", exact = FALSE
              )
              # Zapisanie wyników testu korelacji Spearmana (wartości p-value oraz korelacji- rho)
              korelacje_i[nr_danych, (1 + nr_danych_skutki)] <- paste(
                round(kor_test$p.value, 4), round(kor_test$estimate, 4),
                sep = " p|rho "
              )
            }
          } else {
            korelacje_i[nr_danych, (1 + nr_danych_skutki)] <- "N"
          }
        } else {
          korelacje_i[nr_danych, (1 + nr_danych_skutki)] <- "X"
        }
      }
    }
    names(korelacje_i)[2:ncol(korelacje_i)] <- korelacje_skutki_i
    if (skutek == "zgony" & okresy_korelacji == 1) {
      names(korelacje_i)[-c(1, 2)] <- gsub(
        pattern = "Zgony_osz_mies_2000_2022", replacement = "-||-", x = names(korelacje_i)[-c(1, 2)]
      )
    }
    assign(x = get(paste("kor", skutek, sep = "_"))[okresy_korelacji], value = korelacje_i)
    # Zapisanie otrzymanych wyników korelacji dla poszczególnych okresów w pliku
    korelacje_i_plik <- cbind(korelacje_i, korelacje_i[, 1])
    names(korelacje_i_plik)[length(names(korelacje_i_plik))] <- names(korelacje_i_plik)[1]
    setwd(sciezka_zalacznikow)
    write_xlsx(
      korelacje_i_plik,
      paste(paste(get(paste("kor", skutek, sep = "_"))[okresy_korelacji]), ".xlsx", sep = "")
    )
  }
}
kor_zgony
kor_skutki_zgony
kor_smiert
kor_skutki_smiert


### Sprawdzenie liniowości otrzymanych korelacji danych bez podziału ze względu na rok 1989, COVID
grid.arrange(get(wykresy_kor[1]), get(wykresy_kor[3]), get(wykresy_kor[5]), get(wykresy_kor[7]),
  nrow = 2, ncol = 2
)
grid.arrange(arrangeGrob(get(wykresy_kor[9]), get(wykresy_kor[11]), ncol = 2), get(wykresy_kor[13]))
#### Wykresy nr 3,5,13 pokazują korelację liniową. W przypadku wykresów nr 1,7,9,11 jest to jednak
#### raczej korelacja nieliniowa. Porównania obu wersji dla tych par danych zamieszczono niżej.
grid.arrange(get(wykresy_kor[1]), get(wykresy_kor[2]), get(wykresy_kor[7]), get(wykresy_kor[8]),
  nrow = 2, ncol = 2
)
grid.arrange(get(wykresy_kor[9]), get(wykresy_kor[10]), get(wykresy_kor[11]), get(wykresy_kor[12]),
  nrow = 2, ncol = 2
)
#### Pary danych dla wykresów nr 1,7,11 posiadają faktycznie korelacje nieliniowe, zostaną więc
#### ponownie wykonane dla nich testy Spearmana, a wyniki zapisane we wcześniejszych: ramce i pliku.
cor.test_1 <- cor.test(
  as.numeric(
    pozbaw_woln_rocznie_doz_prawomocnie_1996_2018[
      pozbaw_woln_rocznie_doz_prawomocnie_1996_2018$Lata <= 2020, 2
    ]
  ),
  zgony_ogolem_1950_2020[between(zgony_ogolem_1950_2020[, 1], 1996, 2018), 2],
  method = "spearman", exact = F
)
cor.test_7 <- cor.test(
  as.numeric(nieletni_orzeczenia_czyny_karalne_2000_2019_K[
    between(as.numeric(names(nieletni_orzeczenia_czyny_karalne_2000_2019_K)), 2000, 2019)
  ]),
  zgony_ogolem_1950_2020[between(zgony_ogolem_1950_2020[, 1], 2000, 2019), 2],
  method = "spearman", exact = F
)
cor.test_11 <- cor.test(as.numeric(nieletni_orzeczenia_karalne_demoralizacja_M),
  zgony_ogolem_1950_2020[between(zgony_ogolem_1950_2020[, 1], 2000, 2019), 2],
  method = "spearman", exact = F
)
korelacje_zgony_rocznie[c(56, 80, 82), 2] <-
  c(
    paste(round(cor.test_1$p.value, 4), round(cor.test_1$estimate, 4), sep = " p|rho "),
    paste(round(cor.test_7$p.value, 4), round(cor.test_7$estimate, 4), sep = " p|rho "),
    paste(round(cor.test_11$p.value, 4), round(cor.test_11$estimate, 4), sep = " p|rho ")
  )
write_xlsx(korelacje_zgony_rocznie, "korelacje_zgony_rocznie.xlsx")

## Rankingi korelacji dla danych ogólnych bez podziału, porównania do podzielonych na okresy/płcie
for (okres in okresy_rodzaje_zgony) {
  x <- get(paste("korelacje_zgony", okres, sep = ""))
  assign(
    x = paste("rank_korelacje_zgony", okres, sep = ""),
    value = x[order(abs(as.numeric(str_split(x[, 2], " ", simplify = T)[, 3])), decreasing = T), ]
  )
  write_xlsx(
    get(paste("rank_korelacje_zgony", okres, sep = "")),
    paste(paste("rank_korelacje_zgony", okres, sep = ""), ".xlsx", sep = "")
  )
}
for (okres in okresy_rodzaje_smiert) {
  x <- get(paste("korelacje_smiert", okres, sep = ""))
  assign(
    x = paste("rank_korelacje_smiert", okres, sep = ""),
    value = x[c(order(abs(as.numeric(str_split(x[, 2], " ", simplify = T)[, 3])), decreasing = T)), ]
  )
  write_xlsx(
    get(paste("rank_korelacje_smiert", okres, sep = "")),
    paste(paste("rank_korelacje_smiert", okres, sep = ""), ".xlsx", sep = "")
  )
}
