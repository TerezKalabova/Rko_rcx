# SKRIPT PRO sjednoceni dat pro vyhledavani v kodovnikach

# vyprazdneni promennych z Rka
rm(list=ls())


# knihovny ---------------------

library(stringi)
library(stringr)


# -----------------------------------------------------------------------------------------------
# nacteni dat (z AWK) =============================================================================
# -----------------------------------------------------------------------------------------------

setwd("/home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/02_input_data/04_awk_data")

#--------------------------------------------------
# NKall
# hlavicky: znacka | kod |  popis ||
# vstupni soubor: NKall.txt
# V1=poradi_tk: poradi v souborech (cislo)
# V2=znacka:    znacka (NK/K/Z/ZK/ZZK/prazdne)
# V3=kod:       kod ciselniku/kody_K/cislo polozky v ciselniku (text/cislo)
# V4=popis:     odpoved v ciselniku (text)
#--------------------------------------------------

NKall <- read.table("NKall.txt", header=FALSE, sep=";", quote = "\"", dec= ".") 
names(NKall) <- c("poradi_tk","znacka", "kod", "popis") 


#--------------------------------------------------
# NKB
# hlavicky: kod ciselniku | popis ciselniku | kody K ||
#vstupni soubor: NKB.txt
# V1: kod ciselniku
# V2: nazev ciselniku
# V3: kody_K
#--------------------------------------------------
NKB <- read.table("NKB.txt", header=FALSE, sep=";", quote = "\"", dec= ".") 
names(NKB) <- c("kod_ciselniku", "popis_ciselniku", "kody_K")
# inic uprava vstupnich dat NKB
arch.NKB<-NKB #zaloha pro jistotu. Znovunahrani: NKB<-arch.NKB


#--------------------------------------------------
# seznam_dotazniku 
# arch: seznam_dotazniku<-c("P1", "P2", "P3", "N1", "N3", "N4", "N5", "PN6/1", "PN6/2", "PN6/3", "PN8", "PN18/1", "PN18/2", "PN18/3", "PN18/4", "T1", "T2", "T3", "T4", "T5",  "F1", "F2", "F3", "F4", "F5", "S1", "S2", "S3", "S4", "S5", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "Th4", "Ft1", "Ft2", "Ft3", "Ft4", "Ft5", "Ft6", "Ft7", "Ft8", "Et2", "Et4", "Et5", "Et6", "Et7", "Et8", "Et9", "V18", "Nt1", "Nt2", "Nt3", "Nt4", "Nt5", "Nt6", "Nt8", "Nt9", "V19", "U8", "U11", "U13", "U15", "U18", "V8", "V11", "V13", "V15", "FSS", "FSS8", "FSS11", "FSS13", "FSS15", "FSS17", "FSS19")
# vstupni soubor: seznam_dotazniku.txt
#-----
seznam_dotazniku <- as.vector(unlist(read.table("/home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/02_input_data/seznam_dotazniku.txt", header=TRUE, sep=";")))


# -----------------------------------------------------------------------------------------------
#kontrola a priprava dat
# -----------------------------------------------------------------------------------------------

# odstraneni radku, kde neni _NK kod v prvnim poli
radky_NKB_bez_NK<-rev(which(!grepl("NK",NKB$kod_ciselniku)))
if (length(radky_NKB_bez_NK)>0) {
  NKB<- NKB[-c(radky_NKB_bez_NK),]
}
#ok




# -----------------------------------------------------------------------------------------------
# vypocty =======================================================================================
# -----------------------------------------------------------------------------------------------

setwd("/home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/03_output_data/")


#obsah skriptu - vysledky:
# NK3 - seznam kodu ciselniku a jejich nazvu
# NK1 - list listuu jednotlivych ciselnikuu
# NK4 - list zcela shodnych ciselniku
# NK2 - list listuu, v kazdem listu je seznam otazek a kodu dotazniku vytazenych z kodu K.



#===========================================
# NK3 
# seznam kodu ciselniku a jejich nazvu
# hlavicky: kod_ciselniku | popis ciselniku
# vstup: NKB
# vystup: NK3
#-----

NK3 <- NKB[,1:2]
#ok NK3
# head(NK3) head(NKB)




#===========================================
# NK1 
# list listuu jednotlivych ciselnikuu
#
# uklada jednotlive ciselniky do listu a do txt souboru v podslozce extrahovane ciselniky
# priprava NK1[[]] z NKall (vynecham zcela krok NKA) 
#
# hlavicky velkeho listu: ~~nazvy ciselniku
# hlavicky podlistuu: kod_odpovedi | popis_odpovedi ||
# vstup: NKall
# vystup: NK1 (list listu)
#-----


#vytvoreni podadresare kam se budou ukladat generovane ciselniky
mainDir <- getwd()
subDir <- "extrahovane_ciselniky"
if (file.exists(subDir)){
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}
#opetovne nastaveni puvodniho pracovniho adresare
setwd(mainDir)


# iniciace vysledne struktury -- NK1
NK1<-assign("list", NULL, envir = .GlobalEnv) #prazdny list nazacatku
#uprava hlavicek v NKall
zaloha_jmen_NKall<-names(NKall)
names(NKall)<-c("poradi_tk", "znacka", "kod_odpovedi", "popis_odpovedi") 

#prazdna tabulka kodu NK bez ciselniku
NK_bez_ciselniku<-c()

#vektor zacatkuu a koncuu cisleniku (zacatky: neni to Z ale NK, a tedy NK=Z-2)
NKcis=which(NKall[,2]=="NK")
ZKcis=which(NKall$znacka=="ZK") 
ZZKcis=which(NKall$znacka=="ZZK")

#spojeni koncu cisleniku se znackou ZK a ZZK do promenne ZKcis
if (length(ZZKcis)>0) {
  ZKcis<-sort(c(ZKcis,ZZKcis))
}
 

for (cis in 1 : length(NKcis) ) {
  #priprava 1 ciselniku
  jeden_ciselnik<-c()
  #konrola indexu ZKcis a NKcis
  if (ZKcis[cis]<NKcis[cis+1] || (cis==length(NKcis))) {
    jeden_ciselnik<-NKall[(NKcis[cis]+2) : (ZKcis[cis]), 3:4]
    kod_ciselniku<- gsub("/","",as.character(NKall[NKcis[cis],3]))
    nazev_ciselniku<-as.character(NKall[NKcis[cis],4])
    
    #ulozeni ciselniku do listu
    NK1[[cis]]<-jeden_ciselnik
    names(NK1)[cis]<-kod_ciselniku
    
    #ulozeni ciselniku do txt v podslozce ciselniky
    cis_ulozeni<-jeden_ciselnik
    
    #doplneni nazvu cisleniku do treti bunky prvniho radku
    treti_sl<-rep("", nrow(jeden_ciselnik))
    cis_ulozeni<-cbind(cis_ulozeni, treti_sl)
    names(cis_ulozeni)<-c("kod_odpovedi", "popis_odpovedi", paste("nazev ciselniku: ", nazev_ciselniku, sep="") )
    
    write.table(cis_ulozeni, file = paste("./extrahovane_ciselniky/", kod_ciselniku, ".txt", sep = ""), sep=";", col.names = T, row.names=F, qmethod = "double") 
    
  } else {
    NK_bez_ciselniku<-rbind(NK_bez_ciselniku, NKall[NKcis[cis],])
    #NKcis[cis]<-NKcis[-cis]
    ZKcis<-c( ZKcis[0:(cis-1)], 0, ZKcis[cis:length(ZKcis) ] )
  }
} #for cis

names(NKall)<-zaloha_jmen_NKall
chybejici_ciselniky<-unique(as.vector(NK_bez_ciselniku[,4]))

#ulozeni tabulky NK kodu bez ciselniku
write.table(NK_bez_ciselniku, file = paste("./prehledove_tabulky/", "NK_kody_s_prazdnym_ciselnikem", ".txt", sep = ""), sep=";", col.names = T, row.names=F, qmethod = "double") 
write.table(chybejici_ciselniky, file = paste("./prehledove_tabulky/", "NK_kody_s_prazdnym_ciselnikem_unikatni", ".txt", sep = ""), sep=";", col.names = T, row.names=F, qmethod = "double") 

#ok NK1



#============================================================================
# NK4 
# seznam zcela shodnych ciselniku

# pokud neni zadny shodny ciselnik k hledanemu ciselniku, pak NK4[[cis]] je  nula.
#
# hlavicky listu: ~~nazvy ciselniku
# vstup: NK1
# vystup: NK4 (list vektoru)
#-----

NK4vse<-list() #assign("list", NULL, envir = .GlobalEnv) #prazdny list nazacatku

for (cis in 1:(length(NK1)-1) ) {
  list_cis<-0
  for (listy in (cis+1):length(NK1)) {
    if (cis!=listy && all (!is.na (match(NK1[[cis]][,2], NK1[[listy]][,2]))) && all(!is.na (match(NK1[[listy]][,2], NK1[[cis]][,2]))) ) {
      list_cis <- c(list_cis, names(NK1)[listy])
    }
  }
  NK4vse[[cis]] <-list_cis 
  
}
names(NK4vse)<-names(NK1[1:(length(NK1)-1)])


#---
#vyber jen ciselniku s vyskytem alespon 1 dalsiho zcela shodneho ciselniku
#lapply(NK4, function(x) if (match(x,0)==1) {x<-5})
NK4<-NK4vse
nazvy_ciselniku_NK4<-c()
for (cis in 1:length(NK4vse)) {
  if (all(NK4[[(length(NK4vse))-cis+1]]==0)) { #kdyz ciselnik nema zadnou shodu, je smazan z vypisu NK4
    NK4[[(length(NK4vse))-cis+1]]<-NULL
  }else{ #pokud je aspon jeden shodny ciselnik, vymaze pomocnou nulu na prvnim miste listu a ponecha jen nazvy shodnych ciselniku
    NK4[[(length(NK4vse))-cis+1]] <- NK4[[(length(NK4vse))-cis+1]][-1]
    nazvy_ciselniku_NK4<-c(nazvy_ciselniku_NK4, names(NK4[(length(NK4vse))-cis+1]), unlist(NK4[(length(NK4vse))-cis+1]) )
  }
}
#tabulka nazvu ciselniku se shodou
porovnavaci_nazvy_NK3<-gsub("/","",NK3[,1])
if (length(nazvy_ciselniku_NK4)>0) {
  nazvy_ciselniku_NK4<-unique(nazvy_ciselniku_NK4)
  nazvy_ciselniku_NK4<-cbind(nazvy_ciselniku_NK4, rep(1,length(nazvy_ciselniku_NK4)))
  for (naz in 1:nrow(nazvy_ciselniku_NK4) ) {
    if (nchar(nazvy_ciselniku_NK4[naz,1])>0) {
      nazvy_ciselniku_NK4[naz,2]<- as.character( NK3[(which(porovnavaci_nazvy_NK3==nazvy_ciselniku_NK4[naz,1])) , 2])
    }
  }
}
#ok NK4



#=============================================================================================================
# NK2
# list listuu, v kazdem listu je seznam otazek a kodu dotazniku vytazenych z kodu K.
#
# hlavicky ve velkem listu: ~~nazvy cislenikuu
# hlavicky v podlistech: otazky | dotazniky | otazkyDB". 
# vstup: NKB, seznam_dotazniku
# vystup: NK2
#-----

# iniciace vysledne struktury -- NK2
NK2<-assign("list", NULL, envir = .GlobalEnv) #prazdny list nazacatku

# priprava skal pro moznosti
skala_cisla<-seq(1,49)
skala_rim<-tolower(as.roman(skala_cisla))
skala_pis<-letters[1:26]
prvky_skal<-c(skala_cisla, skala_rim, skala_pis)
typy_skal<-c(rep("cisla",49), rep("rim", 49), rep("pis", 26))
punct_pattern<- "#|%|=|@|#|_|:|<|>|;|'|&"   # odebrana vlnovka, dalsi nefunguji ("~"|"!"|"@"|"#"|"$"|"%"|"^"|"&"|"*"|"("|")"|"{"|"}"|"_"|"+"|":"|"<"|">"|"?"|"."|";"|"'"|"["|"]"|"=") obzvlast pat_zle<-"*, $, ?, ^, +."

#predpriprava dat sloupce kodu_K
NKB$kody_K<-gsub("[[:space:]]", " ", NKB$kody_K)              #nahrazeni vsech typu mezer jednotnou mezerou 1./3 krok
NKB$kody_K<-gsub("\xc2\xa0", " ", NKB$kody_K)                 #nahrazeni vsech typu mezer jednotnou mezerou 2.3 krok
NKB$kody_K<-gsub(" ", " ", NKB$kody_K)                        #nahrazeni vsech typu mezer jednotnou mezerou 3/3 krok
NKB$kody_K<-gsub("_", " ", NKB$kody_K)                        #nahrazeni podtrzitka jednotnou mezerou

NKB$kody_K<-gsub("v", " v ", NKB$kody_K)                      #vlozeni mezer kolem vsech "v"
NKB$kody_K<-gsub("  ", " ", NKB$kody_K)                       #dodatecne odebrani dvojtych mezer
NKB$kody_K<-gsub("  ", " ", NKB$kody_K)                       #dodatecne odebrani dvojtych mezer
NKB$kody_K<-gsub("  ", " ", NKB$kody_K)                       #dodatecne odebrani dvojtych mezer
NKB$kody_K<-gsub("v i", "vi", NKB$kody_K)                     #odebrani mezery u "v" v rim. cisle
NKB$kody_K<-gsub("i v v ", "iv v ", NKB$kody_K)               #odebrani mezery u "v" v rim. cisle
NKB$kody_K<-gsub(" v v ", "v v ", NKB$kody_K)                 #zobecneni odebrani mezer kolem "v" / nutno overit dopady
NKB$kody_K<-gsub("v v ", "v v ", NKB$kody_K)                  #zobecneni odebrani mezer kolem "v" / nutno overit dopady
NKB$kody_K<-gsub(" v ,", "v,", NKB$kody_K)                    #odebrani mezer pokud je v jako koncove pismeno otazky pred carkou
NKB$kody_K<-gsub("x v v", "xv v", NKB$kody_K) 
NKB$kody_K<-gsub("v 1", "v1", NKB$kody_K)                     #odebrani mezery u "v" pred cislem
NKB$kody_K<-gsub("v 2", "v2", NKB$kody_K)         
NKB$kody_K<-gsub("v 3", "v3", NKB$kody_K)                     
NKB$kody_K<-gsub("v 4", "v4", NKB$kody_K)                     
NKB$kody_K<-gsub("v 5", "v5", NKB$kody_K)                     
NKB$kody_K<-gsub("v 6", "v6", NKB$kody_K)                     
NKB$kody_K<-gsub("v 7", "v7", NKB$kody_K)                     
NKB$kody_K<-gsub("v 8", "v8", NKB$kody_K)                     
NKB$kody_K<-gsub("v 9", "v9", NKB$kody_K)      

NKB$kody_K<-gsub("v 1", "v1", NKB$kody_K) 
NKB$kody_K<-gsub("v 2", "v2", NKB$kody_K) 
NKB$kody_K<-gsub("v 3", "v3", NKB$kody_K) 
NKB$kody_K<-gsub("v 4", "v4", NKB$kody_K) 
NKB$kody_K<-gsub("v 5", "v5", NKB$kody_K) 
NKB$kody_K<-gsub("v 6", "v6", NKB$kody_K) 
NKB$kody_K<-gsub("v 7", "v7", NKB$kody_K) 
NKB$kody_K<-gsub("v 8", "v8", NKB$kody_K) 
NKB$kody_K<-gsub("v 9", "v9", NKB$kody_K) 

NKB$kody_K<-(gsub("^ *|(?<= ) | *$", "", NKB$kody_K, perl=T)) #nahrazeni nasobnych mezer jednou mezerou
NKB$kody_K<-gsub("–", "-", NKB$kody_K)                        #nahrazeni spojovniku pomlckou
NKB$kody_K<-gsub("-", "-", NKB$kody_K)                        #nahrazeni divne pomlcky normalni pomlckou snad
#NKB$kody_K<-gsub("- v v ", "-v v ", NKB$kody_K)               #odebrani mezery v retezci "- v v "
NKB$kody_K<-gsub("- ", "-", NKB$kody_K)                       #odebrani mezer kolem pomlcky
NKB$kody_K<-gsub(" -", "-", NKB$kody_K)                       #odebrani mezer kolem pomlcky
  #nahrazeni mezery v casti retezce "pismeno-mezera-cislo" (kod dotazniku, nejen)
 
# prazdna tabulka _NK ciselnikuu s chybou v kodu_K
NK_cis_chyba_kody_K<-c()

# seznam typu chyb
pra_ret<-"Prazdny retezec kody_K"
punct<-"Punct #%=~@#_:<>;'&"
bez_v<-"Chybi delitko otazka-dotaznik"
male_pis<-"Male_pismeno_na_zac_otazky"
velke_V<-"Velke V na >2. miste otazky"
chyba_ret<-"Chyba v retezci"
chyba_skala<-"Nekompatibilni moznosti skaly"
chyba_skala_nevim<-"Neznama chyba ve skale"
velke_pis<-"Vice velkych pismen v otazce / spatne deleni"



for (cis in 1:nrow(NKB)) { #prace po radcich K koduu
  ciselnik<-as.character(NKB$kod_ciselniku[cis])
  kody_K<-as.vector(NKB$kody_K[cis]) #jeden radek vsech kodu K
  chyba_uvnitr_kodu<-NULL
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # kontrola chyb v kody_K, pokud maji evidentne nekompatibilni tvar
  
 #chyby v kody_K, kdy se dale kody_K ani nezkousi analyzovat
  if (is.na(kody_K) || (kody_K=="")) {  #prazdny retezec --> chyba
    #zapise cely chybny radek z NKB do tabulky chyb
      NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba=pra_ret))
      #vypise chybu do listu cis a skace na dalsi kolo forcyklu
      list_K<-as.data.frame(cbind(pra_ret, "chyba", "chyba"))    
    
  } else if (grepl(punct_pattern, kody_K)) { #nektere specialni znaky --> chyba "#|%|=|~|@|#|_|:|<|>|;|'|&"
    #pokud v kody_K je ne ktery divne znaky: !"#$%&'()*+.:;<=>?@[]^_`{|}~. vyhazuju chybu
      #zapise cely chybny radek z NKB do tabulky chyb
      NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba=punct))
      #vypise chybu do listu cis a skace na dalsi kolo forcyklu
      list_K<-as.data.frame(cbind(punct, "chyba", "chyba"))  
            
  }else if (!grepl(" v ", kody_K)) { #kody_K neobsahuje zadne " v " --> chyba
    #zapise cely chybny radek z NKB do tabulky chyb
    NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba=bez_v))
    #vypise chybu do listu cis a skace na dalsi kolo forcyklu
    list_K<-as.data.frame(cbind(bez_v, "chyba", "chyba")) 
    
    
    
  }else{ #kody_K obsahuje asi aspon jeden validni retezec --> ma smysl analyzovat kody_K
   
    #po " v " nenasleduje velke pismeno 
    #reseni problemu typu: kody_K<- "A 8e v a v F5, A 10 e v a v T 5, A 9g v PN 18/2, A 10f v PN 6/2"  
    #vstup: kody_K
    if (grepl("v ", kody_K)) { #pokud kody_K obsahuji spon jedno "v "
       v_ind <- rev(unlist(gregexpr(pattern ="v ", kody_K))) #indexy zacatkuu podretezce "v "
       for (w in 1 : length(v_ind) ) {
          znak_za_v<-substr(kody_K, (v_ind[w]+2), (v_ind[w]+2) )
          if (!grepl("[[:upper:]]",znak_za_v,  perl=TRUE) && znak_za_v!="v") { #neni velke pismeno)
            #smazat mezeru za v
            kody_K<-paste( substr(kody_K, 1, (v_ind[w])), substr(kody_K, (v_ind[w])+2, nchar(kody_K)), sep="" ) 
          }
       }
    }
  # -----------------------------------------------------------------------------------------------
    # docasny pomocny usek, pak to prestehuju TODO
    
             
    #priklad kody_K: 
    # "E 3 v E 5, E 4 v S 5, F 4 v F 5, F 4 v T 5,O 4 v PN 18/2, I 4 v T 2"
    # kody_K<-"A 12bi-v v E 5, A 11i +A 33b1-5 v S 5, A 34b 1-5 v F 5, A 14a1-5 v T 5" 
    # kody_K<-"F 1c1 v E 2 , J 1 c1 v S 2,H 1c v F2, I 1c v T2, O 1c v PN 18/2, J 1iii v PN 6/2 "
    # kody_K<-"A38 aiv,civ v E5"
    # kody_K<-"N14i,ii,iii b 1,2,3 v NT9"
    # kody_K<-"V2i~~i,iia-v v FT5"
  
  #todo 17.8.2015
  #I 2i1,ii1 v P1, F 2i v P 3
  #A 4i v a1,a2,b1,b2 v N3
    
  # -----------------------------------------------------------------------------------------------
    # krajeni 1 kodu K na retezce
          
      #rozdeleni kody_K na useky po vsech carkach
      useky_mezi_carkami<-unlist(strsplit(kody_K, ","))        
      pocet_useku<-length(useky_mezi_carkami)
  
      #slepeni useku kratsich nez 5
      useky_ok<-c()
      nadejny_pahyl<-c()
      useky_predb_ok<-c()
      pahyl_otazek<-c()
      for (us in 1 : length(useky_mezi_carkami) ){ #prochazeni kody_K po usecich
        
        if (nchar(gsub(" ","", useky_mezi_carkami[us]))>=4 && grepl(" v ", useky_mezi_carkami[us])) { # usek ma min delku && obsahuje " v "
          nadejny_pahyl<-gsub(" v "," @ ", useky_mezi_carkami[us] ) #@@@
          nadej_pahyl_ret<-unlist(strsplit(nadejny_pahyl, " @ ")) #@@@
          if (length(nadej_pahyl_ret)==2 &&  is.element(gsub(" ","", nadej_pahyl_ret[2]), seznam_dotazniku) &&  !is.element(gsub(" ","", nadej_pahyl_ret[1]), prvky_skal) ) { # nadejny pahyl se rozpada prave na dve casti  && v nadejnem pahylu to za " v " ma typ seznam_dotazniku, to pred v neni prvek_skal
            useky_predb_ok<-c(useky_predb_ok, paste(pahyl_otazek, nadejny_pahyl, sep="")) #predbezne ulozeno do fin. useku
            pahyl_otazek<-c() #po vlozeni pahylu do vysledneho retezce ho opet vyprazdnim
          } else if (length(nadej_pahyl_ret)==2 &&  is.element(gsub(" ","", nadej_pahyl_ret[2]), seznam_dotazniku) &&  is.element(gsub(" ","", nadej_pahyl_ret[1]), prvky_skal) ) { #nadejny pahyl se rozpada prave na casti: moznost skal - " v " - dotaznik. ->prilozi se k pahylu otazek a ulozi do useky_predb_ok
            #test: if pahyl otazek je prazdny, hod chybu TODO. Zatim predpokladam ze nikdy neni prazdny.
            useky_predb_ok<-c(useky_predb_ok, paste(pahyl_otazek, gsub(" v "," @ ",useky_mezi_carkami[us]), sep="") ) ##@@@
            pahyl_otazek<-c() #po vlozeni pahylu do vysledneho retezce ho opet vyprazdnim ##
          }else {
            #je to asi koncovy pahyl retezce zacinajici moznosti skaly, s vetsi delkou
            pahyl_otazek<-gsub(" ","", paste(pahyl_otazek, nadejny_pahyl,", ", sep="")) #oprava 4.8.2015: pahyl_otazek<-gsub(" ","", paste(pahyl_otazek, useky_mezi_carkami[us],", ", sep=""))
            #tady by asi mela byt kontrola jestli uz je pahyl otazek v dostatecnem formatu a pripadne ho vlozit do useky_predb_ok
            if (us==length(useky_mezi_carkami) && grepl("@", pahyl_otazek) ) { #mozna tu jeste neco chybi (4.8.2015)
              #odstraneni carky nakonci pahylu otazek
              if (str_sub(pahyl_otazek,-1,-1)==",") {
                pahyl_otazek<-str_sub(pahyl_otazek,1,nchar(pahyl_otazek)-1)
              }
              #navraceni mezer kolem zavinace
              pahyl_otazek<-gsub("@", " @ ", pahyl_otazek)
              # ulozi se do vysledneho retezce
              useky_predb_ok<-c(useky_predb_ok, pahyl_otazek)
            }
          }
        } else if  (!grepl(" v ", useky_mezi_carkami[us]) && !is.element(gsub(" ","", useky_mezi_carkami[us]), seznam_dotazniku) ) { # (pahyl otazek) neobsahuje " v " && neni dotaznik
          pahyl_otazek<-gsub(" ","", paste(pahyl_otazek, useky_mezi_carkami[us], ",", sep=""))
        } else if (!grepl(" v ", useky_mezi_carkami[us]) && is.element(gsub(" ","", useky_mezi_carkami[us]), seznam_dotazniku) && (length(useky_predb_ok) >=1) ) { # (pahyl dotaznik) neobsahuje " v " && je na seznamu dotazniku && uz predtim byl aspon jeden usek ulozen
          # doplneni dotazniku k predchozi otazce
          useky_predb_ok[length(useky_predb_ok)] <- paste(useky_predb_ok[length(useky_predb_ok)], ",", useky_mezi_carkami[us], sep="")
        } else if ( nchar(gsub(" ","", useky_mezi_carkami[us]))==4 && grepl(" v ", useky_mezi_carkami[us]) && is.element(substr(useky_mezi_carkami[us],1,1),prvky_skal) && is.element( str_sub ( gsub(" ","", useky_mezi_carkami[us]), -2,-1 ) , seznam_dotazniku)) { #ma to jen 4 znaky, sklada se to z casti: prvek_skal-" v "-dotaznik /vsechny tri casti testovany
          useky_predb_ok<-c(useky_predb_ok, paste(pahyl_otazek, gsub(" v "," @ ",useky_mezi_carkami[us]), sep=""))## @@@
          pahyl_otazek<-c() #po vlozeni pahylu do vysledneho retezce ho opet vyprazdnim ##
          
        } else { # TODO odstranit/ jen pro vyvoj ukazani chyby
          useky_predb_ok<-c(useky_predb_ok, "Chyba nevim kde")
        }
        useky_ok<- useky_predb_ok #pokud je to ok
      }#for u

          

        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        K_po_retezcich<-useky_ok    ###8.6.2015 || unlist(strsplit(kody_K, ", ")) #rozdeleni K kodu do retezcu po dotaznicich podle ", "
  
        if (is.null(useky_ok)) { #retezec se nepovedlo rozdelit ani na jeden validni usek --> chyba ##
          #zapise cely chybny radek z NKB do tabulky chyb
          NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba=chyba_ret))
          #vypise chybu do listu cis a skace na dalsi kolo forcyklu
          list_K<-as.data.frame(cbind(chyba_ret, "chyba", "chyba"))
        } else {
          
            otazky<-dotazniky<-otazkyDB<-c() #vysledne otazky jednoho ciselniku a 1 kodu_K, vkladaji se do 1 listu s nazvem ciselniku
            for (dot in 1: length(K_po_retezcich) ) { #prace po retezcich z kodu K
              #krajeni retezce kodu K na OTAZKA - DOTAZNIK 
              ret<-gsub(" ", "", unlist(strsplit(K_po_retezcich[dot], " @ "))) #rozdeleni K kodu na otazku a dotaznik podle " v " @@@
              OTAZKA<-ret[1]
              DOTAZNIK<-ret[2]
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              # OTAZKA
              
              #cast retezce "OTAZKA":
              #chyby
              if (is.na(OTAZKA)) { #prazdny retezec
                OTAZKA<-"chybejici_kod_otazky" 
              } 
                
              #zakladni tvar otazky
              if (!grepl("-",OTAZKA) && !grepl(",",OTAZKA) ) { #pokud v otazce nejsou zadne pomlcky ani carky, a tedy vice moznosti, je pripravena pro dalsi zpracovani.
                #v poradku, pokracujem
                OTAZKA<- OTAZKA #tohle tu je navic, ale nechci nechat prazdny if, protoze to pak hazi chybu
              }                      
              
              #otazka s carkami
              if (grepl(",",OTAZKA)) { #zmnozena otazka na 1 dotaznik / pokud jsou v otazce carky, otazka se rozdeli do vice podotazek
                podot<-unlist(strsplit(OTAZKA, ",")) 
                #indexy useku ktere nezacinaji velkym pismenem //pro prvni elseif
                notUpperInd<-which(match(tolower(podot),podot) >0)
                #kontrola spojeni osamocenych moznosti typu "i,ii", "g,h", "1,2"
                if (length (which (match (podot, prvky_skal) >0) ) >0) {
                  odstepky_ind=(which (match (podot, prvky_skal) >0) )
                  for (i in 1:length(odstepky_ind)) { #zpetne pripojeni odpojenych moznosti k puvodnim otazkam
                    #priprava zakladu otazky z predchozi otazky odriznutim prvni moznosti
                    posledni_znaky_ot<-c(str_sub(podot[odstepky_ind[i]-1], -1,-1), str_sub(podot[odstepky_ind[i]-1], -2,-1), str_sub(podot[odstepky_ind[i]-1], -3,-1), str_sub(podot[odstepky_ind[i]-1], -4,-1), str_sub(podot[odstepky_ind[i]-1], -5,-1)    )
                    pocet_zn_odriznuti<-length(which (match (posledni_znaky_ot, prvky_skal) >0))
                    zaklad_predch_ot<-str_sub(podot[odstepky_ind[i]-1], 1, -(pocet_zn_odriznuti+1)) 
                    podot[odstepky_ind[i]]<-paste(zaklad_predch_ot, podot[odstepky_ind[i]], sep="")
                  }
                                   
                } else if (length(notUpperInd) >0) {  #aspon jeden zacina malym, ale nebyl to prvek skaly coz je vychytane v predchozim ifu
                    if (min(notUpperInd)>1) { #je druhy a vys
                       #&& prvni znak je ze stejne skaly jako posledni znak predchoziho && da se rozdelit na prvek skaly a zbytek) { # reseni kodu typu "D 6i,iia"
                        for (u in 1 : length(notUpperInd) ) {
                          usek_s_malym<-podot[notUpperInd[u]] #usek s malym
                          usek_pred<-podot[(notUpperInd[u])-1]
                          ### prvni_moznost #?? nevim jestli tu neco nechybi nebo to je navic /4.8.2015
                          
                         # tohle funguje asi jen pro delsi rimska cisla, jinak to nefunnguje pro zackon[2] slozeny ze svou typu skal, protoze prvni moznost orezava jen jeden typ skaly. /5.8.2015
                          zackon<-c(usek_pred, usek_s_malym)
                          posledni_znaky_ot<-c(str_sub(zackon[1], -1,-1), str_sub(zackon[1], -2,-1), str_sub(zackon[1], -3,-1))
                          pocet_zn_odriznuti<-length(which (match (posledni_znaky_ot, prvky_skal) >0))
                          
                          zaklad_ot<-str_sub(zackon[1], 1, -(pocet_zn_odriznuti+1)) 
                          #prvni_moznost<-str_sub(zackon[1], -1, -(pocet_zn_odriznuti)) 
                          prvni_moznost<-str_sub(zackon[1], -(pocet_zn_odriznuti), -1) #oprava 4.8.2015
                          posl_moznost<-zackon[2] 
                          
                         #odchyceni spatneho deleni skaly kvuli chybe v priprave K_kodu typu "D 6i,iia". Toto by melo platit vzdycky.
                         #if (!any(posl_moznost==prvky_skal)) {
                         #  posledni_znaky_ot_kon<-unique(c(str_sub(zackon[2], 1,1), str_sub(zackon[2], 1,2), str_sub(zackon[2], 1,3), str_sub(zackon[2], 1,4), str_sub(zackon[2], 1,5), str_sub(zackon[2], 1,6), str_sub(zackon[2], 1,7), str_sub(zackon[2], 1,8), str_sub(zackon[2], 1,9), str_sub(zackon[2], 1,10)))
                         #  pocet_zn_odriznuti_kon<-length(which (match (posledni_znaky_ot_kon, prvky_skal) >0))
                         #  posl_moznost<-str_sub(zackon[2], 1, pocet_zn_odriznuti_kon)
                         #  koncovy_zaklad_ot<-str_sub(zackon[2], -(length(posledni_znaky_ot_kon)-pocet_zn_odriznuti_kon), -1)
                         if (!is.na(posl_moznost)) {
                           if (!any(posl_moznost==prvky_skal)) {
                             posledni_znaky_ot_kon<-unique(c(str_sub(zackon[2], 1,1), str_sub(zackon[2], 1,2), str_sub(zackon[2], 1,3), str_sub(zackon[2], 1,4), str_sub(zackon[2], 1,5), str_sub(zackon[2], 1,6), str_sub(zackon[2], 1,7), str_sub(zackon[2], 1,8), str_sub(zackon[2], 1,9), str_sub(zackon[2], 1,10)))
                             pocet_zn_odriznuti_kon<-length(which (match (posledni_znaky_ot_kon, prvky_skal) >0))
                             posl_moznost<-str_sub(zackon[2], 1, pocet_zn_odriznuti_kon)
                             koncovy_zaklad_ot<-str_sub(zackon[2], -(length(posledni_znaky_ot_kon)-pocet_zn_odriznuti_kon), -1) 
                             if(prvni_moznost==koncovy_zaklad_ot) {
                                typ_orez_2<-typy_skal[c(which(posl_moznost==prvky_skal))]
                                orez_2<-c(str_sub(zaklad_ot, -1,-1), str_sub(zaklad_ot, -2,-1), str_sub(zaklad_ot, -3,-1))
                                pocet_orez_2<-length(which (match (orez_2, prvky_skal) >0))
                                prvni_moznost<-orez_2[pocet_orez_2]
                                if (any(duplicated(c(typy_skal[c(which(prvni_moznost==prvky_skal))], typy_skal[c(which(posl_moznost==prvky_skal))])))) {
                                  zaklad_ot<-str_sub(zackon[1],1,-(nchar(koncovy_zaklad_ot)+nchar(prvni_moznost) + 1) )
                                } else {
                                  otazky_moznosti<- "chyba_deleni_moznosti"
                                  NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba="chyba_deleni_moznosti"))
                                }
                             } #/if(prvni_moznost==koncovy_zaklad_ot) {
                           } else {
                             koncovy_zaklad_ot<-c()
                           }
                         }else{
                           NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba="posl_moznost_je_NA"))
                         }
                          
                          typy_zackon<-typy_skal[c(which(str_sub(usek_pred,-1,-1)==prvky_skal), which(str_sub(usek_s_malym,1,1)==prvky_skal))]
                          if (all(typy_zackon=="cisla")) {
                            skala<-skala_cisla
                          } else if ((length(typy_zackon)<=3) &&  length(which(typy_zackon=="pis"))==2 ) {
                            skala<-skala_pis
                          } else if ((length(typy_zackon)<=4) && length(which(typy_zackon=="rim"))==2 ) {
                            skala<-skala_rim
                          } else if ((length(typy_zackon)==2) && typy_zackon[1]!=typy_zackon[2]) {
                            #je tam chyba ve skale:
                            #zapise cely chybny radek z NKB do tabulky chyb
                            NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba=chyba_skala))
                            #vypise chybu do listu cis a skace na dalsi kolo forcyklu
                            chyba_uvnitr_kodu<-chyba_skala #list_K<-as.data.frame(cbind(chyba_skala, "chyba", "chyba"))
                          } else { #chyba ve skale nevim
                            NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba=chyba_skala_nevim))
                            #vypise chybu do listu cis a skace na dalsi kolo forcyklu
                            chyba_uvnitr_kodu<-chyba_skala_nevim #list_K<-as.data.frame(cbind(chyba_skala, "chyba", "chyba"))
                          }
                          
                          #rozdeleni usek_s_malym na prvek skaly a neprazdny zbytek
                          if (is.null(chyba_uvnitr_kodu)) { #toto osetruje zatim jen chybu v moznostech skaly (17.6.2015), pokud je to TRUE, neni tam chyba.
                            
                            #vytvoreni moznosti ktere se budou pripojovat k zakladu otazky
                            moznosti<-c(prvni_moznost, posl_moznost)
                            otazky_moznosti<-paste(zaklad_ot, moznosti, koncovy_zaklad_ot, sep="")
                            
                            #vlozeni namnozenych otazek do podot, jez zastupuje ve for cyklu puvodni retezec OTAZKA
                            puv_podot_zac<-podot[-c((notUpperInd[u]-1) : length(podot))]
                            puv_podot_kon<-podot[-c(0 : notUpperInd[u])]
                            podot<-c(puv_podot_zac, otazky_moznosti, puv_podot_kon)
                          } else {
                            podot<-paste(chyba_uvnitr_kodu, ": ", OTAZKA, sep="")
                          }
                          
                          OTAZKA<-podot ####  
                        } #for u
                    } #if min
               } #else if (length(notUpperInd) >0)
                OTAZKA<-podot
              } #if otazka s carkami         
              
              #otazka s pomlckami
              if (any(grepl("-",OTAZKA))) { #v otazce je pomlcka. otazka muze obsahovat jeden i vice prvku vektoru 
                  #cvicna OTAZKA: OTAZKA<-c("S1a3-5" ,  " S2f" ,    " D2e5s"  , "E1di-iii", " A1dg" ,   " A1dh",    " S5d7ii",  " S5d7iii")
                  #cvicna OTAZKA lvl2:  OTAZKA<-"F1a-d4"
                  podot<-OTAZKA
                  pocet_pomlcek<-length(which (grepl("-",OTAZKA))) 
                  for (j in 1 : pocet_pomlcek ) {
                    ot_pomlcka_ind = max(rev(which(grepl("-",OTAZKA)))) #vektor poradi otazek s pomlckou ve vektoru OTAZKA
                    zackon<-unlist(strsplit(OTAZKA[ot_pomlcka_ind], "-"))
                    posledni_znaky_ot<-c(str_sub(zackon[1], -1,-1), str_sub(zackon[1], -2,-1), str_sub(zackon[1], -3,-1))
                    pocet_zn_odriznuti<-length(which (match (posledni_znaky_ot, prvky_skal) >0))
                    
                    zaklad_ot<-str_sub(zackon[1], 1, -(pocet_zn_odriznuti+1)) 
                    prvni_moznost<-str_sub(zackon[1], -1, -(pocet_zn_odriznuti)) 
                    posl_moznost<-zackon[2] 
                    
                    #odchyceni spatneho deleni skaly kvuli chybe v priprave K_kodu
                    if (!is.na(posl_moznost)) {
                      if (!any(posl_moznost==prvky_skal)) {
                        posledni_znaky_ot_kon<-unique(c(str_sub(zackon[2], 1,1), str_sub(zackon[2], 1,2), str_sub(zackon[2], 1,3), str_sub(zackon[2], 1,4), str_sub(zackon[2], 1,5), str_sub(zackon[2], 1,6), str_sub(zackon[2], 1,7), str_sub(zackon[2], 1,8), str_sub(zackon[2], 1,9), str_sub(zackon[2], 1,10)))
                        pocet_zn_odriznuti_kon<-length(which (match (posledni_znaky_ot_kon, prvky_skal) >0))
                        posl_moznost<-str_sub(zackon[2], 1, pocet_zn_odriznuti_kon)
                        koncovy_zaklad_ot<-str_sub(zackon[2], -(length(posledni_znaky_ot_kon)-pocet_zn_odriznuti_kon), -1)
                        ###otazky_moznosti<- "chyba_v_priprave_K_kodu"
                      }else{
                        koncovy_zaklad_ot<-c()
                      } 
                    } else {
                      NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba=chyba_skala))
                    }
                    
                    #kdyz je posl_moznost validni, pokracuju dal 
                    #urceni skaly moznosti -- vybrana skala ulozena v promenne "skala"
                    typy_zackon<-typy_skal[c(which(prvni_moznost==prvky_skal), which(posl_moznost==prvky_skal))]
                    if (all(typy_zackon=="cisla")) {
                      skala<-skala_cisla
                    } else if ((length(typy_zackon)<=3) &&  length(which(typy_zackon=="pis"))==2 ) {
                      skala<-skala_pis
                    } else if ((length(typy_zackon)<=4) && length(which(typy_zackon=="rim"))==2 ) {
                      skala<-skala_rim
                    } else if ((length(typy_zackon)==2) && typy_zackon[1]!=typy_zackon[2]) {
                      #je tam chyba ve skale:
                      #zapise cely chybny radek z NKB do tabulky chyb
                      NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba=chyba_skala))
                      #vypise chybu do listu cis a skace na dalsi kolo forcyklu
                      chyba_uvnitr_kodu<-chyba_skala #list_K<-as.data.frame(cbind(chyba_skala, "chyba", "chyba"))
                    } else { #chyba ve skale nevim
                      NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba=chyba_skala_nevim))
                      #vypise chybu do listu cis a skace na dalsi kolo forcyklu
                      chyba_uvnitr_kodu<-chyba_skala_nevim #list_K<-as.data.frame(cbind(chyba_skala, "chyba", "chyba"))
                    }
                    
                    if (is.null(chyba_uvnitr_kodu) && prvni_moznost!="") { #toto osetruje zatim jen chybu v moznostech skaly (17.6.2015), pokud je to TRUE, neni tam chyba.
                      
                      #vytvoreni moznosti ktere se budou pripojovat k zakladu otazky
                      moznosti<-skala[which(prvni_moznost==skala) : which(posl_moznost==skala)]
                      otazky_moznosti<-paste(zaklad_ot, moznosti, koncovy_zaklad_ot, sep="")
                      
                      #vlozeni namnozenych otazek do podot, jez zastupuje ve for cyklu puvodni retezec OTAZKA
                      puv_podot_zac<-podot[-c(ot_pomlcka_ind : length(podot))]
                      puv_podot_kon<-podot[-c(0 : ot_pomlcka_ind)]
                      podot<-c(puv_podot_zac, otazky_moznosti, puv_podot_kon)
                    } else {
                      podot<-paste(chyba_uvnitr_kodu, ": ", OTAZKA, sep="")
                      NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba="chyba_moznosti"))
                    }
                  
                    OTAZKA<-podot ####  
                  } #for j /namnozeni jedne pomlcky v kazdem cyklu
              } 
              
              if(any(grepl("~~",OTAZKA))) { #prepis vlnovek na podrtzitka
                OTAZKA<-gsub("~~","_",OTAZKA)  
              }
             
              #else {
              #  OTAZKA<-paste("neznama_chyba_otazka: ", OTAZKA, sep="")
              #}
              
              #promenna OTAZKA je pripravena k parovani s dotaznikem. prozatim vse ok. 3.6.2015
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              # DOTAZNIK
              
              #cast retezce DOTAZNIK:

              if (is.na(DOTAZNIK)) { #chyba: prazdny retezec
                  DOTAZNIK<-"chybejici_kod_dotaznikuNA"
                  NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba="chybejici_kod_dotaznikuNA"))
              } else if (grepl("-,",DOTAZNIK))  { #chyba: v dotazniku je pomlcka
                  DOTAZNIK<-"chyba_kodovani_dotazniku-"
                  NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba="chyba_kodovani_dotazniku_pomlcka"))
              } else if (!(!nchar(DOTAZNIK)<2 || DOTAZNIK=="H"))  { #chyba: kod dotazniky nema alespon dva znaky
                DOTAZNIK<-"chyba_kodovani_dotazniku"
                NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba="chyba_dotaznik_alespon_2_znaky"))
              } 
              
              if (grepl(",",DOTAZNIK)) { #v retezci dotazniku je carka -- vice dotazniku patri k otazce
                  DOTAZNIK<-unlist(strsplit(DOTAZNIK, ",")) 
              }
              
              if (!grepl("[[:punct:]]", gsub("/","",DOTAZNIK)) ) { #pokud v otazce nejsou zadne znaky !"#$%&'()*+,-.:;<=>?@[]^_`{|}~., a tedy nic sloziteho ale ocekavam ze jde pouze jediny dotaznik, ten je pripraven pro dalsi zpracovani. NOsetreno i pro dotaznik s lomitkem.
                    #v poradku, pokracujem
                    DOTAZNIK<-DOTAZNIK  #je to tu navic, ale nechtela jsem nechat telo cyklu prazdne.
              } else {
                  DOTAZNIK<-paste("neznama_chyba_dotaznik: ", DOTAZNIK, sep="")
                  NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba="neznama_chyba_v_kodu_dotazniku"))
              }
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              # dokonceni uprav jednoho retezce OTAZKA-DOTAZNIK a jeho ulozeni
              
              # kombinovani dotazniku a otazek -- kartezsky soucin poctu dotazniku v DOTAZNIK a poctu otazek v OTAZKA
              komb_OTAZKA<-rep(OTAZKA, each=length(DOTAZNIK))
              komb_DOTAZNIK<-rep(DOTAZNIK, length(OTAZKA))
              
              #vytvoreni vektoru ve tvaru z DB: DOTAZNIK_OTAZKA
              #komb_DOTAZNIK_OTAZKA <-paste(komb_DOTAZNIK, komb_OTAZKA, sep="_")
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              # vkladani otazek, dotazniku a otazekDB do listu za 1 zpracovany retezec OTAZKA-DOTAZNIK
          
              otazky<-c(otazky,komb_OTAZKA)
              dotazniky<-c(dotazniky, komb_DOTAZNIK)
              #otazkyDB<-c(otazkyDB,komb_DOTAZNIK_OTAZKA)
            }#for dot K_po_retezcich (retezcich)
        
  
            #kontrola velkeho pismena na zacatku otazky
            if (any(str_sub(otazky, 1, 1) != toupper(str_sub(otazky, 1, 1)))) { #odchyceni otazek zacinajich malym pismenem (nejspis spatne rozdelene)
              #zapise cely chybny radek z NKB do tabulky chyb
              NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba=male_pis))
              #vypise chybu do listu cis a skace na dalsi kolo forcyklu
              list_K<-as.data.frame(cbind(male_pis, "chyba", "chyba"))
            } else if (any(grepl("V", str_sub(gsub(" ","",otazky), 2)))) { #odchyceni otazek s velkym V na jinem nez prvnim miste a proto spatne delenych
              #zapise cely chybny radek z NKB do tabulky chyb
              NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba=velke_V))
              #vypise chybu do listu cis a skace na dalsi kolo forcyklu
              list_K<-as.data.frame(cbind(velke_V, "chyba", "chyba"))
            } else if (length(grep("[[:upper:]]",unlist(strsplit(toString(otazky), split="")), perl=TRUE))!=length(otazky)) { #odchyceni otazek s vice velkymi pismeny //spatne deleni otazky
              #zapise cely chybny radek z NKB do tabulky chyb
              NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba=velke_pis))
              #vypise chybu do listu cis a skace na dalsi kolo forcyklu
              list_K<-as.data.frame(cbind(velke_pis, "chyba", "chyba"))
            } else {
              
              
              otazkyDB<-paste(dotazniky, otazky,  sep="_")
              list_K<-as.data.frame(cbind(otazky, dotazniky, otazkyDB))  
            
            }
        } # else / if (is.null(useky_ok)) ###
  } #else - kontrola celkovych chyb v kody_K

  NK2[[cis]]<-list_K
}#for cis NK2
names(NK2)<-NKB$kod_ciselniku
#ok NK2

### vypis chyb:
#  NK_cis_chyba_kody_K
write.table(NK_cis_chyba_kody_K, file = "/home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/03_output_data//prehledove_tabulky/chyby_kody_K.txt", sep=";", col.names = T, row.names=F, qmethod = "double") 

#---------------------------------------------------------------------------
# Ukladani a znovunacitani vysledku ========================================
#---------------------------------------------------------------------------


#pripadne ulozeni a znovunacteni vyslednych promennych:
setwd("/home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/03_output_data/RData_output/")
save(NK1, NK2, NK3, NK4, file = "NK1234.RData")
#   load("NK1234.RData")
# ok





