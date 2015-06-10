# SKRIPT PRO sjednoceni dat pro vyhledavani v kodovnikach

# vyprazdneni promennych z Rka
rm(list=ls())

## change the working directory
getwd()
setwd("/home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/awk")

#knihovny ---------------------------------------------------------------------------------------

library(stringi)
library(stringr)


# nacteni dat z AWK =============================================================================

# struktura A: kod v ciselniku | popis odpovedi |  kod otazky ||
NKA <- read.csv('NKA.csv', header=FALSE, sep=";")
names(NKA) <- c("id_odpovedi_neuniq", "popis_odpovedi", "kod_ciselniku") 

# struktura B: kod otazky | popis otazky | kody K
NKB <- read.table("NKB3.txt", header=FALSE, sep=";")
names(NKB) <- c("kod_ciselniku", "popis_otazky", "kody_K")
arch.NKB<-NKB #zaloha pro jistotu. Znovunahrani: NKB<-arch.NKB

#vsechny dotazniky / zkusebni podmnozina # TODO
seznam_dotazniku<-c("P1", "P2", "P3", "N1", "N3", "N4", "N5", "PN6/1", "PN6/2", "PN6/3", "PN8", "PN18/1", "PN18/2", "PN18/3", "PN18/4", "T1", "T2", "T3", "T4", "T5",  "F1", "F2", "F3", "F4", "F5", "S1", "S2", "S3", "S4", "S5", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "Th4", "Ft1", "Ft2", "Ft3", "Ft4", "Ft5", "Ft6", "Ft7", "Ft8", "Et2", "Et4", "Et5", "Et6", "Et7", "Et8", "Et9", "V18", "Nt1", "Nt2", "Nt3", "Nt4", "Nt5", "Nt6", "Nt8", "Nt9", "V19", "U8", "U11", "U13", "U15", "U18", "V8", "V11", "V13", "V15", "FSS", "FSS8", "FSS11", "FSS13", "FSS15", "FSS17", "FSS19")


#------------------------------------------------------------------------------------------------
# uprava nactenych dat pro dalsi zpracovani

# odstraneni radku, kde neni _NK kod v prvnim poli
radky_NKB_bez_NK<-rev(which(!grepl("_NK",NKB$kod_ciselniku)))
if (length(radky_NKB_bez_NK)>0) {
  NKB<- NKB[-c(radky_NKB_bez_NK),]
}
#ok
  

# ==============================================================================================

# struktura 1 : jeden ciselnik i s hlavickou
# --> List listu, v kazdem listu je jeden ciselnik.
#       Struktura v listech: "id_odpovedi_neuniq" | "popis_odpovedi" | "kod_ciselniku". 
#       Kod_ciselniku je take nazvem listu.

NK1<-assign("list", NULL, envir = .GlobalEnv) #prazdny list nazacatku
for (cis in 1: length(unique(NKA$kod_ciselniku)) ) {
  ciselnik<-as.vector(unique(NKA$kod_ciselniku))[cis] #nazev ciselniku ulozen do promenne "ciselnik"
  jeden_ciselnik<-NKA[which(NKA$kod_ciselniku==ciselnik),] #obsah jednoho ciselniku vlozen do listu jeden_ciselnik
  NK1[[cis]]<-jeden_ciselnik #vlozeni listu jeden_ciselnik do listu listuu - NK1
}#for cis NK1
names(NK1)<-unique(NKA$kod_ciselniku) #pojmenovani listuu v listu listuu podle kod_ciselniku
#ok

#vypis zacatku jednoho ciselniku pomoci jeho kodu nebo poradi v listech
#head(NK1[["S1_NK12"]]) 
#head(NK1[[10]])











#=============================================================================================================

# struktura 2 :  kody K k jedne otazce
# --> List listu, v kazdem listu je seznam otazek a kodu dotazniku vytazenych z kodu K.
#       Struktura v listech: "kod_otazky | kod_dotazniku". 
#       Kod_ciselniku je take nazvem listu.

# iniciace vysledne struktury -- NK2
NK2<-assign("list", NULL, envir = .GlobalEnv) #prazdny list nazacatku

# priprava skal pro moznosti
skala_cisla<-seq(1,49)
skala_rim<-tolower(as.roman(skala_cisla))
skala_pis<-letters[1:26]
prvky_skal<-c(skala_cisla, skala_rim, skala_pis)
typy_skal<-c(rep("cisla",49), rep("rim", 49), rep("pis", 26))
punct_pattern<- "#|%|=|~|@|#|_|:|<|>|;|'|&"   # dalsi nefunguji ("~"|"!"|"@"|"#"|"$"|"%"|"^"|"&"|"*"|"("|")"|"{"|"}"|"_"|"+"|":"|"<"|">"|"?"|"."|";"|"'"|"["|"]"|"=") obzvlast pat_zle<-"*, $, ?, ^, +."

#predpriprava dat sloupce kodu_K
NKB$kody_K<-gsub("[[:space:]]", " ", NKB$kody_K)              #nahrazeni vsech typu mezer jednotnou mezerou 1./3 krok
NKB$kody_K<-gsub("\xc2\xa0", " ", NKB$kody_K)                 #nahrazeni vsech typu mezer jednotnou mezerou 2.3 krok
NKB$kody_K<-gsub(" ", " ", NKB$kody_K)                        #nahrazeni vsech typu mezer jednotnou mezerou 3/3 krok
NKB$kody_K<-gsub("_", " ", NKB$kody_K)                        #nahrazeni podtrzitka jednotnou mezerou
NKB$kody_K<-gsub("v", " v ", NKB$kody_K)                      #vlozeni mezer kolem vsech "v"
NKB$kody_K<-gsub("v i", "vi", NKB$kody_K)                     #odebrani mezery u "v" v rim. cisle
NKB$kody_K<-gsub("x v", "xv", NKB$kody_K) 
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
pra_ret<-"Prazdny retezec"
punct<-"punct #%=~@#_:<>;'&"
bez_v<-"chybi delitko otazka-dotaznik"
male_pis<-"male_pismeno_na_zac_otazky"
velke_V<-"velke V na >2. miste otazky"



for (cis in 1:nrow(NKB)) { #prace po radcich K koduu
  ciselnik<-as.character(NKB$kod_ciselniku[cis])
  kody_K<-as.vector(NKB$kody_K[cis]) #jeden radek vsech kodu K
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # kontrola chyb v kody_K, pokud maji evidentne nekompatibilni tvar
  
 #chyby v kody_K, kdy se dale kody_K ani nezkousi analyzovat
  if (is.na(kody_K)) {  #prazdny retezec --> chyba
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
            
  }else if (!grepl("v", kody_K)) { #kody_K neobsahuje zadne "v" --> chyba
    #zapise cely chybny radek z NKB do tabulky chyb
    NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba=bez_v))
    #vypise chybu do listu cis a skace na dalsi kolo forcyklu
    list_K<-as.data.frame(cbind(bez_v, "chyba", "chyba")) 
    
    
    
  }else{ #kody_K obsahuje asi aspon jeden validni retezec --> ma smysl analyzovat kody_K
    
  # -----------------------------------------------------------------------------------------------
    # docasny pomocny usek, pak to prestehuju TODO
    
             
    #priklad kody_K: 
    # "E 3 v E 5, E 4 v S 5, F 4 v F 5, F 4 v T 5,O 4 v PN 18/2, I 4 v T 2"
    # kody_K<-"A 12bi-v v E 5, A 11i +A 33b1-5 v S 5, A 34b 1-5 v F 5, A 14a1-5 v T 5" 
    # kody_K<-"F 1c1 v E 2 , J 1 c1 v S 2,H 1c v F2, I 1c v T2, O 1c v PN 18/2, J 1iii v PN 6/2 "
    
    
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
        
        if (nchar(gsub(" ","", useky_mezi_carkami[us]))>=5 && grepl(" v ", useky_mezi_carkami[us])) { # usek ma min delku && obsahuje " v "
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
            pahyl_otazek<-gsub(" ","", paste(pahyl_otazek, useky_mezi_carkami[us],", ", sep=""))
            #tady by asi mela byt kontrola jestli uz je pahyl otazek v dostatecnem formatu a pripadne ho vlozit do useky_predb_ok
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
          useky_predb_ok<-c(useky_predb_ok, "chyba nevim kde")
        }
        useky_ok<- useky_predb_ok #pokud je to ok
      }#for u

          

        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        K_po_retezcich<-useky_ok    ###8.6.2015 || unlist(strsplit(kody_K, ", ")) #rozdeleni K kodu do retezcu po dotaznicich podle ", "
        
        otazky<-dotazniky<-otazkyDB<-c() #vysledne otazky jednoho ciselniku a 1 kodu_K, vkladaji se do 1 listu s nazvem ciselniku
        for (dot in 1: length(K_po_retezcich) ) { #prace po retezcich z kodu K
          #krajeni retezce kodu K na OTAZKA - DOTAZNIK 
          ret<-gsub(" ", "", unlist(strsplit(K_po_retezcich[dot], " @ "))) #rozdeleni K kodu na otazku a dotaznik podle " v " @@@
          OTAZKA<-ret[1]
          DOTAZNIK<-ret[2]
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # OTAZKA
          
          #cast retezce "OTAZKA":
          if (!grepl("-",OTAZKA) && !grepl(",",OTAZKA) ) { #pokud v otazce nejsou zadne pomlcky ani carky, a tedy vice moznosti, je pripravena pro dalsi zpracovani.
            #v poradku, pokracujem
            OTAZKA<- OTAZKA #tohle tu je navic, ale nechci nechat prazdny if, protoze to pak hazi chybu
          } else if (is.na(OTAZKA)) { #prazdny retezec
            OTAZKA<-"chybejici_kod_otazky"  
            
          } else if (grepl(",",OTAZKA)) { #zmnozena otazka na 1 dotaznik / pokud jsou v otazce carky, otazka se rozdeli do vice podotazek
            podot<-unlist(strsplit(OTAZKA, ",")) 
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
            }
            OTAZKA<-podot
            
          } else if (any(grepl("-",OTAZKA))) { #v otazce je pomlcka. otazka muze obsahovat jeden i vice prvku vektoru 
              #cvicna OTAZKA: OTAZKA<-c("S1a3-5" ,  " S2f" ,    " D2e5s"  , "E1di-iii", " A1dg" ,   " A1dh",    " S5d7ii",  " S5d7iii")
              podot<-OTAZKA
              for (j in 1 : length(which (grepl("-",OTAZKA))) ) {
                ot_pomlcka_ind = rev(which(grepl("-",OTAZKA))) #vektor poradi otazek s pomlckou ve vektoru OTAZKA
                zackon<-unlist(strsplit(OTAZKA[ot_pomlcka_ind[j]], "-"))
                posledni_znaky_ot<-c(str_sub(zackon[1], -1,-1), str_sub(zackon[1], -2,-1), str_sub(zackon[1], -3,-1))
                pocet_zn_odriznuti<-length(which (match (posledni_znaky_ot, prvky_skal) >0))
                
                zaklad_ot<-str_sub(zackon[1], 1, -(pocet_zn_odriznuti+1)) 
                prvni_moznost<-str_sub(zackon[1], -1, -(pocet_zn_odriznuti)) 
                posl_moznost<-zackon[2] 
                
                #odchyceni spatneho deleni skaly kvuli chybe v priprave K_kodu
                if (!any(posl_moznost==prvky_skal)) {
                  otazky_moznosti<- "chyba_v_priprave_K_kodu"
                } else { #kdyz je posl_moznost validni, pokracuju dal 
                    #urceni skaly moznosti -- vybrana skala ulozena v promenne "skala"
                    typy_zackon<-typy_skal[c(which(prvni_moznost==prvky_skal), which(posl_moznost==prvky_skal))]
                    if (any(typy_zackon=="cisla")) {
                      skala<-skala_cisla
                    }else if ((length(typy_zackon)<=3) &&  length(which(typy_zackon=="pis"))==2 ) {
                      skala<-skala_pis
                    }else {
                      skala<-skala_rim
                    }
                    #vytvoreni moznosti ktere se budou pripojovat k zakladu otazky
                    moznosti<-skala[which(prvni_moznost==skala) : which(posl_moznost==skala)]
                    otazky_moznosti<-paste(zaklad_ot, moznosti, sep="")
                }
                #vlozeni namnozenych otazek do podot, jez zastupuje ve for cyklu puvodni retezec OTAZKA
                puv_podot_zac<-podot[-c(ot_pomlcka_ind[j] : length(podot))]
                puv_podot_kon<-podot[-c(0 : ot_pomlcka_ind[j])]
                podot<-c(puv_podot_zac, otazky_moznosti, puv_podot_kon)
              } #for j /namnozeni jedne pomlcky v kazdem cyklu
              OTAZKA<-podot
      
          } else {
            OTAZKA<-paste("neznama_chyba_otazka: ", OTAZKA, sep="")
          }
          
          #promenna OTAZKA je pripravena k parovani s dotaznikem. prozatim vse ok. 3.6.2015
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # DOTAZNIK
          
          #cast retezce DOTAZNIK:
          if (!grepl("[[:punct:]]", gsub("/","",DOTAZNIK)) ) { #pokud v otazce nejsou zadne znaky !"#$%&'()*+,-.:;<=>?@[]^_`{|}~., a tedy nic sloziteho ale ocekavam ze jde pouze jediny dotaznik, ten je pripraven pro dalsi zpracovani. NOsetreno i pro dotaznik s lomitkem.
              #v poradku, pokracujem
              DOTAZNIK<-DOTAZNIK  #je to tu navic, ale nechtela jsem nechat telo cyklu prazdne.
          } else if (is.na(DOTAZNIK)) { #chyba: prazdny retezec
              DOTAZNIK<-"chybejici_kod_dotaznikuNA"
          } else if (grepl("-,",DOTAZNIK))  { #chyba: v dotazniku je pomlcka
              DOTAZNIK<-"chyba_kodovani_dotazniku-"
          } else if (nchar(DOTAZNIK)<2)  { #chyba: kod dotazniky nema alespon dva znaky
            DOTAZNIK<-"chyba_kodovani_dotazniku"
            
          } else if (grepl(",",DOTAZNIK)) { #v retezci dotazniku je carka -- vice dotazniku patri k otazce
              DOTAZNIK<-unlist(strsplit(DOTAZNIK, ",")) 
          } else {
              DOTAZNIK<-paste("neznama_chyba_dotaznik: ", DOTAZNIK, sep="")
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
        } else if (any(grepl("V", str_sub(gsub(" ","",otazky), 2)))) { #odchyceni otazek s velkym V na jinem ne prvnim miste a proto spatne delenych
          #zapise cely chybny radek z NKB do tabulky chyb
          NK_cis_chyba_kody_K<-rbind(NK_cis_chyba_kody_K , cbind(NKB[cis,], chyba=velke_V))
          #vypise chybu do listu cis a skace na dalsi kolo forcyklu
          list_K<-as.data.frame(cbind(velke_V, "chyba", "chyba"))
        } else{
          
          
        otazkyDB<-paste(dotazniky, otazky,  sep="_")
        list_K<-as.data.frame(cbind(otazky, dotazniky, otazkyDB))  
        
        }
  } #else - kontrola celkovych chyb v kody_K

  NK2[[cis]]<-list_K
}#for cis NK2
names(NK2)<-NKB$kod_ciselniku
#ok




# struktura 3 : kod_ciselniku | popis otazky
NK3 <- NKB[,1:2]
#ok

#----------------------------------------------------------


