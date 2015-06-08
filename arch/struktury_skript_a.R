# SKRIPT PRO sjednoceni dat pro vyhledavani v kodovnikach

# vyprazdneni promennych z Rka
rm(list=ls())

## change the working directory
getwd()
setwd("/home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/awk")

#knihovny ---------------------------------------------------------------------------------------

library(stringi)



# nacteni dat z AWK =============================================================================

# struktura A: kod v ciselniku | popis odpovedi |  kod otazky ||
NKA <- read.csv('NKA.csv', header=FALSE, sep=";")
names(NKA) <- c("id_odpovedi_neuniq", "popis_odpovedi", "kod_ciselniku") 

# struktura B: kod otazky | popis otazky | kody K
NKB <- read.table("NKB3.txt", header=FALSE, sep=";")
names(NKB) <- c("kod_ciselniku", "popis_otazky", "kody_K")


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

NK2<-assign("list", NULL, envir = .GlobalEnv) #prazdny list nazacatku
skala_cisla<-seq(1,49)
skala_rim<-tolower(as.roman(skala_cisla))
skala_pis<-letters[1:26]
prvky_skal<-c(skala_cisla, skala_rim, skala_pis)
typy_skal<-c(rep("cisla",49), rep("rim", 49), rep("pis", 26))

#sjednoceni mezer:
NKB$kody_K<-gsub("[[:space:]]", " ", NKB$kody_K)
NKB$kody_K<-gsub("\xc2\xa0", " ", NKB$kody_K)
NKB$kody_K<-gsub("–", "-", NKB$kody_K) #nahrazeni spojovniku pomlckou
NKB$kody_K<-gsub("- v v ", "-v v ", NKB$kody_K) #odebrani mezery v retezci "- v v "



for (cis in 1:nrow(NKB)) { #prace po radcich K koduu
  ciselnik<-as.character(NKB$kod_ciselniku[cis])
  kody_K<-as.vector(NKB$kody_K[cis]) #jeden radek vsech kodu K
  K_po_dotaznicich<-unlist(strsplit(kody_K, ", ")) #rozdeleni K kodu do retezcu po dotaznicich podle ", "
  
  otazky<-dotazniky<-otazkyDB<-c() #vysledne otazky jednoho ciselniku a 1 kodu_K, vkladaji se do 1 listu s nazvem ciselniku
  for (dot in 1: length(K_po_dotaznicich) ) { #prace po retezcich z kodu K
    ret<-gsub(" ", "", unlist(strsplit(K_po_dotaznicich[dot], " v "))) #rozdeleni K kodu na otazku a dotaznik podle " v "
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
  }#for dot K_po_dotaznicich (retezcich)
  
  otazkyDB<-paste(dotazniky, otazky,  sep="_")
  
  list_K<-as.data.frame(cbind(otazky, dotazniky, otazkyDB))
  NK2[[cis]]<-list_K
}#for cis NK2
names(NK2)<-NKB$kod_ciselniku
#ok

#je to fakt moc pekny /////////////////////////////////////////////////////////////////


# struktura 3 : kod_ciselniku | popis otazky
NK3 <- NKB[,1:2]
#ok

#----------------------------------------------------------


