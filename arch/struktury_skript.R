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



# struktura 2 :  kody K k jedne otazce
# --> List listu, v kazdem listu je seznam otazek a kodu dotazniku vytazenych z kodu K.
#       Struktura v listech: "kod_otazky | kod_dotazniku". 
#       Kod_ciselniku je take nazvem listu.

NK2<-assign("list", NULL, envir = .GlobalEnv) #prazdny list nazacatku
skala_cisla<-seq(1,49)
skala_rim<-tolower(as.roman(skala_cisla))
skala_pis<-letters[1:26]
prvky_skal<-c(skala_cisla, skala_rim, skala_pis)

#sjednoceni mezer:
NKB$kody_K<-gsub("[[:space:]]", " ", NKB$kody_K)
NKB$kody_K<-gsub("\xc2\xa0", " ", NKB$kody_K)
NKB$kody_K<-gsub("- v v ", "-v v ", NKB$kody_K) #odebrani mezery v retezci "- v v "


for (cis in 1:nrow(NKB)) { #prace po radcich K koduu
  ciselnik<-as.character(NKB$kod_ciselniku[cis])
  kody_K<-as.vector(NKB$kody_K[cis]) #jeden radek vsech kodu K
  K_po_dotaznicich<-unlist(strsplit(kody_K, ", ")) #rozdeleni K kodu do retezcu po dotaznicich podle ", "
  
  otazky<-dotazniky<-otazkyDB<-c()
  for (dot in 1: length(K_po_dotaznicich) ) { #prace po retezcich z kodu K
    ret<-gsub(" ", "", unlist(strsplit(K_po_dotaznicich[dot], " v "))) #rozdeleni K kodu na otazku a dotaznik podle " v "
    OTAZKA<-ret[1]
    DOTAZNIK<-ret[2]
    
    #cast retezce "OTAZKA":
    if (!grepl("-",OTAZKA) && !grepl(",",OTAZKA) ) { #pokud v otazce nejsou zadne pomlcky ani carky, a tedy vice moznosti, je pripravena pro dalsi zpracovani.
      #v poradku, pokracujem
      ot_ret<- OTAZKA #prac.retezec pro vlozeni do fin
      
    } else if (grepl(",",OTAZKA) { #zmnozena otazka na dotaznik / pokud jsou v otazce carky, otazka se rozdeli do vice podotazek
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
      
    } else if (grepl("-",OTAZKA)) { #v otazce je pomlcka. otazka muze obsahovat jeden i vice prvku vektoru
        for (j in 1 : length(which (grepl("-",OTAZKA)) )) {
          #ot_pomlcka_ind=which(grepl("-",OTAZKA)) #vektor poradi otazek s pomlckou ve vektoru OTAZKA
          zackon<-unlist(strsplit(OTAZKA[ot_pomlcka_ind[j]], "-"))
          posledni_znaky_ot<-c(str_sub(zackon[1], -1,-1), str_sub(zackon[1], -2,-1), str_sub(zackon[1], -3,-1))
          pocet_zn_odriznuti<-length(which (match (posledni_znaky_ot, prvky_skal) >0))
          zaklad_ot<-str_sub(zackon[1], 1, -(pocet_zn_odriznuti+1)) 
          prvni_moznost<-str_sub(zackon[1], -1, -(pocet_zn_odriznuti)) 
          posl_moznost<-zackon[2] 
          #urceni skaly moznosti
          typ_zac<-which(prvni_moznost==prvky_skal) #1-49 cis, 50-98 rim, 99-124 pis
          typ_kon<-which(posl_moznost==prvky_skal)
          if (length(typ_zac)==1) {
            skala<-skala_cisla
          } else if 
           
           ## TODO: vyresit jak nastavit skalu pro moznosti 
        }
                                  
                     
                     
    }
    
    
    #S1a, S2f, D2e5s,E1di-iii, A1dg,h

    
    
    
    if (!grepl("-",ret[1]) && !is.na(ret[2]) && !grepl(",",ret[1]) && !grepl(",",ret[2]) ) { #pokud v otazce nejsou zadne pomlcky ani carky} ani v dotazniku, a tedy vice moznosti, vlozi se radek do vektoru pro pridani do list_K
      #v poradku, pokracujem
      otazka<- ret[1]
      dotaznik<- ret[2]
      otazkaDB<- paste(ret[2],"_",ret[1], sep="")
      
    } else if (is.na(ret[2])){ #samotny kod dotazniku - doplnit otazku z predchozi otazky
      #otazka zustava z minuleho behu cyklu
      dotaznik<-rep(ret[1], length(otazka))
      otazkaDB<-paste(dotaznik,"_",otazka, sep="")
    } else if ( (grepl("1-",ret[1]) || grepl("i-", ret[1]) || grepl("a-", ret[1]) ) && grep("-",ret[1])==1) { #pokud otazka pobsahuje prave jednu pomlcku, a tedy jedno cleneni na vice moznosti, pak se moznosti rozdeli do vice otazek a vlozi do vektoru
        moznosti<-unlist(strsplit(ret[1], "-"))
            zaklad_otazky=substr(moznosti[1], start=1, stop=nchar(moznosti[1])-1)
            prvni_moznost=stri_sub(moznosti[1], -1, length=1)
            posledni_moznost=moznosti[2]
        
            if (prvni_moznost == 1) { skala<-skala_cisla #deleni podle typu skaly moznosti
            } else if (prvni_moznost == "i") { skala<-skala_rim
            } else if (prvni_moznost == "a") { skala<-skala_pis
            }
            otazka<-dotaznik<-otazkaDB<-c()
            for (moz in 1 : which(skala == posledni_moznost)) { #rozdelovani otazky s vice pozmosti do vice samostatnych otazek vc ukladani do vektoru
              otazka<- c(otazka,paste(zaklad_otazky,skala[moz], sep=""))
              dotaznik<- c(dotaznik, ret[2])
              otazkaDB<- c(otazkaDB, paste(ret[2],"_",paste(zaklad_otazky,skala[moz], sep=""), sep=""))
            } #for moz
    } else{
      otazka<- ret[1]
      dotaznik<- ret[2]
      otazkaDB<- "chyba_v_kodovani_K_kodu"
    }

    otazky<-c(otazky,otazka)
    dotazniky<-c(dotazniky, dotaznik)
    otazkyDB<-c(otazkyDB,otazkaDB)
  }#for dot K_po_dotaznicich
 
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


