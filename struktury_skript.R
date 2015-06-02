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
NKB <- read.table("NKB.txt", header=FALSE, sep=";")
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
skala_cisla<-seq(1,100)
skala_rim<-tolower(as.roman(skala_cisla))
skala_pis<-letters[1:26]
for (cis in 1:nrow(NKB)) {
  ciselnik<-as.character(NKB$kod_ciselniku[cis])
  kody_K<-as.vector(NKB$kody_K[cis]) #jeden radek vsech kodu K
  K_po_dotaznicich<-unlist(strsplit(kody_K, ", ")) #rozdeleni K kodu do retezcu po dotaznicich podle ", "
  
  otazky<-dotazniky<-otazkyDB<-c()
  for (dot in 1: length(K_po_dotaznicich) ) { #prace po retezcich z kodu K
    ret<-gsub(" ", "", unlist(strsplit(K_po_dotaznicich[dot], " v "))) #rozdeleni K kodu na otazku a dotaznik podle " v "

    if (!grepl("-",ret[1]) && !is.na(ret[2])) { #pokud v otazce nejsou zadne pomlcky, a tedy vice moznosti, vlozi se radek do vektoru pro pridani do list_K
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

#je to fakt moc pekny


# struktura 3 : kod_ciselniku | popis otazky
NK3 <- NKB[,1:2]
#ok

#----------------------------------------------------------


