# SKRIPT PRO sjednoceni dat pro vyhledavani v kodovnikach
# v0.0: 26.8.2015
# @Tereza Kalabova

# vyprazdneni promennych z Rka
rm(list=ls())

# knihovny ---------------------
library(stringi)
library(stringr)
library(grid) #Hmisc
library(lattice) #Hmisc
library(survival) #Hmisc
library(Formula) #Hmisc
library(ggplot2) #Hmisc
library(Hmisc) #capitalize
library(haven) #nacteni db ze sav ()

# -----------------------------------------------------------------------------------------------
# nacteni dat ===================================================================================
# -----------------------------------------------------------------------------------------------

#nacteni dat sjednoceni ciselniku
###load("/home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/03_output_data/RData_output/NK1234.RData")


#nacteni dat Elspac databaze
#load("/home/kalabava/sya/60_Elspac/05_skript_R_MKN_kody/Elspac_23_03_2015.Rdata") #toto je stara databaze
Elspac_data <- read_sav("/home/kalabava/sya/60_Elspac/02_data/ELSPAC_v2.0.sav") #haven balik
  #vytvoreni noveho souboru dat Elspac s upravenymi velikostmi pisma v nazvech: prvni velke pismeno, zbytek maly.
  Elspac_dataCL<-Elspac_data
  names(Elspac_dataCL)<-capitalize(tolower(names(Elspac_dataCL)))
  #ok Elspac_dataCL

#nacteni souboru s opravami podle piskvorek
opravy_pisk <- read.csv("/home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/05_kontrola_sjednoceni_dat/otazky_mimo_db29_08_2015_sjednocovani_vse.csv", header=TRUE, sep=";", quote = "\"", dec= ".") 
  #tab unikatnich oprav kodu K
  tab_kody_K<-opravy_pisk[,c("kod..ciselniku", "Oprava.kody.k.29.8.2015")]
  tab_kody_K<-tab_kody_K[-c(which(tab_kody_K[,2]=="")),]
  #tab novych nazvu ciselniku
  tab_nove_nazvy_cis<-opravy_pisk[,c("kod..ciselniku", "novy.nazev.ciselniku", "nazev_ciselniku" )]
  tab_nove_nazvy_cis<-tab_nove_nazvy_cis[-c(which(tab_nove_nazvy_cis[,2]=="")),]
  #tab neshodujicich se otazekdb
  tab_neshod<-opravy_pisk[,c("kod..ciselniku", "neshodujici_se_kody", "otazkyDB")]
  tab_neshod<-tab_neshod[-c(which(tab_neshod[,2]=="")),]
  vect_neshod<-as.character(unique(tab_neshod[,3])) #tyto kody nemaji shodu ani nic shode podobneho - vyzaduji zvlastni zachazeni
  #ok tab_kody_K, tab_neshod, tab_nove_nazvy_cis



#nacteni externich ciselniku jako napr MKN
  #...   #zatim je nemam. (9.9.2015)



#uprava souboru NKB - nahrazeni opravenych kodu K a nazvu ciselniku podle souboru tab_kody_K a tab_nove_nazvy_cis
NKBarch<-NKB
#oprava kodu_K
NKB3pom<-as.character(NKB[,3])
for (i in 1:nrow(tab_kody_K)) {
  NKB3pom[which(NKB[,1]==as.vector(tab_kody_K[i,1]))] <- as.character(tab_kody_K[i,2])
}
NKB_opr<-cbind(as.vector(NKB[,c(1,2)]), NKB3pom)
#ok opravene kody_K

#oprava nazvu ciselniku
NKB2pom<-as.character(NKB_opr[,2])
for (i in 1:nrow(tab_nove_nazvy_cis)) {
  NKB2pom[which(NKB[,1]==as.vector(tab_nove_nazvy_cis[i,1]))] <- as.character(tab_nove_nazvy_cis[i,3])
}
NKB_opr<-cbind(as.vector(NKB_opr[,1]), NKB2pom, as.vector(NKB_opr[,3]))

#finalni vraceni nazvy sloupcu
colnames(NKB_opr) <- c("kod_ciselniku", "nazev_ciselniku", "kody_K")
NKB<-as.data.frame(NKB_opr)
#ok NKB

rm(NKB2pom, NKB3pom)





# -----------------------------------------------------------------------------------------------
# vypocty =======================================================================================
# -----------------------------------------------------------------------------------------------

setwd("/home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/03_output_data/04_kontrola_sjednoceni_dat/")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.cast - kontrola vytvorenych kodu otazek, zdali existuji v databazi (v Rkovem souboru)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

NK2tab[,7]<-capitalize(tolower(NK2tab[,3]))
NK2tab[,8]<-rep(0, nrow(NK2tab))
NK2tab[,9]<-rep(0, nrow(NK2tab))
NK2tab[,10]<-rep(0, nrow(NK2tab))
#TODO 28.8.2015 - vlozit do 11.sloupce nazvy ze 7. sloupce s odebranymi podtrzitky pres while!!
NK2tab[,11]<-NK2tab[,7]
while (any(!(sub("_","",NK2tab[,11])==NK2tab[,11]))) { #muj prvni fungujici while, jupi.
  NK2tab[,11]<-sub("_","",NK2tab[,11])
}
NK2tab[,12]<-rep(0, nrow(NK2tab))
NK2tab[,13]<-seq(1, nrow(NK2tab))  
    
  
  
  
  
  

names(NK2tab)<-c(names(NK2tab)[1:6], "Otazky_cis", "Otazky_elspdata_znacka", "Orig_nazev_v_DB", "Alt_orig_nazev_duplic", "Otazkycis", "puv_soubor", "poradi")

head(NK2tab)

otazky_elspdata<-capitalize(tolower(names(Elspac_23_03_2015)))
head(otazky_elspdata)

otazkyelspdata<-otazky_elspdata
while (any(!(sub("_","",otazkyelspdata)==otazkyelspdata))) { #muj prvni fungujici while, jupi.
  otazkyelspdata<-sub("_","",otazkyelspdata)
}

#kontrola duplicit v elspdata nazvech
library(sets)
#puvodni nazvy promennych: names(Elspac_23_03_2015)
length(names(Elspac_23_03_2015))==length(unique(names(Elspac_23_03_2015))) #ok, 52317
#capitalize tolower Elsp nazvy: otazky_elspdata
length(otazky_elspdata)==length(unique(otazky_elspdata)) #ok, 52317
#capitalize tolower Elsp nazvy bez podrtzitek: otazkyelspdata
length(otazkyelspdata)==length(unique(otazkyelspdata)) #rozdil:52295 >>22*2 otazek se prekryva po odebrani pomlcek. jejich seznam je ulozen v promenne shoda_nazvu_podtrzitko
shoda_nazvu_podrtzitko<-otazkyelspdata[which(duplicated(otazkyelspdata))] #mnozina potencialne spatne pojmenovanych otazek kvuli duplicite nazvu kodu otazky po odstraneni podtrzitek

otazky_podtrzitko_dup_44<-is.element(c(1), c(3,2,1))






#kontrola shod otazek z ciselnikovych dat NK2tab a z dat databaze Elspac_23_03_2015
otazky_mimo_db<-c()
otazky_s_duplicitou_bez_podtrzitek<-c()
for (i in 1:nrow(NK2tab)) {
  #NK2tab[i,12]<-unlist(strsplit(as.character(NK2tab[i,4]), split='_', fixed=TRUE))[1]
  NK2tab[i,12]<-str_sub(gsub("/","",as.character(NK2tab[i,4])), 1,5)
  if (is.element(NK2tab[i,7], otazky_elspdata)) {
    NK2tab[i,8]<-1 #shoda nazvu s podtrzitkem capitalize tolower
    NK2tab[i,9]<-names(Elspac_23_03_2015)[which(NK2tab[i,7]==otazky_elspdata)] 
  } else if (is.element(sub("_","",NK2tab[i,7]), otazkyelspdata)) {
    NK2tab[i,8]<-2 #shoda nazvu bez podtrzitek capitalize tolower
    if (length(names(Elspac_23_03_2015)[which(NK2tab[i,11]==otazkyelspdata)])==1) {
    NK2tab[i,9]<-names(Elspac_23_03_2015)[which(NK2tab[i,11]==otazkyelspdata)] 
    } else if (is.element(NK2tab[i,11], shoda_nazvu_podrtzitko)) { #kontrola jestli se jedna o potencialne spatne pojmenovanou otazky kvuli duplicite po odebrani podtrzitek
      NK2tab[i,8]<-22 #shoda nazvu s podtrzitkem capitalize tolower
      NK2tab[i,9:10]<-names(Elspac_23_03_2015)[which(NK2tab[i,11]==otazkyelspdata)]
      otazky_s_duplicitou_bez_podtrzitek<-rbind(otazky_s_duplicitou_bez_podtrzitek, NK2tab[i,])
    }
  }else{
    if (is.element(NK2tab[i,2],c("H","N2"))) { # chybejici dotazniky N2 a H
      NK2tab[i,8]<-23 #dotaznik chybejici v databazi
    } else if (is.element(NK2tab[i,2],c("chyba"))) { #ve zkoumane otazce je chyba zaznamenana uz pri sjednocovani ciselniku
      NK2tab[i,8]<-24 #dotaznik je "chyba"
    }
    otazky_mimo_db<-rbind(otazky_mimo_db, NK2tab[i,])
  }
}
NK2tab[23865:nrow(NK2tab),12]<-"H_NK"
#otazky_mimo_db[6115:nrow(otazky_mimo_db),12]<-"H_NK"

write.table(otazky_mimo_db, file = "/home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/03_output_data/04_kontrola_sjednoceni_dat/otazky_mimo_db.txt", sep=";", col.names = T, row.names=F, qmethod = "double") 
#sort(table(otazky_mimo_db[,2]))

#vypis frekvenci chybnych rozlozenych otazek v puvodnich souborech
frekvence_chyb_v_puv_souborech<-sort(table(otazky_mimo_db[,12]))
pocet_puv_souboru_s_chybou<-length(sort(table(otazky_mimo_db[,12])))
pocet_chybnych_rozlozenych_otazek<-nrow(otazky_mimo_db)
procento_chybnych_rozlozenych_otazek<-pocet_chybnych_rozlozenych_otazek*100/nrow(NK2tab)
#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# konec skriptu. ================================================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~