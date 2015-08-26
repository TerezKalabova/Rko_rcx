#!/bin/bash

# skript pripravuje soubor NKall.txt a NKB.txt

# NKall: 
# V1: poradi v souborech (cislo)
# V2: znacka (NK/K/Z/ZK/ZZK/prazdne)
# V3: kod ciselniku/kody_K/cislo polozky v ciselniku (text/cislo)
# V4: odpoved v ciselniku (text)

# NKB:
# V1: kod ciselniku
# V2: nazev ciselniku
# V3: kody_K
# =================================================


#--------------------------------------------------
# NKall
#--------------------------------------------------

#nastaveni cesty
cd /home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/02_input_data/03_csv_ciselniky_upravene


#real NKall prikaz - serazene dotazniky
awk -F ";" '{print $1 ";" $2 ";" $3 ";" $4}' P1_LNK.csv  P1_NK.csv	P2_LNK.csv	P2_NK.csv	P3_LNK.csv	P3_NK.csv	N1_LNK.csv	N2_LNK.csv	N3_LNK.csv	N3_NK.csv	N4_LNK.csv	N4_NK.csv	N5_LNK.csv	N5_NK.csv	PN61_LNK.csv	PN61_NK.csv	PN62_LNK.csv	PN62_NK.csv	PN63_LNK.csv	PN63_NK.csv	PN8_LNK.csv	PN181_LNK.csv	PN181_NK.csv	PN182_LNK.csv	PN182_NK.csv	PN183_LNK.csv	PN183_NK.csv	PN184_LNK.csv	T1_LNK.csv	T1_NK.csv	T2_LNK.csv	T2_NK.csv	T3_LNK.csv	T3_NK.csv	T4_LNK.csv	T5_LNK.csv	T5_NK.csv	F1_LNK.csv	F1_NK.csv	F2_LNK.csv	F2_NK.csv	F3_LNK.csv	F3_NK.csv	F4_LNK.csv	F5_LNK.csv	F5_NK.csv	S1_LNK.csv	S1_NK.csv	S2_LNK.csv	S2_NK.csv	S3_LNK.csv	S3_NK.csv	S4_LNK.csv	S5_LNK.csv	S5_NK.csv	E1_LNK.csv	E1_NK.csv	E2_NK.csv	E3_LNK.csv	E3_NK.csv	E4_LNK.csv	E5_LNK.csv	E5_NK.csv	E6_NK.csv	E7_NK_opr.csv	TH4_LNK.csv	FT1_LNK.csv	FT1_NK.csv	FT2_NK.csv	FT3_LNK.csv	FT3_NK.csv	FT4_LNK.csv	FT5_LNK.csv	FT5_NK.csv	FT6_NK.csv	FT7_NK.csv	FT8_NK.csv	ET2NK.csv	ET4_LNK.csv	ET5_NK.csv	ET5LNK.csv	ET6_NK.csv	ET7_NK.csv	ET8_NK.csv	NT1_LNK.csv	NT1_NK.csv	NT2_NK.csv	NT3_LNK.csv	NT3_NK.csv	NT4_LNK-dop_prejmNK-LNK.csv	NT5_LNK.csv	NT5_NK.csv	NT6_NK.csv	NT8_NK.csv	NT9_LNK.csv	NT9_NK.csv	U11_NK.csv	U13_NK.csv	U15_NK.csv	U18_NK.csv	U8_NK.csv	V11_NK.csv	V13_NK.csv	V15_NK.csv	V18_NK.csv	V19_NK-po-kodovani.csv	V8_NK.csv	H_NK.csv > NKall.txt 

mv NKall.txt /home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/02_input_data/04_awk_data/NKall.txt

cd /home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/02_input_data/04_awk_data


#--------------------------------------------------
#kontrola NKall.txt v RKu
#--------------------------------------------------
### setwd("/home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/02_input_data/04_awk_data")
### NKall <- read.table("NKall.txt", header=FALSE, sep=";", quote = "\"", dec= ".")         #, na.strings = "NA")

#vypis radku kde je v prvnim sloupci hodnota 500. (prazdna bunka se znaci pomoci "")
### NKall[which(NKall[,1]==""),]
#pocet radku NKall je 293529. ok.


#--------------------------------------------------
#uprava textu uvnitr souboru NKall
#--------------------------------------------------
cp NKall.txt arch.NKall.txt                     #zkopirovani zalohy souboru NKall

awk -v OFS=" " '$1=$1' NKall.txt > NKall2.txt   #nahradi nasobne mezery jednou mezerou, http://stackoverflow.com/questions/16903005/awk-syntax-what-is-ofs-and-why-the-1-at-the-end
sed 's/; /;/' NKall2.txt > NKall3.txt           #odstraneni mezery na zacatku retezce
sed 's/; /;/' NKall3.txt > NKall4.txt           #odstraneni jedne zbyvajici mezery na zacatku retezce v prvnim radku
sed 's/ ;/;/' NKall4.txt > NKall5.txt           #odstraneni mezery nakonci retezce
sed s'/:$//' NKall5.txt > NKall6.txt            #odstraneni dvojtecky pokud je nakonci, http://www.gnu.org/software/sed/manual/html_node/The-_0022s_0022-Command.html

mv NKall6.txt NKall.txt                         #zkopirovani schvaleneho souboru NKall

#NKall ok

#smazani pracovnich souboru NKall
rm NKall2.txt NKall3.txt NKall4.txt NKall5.txt


#--------------------------------------------------
# NKB:
#--------------------------------------------------

cd /home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/02_input_data/04_awk_data

#B1 struktura:
#vypise jen radky s NK a K, jen 2. a 3. sloupec
#ps: jsou tam i retezce z textu obsahujicich "NK"
awk -F ";" 'c&&!--c; /NK/  {c=1; print $2 ";" $3 ";" $4}' NKall.txt > NKB1.txt

#B2 struktura:
#spojeni dvou radku NK a K za sebe
#ps: jsou tam i retezce z textu obsahujicich "NK", je jich tam 24, jinak je to zarovnane v poradku
awk '{ ORS = (NR%2 ? ";" : RS) } 1'  NKB1.txt > NKB2.txt

#odstraneni radku kde se prvni sloupec nerovna "NK"
awk -F";" '$1 ~ /NK/' NKB2.txt > NKB2b2.txt

#B3 struktura:
#vymazani sloupce s “K”:
awk -F ";" '{print $2 ";" $3 ";" $6}' NKB2b2.txt > NKB3.txt  

#prejmenovani na vysledne NKB do noveho souboru
mv NKB3.txt NKB.txt

#NKB ok.


#smazani pracovnich souboru NKB
rm NKB1.txt NKB2.txt NKB2b2.txt

#--------------------------------------------------
#smazani pracovnich souboru
#--------------------------------------------------






#--------------------------------------------------
#nacteni NKB.txt do Rka
#--------------------------------------------------
### NKB <- read.table("NKB.txt", header=FALSE, sep=";", quote = "\"", dec= ".") 


