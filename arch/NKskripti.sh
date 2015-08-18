#!/bin/bash

# skript co dela vse co potrebuju pro NK

# =================================================

#nastaveni cesty
cd /home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/02_input_data/03_csv_ciselniky_upravene

# NKall
#awk '{$1=""; print $0}' E1_NK_pokus.csv S1_NK_pokus.csv > NKall.txt 
awk -F ";" '{print $1 ";" $2 ";" $3 ";" $4}' F1_NK.csv   F2_NK.csv   F3_NK.csv  F5_LNK.csv F1_LNK.csv F2_LNK.csv  F3_LNK.csv  F4_NK.csv  F5_NK.csv > NKall.txt 


#--------------------------------------------------

# NKB:
# NKB1 --> NKB2 --> NKB3 --> NKB

#B1 struktura:
#vypise jen radky s NK a K, jen 2. a 3. sloupec
awk -F ";" 'c&&!--c; /NK/  {c=1; print $2 ";" $3 ";" $4}' F1_NK.csv   F2_NK.csv   F3_NK.csv  F5_LNK.csv F1_LNK.csv F2_LNK.csv  F3_LNK.csv  F4_NK.csv  F5_NK.csv > NKB1.txt 



#B2 struktura:
#spojeni dvou radku NK a K za sebe
awk '{ ORS = (NR%2 ? ";" : RS) } 1'  NKB1.txt > NKB2.txt

#B3 struktura:
#vymazani sloupce s “K”:
awk -F ";" '{print $2 ";" $3 ";" $6}' NKB2.txt > NKB3.txt  

#prejmenovani na vysledne NKB
cp NKB3.txt NKB.txt

#smazani pracovnich souboru
rm NKB1.txt NKB2.txt NKB3.txt

#presunuti vyslednych souboru do adresare awk data
mv NKB.txt /home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/02_input_data/04_awk_data/ 
mv NKall.txt /home/kalabava/sya/60_Elspac/03_dokumenty_k_Elspacu/07_sjednoceni_kodovniku/02_input_data/04_awk_data/ 

