install.packages('arules')
#Menggunakan library arules
library(arules)

#Membaca transaksi dari file data_transaksi.txt
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)

#Menampilkan data transaksi dengan print dan inspect 
inspect(transaksi)

#Menghasilkan model Market Basket Analysis
mba <- apriori(transaksi,parameter = list(supp = 0.1, confidence = 0.5))

#Menampilkan paket produk
inspect(subset(mba))

itemFrequencyPlot(transaksi, type="absolute")

#Membaca transaksi dari file data_transaksi.txt
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)

#Menampilkan jumlah kombinasi dari produk yang terdapat pada daftar transaksi yang ada
inspect(apriori(transaksi, parameter = list(support=.1, minlen=2, target='frequent itemsets')))

#Menggunakan library arules
library(arules)

#Membaca transaksi dari file data_transaksi.txt
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)

#Menampilkan jumlah kombinasi dari produk yang terdapat pada daftar transaksi yang ada
inspect(apriori(transaksi, parameter = list(support=.1, minlen=2, target='frequent itemsets')))


#Menggunakan library arules
library(arules)

#Membaca transaksi dari file data_transaksi2.txt
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi2.txt", format="single", sep="\t", cols=c(1,2), skip=1)

#Menampilkan jumlah kombinasi dari produk yang terdapat pada daftar transaksi yang ada
inspect(apriori(transaksi, parameter = list(support=.03, minlen=2, target='frequent itemsets')))


#Membaca file yang berlokasi di https://academy.dqlab.id/dataset/data_transaksi.txt dengan fungsi read.csv, dan kemudian disimpan pada variable transaksi_tabular
transaksi_tabular <- read.csv("https://academy.dqlab.id/dataset/data_transaksi.txt", sep="\t")

#Menampilkan variable transaksi_tabular dengan fungsi print
print(transaksi_tabular)


library(arules)
read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)
transaksi@itemInfo


library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)
transaksi@itemsetInfo


library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)
transaksi@data


library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)
itemFrequency(transaksi, type="absolute")


library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)

data_item <- itemFrequency(transaksi, type="absolute")

#Melakukan sorting pada data_item
data_item <- sort(data_item, decreasing = TRUE)

#Mengambil 3 item pertama
data_item <- data_item[1:3]

#Konversi data_item menjadi data frame dengan kolom Nama_Produk dan Jumlah
data_item <- data.frame("Nama Produk"=names(data_item), "Jumlah"=data_item, row.names=NULL)

print(data_item)



library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)

data_item <- itemFrequency(transaksi, type="absolute")

#Melakukan sorting pada data_item
data_item <- sort(data_item, decreasing = TRUE)

#Mengambil 3 item pertama
data_item <- data_item[1:3]

#Konversi data_item menjadi data frame dengan kolom Nama_Produk dan Jumlah
data_item <- data.frame("Nama Produk"=names(data_item), "Jumlah"=data_item, row.names=NULL)

#Menulis File Statistik Top 3
write.csv(data_item, file="top3_item_retail.txt", eol = "\r\n")


library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)

#Tampilan item frequency plot
itemFrequencyPlot(transaksi)

install.packages('apriori')
library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)

#Menggunakan inspect terhadap transaksi
inspect(transaksi)


library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)

#Menghasilkan associaton rules
apriori(transaksi)


library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)

#Menghasilkan association rules dan disimpan sebagai variable mba
mba <- apriori(transaksi)

#Melihat isi dari rules dengan menggunakan fungsi inspect
inspect(mba)


library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)

#Menghasilkan association rules dan disimpan sebagai variable mba
mba <- apriori(transaksi)

#Filter rhs dengan item "Sirup" dan tampilkan
inspect(subset(mba, rhs %in% "Sirup"))


library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)
mba <- apriori(transaksi)
inspect(subset(mba, lhs %in% "Gula"))
inspect(subset(mba, lhs %in% "Gula"))

library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)
apriori(transaksi,parameter = list(supp = 0.1, confidence = 0.5))
apriori(transaksi,parameter = list(supp = 0.1, confidence = 0.5))
library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)
mba <- apriori(transaksi)
inspect(subset(mba, lhs %in% "Pet Food" & rhs %in% "Sirup"))



library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)
apriori(transaksi,parameter = list(supp = 0.1, confidence = 0.5))
inspect(mba)


library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)
mba <- apriori(transaksi,parameter = list(supp = 0.1, confidence = 0.5))
inspect(subset(mba, lhs %in% "Teh Celup" | rhs %in% "Teh Celup"))


library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)
mba <- apriori(transaksi,parameter = list(supp = 0.1, confidence = 0.5))
inspect(subset(mba, (lhs %in% "Teh Celup" | rhs %in% "Teh Celup") & lift>1))


library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)
mba <- apriori(transaksi,parameter = list(supp = 0.1, confidence = 0.5))
inspect(subset(mba, (lhs %ain% c("Pet Food", "Gula" ))))


library(arules)
library(arulesViz)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)
mba <- apriori(transaksi,parameter = list(supp = 0.1, confidence = 0.5))
plot(subset(mba, lift>1.1), method="graph")
plot(subset(mba, lift>1.1), method="graph")