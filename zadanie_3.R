# install.packages("tidyverse")
# install.packages("data.table")
# install.packages("plyr")



library(dplyr)
library(ggplot2)
library(DBI)
library(RMySQL)
library(stringr)
library(data.table)
library(plyr)
library(tidyr)

myHost<- "54.37.136.190"
myUsername<-"student"
myPort<-3306
myDbname<-"auta2"
myPassword<-".studenT800."

### encoding ? sprwdzic czy dziala!
conM<-dbConnect( MySQL(),user=myUsername,host=myHost,password=myPassword,dbname=myDbname,port=myPort,encoding='UTF-8' )

dbListTables(conM)

query <- "SELECT * FROM daty ORDER BY data DESC LIMIT 1"

data <- dbGetQuery(conM, query)

write.csv(data,file="small.csv",fileEncoding = "UTF-8")

data$data

tbl(conM,"daty") %>% show_query()
datytbl<- tbl(conM,"daty")

daty<-datytbl%>%collect()

#dplyr
daty %>% select(data)

#arrange
data1 <- daty%>% arrange( desc(data) )  %>%  head(1) %>% select(data) 
daty%>% filter( data=="2022-04-10") 
data1$data

dbListFields(conM,"auta2weeks")

auta2tbl<- tbl(conM,"auta2weeks")



auta2<-read.csv("C:\\Users\\Ukasz\\Desktop\\BIG DATA\\_R\\LAB2\\auta2.csv",fileEncoding = "UTF-8")


# ZADANIE 3
zad3 <- auta2 %>%select(cena,Przebieg,Rok.produkcji,Marka.pojazdu, Model.pojazdu,Wersja,Generacja,Rodzaj.paliwa, Pojemność.skokowa,Moc,Skrzynia.biegów, Napęd,Typ.nadwozia,Liczba.drzwi,Liczba.miejsc, Bezwypadkowy,Uszkodzony,Stan,Kolor)

# selecting cars bezwypadkowy
zad4 <- filter(zad3, Bezwypadkowy == 'Tak')

# selecting cars from 2013
zad4 <- filter(zad4, Rok.produkcji >= 2013)

#View(zad4)
#colnames(zad4)

# selecting cars that are up to 50k zl
zad4 <- filter(zad4, cena <= 50000)
View(zad4)


# Przedstaw w tabeli: średnie ceny, przebieg, rocznik, liczbę ogłoszeń wybranych marek A
# checking how many empty values in the column
sum(is.na(zad4$Przebieg))

# checking how many empty values in every  column
cbind(
  lapply(
    lapply(zad4, is.na)
    , sum)
)

# dropping rows where przebieg is empty using tidyr
library(tidyr)
zad4 <- zad4 %>% drop_na(Przebieg)
sum(is.na(zad4$Przebieg))
dim(zad4)

# cleaning pezebieg column
zad4$Przebieg <- gsub(' ', '', zad4$Przebieg)  # removing empty spaces
zad4$Przebieg <- gsub('km', '', zad4$Przebieg) # removing km
zad4$Przebieg <- as.numeric(as.character(zad4$Przebieg)) # converting string to numeric


srednia_cena_cal <- sum(zad4[,'cena']) / dim(zad4)[1]
sredni_przebieg_cal <- sum(zad4[,'Przebieg']) / dim(zad4)[1]
srednia_rocznik_cal <- round(sum(zad4[,'Rok.produkcji']) / dim(zad4)[1], digits = 0)



# creating dataframe from scratch basic sumarizin whole table
df_srednia_basic <- data.frame (srednia_cena  = c(srednia_cena_cal),
                  srednia_przebieg  = c(sredni_przebieg_cal),
                  srednia_rocznik  = c(srednia_rocznik_cal),
                  liczba_marek  = c(length(unique(zad4[["Marka.pojazdu"]])))
)

df_srednia_basic
View(df_srednia_basic)

# statystics for the whole dataframe
summary(zad4)


# Przedstaw w tabeli: średnie ceny, przebieg, rocznik, liczbę ogłoszeń wybranych marki B
marki <- c(unique(zad4[["Marka.pojazdu"]]))

srednia_ceny_lista <- c() # creating empty vectors
sredni_przebieg_lista <- c()
srednia_rocznik_lista <- c()
liczba_marek_lista <- c()

marki


for (marka in marki) {
  temp_frame <- filter(zad4, Marka.pojazdu == marka) # selecting only rows for specific marka
  
  srednia_cena_cal <- sum(temp_frame[,'cena']) / dim(temp_frame)[1]
  sredni_przebieg_cal <- sum(temp_frame[,'Przebieg']) / dim(temp_frame)[1]
  srednia_rocznik_cal <- round(sum(temp_frame[,'Rok.produkcji']) / dim(temp_frame)[1], digits = 0)
  liczba_marek  <- sum(zad4$Marka.pojazdu == marka)

  srednia_ceny_lista <- c(srednia_ceny_lista, srednia_cena_cal) # adding values to the vectors
  sredni_przebieg_lista <- c(sredni_przebieg_lista, sredni_przebieg_cal)
  srednia_rocznik_lista <- c(srednia_rocznik_lista, srednia_rocznik_cal)
  liczba_marek_lista <- c(liczba_marek_lista, liczba_marek)
  
}


# creating dataframe from scratch basic summarizing for each model
df_srednia_for_model <- data.frame (
                                Marka = marki,
                                srednia_cena_marka  = srednia_ceny_lista,
                                srednia_przebieg_marka  = sredni_przebieg_lista,
                                srednia_rocznik_marka  = srednia_rocznik_lista,
                                liczba_marek_marka  = liczba_marek_lista
)

df_srednia_for_model
View(df_srednia_for_model)



# Przedstaw w tabeli: średnie ceny, przebieg, rocznik, liczbę ogłoszeń wybranych MODELI C
marki <- c(unique(zad4[["Marka.pojazdu"]]))

marka_name_lista <- c()
model_name_lista <- c()
srednia_ceny_lista <- c() # creating empty vectors
sredni_przebieg_lista <- c()
srednia_rocznik_lista <- c()
liczba_modeli_lista <- c()



# we do the same but this time we use nested loop to extract info for each model
for (marka in marki) {
  temp_frame_marka <- filter(zad4, Marka.pojazdu == marka)
  
  models <- c(unique(temp_frame_marka[["Model.pojazdu"]]))
 
  for (model in models){
    temp_frame_model <- filter(temp_frame_marka, Model.pojazdu == model)
    
    srednia_cena_cal <- sum(temp_frame_model[,'cena']) / dim(temp_frame_model)[1]
    sredni_przebieg_cal <- sum(temp_frame_model[,'Przebieg']) / dim(temp_frame_model)[1]
    srednia_rocznik_cal <- round(sum(temp_frame_model[,'Rok.produkcji']) / dim(temp_frame_model)[1], digits = 0)
    liczba_modeli <- sum(zad4$Model.pojazdu == model)
    
    
    
    marka_name_lista <- c(marka_name_lista, marka)
    model_name_lista <- c(model_name_lista, model)
    
    srednia_ceny_lista <- c(srednia_ceny_lista, srednia_cena_cal) # adding values to the vectors
    sredni_przebieg_lista <- c(sredni_przebieg_lista, sredni_przebieg_cal)
    srednia_rocznik_lista <- c(srednia_rocznik_lista, srednia_rocznik_cal)
    liczba_modeli_lista <- c(liczba_modeli_lista, liczba_modeli)
  
  }
}


# creating dataframe from scratch basic summarizing for each model
df_model <- data.frame (
  Marka  = marka_name_lista,
  Model = model_name_lista,
  srednia_cena_model  = srednia_ceny_lista,
  srednia_przebieg_model  = sredni_przebieg_lista,
  srednia_rocznik_model  = srednia_rocznik_lista,
  liczba_ogloszen  = liczba_modeli_lista

)

df_model
View(df_model)

# Przedstaw w tabeli: mediany cen, przebiegu, rocznika, liczbę ogłoszeń wybranych modeli D

#median(zad4$Marka.pojazdu, na.rm = FALSE)

marki <- c(unique(zad4[["Marka.pojazdu"]]))

marka_name_lista <- c()
model_name_lista <- c()
mediana_ceny_lista <- c() # creating empty vectors
mediana_przebieg_lista <- c()
mediana_rocznik_lista <- c()
liczba_modeli_lista <- c()



# we do the same but this time we use nested loop to extract info for each model
for (marka in marki) {
  temp_frame_marka <- filter(zad4, Marka.pojazdu == marka)
  
  models <- c(unique(temp_frame_marka[["Model.pojazdu"]]))
  
  for (model in models){
    temp_frame_model <- filter(temp_frame_marka, Model.pojazdu == model)
    
    mediana_cena_cal <- median(temp_frame_model$cena, na.rm = FALSE)
    mediana_przebieg_cal <- median(temp_frame_model$Przebieg, na.rm = FALSE)
    mediana_rocznik_cal <- median(temp_frame_model$Rok.produkcji, na.rm = FALSE)
    liczba_modeli <- sum(zad4$Model.pojazdu == model)
    
    
    
    marka_name_lista <- c(marka_name_lista, marka)
    model_name_lista <- c(model_name_lista, model)
    
    mediana_ceny_lista <- c(mediana_ceny_lista, mediana_cena_cal) # adding values to the vectors
    mediana_przebieg_lista <- c(mediana_przebieg_lista, mediana_przebieg_cal)
    mediana_rocznik_lista <- c(mediana_rocznik_lista, mediana_rocznik_cal)
    liczba_modeli_lista <- c(liczba_modeli_lista, liczba_modeli)
    
  }
}


# creating dataframe from scratch basic summarizing for each model
df_model_mediana <- data.frame (
  Marka  = marka_name_lista,
  Model = model_name_lista,
  mediana_cena_model  = mediana_ceny_lista,
  mediana_przebieg_model  = mediana_przebieg_lista,
  mediana_rocznik_model  = mediana_rocznik_lista,
  liczba_ogloszen  = liczba_modeli_lista
  
)

df_model_mediana
View(df_model_mediana)


# # # # # # # # # # # #
# WYKRESY LICZBY
# Liczba ogloszen na marke 1
df_srednia_for_model
x <- df_srednia_for_model$Marka
y <- df_srednia_for_model$liczba_marek_marka

barplot(y, names.arg = x, density = 10, main="Liczba ogloszen - Marka")

# Liczba rocznik 2
rok_produkcji <- zad4$Rok.produkcji
df <- as.data.frame(table(rok_produkcji))

x <- df$rok_produkcji
y <- df$Freq

barplot(y, names.arg = x, density = 10, main="Liczba ogloszen - Rok")

# Liczba Typ napedu 3
# df1 <- zad4 %>% drop_na(Naped) #dropping NAN values
naped <- zad4$Naped
df <- as.data.frame(table(naped))

x <- df$naped
y <- df$Freq

barplot(y, names.arg = x, density = 10, main="Liczba ogloszen - Naped", cex.names=0.5)



# Liczba Skrzynia.biegów 4

df1 <- zad4 %>% drop_na(Skrzynia.biegów) #dropping NAN values

skrzynia_biegow <- df1$Skrzynia.biegów
df <- as.data.frame(table(skrzynia_biegow))

x <- df$skrzynia_biegow
y <- df$Freq

barplot(y, names.arg = x, density = 10, main="Liczba ogloszen - Skrzynia biegów")

# Liczba Rodzaj.paliwa 5

df1 <- zad4 %>% drop_na(Rodzaj.paliwa) #dropping NAN values

rodzaj_paliwa <- df1$Rodzaj.paliwa
df <- as.data.frame(table(rodzaj_paliwa))

x <- df$rodzaj_paliwa
y <- df$Freq

barplot(y, names.arg = x, density = 10, main="Liczba ogloszen - Rodzaj paliwa", cex.names=0.5)


# Liczba Typ.nadwozia 6

df1 <- zad4 %>% drop_na(Typ.nadwozia) #dropping NAN values

typ_nadwozia <- df1$Typ.nadwozia
df <- as.data.frame(table(typ_nadwozia))

x <- df$typ_nadwozia
y <- df$Freq

barplot(y, names.arg = x, density = 10, main="Liczba ogloszen - Typ nadwozia", cex.names=0.5)

# # # # # # # # # # # #
# WYKRESY CENA W ZALEZNOSCI
# Cena w zaleznosci od Rocznika 1
plot(zad4$Rok.produkcji, zad4$cena, pch=20, main="Cena w zaleznosci od Rocznika", xlab='Rok', ylab='Cena')

# Cena w zaleznosci od napedu 2
df1 <- zad4 %>% drop_na(Naped) #dropping NAN values

df <- data.frame(
  Cena  = df1$cena, 
  Naped = df1$Naped
)

out <- aggregate(x= df$Cena,     
                 
                 # Specify group indicator
                 by = list(df$Naped),      
                 
                 # Specify function (i.e. mean)
                 FUN = mean)

barplot(out$x, names.arg = out$Group.1, density = 10, main="Srednia cena wg Napedu", xlab='Typ', ylab='Srednia cena')


# Cena w zaleznosci od skrzyni biegow 3
df1 <- zad4 %>% drop_na(Skrzynia.biegów) #dropping NAN values

df <- data.frame(
  Cena  = df1$cena, 
  Skrzynia.biegów = df1$Skrzynia.biegów
)

out <- aggregate(x= df$Cena,     
                 
                 # Specify group indicator
                 by = list(df$Skrzynia.biegów),      
                 
                 # Specify function (i.e. mean)
                 FUN = mean)

barplot(out$x, names.arg = out$Group.1, density = 10, main="Srednia cena wg Skrzynia biegów", xlab='Typ', ylab='Srednia cena')


# Cena w zaleznosci od Rodzaj.paliwa 4
df1 <- zad4 %>% drop_na(Rodzaj.paliwa) #dropping NAN values

df <- data.frame(
  Cena  = df1$cena, 
  Rodzaj.paliwa = df1$Rodzaj.paliwa
)

out <- aggregate(x= df$Cena,     
                 
                 # Specify group indicator
                 by = list(df$Rodzaj.paliwa),      
                 
                 # Specify function (i.e. mean)
                 FUN = mean)

barplot(out$x, names.arg = out$Group.1, density = 10, main="Srednia cena wg Rodzaj paliwa", xlab='Typ', ylab='Srednia cena')



# Cena w zaleznosci od Typ Nadwozia 5
df1 <- zad4 %>% drop_na(Typ.nadwozia) #dropping NAN values

df <- data.frame(
  Cena  = df1$cena, 
  Typ.nadwozia = df1$Typ.nadwozia
)

out <- aggregate(x= df$Cena,     
                 
                 # Specify group indicator
                 by = list(df$Typ.nadwozia),      
                 
                 # Specify function (i.e. mean)
                 FUN = mean)

barplot(out$x, names.arg = out$Group.1, density = 10, main="Srednia cena wg Typ nadwozia", xlab='Typ', ylab='Srednia cena')

