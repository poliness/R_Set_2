# Zadanie 1
# Pobierz i zapisz do csv dane porcjami z bazy danych.

#install.packages("RMySQL")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(DBI)
library(RMySQL)
library(stringr)

myHost <- "54.37.136.190"
myUsername <- "student"
myPort <- 3306
myDbname<-"auta2"
myPassword <- ".studenT800."
conM <- dbConnect(MySQL(),user=myUsername,host=myHost,password=myPassword,
                  dbname=myDbname,port=myPort)

dbListTables(conM)

# Zadanie 2
# Przefiltruj dane z bazy wybierając najnowsze ogłoszenia,
# daty wybierz z tabeli data.

query<-"SELECT * FROM daty ORDER BY data DESC LIMIT 1"
data <- dbGetQuery(conM,query)
data$data

tbl(conM,"daty") %>%show_query()
datytbl <- tbl(conM,"daty")

daty <- datytbl %>% collect()

#dplyr
daty %>% select(data)

#arrange
data1 <- daty %>% arrange(desc(data)) %>% head(1) %>% select(data)
daty %>% filter(data=="2022-04-10")
data1$data

dbListFields(conM,"auta2weeks")

auta2tbl <- tbl(conM,"auta2weeks")

#auta2 <- auta2tbl %>% filter(data=="2022-04-10")%>%collect()
#dbDisconnect(conM)


# Zadanie 3 
# Wybierz kolumny:
# cena,Przebieg,Rok.produkcji,Marka.pojazdu, Model.pojazdu,Wersja,Generacja,Rodzaj.paliwa,
# Pojemność.skokowa,Moc,Skrzynia.biegów, Napęd,Typ.nadwozia,Liczba.drzwi,Liczba.miejsc, Bezwypadkowy,Uszkodzony,Stan,Kolor
# Oraz ‘napraw’ kolumny :Przebieg, Pojemność.skokowa,Moc

auta2 <- read.csv("auta2.csv",encoding = "UTF-8")
View(auta2)

zad3 <- auta2 %>%select(cena,Przebieg,Rok.produkcji,Marka.pojazdu, Model.pojazdu,
                         Wersja,Generacja,Rodzaj.paliwa,
                         Pojemność.skokowa,Moc,Skrzynia.biegów, Napęd,Typ.nadwozia,
                         Liczba.drzwi,Liczba.miejsc, Bezwypadkowy,Uszkodzony,Stan,Kolor)

View(zad3)

#dbDisconnect(conM)

str(zad3)

zad3 <- zad3 %>% mutate(Przebieg=as.numeric(Przebieg%>%str_replace_all("[^\\d,]", "")%>%str_replace_all(",", ".")))
zad3 <- zad3 %>% mutate(Moc=as.numeric(Moc%>%str_replace_all("[^\\d,]", "")%>%str_replace_all(",", ".")))
zad3 <- zad3 %>% mutate(Pojemność.skokowa=as.numeric(Pojemność.skokowa%>%str_replace_all("cm3", "")%>%str_replace_all("[^\\d,]", "")))

zad3 <- mutate_if(zad3,is.character,as.factor)

str(zad3)

summary(zad3)


zad3 %>% group_by(Model.pojazdu) %>% summarise(n=n(),mean_cena=mean(cena),mean_przebieg=mean(na.omit(Przebieg))) %>% arrange(desc(n),desc(mean_cena))



# Zadanie 4

auto <- zad3 %>% filter(Bezwypadkowy=='Tak' & Rok.produkcji >= 2013 & cena <= 50000)

auto1 <- auto %>% group_by(Model.pojazdu) %>% summarise(srednia_cena=mean(cena),sredni_przebieg=mean(na.omit(Przebieg)),sredni_rocznik=mean(Rok.produkcji),liczba_ogloszen=n()) %>% arrange(desc(sredni_rocznik),desc(sredni_przebieg),desc(srednia_cena))

auto1

auto2 <- auto %>% group_by(Model.pojazdu) %>% summarise(mediana_cena=median(cena),mediana_przebieg=median(na.omit(Przebieg)),mediana_rocznik=median(Rok.produkcji),liczba_ogloszen=n()) %>% arrange(desc(mediana_rocznik),desc(mediana_cena))

auto2

#wykresy dla wybranych modeli
choice <- auto %>% filter(Marka.pojazdu=="Volkswagen" | Marka.pojazdu=="Audi")
choice1 <- choice %>% filter(Model.pojazdu=="Tiguan" |
                             Model.pojazdu=="T-Roc" | Model.pojazdu=="Scirocco" |
                             Model.pojazdu=="Q5" | Model.pojazdu=="Q3" |
                             Model.pojazdu=="A6" | Model.pojazdu=="A5" |
                             Model.pojazdu=="A4")


ggplot(choice1, aes(x=Model.pojazdu)) + geom_bar(fill="turquoise4") +
  ggtitle("Liczba ogłoszeń vs. model pojazdu") + labs(x= "Model pojazdu", y = "Liczba ogłoszeń")

ggplot(choice1, aes(x=Rok.produkcji)) + geom_bar(fill="violetred4") +
  ggtitle("Liczba ogłoszeń vs. rok produkcji") + labs(x= "Rok produkcji", y = "Liczba ogłoszeń")

ggplot(choice1, aes(x=Napęd)) + geom_bar(fill="springgreen4") + coord_flip() +
  ggtitle("Liczba ogłoszeń vs. rodzaj napędu") + labs(x= "Rodzaj napędu", y = "Liczba ogłoszeń")

ggplot(choice1, aes(x=Skrzynia.biegów)) + geom_bar(fill="deepskyblue4") +
  ggtitle("Liczba ogłoszeń vs. rodzaj skrzyni biegów") + labs(x= "Rodzaj skrzyni biegów", y = "Liczba ogłoszeń")

ggplot(choice1, aes(x=Rodzaj.paliwa)) + geom_bar(fill="indianred4") + 
  ggtitle("Liczba ogłoszeń vs. rodzaj paliwa") + labs(x= "Rodzaj paliwa", y = "Liczba ogłoszeń")

ggplot(choice1, aes(x=Typ.nadwozia)) + geom_bar(fill="orange2") +
  ggtitle("Liczba ogłoszeń vs. typ nadwozia") + labs(x= "Typ nadwozia", y = "Liczba ogłoszeń")



g <- ggplot(choice1, aes(x=cena, y=Rok.produkcji))
g + geom_jitter(aes(color = Model.pojazdu)) + ggtitle("Cena vs. rok produkcji") + 
  labs(x= "Cena", y = "Rok produkcji")  

g <- ggplot(choice1, aes(x=cena, y=Napęd))
g + geom_jitter(aes(color = Model.pojazdu)) + ggtitle("Cena vs. rodzaj napędu") + 
  labs(x= "Cena", y = "Rodzaj napędu")  

g <- ggplot(choice1, aes(x=cena, y=Skrzynia.biegów))
g + geom_jitter(aes(color = Model.pojazdu)) + ggtitle("Cena vs. rodzaj skrzyni biegów") + 
  labs(x= "Cena", y = "Rodzaj skrzyni biegów")

g <- ggplot(choice1, aes(x=cena, y=Rodzaj.paliwa))
g + geom_jitter(aes(color = Model.pojazdu)) + ggtitle("Cena vs. rodzaj paliwa") + 
  labs(x= "Cena", y = "Rodzaj paliwa")

g <- ggplot(choice1, aes(x=cena, y=Typ.nadwozia))
g + geom_jitter(aes(color = Model.pojazdu)) + ggtitle("Cena vs. typ nadwozia") + 
  labs(x= "Cena", y = "Typ nadwozia")




#na zajęciach

options(scipen=999)

ggplot(zad3,aes(x=Marka.pojazdu))+
  geom_bar()

audi <- zad3 %>% filter(Marka.pojazdu=="Audi")
audi <- audi %>% filter(Rok.produkcji==2012)
ggplot(audi, aes(x=Model.pojazdu))+
  geom_bar()+coord_flip()

audi <- audi %>% filter(Model.pojazdu=="A4" | Model.pojazdu=="A3")

g<-ggplot(audi, aes(x=Model.pojazdu,y=cena))
  g+geom_point(aes(color=Rok.produkcji))
  
  g<-ggplot(audi, aes(x=Model.pojazdu,y=cena))
  g+geom_point(aes(color=Przebieg))
  
  g<-ggplot(audi, aes(x=Model.pojazdu,y=cena))
  g+geom_point(aes(color=Przebieg))+facet_wrap(~Rok.produkcji)
  
  g<-ggplot(audi, aes(x=Model.pojazdu,y=cena))
  g+geom_point(aes(color=Przebieg))+facet_grid(~Rok.produkcji)
  
  g<-ggplot(audi, aes(x=Model.pojazdu,y=cena))
  g+geom_boxplot(aes(color=as.factor(Rok.produkcji)))

