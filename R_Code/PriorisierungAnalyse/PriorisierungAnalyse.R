# LOAD DATA ################################################

install.packages("readxl")
install.packages("writexl")
library("readxl")
library("writexl")
install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")
library(easyGgplot2)
library(tibble)
library(RColorBrewer)
library(lubridate)
install.packages("devtools")
install.packages("Rtools")
install_github("kassambara/easyGgplot2")
library(devtools)
library(easyGgplot2)
library(plyr)
library(ggplot2)
library(dplyr)
install.packages("hrbrthemes")
library(hrbrthemes)
library(RColorBrewer)
coul <- brewer.pal(8, "Set2")
install.packages("wesanderson")
library(wesanderson)
suppressPackageStartupMessages(library(tidyverse))
install.packages("tidyverse")


prioriseriungs_Daten <- read_excel("C:/Users/Jonas/Desktop/Relevanzwahrnehmung_Priorisierung.xlsx")

View(prioriseriungs_Daten)

zeilenanzahl <- length(prioriseriungs_Daten$Question_No)
print(zeilenanzahl)
maxRespondend_Id = max(prioriseriungs_Daten$Respondent_ID)
print(maxRespondend_Id)
minRespondend_Id = min(prioriseriungs_Daten$Respondent_ID)
print(minRespondend_Id)

oneCounter <- 0
twoCounter <- 0
threeCounter <- 0
#oneTwoCounter <- 0
OOO <- 0
OOX <- 0
OXO <- 0
OXX <- 0
XOO <- 0
XOX <- 0
XXO <- 0
XXX <- 0

kp <- 0

i <- minRespondend_Id
while (i < maxRespondend_Id + 1){
  print(i)
  zeilenZw = prioriseriungs_Daten[prioriseriungs_Daten$Respondent_ID == i,]
  
  len = length(zeilenZw$Respondent_ID)
  if(len > 0){
    kp = kp + 1
  }
  
  spalteZw_1 = zeilenZw[zeilenZw$Question_No == 1,]
  anzahl_1 <- length(spalteZw_1$Question_No)
  print(anzahl_1)
  oneCounter = oneCounter + anzahl_1
  
  spalteZw_2 = zeilenZw[zeilenZw$Question_No == 2,]
  anzahl_2 <- length(spalteZw_2$Question_No)
  print(anzahl_2)
  twoCounter = twoCounter + anzahl_2
  
  spalteZw_3 = zeilenZw[zeilenZw$Question_No == 3,]
  anzahl_3 <- length(spalteZw_3$Question_No)
  print(anzahl_3)
  threeCounter = threeCounter + anzahl_3
  
  if(anzahl_1 == 1 & anzahl_2 == 1 & anzahl_3 == 1){
    OOO = OOO + 1
  }
  if(anzahl_1 == 1 & anzahl_2 == 1 & anzahl_3 != 1){
    OOX = OOX + 1
  }
  if(anzahl_1 == 1 & anzahl_2 != 1 & anzahl_3 != 1){
    OXX = OXX + 1
  }
  if(anzahl_1 == 1 & anzahl_2 != 1 & anzahl_3 == 1){
    OXO = OXO + 1
  }
  
  if(anzahl_1 != 1 & anzahl_2 == 1 & anzahl_3 == 1){
    XOO =XOO + 1
  }
  if(anzahl_1 != 1 & anzahl_2 == 1 & anzahl_3 != 1){
    XOX = XOX + 1
  }
  if(anzahl_1 != 1 & anzahl_2 != 1 & anzahl_3 == 1){
    XXO =XXO + 1
  }
  if(anzahl_1 != 1 & anzahl_2 != 1 & anzahl_3 != 1){
    XXX = XXX + 1
    #kp = i
  }
  
  i = i + 1
  
  #fragenAnzahl = prioriseriungs_Daten$Question_No

}

print(oneCounter)
print(twoCounter)
print(threeCounter)
print(oneTwoCounter)
print(OOO) 
print(OOX) 
print(OXO) 
print(OXX) 
print(XOO) 
print(XOX)
print(XXO) 
print(XXX) 
print(kp)




#_________________________________________________________________________________
# Ben?tigte Zeit
#

zeilenanzahl <- length(prioriseriungs_Daten$Question_No)
print(zeilenanzahl)
maxRespondend_Id = max(prioriseriungs_Daten$Respondent_ID)
print(maxRespondend_Id)
minRespondend_Id = min(prioriseriungs_Daten$Respondent_ID)
print(minRespondend_Id)

# Checken ob eine Befragung Datum?bergreifend stattgefunden hat also z.B. eine Frage am 17.02 (um Mitternacht) und eine am 18.02 beantwortet wurde
i <- minRespondend_Id
while (i < maxRespondend_Id + 1){
  zeilenZw = prioriseriungs_Daten[prioriseriungs_Daten$Respondent_ID == i,]
  len = length(zeilenZw$Respondent_ID)
  spa = zeilenZw$`Timestamp (DD-MM-YY)`
  k <- 1
  while (k < len) {
    if(spa[k] != spa[k+1]){
      print(spa[k])
      print(spa[k+1])
      print("Pause")
    }
    k = k + 1
  }
  i = i + 1 
}  


prioriseriungs_Daten <- read_excel("C:/Users/Jonas/Desktop/Relevanzwahrnehmung_Priorisierung.xlsx")


#prioriseriungs_Daten <- as.POSIXct(prioriseriungs_Daten$`Timestamp (HH-MM-SS)`, format="%H:%M:%S", tz="UTC")
install.packages("chron")
library(chron)

prioriseriungs_Daten$BearbeitungszeitOOO = prioriseriungs_Daten$Response_ID
prioriseriungs_Daten$BearbeitungszeitOOX = prioriseriungs_Daten$Response_ID
prioriseriungs_Daten$BearbeitungszeitOXX = prioriseriungs_Daten$Response_ID
prioriseriungs_Daten$BearbeitungszeitOXO = prioriseriungs_Daten$Response_ID
prioriseriungs_Daten$BearbeitungszeitXOO = prioriseriungs_Daten$Response_ID
prioriseriungs_Daten$BearbeitungszeitXOX = prioriseriungs_Daten$Response_ID
prioriseriungs_Daten$BearbeitungszeitXXO = prioriseriungs_Daten$Response_ID

i <- 1
while (i < 2069){
  prioriseriungs_Daten$Bearbeitungszeit[i] = -1
  prioriseriungs_Daten$BearbeitungszeitOOO = -1
  prioriseriungs_Daten$BearbeitungszeitOOX = -1
  prioriseriungs_Daten$BearbeitungszeitOXX = -1
  prioriseriungs_Daten$BearbeitungszeitOXO = -1
  prioriseriungs_Daten$BearbeitungszeitXOO = -1
  prioriseriungs_Daten$BearbeitungszeitXOX = -1
  prioriseriungs_Daten$BearbeitungszeitXXO = -1
  
  i = i + 1
}

#time <- times(strftime(prioriseriungs_Daten, tz="UTC", format="%H:%M:%S"))
time <- times(strftime(prioriseriungs_Daten, tz="UTC", format='%Y%m%d:%H:%M:%S'))
View(time)

prioriseriungs_Daten$`Timestamp (HH-MM-SS)` = time


#View(prioriseriungs_Daten$`Timestamp (DD-MM-YY)`)
#View(prioriseriungs_Daten$`Timestamp (HH-MM-SS)`)
#my <- as.POSIXct(paste(prioriseriungs_Daten$`Timestamp (DD-MM-YY)` ,time), format='%Y%m%d:%H:%M:%S')
#View(my)
#prioriseriungs_Daten$`Timestamp (HH-MM-SS)` <- as.POSIXct(prioriseriungs_Daten$`Timestamp (HH-MM-SS)`, format='%Y/%m/%d %H:%M:%S')
#prioriseriungs_Daten$`Timestamp (HH-MM-SS)` <- as.POSIXct(prioriseriungs_Daten$`Timestamp (HH-MM-SS)`, format = '%H:%M:%S')
#prioriseriungs_Daten$`Timestamp (HH-MM-SS)` <- as.POSIXct(prioriseriungs_Daten$`Timestamp (HH-MM-SS)`, format = '%d%b%Y:%H:%M:%S')

View(prioriseriungs_Daten)

#format(parse_date_time(time, c("HMS", "HM"), tz = "UTC"), "%H:%M:%S")

prioriseriungs_Daten$`Timestamp (HH-MM-SS)` <- format(parse_date_time(prioriseriungs_Daten$`Timestamp (HH-MM-SS)`, c("HMS", "HM"), tz = "GMT"), "%H:%M:%S")

BearbeitungszeitOOO_1_3 = NULL
BearbeitungszeitOOX_1_2 = NULL
BearbeitungszeitOXO_1_3 = NULL
BearbeitungszeitXOO_2_3 = NULL
BearbeitungszeitOXX_1 = NULL

demografische_Daten <- read_excel("C:/Users/Jonas/Desktop/Relevanzwahrnehmung_demografische_Daten_Date_chenged.xlsx")
View(demografische_Daten)

  

kp <- 0
i <- minRespondend_Id
while (i < maxRespondend_Id + 1){
  #print(i)
  
  zeilenZw = prioriseriungs_Daten[prioriseriungs_Daten$Respondent_ID == i,]
  len = length(zeilenZw$Respondent_ID)
  spa = zeilenZw$`Timestamp (DD-MM-YY)`

  spalteZw_1 = zeilenZw[zeilenZw$Question_No == 1,]
  anzahl_1 <- length(spalteZw_1$Question_No)

  spalteZw_2 = zeilenZw[zeilenZw$Question_No == 2,]
  anzahl_2 <- length(spalteZw_2$Question_No)
  
  spalteZw_3 = zeilenZw[zeilenZw$Question_No == 3,]
  anzahl_3 <- length(spalteZw_3$Question_No)
  

  
  if(anzahl_1 == 1 & anzahl_2 == 1 & anzahl_3 == 1){
    
    zeile = zeilenZw$Response_ID[3] - 40
    print(zeile)
  
    diff12 = period_to_seconds(hms(spalteZw_2[7])) - period_to_seconds(hms(spalteZw_1[7]))
    diff23 = period_to_seconds(hms(spalteZw_3[7])) - period_to_seconds(hms(spalteZw_2[7])) 
    diff13 = period_to_seconds(hms(spalteZw_3[7])) - period_to_seconds(hms(spalteZw_1[7])) 
    
    dm <- demografische_Daten[demografische_Daten$Respondent_ID == i,]
    Gender <- dm$Gender[1]
    Education <- dm$Education[1]
    #View(dm)
    print(Gender)
    Respondent_ID = i
    Bearbeitungszeit_1_3 = diff13  
    BearbeitungszeitOOO_1_3 = rbind(BearbeitungszeitOOO_1_3, data.frame(Respondent_ID, Bearbeitungszeit_1_3, Gender, Education))
    
    #kp1 = spalteZw_3[7]
    #kp2 = spalteZw_1[7]
    #as.times(kp1,format='%H:%M:%S')
    #as.times(as.POSIXlt(kp1, format= "%H:%M.%S"), format= "%S:%M.%H")
    #cc2 <- period_to_seconds(hms("12:12:54"))
    #kpp <- str(as.POSIXlt(kp1))
    #kpp <- strptime(kp1, format = "%H:%M:%S", tz = "UTC")
    #print(kp1)
    #print(kpp)
    #cc2 <- period_to_seconds(hms(kpp))
    #cc = as.numeric(kp1, units = "secs")
    #print(cc)
    #cc <- as.numeric(difftime(strptime(kp1,"%H:%M:%S"), strptime(kp2,"%H:%M:%S")))
    #print(cc)
    #c <- hour(kp2)
    #c2 <- minute(kp2)
    #diff133 = difftime(kp2, kp1, units='mins')
    
    prioriseriungs_Daten$Bearbeitungszeit[zeile-2] = diff12
    prioriseriungs_Daten$Bearbeitungszeit[zeile-1] = diff23
    prioriseriungs_Daten$Bearbeitungszeit[zeile] = diff13
    
    prioriseriungs_Daten$BearbeitungszeitOOO[zeile-2] = diff12
    prioriseriungs_Daten$BearbeitungszeitOOO[zeile-1] = diff23
    prioriseriungs_Daten$BearbeitungszeitOOO[zeile] = diff13
    
    print(diff13)
  }
  
  if(anzahl_1 == 1 & anzahl_2 == 1 & anzahl_3 != 1){
    
    zeile = zeilenZw$Response_ID[2] - 40
    #print(zeile)
    
    diff12 = period_to_seconds(hms(spalteZw_2[7])) - period_to_seconds(hms(spalteZw_1[7]))
    #diff23 = period_to_seconds(hms(spalteZw_3[7])) - period_to_seconds(hms(spalteZw_2[7])) 
    #diff13 = period_to_seconds(hms(spalteZw_3[7])) - period_to_seconds(hms(spalteZw_1[7])) 
    
    prioriseriungs_Daten$Bearbeitungszeit[zeile-1] = diff12
    prioriseriungs_Daten$Bearbeitungszeit[zeile] = diff12
    #prioriseriungs_Daten$Bearbeitungszeit[zeile] = diff13
    
    prioriseriungs_Daten$BearbeitungszeitOOX[zeile-1] = diff12
    prioriseriungs_Daten$BearbeitungszeitOOX[zeile] = diff12
    
    dm <- demografische_Daten[demografische_Daten$Respondent_ID == i,]
    Gender <- dm$Gender[1]
    Education <- dm$Education[1]
    print(Gender)
    Respondent_ID = i
    Bearbeitungszeit_1_2 = diff12
    BearbeitungszeitOOX_1_2 = rbind(BearbeitungszeitOOX_1_2, data.frame(Respondent_ID, Bearbeitungszeit_1_2, Gender, Education))
    
    #print(diff12)

  }
  if(anzahl_1 == 1 & anzahl_2 != 1 & anzahl_3 != 1){
    
    zeile = zeilenZw$Response_ID[1] - 40
    
    #prioriseriungs_Daten$Bearbeitungszeit[zeile-1] = diff12
    prioriseriungs_Daten$Bearbeitungszeit[zeile] =  0    #diff12
    #prioriseriungs_Daten$Bearbeitungszeit[zeile] = diff13
    
    prioriseriungs_Daten$BearbeitungszeitOXX[zeile] =  0    #diff12
    
    dm <- demografische_Daten[demografische_Daten$Respondent_ID == i,]
    Gender <- dm$Gender[1]
    Education <- dm$Education[1]
    print(Gender)
    Respondent_ID = i
    Bearbeitungszeit_1 = diff12
    BearbeitungszeitOXX_1 = rbind(BearbeitungszeitOXX_1, data.frame(Respondent_ID, Bearbeitungszeit_1, Gender, Education))
    
  }
  if(anzahl_1 == 1 & anzahl_2 != 1 & anzahl_3 == 1){
    
    zeile = zeilenZw$Response_ID[2] - 40
    #print(zeile)
    
    diff12 = period_to_seconds(hms(spalteZw_3[7])) - period_to_seconds(hms(spalteZw_1[7]))
    #diff23 = period_to_seconds(hms(spalteZw_3[7])) - period_to_seconds(hms(spalteZw_2[7])) 
    #diff13 = period_to_seconds(hms(spalteZw_3[7])) - period_to_seconds(hms(spalteZw_1[7])) 
    
    prioriseriungs_Daten$Bearbeitungszeit[zeile-1] = diff12
    prioriseriungs_Daten$Bearbeitungszeit[zeile] = diff12
    #prioriseriungs_Daten$Bearbeitungszeit[zeile] = diff13
    
    prioriseriungs_Daten$BearbeitungszeitOXO[zeile-1] = diff12
    prioriseriungs_Daten$BearbeitungszeitOXO[zeile] = diff12
    
    dm <- demografische_Daten[demografische_Daten$Respondent_ID == i,]
    Gender <- dm$Gender[1]
    Education <- dm$Education[1]
    print(Gender)
    Respondent_ID = i
    Bearbeitungszeit_1_3 = diff12
    BearbeitungszeitOXO_1_3 = rbind(BearbeitungszeitOXO_1_3, data.frame(Respondent_ID, Bearbeitungszeit_1_3, Gender, Education))
    
    
    #print(diff12)
  }
  
  
  if(anzahl_1 != 1 & anzahl_2 == 1 & anzahl_3 == 1){
    zeile = zeilenZw$Response_ID[2] - 40
    #print(zeile)
    
    diff12 = period_to_seconds(hms(spalteZw_3[7])) - period_to_seconds(hms(spalteZw_2[7]))
    #diff23 = period_to_seconds(hms(spalteZw_3[7])) - period_to_seconds(hms(spalteZw_2[7])) 
    #diff13 = period_to_seconds(hms(spalteZw_3[7])) - period_to_seconds(hms(spalteZw_1[7])) 
    
    prioriseriungs_Daten$Bearbeitungszeit[zeile-1] = diff12
    prioriseriungs_Daten$Bearbeitungszeit[zeile] = diff12
    #prioriseriungs_Daten$Bearbeitungszeit[zeile] = diff13
    
    prioriseriungs_Daten$BearbeitungszeitXOO[zeile-1] = diff12
    prioriseriungs_Daten$BearbeitungszeitXOO[zeile] = diff12
    
    dm <- demografische_Daten[demografische_Daten$Respondent_ID == i,]
    Gender <- dm$Gender[1]
    Education <- dm$Education[1]
    print(Gender)
    Respondent_ID = i
    Bearbeitungszeit_2_3 = diff12
    BearbeitungszeitXOO_2_3 = rbind(BearbeitungszeitXOO_2_3, data.frame(Respondent_ID, Bearbeitungszeit_2_3, Gender, Education))
    
    #print(diff12)

  }
  if(anzahl_1 != 1 & anzahl_2 == 1 & anzahl_3 != 1){
    zeile = zeilenZw$Response_ID[1] - 40
    
    #prioriseriungs_Daten$Bearbeitungszeit[zeile-1] = diff12
    prioriseriungs_Daten$Bearbeitungszeit[zeile] = 0
    #prioriseriungs_Daten$Bearbeitungszeit[zeile] = diff13
    
    prioriseriungs_Daten$BearbeitungszeitXOX[zeile] = 0
    
    #print(diff12)
  }
  if(anzahl_1 != 1 & anzahl_2 != 1 & anzahl_3 == 1){
    zeile = zeilenZw$Response_ID[1] - 40
    
    #prioriseriungs_Daten$Bearbeitungszeit[zeile-1] = diff12
    prioriseriungs_Daten$Bearbeitungszeit[zeile] = 0
    #prioriseriungs_Daten$Bearbeitungszeit[zeile] = diff13
    
    prioriseriungs_Daten$BearbeitungszeitXXO[zeile] = 0
    
    #print(diff12)
  }
  
  if(anzahl_1 != 1 & anzahl_2 != 1 & anzahl_3 != 1){
   
  }
  
  i = i + 1
  
  #fragenAnzahl = prioriseriungs_Daten$Question_No

}

View(prioriseriungs_Daten)




write_xlsx(prioriseriungs_Daten, "C:/Users/Jonas/Desktop/Priorisierungsdaten_Mit_Time_Intervall.xlsx")


View(BearbeitungszeitOOO_1_3)
View(BearbeitungszeitOOX_1_2)
View(BearbeitungszeitOXO_1_3)
View(BearbeitungszeitXOO_2_3)
View(BearbeitungszeitOXX_1)
write_xlsx(BearbeitungszeitOOO_1_3, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_BearbeitungszeitOOO_1_3_Mit_Gender_Education.xlsx")
write_xlsx(BearbeitungszeitOOX_1_2, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_BearbeitungszeitOOX_1_2_Mit_Gender_Education.xlsx")
write_xlsx(BearbeitungszeitOXO_1_3, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_BearbeitungszeitOXO_1_3_Mit_Gender_Education.xlsx")
write_xlsx(BearbeitungszeitXOO_2_3, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_BearbeitungszeitXOO_2_3_Mit_Gender_Education.xlsx")
write_xlsx(BearbeitungszeitOXX_1, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_BearbeitungszeitOXX_1_Mit_Gender_Education.xlsx")

newTab = NULL
t <- 0
while (t < 250) {
  gruppe <- BearbeitungszeitOOO_1_3[BearbeitungszeitOOO_1_3$Bearbeitungszeit_1_3 <= t +10 & BearbeitungszeitOOO_1_3$Bearbeitungszeit_1_3 > t,]
  Anzahl = length(gruppe$Respondent_ID)
  print(Anzahl)
  a = "von"
  b = "bis"
  Bearbeitungszeitraum = paste(a,t,b,(t+10))#"von " + str(t) + "bis " + str((t+10))
  newTab = rbind(newTab, data.frame(Bearbeitungszeitraum, Anzahl))
  
  t = t + 10
  
}
View(newTab)

#nach Gruppen
par(mar=c(4,6,2,1)+0.4)
barplot(newTab$Anzahl, 
        names.arg = newTab$Bearbeitungszeitraum,
        cex.names=.8,
        horiz=TRUE,
        las = 1,
        #col = "#5c00cc",
        #col = coul,
        col = rainbow(16),
        lwd = 3,
        xlim=c(0,120),
        main = "BearbeitungszeitOOO_1_3",
        xlab = "Anzahl")
axis(1, at = seq(0, 120, 10), las = 1)

newTab2 <- NULL
t <- 0
while (t < 200) {
  gruppe <- BearbeitungszeitOOO_1_3[BearbeitungszeitOOO_1_3$Bearbeitungszeit_1_3 == t,]
  Anzahl = length(gruppe$Respondent_ID)
  Bearbeitungszeitraum = t #"von " + str(t) + "bis " + str((t+10))
  newTab2 = rbind(newTab2, data.frame(Bearbeitungszeitraum, Anzahl))
  
  t = t + 1
}
View(newTab2)
write_xlsx(newTab, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_BearbeitungszeitOOO_1_3_Verteilung_nach_Gruppen_10_Sek.xlsx")
write_xlsx(newTab2, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_BearbeitungszeitOOO_1_3_Verteilung_nach_Sekunden.xlsx")

# alt
par(mar=c(4,6,2,1)+0.4)
barplot(newTab2$Anzahl, 
        names.arg = newTab2$Bearbeitungszeitraum,
        cex.names=.8,
        horiz=FALSE,
        las = 1,
        #col = "#5c00cc",
        #col = coul,
        col = rainbow(200),
        lwd = 3,
        xlim=c(0,190),
        ylim=c(0,18),
        main = "BearbeitungszeitOOO_1_3",
        xlab = "Bearbeitungszeit (in sec)",
        ylab="Anzahl")
axis(1, at = seq(0, 200, 10), las = 1)

# Plot mit Punkten und Linien
#newTab2 %>%
  #tail(200) %>%
  #ggplot( aes(x=Bearbeitungszeitraum, y=Anzahl)) +
  #geom_line() +
  #geom_point()

# neu absolut
ggplot(newTab2, aes(x=Bearbeitungszeitraum, y=Anzahl )) + 
  geom_bar(stat = "identity", width=0.5, fill = terrain.colors(200),col= terrain.colors(200))+ #col = wes_palette("FantasticFox1", 200, type = "continuous")) +
  ggtitle("Verteilung der Bearbeitungszeit f?r Priorisierung (3 Fragen)") +
  ylab("Anzahl") +
  xlab("Bearbeitungszeit (in sec)")+ 
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210, 220,230,240,250))+
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20))

View(BearbeitungszeitOOO_1_3)

# relativ
BearbeitungszeitOOO_1_3 %>%
  filter( Bearbeitungszeit_1_3 < 300 ) %>%
  ggplot( aes(x=Bearbeitungszeit_1_3)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Bearbeitungszeit f?r Priorisierung (3 Fragen)") +
  ylab("Prozent") +
  xlab("Bearbeitungszeit (in sec)")+ 
  #theme_ipsum()+
  scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140,160,180,200,220,240))


  
  
  
  
# Bearbeitungszeit nach Gender

  Data = NULL
  Data <- BearbeitungszeitOOO_1_3[!(is.na(BearbeitungszeitOOO_1_3$Gender) | BearbeitungszeitOOO_1_3$Gender ==""), ]
  
  Data$Gender <- revalue(Data$Gender, c("Male"="M?nnlich"))
  Data$Gender <- revalue(Data$Gender, c("Female"="Weiblich"))
  Data$Gender <- revalue(Data$Gender, c("No answer"="Keine Antwort"))
  View(Data)
  
  DataFemale = Data[Data$Gender == "Weiblich",]
  DataMale = Data[Data$Gender == "M?nnlich",]
  DataKeine = Data[Data$Gender == "Keine Antwort",]
  DataAndere = Data[Data$Gender == "Andere",]
  View(DataAndere)
  
  Gender_Median_Mean_Verteilung = NULL
  
# Mittelwerte (Mean)  
  mean_Male <- DataMale %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    mean() %>%
    signif(6)
  print(mean_Male) # 64,6211
  median_Male <- DataMale %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    median() %>%
    signif(6)
  print(median_Male) # 61
  Mean = mean_Male
  Median = median_Male
  Gender = "M?nnlich"
  Gender_Median_Mean_Verteilung = rbind(Gender_Median_Mean_Verteilung, data.frame(Gender, Mean, Median))
  
  mean_Female <- DataFemale %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    mean() %>%
    signif(6)
  print(mean_Female) # 68,3051
  median_Female <- DataFemale %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    median() %>%
    signif(6)
  print(median_Female) #65
  Mean = mean_Female
  Median = median_Female
  Gender = "Weiblich"
  Gender_Median_Mean_Verteilung = rbind(Gender_Median_Mean_Verteilung, data.frame(Gender, Mean, Median))
  
  mean_Keine <- DataKeine %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    mean() %>%
    signif(6)
  print(mean_Keine) # 60,5385
  median_Keine <- DataKeine %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    median() %>%
    signif(6)
  print(median_Keine) # 58
  Mean = mean_Keine
  Median = median_Keine
  Gender = "Keine Antwort"
  Gender_Median_Mean_Verteilung = rbind(Gender_Median_Mean_Verteilung, data.frame(Gender, Mean, Median))
  
  mean_Andere <- DataAndere %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    mean() %>%
    signif(6)
  print(mean_Andere) # 47,25
  median_Andere <- DataAndere %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    median() %>%
    signif(6)
  print(median_Andere) # 48,5 
  Mean = mean_Andere
  Median = median_Andere
  Gender = "Andere"
  Gender_Median_Mean_Verteilung = rbind(Gender_Median_Mean_Verteilung, data.frame(Gender, Mean, Median))
  
  
  View(Gender_Median_Mean_Verteilung)
  write_xlsx(Gender_Median_Mean_Verteilung, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_BearbeitungszeitOOO_1_3_Gender_Median_Mean_Verteilung.xlsx")    
  
  
  NewOne2 = df <- data.frame(Gender_Median_Mean_Verteilung$Mean,Gender_Median_Mean_Verteilung$Median)
  #data.frame(NewOne2, stringsAsFactors = TRUE)
  names(NewOne2) <- c('Median', 'Mean')
  View(NewOne2)
  
  tnew <- t(NewOne2) # Transpose Data
  View(tnew)
  colnames(tnew) <- Gender_Median_Mean_Verteilung$Gender
  
  pal <- colorRampPalette(colors = c("red", "turquoise"))(2)
  par(mar=c(3,5,1,0)+1.4)
  par(mar=c(1,10,1,1)+1.5)
  mp <- barplot( as.matrix(tnew), #Education_Median_Mean_Verteilung$Median,
                 cex.names=.9,
                 horiz=TRUE,
                 las = 1,
                 beside=TRUE,
                 #col = "#5c00cc",
                 #col = coul,
                 #col = rainbow(2),
                 col = pal,
                 lwd = 2,
                 xlim=c(0,80),
                 main = "Median/Mean der Bearbeitungszeit (nach Gender)",
                 xlab = "Bearbeitungszeit (in sec)")
  legend("topright", c("Median","Mean"), cex=1.2, bty="n", fill=pal, xpd=FALSE)
  xval = seq(0, 80, 10)
  axis(side = 1, at = xval, labels = TRUE, xpd=T, las = 1)

  
  
  
  
  
# Plot   BearbeitungszeitOOO_1_3_Gender_Density_klein_filled_x10
  ggplot2.density(data=Data, xName='Bearbeitungszeit_1_3', fill ='Gender' , groupName='Gender',
                  legendPosition="top")


  ggplot2.density(data=Data, xName='Bearbeitungszeit_1_3', groupName='Gender',
                  legendPosition="top",
                  alpha=0.5, fillGroupDensity=TRUE )+
                  
  theme(legend.justification=c(1,0),
        legend.key.size = unit(1, "cm"),
        legend.position=c(0.98, 0.45))+
    #scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210, 220,230,240,250))
  scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140,160,180,200,220,240,260))

  
  # Density plots with mean lines
  ggplot2.density(data=kpData, xName='Bearbeitungszeit_1_3', groupName='Gender',
                  legendPosition="top",addMeanLine=TRUE) +
    scale_colour_discrete(name = "Gender (Mean-Value/Median)", 
                          labels = c(paste("M?nnlich",mean_Male,"/", median_Male), paste("Weiblich",mean_Female,"/",median_Female), 
                                     paste("Keine Antwort",mean_Keine,"/",median_Keine),paste("Andere",mean_Andere,"/",median_Andere))) +
    theme(legend.justification=c(1,0),
          legend.key.size = unit(1, "cm"),
          legend.position=c(0.98, 0.45))+
    #scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210, 220,230,240,250))
 
  scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140,160,180,200,220,240,260))
    

  write_xlsx(BearbeitungszeitOOO_1_3, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_BearbeitungszeitOOO_1_3_Mit_Gender_Education.xlsx")    
  write_xlsx(Data, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_BearbeitungszeitOOO_1_3_Mit_Gender_Only_without_NA.xlsx")
  
#_______________________________________________________________________________________

  
  
  
  
  
  
  
  
# Bearbeitungszeit nach Education  
  
  Data = NULL
  Data <- BearbeitungszeitOOO_1_3[!(is.na(BearbeitungszeitOOO_1_3$Education) | BearbeitungszeitOOO_1_3$Education ==""), ]
  
  #Data$Education <- revalue(Data$Education, c("Male"="M?nnlich"))
  #Data$Education <- revalue(Data$Education, c("Female"="Weiblich"))
  #Data$Education <- revalue(Data$Education, c("No answer"="Keine Antwort"))
  View(Data)
 
  
  DataOther = Data[Data$Education == "Other",]
  DataAnderer = Data[Data$Education == "Anderer",]
  DataKein = Data[Data$Education == "Kein Schulabschluss",]
  DataGrund = Data[Data$Education == "Grund-/Hauptschulabschluss",]
  DataReal = Data[Data$Education == "Realschule (Mittlere Reife)",]
  DataAbi = Data[Data$Education == "Gymnasium (Abitur)",]
  DataAusbildung = Data[Data$Education == "Abgeschlossene Ausbildung",]
  DataFachhochschulabschluss = Data[Data$Education == "Fachhochschulabschluss",]
  DataBachelor = Data[Data$Education == "Bachelor",]
  DataDiplom = Data[Data$Education == "Diplom",]
  DataMaster = Data[Data$Education == "Master",]
  DataPromotion = Data[Data$Education == "Promotion",]
  DataPhD = Data[Data$Education == "PhD",]
  

  
  Education_Median_Mean_Verteilung = NULL
  
  #Education_Median_Mean_Verteilung = rbind(Education_Median_Mean_Verteilung, data.frame(Bildungsabschluss, Mean, Median))
  
  
  
  View(DataKein)
  
  # Mittelwerte (Mean)  
  mean_Other <- DataOther %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    mean() %>%
    signif(6)
  print(mean_Other) # 46
  median_Other <- DataOther %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    median() %>%
    signif(6)
  print(median_Other) # 46
  Mean = mean_Other
  Median = median_Other
  Bildungsabschluss = "Other"
  Education_Median_Mean_Verteilung = rbind(Education_Median_Mean_Verteilung, data.frame(Bildungsabschluss, Mean, Median))
  
  mean_Anderer <- DataAnderer %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    mean() %>%
    signif(6)
  print(mean_Anderer) # 89,4286
  median_Anderer <- DataAnderer %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    median() %>%
    signif(6)
  print(median_Anderer) # 78
  Mean = mean_Anderer
  Median = median_Anderer
  Bildungsabschluss = "Anderer"
  Education_Median_Mean_Verteilung = rbind(Education_Median_Mean_Verteilung, data.frame(Bildungsabschluss, Mean, Median))
  
  mean_Kein <- DataKein %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    mean() %>%
    signif(6)
  print(mean_Kein) # 42,25
  median_Kein <- DataKein %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    median() %>%
    signif(6)
  print(median_Kein) # 49
  Mean = mean_Kein
  Median = median_Kein
  Bildungsabschluss = "Kein Schulabschluss"
  Education_Median_Mean_Verteilung = rbind(Education_Median_Mean_Verteilung, data.frame(Bildungsabschluss, Mean, Median))
  
  mean_Grund <- DataGrund %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    mean() %>%
    signif(6)
  print(mean_Grund) # 47,875
  median_Grund <- DataGrund %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    median() %>%
    signif(6)
  print(median_Grund) # 59
  Mean = mean_Grund
  Median = median_Grund
  Bildungsabschluss = "Grund-/Hauptschulabschluss"
  Education_Median_Mean_Verteilung = rbind(Education_Median_Mean_Verteilung, data.frame(Bildungsabschluss, Mean, Median))
  
  mean_Real <- DataReal %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    mean() %>%
    signif(6)
  print(mean_Real) # 61,5
  median_Real <- DataReal %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    median() %>%
    signif(6)
  print(median_Real) # 58,5
  Mean = mean_Real
  Median = median_Real
  Bildungsabschluss = "Realschule (Mittlere Reife)"
  Education_Median_Mean_Verteilung = rbind(Education_Median_Mean_Verteilung, data.frame(Bildungsabschluss, Mean, Median))
  
  mean_Abi <- DataAbi %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    mean() %>%
    signif(6)
  print(mean_Abi) # 55,7931
  median_Abi <- DataAbi %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    median() %>%
    signif(6)
  print(median_Abi) # 52
  Mean = mean_Abi
  Median = median_Abi
  Bildungsabschluss = "Gymnasium (Abitur)"
  Education_Median_Mean_Verteilung = rbind(Education_Median_Mean_Verteilung, data.frame(Bildungsabschluss, Mean, Median))
  
  mean_Ausbildung <- DataAusbildung %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    mean() %>%
    signif(6)
  print(mean_Ausbildung) # 68,5455
  median_Ausbildung <- DataAusbildung %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    median() %>%
    signif(6)
  print(median_Ausbildung) # 61
  Mean = mean_Ausbildung
  Median = median_Ausbildung
  Bildungsabschluss = "Abgeschlossene Ausbildung"
  Education_Median_Mean_Verteilung = rbind(Education_Median_Mean_Verteilung, data.frame(Bildungsabschluss, Mean, Median))
  
  mean_Fachhochschulabschluss <- DataFachhochschulabschluss %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    mean() %>%
    signif(6)
  print(mean_Fachhochschulabschluss) # 70,4167
  median_Fachhochschulabschluss <- DataFachhochschulabschluss %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    median() %>%
    signif(6)
  print(median_Fachhochschulabschluss) # 69,5
  Mean = mean_Fachhochschulabschluss
  Median = median_Fachhochschulabschluss
  Bildungsabschluss = "Fachhochschulabschluss"
  Education_Median_Mean_Verteilung = rbind(Education_Median_Mean_Verteilung, data.frame(Bildungsabschluss, Mean, Median))
  
  mean_Bachelor <- DataBachelor %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    mean() %>%
    signif(6)
  print(mean_Bachelor) # 67,6471
  median_Bachelor <- DataBachelor %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    median() %>%
    signif(6)
  print(median_Bachelor) # 63
  Mean = mean_Bachelor
  Median = median_Bachelor
  Bildungsabschluss = "Bachelor"
  Education_Median_Mean_Verteilung = rbind(Education_Median_Mean_Verteilung, data.frame(Bildungsabschluss, Mean, Median))
  
  mean_Diplom <- DataDiplom %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    mean() %>%
    signif(6)
  print(mean_Diplom) # 74,1667
  median_Diplom <- DataDiplom %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    median() %>%
    signif(6)
  print(median_Diplom) # 62,5
  Mean = mean_Diplom
  Median = median_Diplom
  Bildungsabschluss = "Diplom"
  Education_Median_Mean_Verteilung = rbind(Education_Median_Mean_Verteilung, data.frame(Bildungsabschluss, Mean, Median))
  
  mean_Master <- DataMaster %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    mean() %>%
    signif(6)
  print(mean_Master) # 65,5625
  median_Master <- DataMaster %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    median() %>%
    signif(6)
  print(median_Master) # 61,5
  Mean = mean_Master
  Median = median_Master
  Bildungsabschluss = "Master"
  Education_Median_Mean_Verteilung = rbind(Education_Median_Mean_Verteilung, data.frame(Bildungsabschluss, Mean, Median))
  
  mean_Promotion <- DataPromotion %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    mean() %>%
    signif(6)
  print(mean_Promotion) # 61,4884
  median_Promotion <- DataPromotion %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    median() %>%
    signif(6)
  print(median_Promotion) # 59
  Mean = mean_Promotion
  Median = median_Promotion
  Bildungsabschluss = "Promotion"
  Education_Median_Mean_Verteilung = rbind(Education_Median_Mean_Verteilung, data.frame(Bildungsabschluss, Mean, Median))
  
  mean_PhD <- DataPhD %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    mean() %>%
    signif(6)
  print(mean_PhD) # 74,6
  median_PhD <- DataPhD %>% 
    pull(Bearbeitungszeit_1_3) %>% 
    median() %>%
    signif(6)
  print(median_PhD) # 79
  Mean = mean_PhD
  Median = median_PhD
  Bildungsabschluss = "PhD"
  Education_Median_Mean_Verteilung = rbind(Education_Median_Mean_Verteilung, data.frame(Bildungsabschluss, Mean, Median))
  
  
  
  View(Education_Median_Mean_Verteilung)
  write_xlsx(Education_Median_Mean_Verteilung, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_BearbeitungszeitOOO_1_3_Education_Median_Mean_Verteilung.xlsx")    
  
  
  NewOne2 = df <- data.frame(Education_Median_Mean_Verteilung$Mean,Education_Median_Mean_Verteilung$Median)
  data.frame(NewOne2, stringsAsFactors = TRUE)
  names(NewOne2) <- c('Median', 'Mean')
  View(NewOne2)
  
  tnew <- t(NewOne2) # Transpose Data
  View(tnew)
  colnames(tnew) <- Education_Median_Mean_Verteilung$Bildungsabschluss

  
  # Test
  #barplot(as.matrix(tnew), main="My Barchart", ylab = "Numbers", 
          #cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours)
  
  pal <- colorRampPalette(colors = c("lightblue", "blue"))(2)
  par(mar=c(3,3,1,0)+1.4)
  par(mar=c(1,10,1,1)+1.5)
  mp <- barplot( as.matrix(tnew), #Education_Median_Mean_Verteilung$Median,
                cex.names=.9,
                horiz=TRUE,
                las = 1,
                beside=TRUE,
                #col = "#5c00cc",
                #col = coul,
                #col = rainbow(2),
                col = pal,
                lwd = 3,
                xlim=c(0,100),
                main = "Median/Mean der Bearbeitungszeit (nach Bildungsabschluss)",
                xlab = "Anzahl")
  legend("right", c("Median","Mean"), cex=1.2, bty="n", fill=pal, xpd=FALSE)
  
  #text(mp, par("usr")[3], labels =  Education_Median_Mean_Verteilung$Bildungsabschluss, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
  #text(Education_Median_Mean_Verteilung$Median+3, mp , paste(Education_Median_Mean_Verteilung$Median, sep=""), col = "red" ,cex=0.9) 

  
  
  DataOther = Data[Data$Education == "Other",]
  DataAnderer = Data[Data$Education == "Anderer",]
  DataKein = Data[Data$Education == "Kein Schulabschluss",]
  DataGrund = Data[Data$Education == "Grund-/Hauptschulabschluss",]
  DataReal = Data[Data$Education == "Realschule (Mittlere Reife)",]
  DataAbi = Data[Data$Education == "Gymnasium (Abitur)",]
  DataAusbildung = Data[Data$Education == "Abgeschlossene Ausbildung",]
  DataFachhochschulabschluss = Data[Data$Education == "Fachhochschulabschluss",]
  DataBachelor = Data[Data$Education == "Bachelor",]
  DataDiplom = Data[Data$Education == "Diplom",]
  DataMaster = Data[Data$Education == "Master",]
  DataPromotion = Data[Data$Education == "Promotion",]
  DataPhD = Data[Data$Education == "PhD",]

  #dataZw = Data[Data$Education == "Bachelor" |Data$Education == "Diplom" | 
  #                Data$Education == "Kein Schulabschluss"
  #              | Data$Education == "Abgeschlossene Ausbildung",]
  
  View(DataAnderer)
  # Other hat weniger als 2 Datenpunkte und wird nicht eingezeichnet
  dataZw0 = Data[Data$Education == "Anderer" | 
                  Data$Education == "Kein Schulabschluss",]
  
  dataZw1 = Data[Data$Education == "Grund-/Hauptschulabschluss" | 
                  Data$Education == "Realschule (Mittlere Reife)" |
                   Data$Education == "Gymnasium (Abitur)",]
  dataZw2 = Data[Data$Education == "Abgeschlossene Ausbildung" | 
                   Data$Education == "Fachhochschulabschluss" |
                   Data$Education == "Bachelor",]
  dataZw3 = Data[Data$Education == "Diplom" |Data$Education == "Master" | 
                  Data$Education == "Promotion"
                | Data$Education == "PhD",]
  
  
  
  # Plot   BearbeitungszeitOOO_1_3_Gender_Density_klein_filled_x10
  ggplot2.density(data=Data, xName='Bearbeitungszeit_1_3', fill ='Education' , groupName='Education',
                  legendPosition="top")
  
  
  ggplot2.density(data=dataZw0, xName='Bearbeitungszeit_1_3', groupName='Education',
                  legendPosition="top",
                  alpha=0.5, fillGroupDensity=TRUE )+
    #xlim(0,200)+
    theme(legend.justification=c(1,0),
          legend.key.size = unit(1, "cm"),
          legend.position=c(0.98, 0.45))+
    #scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210, 220,230,240,250))
    scale_x_continuous(limits =c(0,240), breaks = c(0,20,40,60,80,100,120,140,160,180,200,220,240,260))
  
  
  # Density plots with mean lines
  ggplot2.density(data=dataZw0, xName='Bearbeitungszeit_1_3', groupName='Education',
                  legendPosition="topright",addMeanLine=TRUE) +
    scale_colour_discrete(name = "Education (Mean-Value/Median)", 
                          labels = c(paste("Anderer",mean_Anderer,"/", median_Anderer), 
                                     #paste("Master",mean_Master,"/",median_Master), 
                                     #paste("Promotion",mean_Promotion,"/",median_Promotion),
                                     paste("Kein Schulabschluss",mean_Kein,"/",median_Kein))) +
    theme(legend.justification=c(1,0),
          legend.key.size = unit(1, "cm"),
          legend.position=c(0.98, 0.65))+
    #scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210, 220,230,240,250))
    
    scale_x_continuous(limits =c(0,240), breaks = c(0,20,40,60,80,100,120,140,160,180,200,220,240,260))
  
  
  write_xlsx(BearbeitungszeitOOO_1_3, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_BearbeitungszeitOOO_1_3_Mit_Gender_Education.xlsx") 
  write_xlsx(Data, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_BearbeitungszeitOOO_1_3_Mit_Education_Only_without_NA.xlsx")    
  
  #BearbeitungszeitOOO_1_3_Education3_Density_filled_x20
  #BearbeitungszeitOOO_1_3_Education3_Density_x20_pdf
  
  
  
  
  
  
  
  #_______________________________________________________________________________________
  # Relevanzauswertung (nur die alle 3 Fragenbeantwortet haben)
  #
  
  
  prioriseriungs_Daten_mit_Zeiten <- read_excel("C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Mit_Time_Intervall.xlsx")
  View(prioriseriungs_Daten_mit_Zeiten) #2068
  
  View(BearbeitungszeitOOO_1_3)

  Beantwortet_1_3 <- subset(prioriseriungs_Daten_mit_Zeiten, Respondent_ID %in% BearbeitungszeitOOO_1_3$Respondent_ID)
  View(Beantwortet_1_3) # 1815, 605
  

  Beantwortet_1_3_1 = Beantwortet_1_3[Beantwortet_1_3$Question_No == 1,]
  View(Beantwortet_1_3_1) #605
  
  Beantwortet_1_3_2 = Beantwortet_1_3[Beantwortet_1_3$Question_No == 2,]
  View(Beantwortet_1_3_2) #605
  
  Beantwortet_1_3_3 = Beantwortet_1_3[Beantwortet_1_3$Question_No == 3,]
  View(Beantwortet_1_3_3) #605


  #dat = Beantwortet_1_3_1$PubIDs_Order_Chosen[1]
  #print(dat)
  
  Priorisierungsverteilung_1_3_1 = NULL
  
# 1,2 Numerisch  
  counter_1_2 = 0
  counter_2_1 = 0
  laenge = length(Beantwortet_1_3_1$Respondent_ID)
  index <- 1
  while (index < laenge + 1) {
    dat = Beantwortet_1_3_1$PubIDs_Order_Chosen[index]
   if(dat == "1.2"){
     print(dat)
     counter_1_2 = counter_1_2 + 1
   }
   if(dat == "2.1"){
     print(dat)
     counter_2_1 = counter_2_1 + 1
   }
    index = index + 1
  }
  print(counter_1_2)
  print(counter_2_1)
  
  Reihenfolge = "1.2"
  Anzahl = counter_1_2
  Priorisierungsverteilung_1_3_1 = rbind(Priorisierungsverteilung_1_3_1, data.frame(Reihenfolge, Anzahl))
  Reihenfolge = "2.1"
  Anzahl = counter_2_1
  Priorisierungsverteilung_1_3_1 = rbind(Priorisierungsverteilung_1_3_1, data.frame(Reihenfolge, Anzahl))

  View(Priorisierungsverteilung_1_3_1)
  write_xlsx(Priorisierungsverteilung_1_3_1, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungsverteilung_1_3_1.xlsx")    
  
  pal <- colorRampPalette(colors = c("lightgreen", "lightgreen"))(6)
  pal <- colorRampPalette(colors = c("lightblue", "blue"))(6)
  par(mar=c(3,3,1,0)+1.4)
  prioplot <- barplot( Priorisierungsverteilung_1_3_1$Anzahl, #Education_Median_Mean_Verteilung$Median,
                       names.arg = Priorisierungsverteilung_1_3_1$Reihenfolge,
                       cex.names=.9,
                       horiz=FALSE,
                       las = 1,
                       beside=FALSE,
                       #col = "#5c00cc",
                       #col = coul,
                       #col = rainbow(2),
                       col = pal,
                       lwd = 3,
                       ylim=c(0,600),
                       main = "Priorisierungsverteilung_1_3 (Numerisch (1,2)",
                       xlab = "Priorisierung",
                       ylab = "Anzahl")
  text(prioplot, Priorisierungsverteilung_1_3_1$Anzahl+14, paste(Priorisierungsverteilung_1_3_1$Anzahl, sep=""), col = "red" ,cex=1.0) 
  #Priorisierungsverteilung_1_3_1_Lightgreen_mit_Number_klein
  #Priorisierungsverteilung_1_3_1_blue_Lightblue_mit_Number_klein
  
  
  
# 1,2,3 Numerisch
  counter_1_2_3 = 0
  counter_1_3_2 = 0
  counter_2_1_3 = 0
  counter_2_3_1 = 0
  counter_3_1_2 = 0
  counter_3_2_1 = 0
  laenge = length(Beantwortet_1_3_2$Respondent_ID)
  index <- 1
  while (index < laenge + 1) {
    dat = Beantwortet_1_3_2$PubIDs_Order_Chosen[index]
    #print(dat)
    
    if(dat == "1,2,3"){ print(dat)
      counter_1_2_3 = counter_1_2_3 + 1
    }
    if(dat == "1,3,2"){ print(dat)
      counter_1_3_2 = counter_1_3_2 + 1
    }
    if(dat == "2,1,3"){ print(dat)
      counter_2_1_3 = counter_2_1_3 + 1
    }
    if(dat == "2,3,1"){ print(dat)
      counter_2_3_1 = counter_2_3_1 + 1
    }
    if(dat == "3,1,2"){ print(dat)
      counter_3_1_2 = counter_3_1_2 + 1
    }
    if(dat == "3,2,1"){ print(dat)
      counter_3_2_1 = counter_3_2_1 + 1
    }
  
    index = index + 1
  }
  print(counter_1_2_3)
  print(counter_1_3_2)
  print(counter_2_1_3)
  print(counter_2_3_1)
  print(counter_3_1_2)
  print(counter_3_2_1)
  
  Priorisierungsverteilung_1_3_2 = NULL
  
  Reihenfolge = "1,2,3"
  Anzahl = counter_1_2_3
  Priorisierungsverteilung_1_3_2 = rbind(Priorisierungsverteilung_1_3_2, data.frame(Reihenfolge, Anzahl))
  Reihenfolge = "1,3,2"
  Anzahl = counter_1_3_2
  Priorisierungsverteilung_1_3_2 = rbind(Priorisierungsverteilung_1_3_2, data.frame(Reihenfolge, Anzahl))
  
  Reihenfolge = "2,1,3"
  Anzahl = counter_2_1_3
  Priorisierungsverteilung_1_3_2 = rbind(Priorisierungsverteilung_1_3_2, data.frame(Reihenfolge, Anzahl))
  Reihenfolge = "2,3,1"
  Anzahl = counter_2_3_1
  Priorisierungsverteilung_1_3_2 = rbind(Priorisierungsverteilung_1_3_2, data.frame(Reihenfolge, Anzahl))
  
  Reihenfolge = "3,1,2"
  Anzahl = counter_3_1_2
  Priorisierungsverteilung_1_3_2 = rbind(Priorisierungsverteilung_1_3_2, data.frame(Reihenfolge, Anzahl))
  Reihenfolge = "3,2,1"
  Anzahl = counter_3_2_1
  Priorisierungsverteilung_1_3_2 = rbind(Priorisierungsverteilung_1_3_2, data.frame(Reihenfolge, Anzahl))
  
  View(Priorisierungsverteilung_1_3_2)
  write_xlsx(Priorisierungsverteilung_1_3_2, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungsverteilung_1_3_2.xlsx")    

  pal <- colorRampPalette(colors = c("lightgreen", "lightgreen"))(6)
  pal <- colorRampPalette(colors = c("lightblue", "blue"))(6)
  par(mar=c(3,3,3,0)+1.4)
  prioplot <- barplot( Priorisierungsverteilung_1_3_2$Anzahl, #Education_Median_Mean_Verteilung$Median,
                       names.arg = Priorisierungsverteilung_1_3_2$Reihenfolge,
                       cex.names=.9,
                       horiz=FALSE,
                       las = 1,
                       beside=FALSE,
                       #col = "#5c00cc",
                       #col = coul,
                       #col = rainbow(2),
                       col = pal,
                       lwd = 3,
                       ylim=c(0,360),
                       main = "Priorisierungsverteilung_1_3 (Numerisch)",
                       xlab = "Priorisierung",
                       ylab = "Anzahl")
  text(prioplot, Priorisierungsverteilung_1_3_2$Anzahl+8, paste(Priorisierungsverteilung_1_3_2$Anzahl, sep=""), col = "red" ,cex=1.0) 
  
  
  
  
  
  
  # 1,2,3 Altmetric
  counter_1_2_3 = 0
  counter_1_3_2 = 0
  counter_2_1_3 = 0
  counter_2_3_1 = 0
  counter_3_1_2 = 0
  counter_3_2_1 = 0
  laenge = length(Beantwortet_1_3_3$Respondent_ID)
  index <- 1
  while (index < laenge + 1) {
    dat = Beantwortet_1_3_3$PubIDs_Order_Chosen[index]
    #print(dat)
    
    if(dat == "1,2,3"){ print(dat)
      counter_1_2_3 = counter_1_2_3 + 1
    }
    if(dat == "1,3,2"){ print(dat)
      counter_1_3_2 = counter_1_3_2 + 1
    }
    if(dat == "2,1,3"){ print(dat)
      counter_2_1_3 = counter_2_1_3 + 1
    }
    if(dat == "2,3,1"){ print(dat)
      counter_2_3_1 = counter_2_3_1 + 1
    }
    if(dat == "3,1,2"){ print(dat)
      counter_3_1_2 = counter_3_1_2 + 1
    }
    if(dat == "3,2,1"){ print(dat)
      counter_3_2_1 = counter_3_2_1 + 1
    }
    
    index = index + 1
  }
  print(counter_1_2_3)
  print(counter_1_3_2)
  print(counter_2_1_3)
  print(counter_2_3_1)
  print(counter_3_1_2)
  print(counter_3_2_1)
  
  Priorisierungsverteilung_1_3_3 = NULL
  
  Reihenfolge = "1,2,3"
  Anzahl = counter_1_2_3
  Priorisierungsverteilung_1_3_3 = rbind(Priorisierungsverteilung_1_3_3, data.frame(Reihenfolge, Anzahl))
  Reihenfolge = "1,3,2"
  Anzahl = counter_1_3_2
  Priorisierungsverteilung_1_3_3 = rbind(Priorisierungsverteilung_1_3_3, data.frame(Reihenfolge, Anzahl))
  
  Reihenfolge = "2,1,3"
  Anzahl = counter_2_1_3
  Priorisierungsverteilung_1_3_3 = rbind(Priorisierungsverteilung_1_3_3, data.frame(Reihenfolge, Anzahl))
  Reihenfolge = "2,3,1"
  Anzahl = counter_2_3_1
  Priorisierungsverteilung_1_3_3 = rbind(Priorisierungsverteilung_1_3_3, data.frame(Reihenfolge, Anzahl))
  
  Reihenfolge = "3,1,2"
  Anzahl = counter_3_1_2
  Priorisierungsverteilung_1_3_3 = rbind(Priorisierungsverteilung_1_3_3, data.frame(Reihenfolge, Anzahl))
  Reihenfolge = "3,2,1"
  Anzahl = counter_3_2_1
  Priorisierungsverteilung_1_3_3 = rbind(Priorisierungsverteilung_1_3_3, data.frame(Reihenfolge, Anzahl))
  
  View(Priorisierungsverteilung_1_3_3)
  write_xlsx(Priorisierungsverteilung_1_3_3, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungsverteilung_1_3_3.xlsx")    
  
  
  pal <- colorRampPalette(colors = c("lightgreen", "lightgreen"))(6)
  par(mar=c(3,3,1,0)+1.4)
  prioplot <- barplot( Priorisierungsverteilung_1_3_3$Anzahl, #Education_Median_Mean_Verteilung$Median,
                 names.arg = Priorisierungsverteilung_1_3_3$Reihenfolge,
                 cex.names=.9,
                 horiz=FALSE,
                 las = 1,
                 beside=FALSE,
                 #col = "#5c00cc",
                 #col = coul,
                 #col = rainbow(2),
                 col = pal,
                 lwd = 3,
                 ylim=c(0,350),
                 main = "Priorisierungsverteilung_1_3 (Altmetric)",
                 xlab = "Priorisierung",
                 ylab = "Anzahl")
  text(prioplot, Priorisierungsverteilung_1_3_3$Anzahl+10, paste(Priorisierungsverteilung_1_3_3$Anzahl, sep=""), col = "red" ,cex=1.0) 
  #Priorisierungsverteilung_1_3_3_Lightgreen_mit_Number_klein
  
  
  
# Barplot Priorisierung Numerisch vs Altmetric  
  Priorisierungsverteilung_1_3_2_3  <- data.frame(Priorisierungsverteilung_1_3_2$Anzahl,Priorisierungsverteilung_1_3_3$Anzahl)
  data.frame(NewOne2, stringsAsFactors = TRUE)
  names(Priorisierungsverteilung_1_3_2_3) <- c('Numerisch', 'Altmetric')
  View(Priorisierungsverteilung_1_3_2_3)
  
  tPriorisierungsverteilung_1_3_2_3 <- t(Priorisierungsverteilung_1_3_2_3) # Transpose Data
  View(tPriorisierungsverteilung_1_3_2_3)
  colnames(tPriorisierungsverteilung_1_3_2_3) <- Priorisierungsverteilung_1_3_2$Reihenfolge
  
  
  
  pal <- colorRampPalette(colors = c("turquoise", "red"))(2)
  par(mar=c(3,3,1,0)+1.4)
  prioplot <- barplot( as.matrix(tPriorisierungsverteilung_1_3_2_3), #Education_Median_Mean_Verteilung$Median,
                       #names.arg = Priorisierungsverteilung_1_3_3$Reihenfolge,
                       cex.names=.9,
                       horiz=FALSE,
                       las = 1,
                       beside=TRUE,
                       #col = "#5c00cc",
                       #col = coul,
                       #col = rainbow(2),
                       col = pal,
                       lwd = 3,
                       ylim=c(0,350),
                       main = "Priorisierungsverteilung_1_3 (Numerisch/Altmetric)",
                       xlab = "Priorisierung",
                       ylab = "Anzahl")
  legend("topright", c("Numerisch","Altmetric"), cex=1.2, bty="n", fill=pal, xpd=FALSE)
  #Priorisierungsverteilung_1_3_2_3_blue_red_klein
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #_______________________________________________________________________________________
  # Relevanzauswertung (alle die irgendwie  Fragen beantwortet haben)
  #
  
  
  prioriseriungs_Daten_mit_Zeiten <- read_excel("C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Mit_Time_Intervall.xlsx")
  View(prioriseriungs_Daten_mit_Zeiten) #2068
  
  #Beantwortet <- subset(prioriseriungs_Daten_mit_Zeiten, Respondent_ID %in% BearbeitungszeitOOO_1_3$Respondent_ID)
  #View(Beantwortet) # 1815, 605
  Beantwortet = prioriseriungs_Daten_mit_Zeiten
  
  Beantwortet_1 = Beantwortet[Beantwortet$Question_No == 1,]
  View(Beantwortet_1) # 760
  
  Beantwortet_2 = Beantwortet[Beantwortet$Question_No == 2,]
  View(Beantwortet_2) # 701
  
  Beantwortet_3 = Beantwortet[Beantwortet$Question_No == 3,]
  View(Beantwortet_3) # 607
  
  
  Priorisierungsverteilung_1 = NULL
  
  # 1,2 Numerisch  
  counter_1_2 = 0
  counter_2_1 = 0
  laenge = length(Beantwortet_1$Respondent_ID)
  index <- 1
  while (index < laenge + 1) {
    dat = Beantwortet_1$PubIDs_Order_Chosen[index]
    if(dat == "1.2"){
      print(dat)
      counter_1_2 = counter_1_2 + 1
    }
    if(dat == "2.1"){
      print(dat)
      counter_2_1 = counter_2_1 + 1
    }
    index = index + 1
  }
  print(counter_1_2) #630
  print(counter_2_1) #130
  
  Reihenfolge = "1.2"
  Anzahl = counter_1_2
  Priorisierungsverteilung_1 = rbind(Priorisierungsverteilung_1, data.frame(Reihenfolge, Anzahl))
  Reihenfolge = "2.1"
  Anzahl = counter_2_1
  Priorisierungsverteilung_1 = rbind(Priorisierungsverteilung_1, data.frame(Reihenfolge, Anzahl))
  
  View(Priorisierungsverteilung_1)
  write_xlsx(Priorisierungsverteilung_1, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungsverteilung_1.xlsx")    
  
  
  pal <- colorRampPalette(colors = c("lightgreen", "lightgreen"))(6)
  pal <- colorRampPalette(colors = c("lightblue", "blue"))(6)
  par(mar=c(3,3,1,0)+1.4)
  prioplot <- barplot( Priorisierungsverteilung_1$Anzahl, 
                       names.arg = Priorisierungsverteilung_1$Reihenfolge,
                       cex.names=.9,
                       horiz=FALSE,
                       las = 1,
                       beside=FALSE,
                       #col = "#5c00cc",
                       #col = coul,
                       #col = rainbow(2),
                       col = pal,
                       lwd = 3,
                       ylim=c(0,750),
                       main = "Priorisierungsverteilung (Numerisch (1,2))",
                       xlab = "Priorisierung",
                       ylab = "Anzahl")
  text(prioplot, Priorisierungsverteilung_1$Anzahl+14, paste(Priorisierungsverteilung_1$Anzahl, sep=""), col = "red" ,cex=1.0) 
  #Priorisierungsverteilung_1_Lightgreen_mit_Number_klein
  #Priorisierungsverteilung_1_blue_Lightblue_mit_Number_klein
  
  
  
  # 1,2,3 Numerisch
  counter_1_2_3 = 0
  counter_1_3_2 = 0
  counter_2_1_3 = 0
  counter_2_3_1 = 0
  counter_3_1_2 = 0
  counter_3_2_1 = 0
  laenge = length(Beantwortet_2$Respondent_ID)
  index <- 1
  while (index < laenge + 1) {
    dat = Beantwortet_2$PubIDs_Order_Chosen[index]
    #print(dat)
    
    if(dat == "1,2,3"){ print(dat)
      counter_1_2_3 = counter_1_2_3 + 1
    }
    if(dat == "1,3,2"){ print(dat)
      counter_1_3_2 = counter_1_3_2 + 1
    }
    if(dat == "2,1,3"){ print(dat)
      counter_2_1_3 = counter_2_1_3 + 1
    }
    if(dat == "2,3,1"){ print(dat)
      counter_2_3_1 = counter_2_3_1 + 1
    }
    if(dat == "3,1,2"){ print(dat)
      counter_3_1_2 = counter_3_1_2 + 1
    }
    if(dat == "3,2,1"){ print(dat)
      counter_3_2_1 = counter_3_2_1 + 1
    }
    
    index = index + 1
  }
  print(counter_1_2_3)
  print(counter_1_3_2)
  print(counter_2_1_3)
  print(counter_2_3_1)
  print(counter_3_1_2)
  print(counter_3_2_1)
  
  Priorisierungsverteilung_2 = NULL
  
  Reihenfolge = "1,2,3"
  Anzahl = counter_1_2_3
  Priorisierungsverteilung_2 = rbind(Priorisierungsverteilung_2, data.frame(Reihenfolge, Anzahl))
  Reihenfolge = "1,3,2"
  Anzahl = counter_1_3_2
  Priorisierungsverteilung_2 = rbind(Priorisierungsverteilung_2, data.frame(Reihenfolge, Anzahl))
  
  Reihenfolge = "2,1,3"
  Anzahl = counter_2_1_3
  Priorisierungsverteilung_2 = rbind(Priorisierungsverteilung_2, data.frame(Reihenfolge, Anzahl))
  Reihenfolge = "2,3,1"
  Anzahl = counter_2_3_1
  Priorisierungsverteilung_2 = rbind(Priorisierungsverteilung_2, data.frame(Reihenfolge, Anzahl))
  
  Reihenfolge = "3,1,2"
  Anzahl = counter_3_1_2
  Priorisierungsverteilung_2 = rbind(Priorisierungsverteilung_2, data.frame(Reihenfolge, Anzahl))
  Reihenfolge = "3,2,1"
  Anzahl = counter_3_2_1
  Priorisierungsverteilung_2 = rbind(Priorisierungsverteilung_2, data.frame(Reihenfolge, Anzahl))
  
  View(Priorisierungsverteilung_2)
  write_xlsx(Priorisierungsverteilung_2, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungsverteilung_2.xlsx")    
  
  pal <- colorRampPalette(colors = c("lightgreen", "lightgreen"))(6)
  pal <- colorRampPalette(colors = c("lightblue", "blue"))(6)
  par(mar=c(3,3,3,0)+1.4)
  prioplot <- barplot( Priorisierungsverteilung_2$Anzahl, #Education_Median_Mean_Verteilung$Median,
                       names.arg = Priorisierungsverteilung_2$Reihenfolge,
                       cex.names=.9,
                       horiz=FALSE,
                       las = 1,
                       beside=FALSE,
                       #col = "#5c00cc",
                       #col = coul,
                       #col = rainbow(2),
                       col = pal,
                       lwd = 3,
                       ylim=c(0,450),
                       main = "Priorisierungsverteilung (Numerisch)",
                       xlab = "Priorisierung",
                       ylab = "Anzahl")
  text(prioplot, Priorisierungsverteilung_2$Anzahl+12, paste(Priorisierungsverteilung_2$Anzahl, sep=""), col = "red" ,cex=1.0) 
  #Priorisierungsverteilung_2_blue_Lightblue_mit_Number_klein
  #Priorisierungsverteilung_2_Lightgreen_mit_Number_klein
  
  
  
  
  
  # 1,2,3 Altmetric
  counter_1_2_3 = 0
  counter_1_3_2 = 0
  counter_2_1_3 = 0
  counter_2_3_1 = 0
  counter_3_1_2 = 0
  counter_3_2_1 = 0
  laenge = length(Beantwortet_3$Respondent_ID)
  index <- 1
  while (index < laenge + 1) {
    dat = Beantwortet_3$PubIDs_Order_Chosen[index]
    #print(dat)
    
    if(dat == "1,2,3"){ print(dat)
      counter_1_2_3 = counter_1_2_3 + 1
    }
    if(dat == "1,3,2"){ print(dat)
      counter_1_3_2 = counter_1_3_2 + 1
    }
    if(dat == "2,1,3"){ print(dat)
      counter_2_1_3 = counter_2_1_3 + 1
    }
    if(dat == "2,3,1"){ print(dat)
      counter_2_3_1 = counter_2_3_1 + 1
    }
    if(dat == "3,1,2"){ print(dat)
      counter_3_1_2 = counter_3_1_2 + 1
    }
    if(dat == "3,2,1"){ print(dat)
      counter_3_2_1 = counter_3_2_1 + 1
    }
    
    index = index + 1
  }
  print(counter_1_2_3)
  print(counter_1_3_2)
  print(counter_2_1_3)
  print(counter_2_3_1)
  print(counter_3_1_2)
  print(counter_3_2_1)
  
  Priorisierungsverteilung_3 = NULL
  
  Reihenfolge = "1,2,3"
  Anzahl = counter_1_2_3
  Priorisierungsverteilung_3 = rbind(Priorisierungsverteilung_3, data.frame(Reihenfolge, Anzahl))
  Reihenfolge = "1,3,2"
  Anzahl = counter_1_3_2
  Priorisierungsverteilung_3 = rbind(Priorisierungsverteilung_3, data.frame(Reihenfolge, Anzahl))
  
  Reihenfolge = "2,1,3"
  Anzahl = counter_2_1_3
  Priorisierungsverteilung_3 = rbind(Priorisierungsverteilung_3, data.frame(Reihenfolge, Anzahl))
  Reihenfolge = "2,3,1"
  Anzahl = counter_2_3_1
  Priorisierungsverteilung_3 = rbind(Priorisierungsverteilung_3, data.frame(Reihenfolge, Anzahl))
  
  Reihenfolge = "3,1,2"
  Anzahl = counter_3_1_2
  Priorisierungsverteilung_3 = rbind(Priorisierungsverteilung_3, data.frame(Reihenfolge, Anzahl))
  Reihenfolge = "3,2,1"
  Anzahl = counter_3_2_1
  Priorisierungsverteilung_3 = rbind(Priorisierungsverteilung_3, data.frame(Reihenfolge, Anzahl))
  
  View(Priorisierungsverteilung_3)
  write_xlsx(Priorisierungsverteilung_3, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungsverteilung_3.xlsx")    
  
  
  pal <- colorRampPalette(colors = c("lightgreen", "lightgreen"))(6)
  pal <- colorRampPalette(colors = c("lightblue", "blue"))(6)
  par(mar=c(3,3,1,0)+1.4)
  prioplot <- barplot( Priorisierungsverteilung_3$Anzahl,
                       names.arg = Priorisierungsverteilung_3$Reihenfolge,
                       cex.names=.9,
                       horiz=FALSE,
                       las = 1,
                       beside=FALSE,
                       #col = "#5c00cc",
                       #col = coul,
                       #col = rainbow(2),
                       col = pal,
                       lwd = 3,
                       ylim=c(0,380),
                       main = "Priorisierungsverteilung (Altmetric)",
                       xlab = "Priorisierung",
                       ylab = "Anzahl")
  text(prioplot, Priorisierungsverteilung_3$Anzahl+10, paste(Priorisierungsverteilung_3$Anzahl, sep=""), col = "red" ,cex=1.0) 
  #Priorisierungsverteilung_3_blau_Lightblau_mit_Number_klein
  #Priorisierungsverteilung_1_3_3_Lightgreen_mit_Number_klein
  
  
  
# Barplot Priorisierung Numerisch vs Altmetric (s.o. macht hier nur mit den Personen,
# die alle 3 Fragen Beantwortet haben Sinn) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#___________________________________________________________________________________________
  
# Priorisierungsgruppierung | 1,2 | 1,2,3 | 1,2,3 |
  
  prioriseriungs_Daten_mit_Zeiten <- read_excel("C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Mit_Time_Intervall.xlsx")
  View(prioriseriungs_Daten_mit_Zeiten) #2068

  Priorisierungsverteilung = NULL
 
  maxRespondend_Id = max(prioriseriungs_Daten_mit_Zeiten$Respondent_ID)
  print(maxRespondend_Id)
  minRespondend_Id = min(prioriseriungs_Daten_mit_Zeiten$Respondent_ID)
  print(minRespondend_Id)
  
  i <- minRespondend_Id
  while (i < maxRespondend_Id + 1){
    zeilenZw = prioriseriungs_Daten_mit_Zeiten[prioriseriungs_Daten_mit_Zeiten$Respondent_ID == i,]
    len = length(zeilenZw$Respondent_ID)
    if(len >2){
      #spa = zeilenZw$`Timestamp (DD-MM-YY)
      order_Chosen = zeilenZw$PubIDs_Order_Chosen
      k <- 1
      gesamt = NULL
      gesamt = paste0(gesamt, "| ")
      while (k < len + 1) {
        gesamt = paste0(gesamt,order_Chosen[k], " | ")
        k = k + 1
      }
      print(gesamt)
      Priorisierung = gesamt
      Respondent_ID = i
      Priorisierungsverteilung = rbind(Priorisierungsverteilung, data.frame(Respondent_ID, Priorisierung))
    }
    i = i + 1 
  }  
  View(Priorisierungsverteilung)
  
  Prio21_231_231 = unique(Priorisierungsverteilung$Priorisierung)
  laengePrio21_231_231 = length(Prio21_231_231)
  print(laengePrio21_231_231)
  PrioSubTable = NULL
  j <- 1
  while(j < laengePrio21_231_231 +1){
    PrioSub <- subset(Priorisierungsverteilung, Priorisierung %in% Prio21_231_231[j])
    Anzahl = length(PrioSub$Respondent_ID)
    Reihenfolge = Prio21_231_231[j]
    PrioSubTable = rbind(PrioSubTable, data.frame(Reihenfolge, Anzahl))
    j = j + 1
  }
  View(PrioSubTable)
  print(sum(PrioSubTable$Anzahl))
  
  PrioSubTableOver5 = PrioSubTable[PrioSubTable$Anzahl >= 5, ]
  View(PrioSubTableOver5)
  
  PrioSubTable = PrioSubTable[order(PrioSubTable$Anzahl),]
  
  #write_xlsx(PrioSubTableOver5, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungs_Reihenfolge_Anzahl_ueber_5_1_3.xlsx")
  #write_xlsx(PrioSubTableOver5, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungs_Reihenfolge_Anzahl_ueber_5_1_3_schoener.xlsx")
  #write_xlsx(PrioSubTable, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungs_Reihenfolge_alle_1_3_schoener.xlsx")    
  #write_xlsx(PrioSubTableOver5, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungs_Reihenfolge_Anzahl_ueber_5_1_3_schoener_ordered.xlsx")
  #write_xlsx(PrioSubTable, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungs_Reihenfolge_alle_1_3_schoener_ordered.xlsx")    
  
  PrioSubTableOver5 = PrioSubTableOver5[order(PrioSubTableOver5$Anzahl),]
  
  pal <- colorRampPalette(colors = c("lightgreen", "lightgreen"))(14)
  pal <- colorRampPalette(colors = c("lightblue", "blue"))(14)
  par(mar=c(3,6,1,0)+1.4)
  par(mgp=c(2,0.5,0))
  prioplot <- barplot( PrioSubTableOver5$Anzahl,
                       names.arg = PrioSubTableOver5$Reihenfolge,
                       cex.names=.9, horiz=TRUE, las = 1, beside=FALSE,
                       #col = "#5c00cc",
                       #col = coul,
                       col = pal,lwd = 3,xlim=c(0,300),
                       main = "Priorisierungen/Gew?hlte Reihenfolgen (absolut)",xlab = "Anzahl",)
  text(PrioSubTableOver5$Anzahl+8,prioplot,  paste(PrioSubTableOver5$Anzahl, sep=""), col = "red" ,cex=1.0) 
  #Priorisierungsverteilung_alle_Lightgreen_mit_Number_klein_less_margin
  #Priorisierungsverteilung_alle_blau_Lightblau_mit_Number_klein
  
  

# Nur die 2,3 Frage
  Priorisierungsverteilung = NULL
  
  maxRespondend_Id = max(prioriseriungs_Daten_mit_Zeiten$Respondent_ID)
  print(maxRespondend_Id)
  minRespondend_Id = min(prioriseriungs_Daten_mit_Zeiten$Respondent_ID)
  print(minRespondend_Id)
  
  i <- minRespondend_Id
  while (i < maxRespondend_Id + 1){
    zeilenZw = prioriseriungs_Daten_mit_Zeiten[prioriseriungs_Daten_mit_Zeiten$Respondent_ID == i &
                                                 (prioriseriungs_Daten_mit_Zeiten$Question_No == 2 |
                                                 prioriseriungs_Daten_mit_Zeiten$Question_No == 3),]
    len = length(zeilenZw$Respondent_ID)
    if(len >1){
      #spa = zeilenZw$`Timestamp (DD-MM-YY)
      order_Chosen = zeilenZw$PubIDs_Order_Chosen
      k <- 1
      gesamt = NULL
      gesamt = paste0(gesamt, "| ")
      while (k < len + 1) {
        gesamt = paste0(gesamt,order_Chosen[k], " | ")
        k = k + 1
      }
      print(gesamt)
      Priorisierung = gesamt
      Respondent_ID = i
      Priorisierungsverteilung = rbind(Priorisierungsverteilung, data.frame(Respondent_ID, Priorisierung))
    }
    i = i + 1 
  }  
  
  Prio21_231_231 = unique(Priorisierungsverteilung$Priorisierung)
  laengePrio21_231_231 = length(Prio21_231_231)
  print(laengePrio21_231_231)
  PrioSubTable = NULL
  View(Priorisierungsverteilung)
  j <- 1
  while(j < laengePrio21_231_231 +1){
    PrioSub <- subset(Priorisierungsverteilung, Priorisierung %in% Prio21_231_231[j])
    Anzahl = length(PrioSub$Respondent_ID)
    Reihenfolge = Prio21_231_231[j]
    PrioSubTable = rbind(PrioSubTable, data.frame(Reihenfolge, Anzahl))
    j = j + 1
  }
  View(PrioSubTable)
  print(sum(PrioSubTable$Anzahl))
  
  PrioSubTableOver5 = PrioSubTable[PrioSubTable$Anzahl >= 5, ]
  View(PrioSubTableOver5)
  PrioSubTable = PrioSubTable[order(PrioSubTable$Anzahl),]
  PrioSubTableOver5 = PrioSubTableOver5[order(PrioSubTableOver5$Anzahl),]
  
  write_xlsx(PrioSubTableOver5, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungs_Reihenfolge_Anzahl_ueber_5_2_3_schoener_ordered.xlsx")
  write_xlsx(PrioSubTable, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungs_Reihenfolge_alle_2_3_schoener_ordered.xlsx")    
  
  
  pal <- colorRampPalette(colors = c("lightgreen", "lightgreen"))(14)
  pal <- colorRampPalette(colors = c("lightblue", "blue"))(14)
  par(mar=c(3,5,1,0)+1.0)
  par(mgp=c(2,0.5,0))
  prioplot <- barplot( PrioSubTableOver5$Anzahl,
                       names.arg = PrioSubTableOver5$Reihenfolge,
                       cex.names=.9, horiz=TRUE, las = 1, beside=FALSE,
                       #col = "#5c00cc",
                       #col = coul,
                       col = pal,lwd = 3,xlim=c(0,300),
                       main = "Priorisierungen/Gew?hlte Reihenfolgen (2. & 3. Frage)",xlab = "Anzahl",)
  text(PrioSubTableOver5$Anzahl+8,prioplot,  paste(PrioSubTableOver5$Anzahl, sep=""), col = "red" ,cex=1.0) 
  #Priorisierungsverteilung_alle_2_3_Lightgreen_mit_Number_klein
  #Priorisierungsverteilung_alle_2_3_blau_Lightblau_mit_Number_klein
  
  
  
  
# Nur die 2,3 Frage Unterschiede/Switches
  Priorisierungsverteilung = NULL
  
  maxRespondend_Id = max(prioriseriungs_Daten_mit_Zeiten$Respondent_ID)
  print(maxRespondend_Id)
  minRespondend_Id = min(prioriseriungs_Daten_mit_Zeiten$Respondent_ID)
  print(minRespondend_Id)
  
  counterEqual <- 0
  counterNotEqual <- 0
  i <- minRespondend_Id
  while (i < maxRespondend_Id + 1){
    zeilenZw = prioriseriungs_Daten_mit_Zeiten[prioriseriungs_Daten_mit_Zeiten$Respondent_ID == i &
                                                 (prioriseriungs_Daten_mit_Zeiten$Question_No == 2 |
                                                    prioriseriungs_Daten_mit_Zeiten$Question_No == 3),]
    len = length(zeilenZw$Respondent_ID)
    if(len >1){
      order_Chosen = zeilenZw$PubIDs_Order_Chosen
      if(order_Chosen[1] == order_Chosen[2]){
        counterEqual = counterEqual + 1
      }else{
        counterNotEqual = counterNotEqual + 1 
        k <- 1
        gesamt = NULL
        gesamt = paste0(gesamt, "| ")
        while (k < len + 1) {
          gesamt = paste0(gesamt,order_Chosen[k], " | ")
          k = k + 1
        }
        print(gesamt)
        Priorisierung = gesamt
        Respondent_ID = i
        Priorisierungsverteilung = rbind(Priorisierungsverteilung, data.frame(Respondent_ID, Priorisierung))
        }
    }
    i = i + 1 
  }  
  print(counterEqual)
  print(counterNotEqual)
  View(Priorisierungsverteilung)
  
  Prio = unique(Priorisierungsverteilung$Priorisierung)
  laengePrio = length(Prio)
  print(laengePrio)
  PrioSubTable = NULL

  j <- 1
  while(j < laengePrio +1){
    PrioSub <- subset(Priorisierungsverteilung, Priorisierung %in% Prio[j])
    Anzahl = length(PrioSub$Respondent_ID)
    Reihenfolge = Prio[j]
    PrioSubTable = rbind(PrioSubTable, data.frame(Reihenfolge, Anzahl))
    j = j + 1
  }
  View(PrioSubTable)
  print(sum(PrioSubTable$Anzahl))
  PrioSubTableOver5 = PrioSubTable[PrioSubTable$Anzahl >= 5, ]
  View(PrioSubTableOver5)
  PrioSubTable = PrioSubTable[order(PrioSubTable$Anzahl),]
  PrioSubTableOver5 = PrioSubTableOver5[order(PrioSubTableOver5$Anzahl),]
  
  write_xlsx(PrioSubTableOver5, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungs_Reihenfolge_Anzahl_Priorisierungs_Switch_ueber_5_2_3_schoener_ordered.xlsx")
  write_xlsx(PrioSubTable, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungs_Reihenfolge_alle_Priorisierungs_Switch_2_3_schoener_ordered.xlsx")    
  
  pal <- colorRampPalette(colors = c("lightgreen", "lightgreen"))(12)
  pal <- colorRampPalette(colors = c("lightblue", "blue"))(12)
  par(mar=c(3,5,1,0)+1.0)
  par(mgp=c(2,0.5,0))
  prioplot <- barplot( PrioSubTableOver5$Anzahl,
                       names.arg = PrioSubTableOver5$Reihenfolge,
                       cex.names=.9, horiz=TRUE, las = 1, beside=FALSE,
                       #col = "#5c00cc",
                       #col = coul,
                       col = pal,lwd = 3,xlim=c(0,50),
                       main = "Priorisierungen/Gew?hlte Reihenfolgen\n [Priorisierungsunterschied (Numerisch vs. Altmetric)]",xlab = "Anzahl",)
  text(PrioSubTableOver5$Anzahl+1,prioplot,  paste(PrioSubTableOver5$Anzahl, sep=""), col = "red" ,cex=1.0) 
  #Priorisierungsverteilung_alle_2_3_Priorisierungs_Switch_Lightgreen_mit_Number_klein
  #Priorisierungsverteilung_alle_2_3_Priorisierungs_Switchblau_Lightblau_mit_Number_klein
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #___________________________________________________________________________________________
  
  # Nach GENDER,EDUCATION,ALTER Priorisierungsgruppierung | 1,2 | 1,2,3 | 1,2,3 |
  
  prioriseriungs_Daten_mit_Zeiten <- read_excel("C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Mit_Time_Intervall.xlsx")
  View(prioriseriungs_Daten_mit_Zeiten) #2068
  demografische_Daten <- read_excel("C:/Users/Jonas/Desktop/Relevanzwahrnehmung_demografische_Daten_Date_chenged.xlsx")
  View(demografische_Daten) # 1052
  
  
  PriorisierungMitDemo = NULL
  
  maxRespondend_Id = max(prioriseriungs_Daten_mit_Zeiten$Respondent_ID)
  print(maxRespondend_Id)
  minRespondend_Id = min(prioriseriungs_Daten_mit_Zeiten$Respondent_ID)
  print(minRespondend_Id)
  
  i <- minRespondend_Id
  while (i < maxRespondend_Id + 1){
    
    zeilenZw = prioriseriungs_Daten_mit_Zeiten[prioriseriungs_Daten_mit_Zeiten$Respondent_ID == i,]
    len = length(zeilenZw$Respondent_ID)
    if(len >2){
      
      
      order_Chosen = zeilenZw$PubIDs_Order_Chosen
      k <- 1
      gesamt = NULL
      gesamt = paste0(gesamt, "| ")
      while (k < len + 1) {
        gesamt = paste0(gesamt,order_Chosen[k], " | ") #| is.na(dm$Birthyear[1]) |is.null(dm$Birthyear[1])
        k = k + 1
      }
      print(gesamt)
      Priorisierung = gesamt
        
      Bearbeitungszeit_1_2 = zeilenZw$Bearbeitungszeit[1]
      Bearbeitungszeit_2_3 = zeilenZw$Bearbeitungszeit[2] 
      Bearbeitungszeit_1_3 = zeilenZw$Bearbeitungszeit[3]
        
      dm <- demografische_Daten[demografische_Daten$Respondent_ID == i,]
      Gender <- dm$Gender[1]
      Education <- dm$Education[1]
      Wissenschaftlich_gearbeitet = dm$`Published scientifically before (agree = yes)`[1]
      Altmetric_bekannt = dm$`Knew Altmetric Badges before (agree = yes)`[1]  
      Respondent_ID = i
      
      if(dm$Birthyear[1] == 0 ){
        Alter = 0
      }else{
        Alter <- 2019 - dm$Birthyear[1]
      }
      print(Gender)
      PriorisierungMitDemo = rbind(PriorisierungMitDemo, data.frame(Respondent_ID, Priorisierung, Bearbeitungszeit_1_2, Bearbeitungszeit_2_3, Bearbeitungszeit_1_3, Gender, Education, Alter, Wissenschaftlich_gearbeitet, Altmetric_bekannt))
      # Priorisierungsverteilung = rbind(Priorisierungsverteilung, data.frame(Respondent_ID, Priorisierung))
    }
    i = i + 1 
  }  
  
  View(PriorisierungMitDemo)
  
  #_________________________________________
  
  
  
  
  # Priorisierung nach Gender
  
  Data = NULL
  Data <- PriorisierungMitDemo[!(is.na(PriorisierungMitDemo$Gender) | PriorisierungMitDemo$Gender ==""), ]
  
  Data$Gender <- revalue(Data$Gender, c("Male"="M?nnlich"))
  Data$Gender <- revalue(Data$Gender, c("Female"="Weiblich"))
  Data$Gender <- revalue(Data$Gender, c("No answer"="Keine Antwort"))
  View(Data) # 364
  
  DataFemale = Data[Data$Gender == "Weiblich",]
  DataMale = Data[Data$Gender == "M?nnlich",]
  DataKeine = Data[Data$Gender == "Keine Antwort",]
  DataAndere = Data[Data$Gender == "Andere",]
  View(DataKeine)
  
# Female  
  Prio = unique(DataFemale$Priorisierung)
  laengePrio = length(Prio)
  laengeFemale = length(DataFemale$Respondent_ID)
  print(laengeFemale)
  print(laengePrio)
  PrioSubTableFemale = NULL
  j <- 1
  while(j < laengePrio + 1){
    PrioSub <- subset(DataFemale, Priorisierung %in% Prio[j])
    Anzahl = length(PrioSub$Respondent_ID)
    Reihenfolge = Prio[j]
    RelativerAnteil_in_Prozent = round((Anzahl/laengeFemale) * 100,2)
    PrioSubTableFemale = rbind(PrioSubTableFemale, data.frame(Reihenfolge, Anzahl, RelativerAnteil_in_Prozent))
    j = j + 1
  }

  print(sum(PrioSubTableFemale$Anzahl))
  PrioSubTableFemaleOver5 = PrioSubTableFemale[PrioSubTableFemale$Anzahl >= 5, ]
  PrioSubTableFemale = PrioSubTableFemale[order(PrioSubTableFemale$Anzahl),]
  PrioSubTableFemaleOver5 = PrioSubTableFemaleOver5[order(PrioSubTableFemaleOver5$Anzahl),]
  View(PrioSubTableFemale)
  View(PrioSubTableFemaleOver5)
  write_xlsx(PrioSubTableFemale, "C:/Users/Jonas/Desktop/Prio/ExcelTabellen_Priorisierung/nach_Gender/Priorisierungsreihenfolge_Female.xlsx")
  write_xlsx(PrioSubTableFemaleOver5, "C:/Users/Jonas/Desktop/Prio/ExcelTabellen_Priorisierung/nach_Gender/Priorisierungsreihenfolge_Female_over_5.xlsx")    

# Male  
  Prio = unique(DataMale$Priorisierung)
  laengePrio = length(Prio)
  laengeMale = length(DataMale$Respondent_ID)
  print(laengeMale)
  print(laengePrio)
  PrioSubTableMale = NULL
  j <- 1
  while(j < laengePrio + 1){
    PrioSub <- subset(DataMale, Priorisierung %in% Prio[j])
    Anzahl = length(PrioSub$Respondent_ID)
    Reihenfolge = Prio[j]
    RelativerAnteil_in_Prozent = round((Anzahl/laengeMale) * 100,2)
    PrioSubTableMale = rbind(PrioSubTableMale, data.frame(Reihenfolge, Anzahl, RelativerAnteil_in_Prozent))
    j = j + 1
  }
  
  print(sum(PrioSubTableMale$Anzahl))
  PrioSubTableMaleOver5 = PrioSubTableMale[PrioSubTableMale$Anzahl >= 5, ]
  PrioSubTableMale = PrioSubTableMale[order(PrioSubTableMale$Anzahl),]
  PrioSubTableMaleOver5 = PrioSubTableMaleOver5[order(PrioSubTableMaleOver5$Anzahl),]
  View(PrioSubTableMale)
  View(PrioSubTableMaleOver5)
  write_xlsx(PrioSubTableMale, "C:/Users/Jonas/Desktop/Prio/ExcelTabellen_Priorisierung/nach_Gender/Priorisierungsreihenfolge_Male.xlsx")
  write_xlsx(PrioSubTableMaleOver5, "C:/Users/Jonas/Desktop/Prio/ExcelTabellen_Priorisierung/nach_Gender/Priorisierungsreihenfolge_Male_over_5.xlsx")    
  
  
# Keine
  Prio = unique(DataKeine$Priorisierung)
  laengePrio = length(Prio)
  laengeKeine = length(DataKeine$Respondent_ID)
  print(laengeKeine)
  print(laengePrio)
  PrioSubTableKeine = NULL
  j <- 1
  while(j < laengePrio + 1){
    PrioSub <- subset(DataKeine, Priorisierung %in% Prio[j])
    Anzahl = length(PrioSub$Respondent_ID)
    Reihenfolge = Prio[j]
    RelativerAnteil_in_Prozent = round((Anzahl/laengeKeine) * 100,2)
    PrioSubTableKeine = rbind(PrioSubTableKeine, data.frame(Reihenfolge, Anzahl, RelativerAnteil_in_Prozent))
    j = j + 1
  }
  
  print(sum(PrioSubTableKeine$Anzahl))
  PrioSubTableKeineOver5 = PrioSubTableKeine[PrioSubTableKeine$Anzahl >= 5, ]
  PrioSubTableKeine = PrioSubTableKeine[order(PrioSubTableKeine$Anzahl),]
  PrioSubTableKeineOver5 = PrioSubTableKeineOver5[order(PrioSubTableKeineOver5$Anzahl),]
  View(PrioSubTableKeine)
  View(PrioSubTableKeineOver5)
  write_xlsx(PrioSubTableKeine, "C:/Users/Jonas/Desktop/Prio/ExcelTabellen_Priorisierung/nach_Gender/Priorisierungsreihenfolge_Keine.xlsx")
  write_xlsx(PrioSubTableKeineOver5, "C:/Users/Jonas/Desktop/Prio/ExcelTabellen_Priorisierung/nach_Gender/Priorisierungsreihenfolge_Keine_over_5.xlsx")    
  
  
# Andere
  Prio = unique(DataAndere$Priorisierung)
  laengePrio = length(Prio)
  laengeAndere = length(DataAndere$Respondent_ID)
  print(laengeAndere)
  print(laengePrio)
  PrioSubTableAndere = NULL
  j <- 1
  while(j < laengePrio + 1){
    PrioSub <- subset(DataAndere, Priorisierung %in% Prio[j])
    Anzahl = length(PrioSub$Respondent_ID)
    Reihenfolge = Prio[j]
    RelativerAnteil_in_Prozent = round((Anzahl/laengeAndere) * 100,2)
    PrioSubTableAndere = rbind(PrioSubTableAndere, data.frame(Reihenfolge, Anzahl, RelativerAnteil_in_Prozent))
    j = j + 1
  }
  
  print(sum(PrioSubTableAndere$Anzahl))
  PrioSubTableAndereOver5 = PrioSubTableAndere[PrioSubTableAndere$Anzahl >= 5, ]
  PrioSubTableAndere = PrioSubTableAndere[order(PrioSubTableAndere$Anzahl),]
  PrioSubTableAndereOver5 = PrioSubTableAndereOver5[order(PrioSubTableAndereOver5$Anzahl),]
  View(PrioSubTableAndere)
  View(PrioSubTableAndereOver5)
  write_xlsx(PrioSubTableAndere, "C:/Users/Jonas/Desktop/Prio/ExcelTabellen_Priorisierung/nach_Gender/Priorisierungsreihenfolge_Andere.xlsx")
  write_xlsx(PrioSubTableAndereOver5, "C:/Users/Jonas/Desktop/Prio/ExcelTabellen_Priorisierung/nach_Gender/Priorisierungsreihenfolge_Andere_over_5.xlsx")    
  
  
  
  
  
  
  
  # ALLE GENDER
  Prio = unique(Data$Priorisierung)
  laengePrio = length(Prio)
  print(laengePrio)
  PrioSubTableAll = NULL
  PrioSubTableAllRelative = NULL
  j <- 1
  while(j < laengePrio + 1){
    
    PrioSub_Female <- subset(DataFemale, Priorisierung %in% Prio[j])
    PrioSub_Male <- subset(DataMale, Priorisierung %in% Prio[j])
    PrioSub_Keine <- subset(DataKeine, Priorisierung %in% Prio[j])
    PrioSub_Andere <- subset(DataAndere, Priorisierung %in% Prio[j])
    
    laengeFemale = length(DataFemale$Respondent_ID)
    laengeMale = length(DataMale$Respondent_ID)
    laengeKeine = length(DataKeine$Respondent_ID)
    laengeAndere = length(DataAndere$Respondent_ID)
    
    Anzahl_Female = length(PrioSub_Female$Respondent_ID)
    Anzahl_Male = length(PrioSub_Male$Respondent_ID)
    Anzahl_Keine = length(PrioSub_Keine$Respondent_ID)
    Anzahl_Andere = length(PrioSub_Andere$Respondent_ID)
    Reihenfolge = Prio[j]
    RelativerAnteil_in_Prozent_Female = round((Anzahl_Female/laengeFemale) * 100,2)
    RelativerAnteil_in_Prozent_Male = round((Anzahl_Male/laengeMale) * 100,2)
    RelativerAnteil_in_Prozent_Keine = round((Anzahl_Keine/laengeKeine) * 100,2)
    RelativerAnteil_in_Prozent_Andere = round((Anzahl_Andere/laengeAndere) * 100,2)
    
    PrioSubTableAll = rbind(PrioSubTableAll, data.frame(Reihenfolge, Anzahl_Female,
                                                        Anzahl_Male, Anzahl_Keine,
                                                        Anzahl_Andere))
    PrioSubTableAllRelative = rbind(PrioSubTableAllRelative, data.frame(Reihenfolge, RelativerAnteil_in_Prozent_Female,
                                                                        RelativerAnteil_in_Prozent_Male,
                                                                        RelativerAnteil_in_Prozent_Keine,
                                                                        RelativerAnteil_in_Prozent_Andere))
    
    
    j = j + 1
  }
  View(PrioSubTableAll)
  View(PrioSubTableAllRelative)

  print(sum(PrioSubTableAll$Anzahl_Male))
  
  PrioSubTableAllOver5 = PrioSubTableAll[PrioSubTableAll$Anzahl_Female >= 4, ]
  PrioSubTableAll = PrioSubTableAll[order(PrioSubTableAll$Anzahl_Female),]
  PrioSubTableAllOver5 = PrioSubTableAllOver5[order(PrioSubTableAllOver5$Anzahl_Female),]
  
  PrioSubTableAllRelativeOver5 = PrioSubTableAllRelative[PrioSubTableAllRelative$RelativerAnteil_in_Prozent_Female >= 1.4, ]
  PrioSubTableAllRelative = PrioSubTableAllRelative[order(PrioSubTableAllRelative$RelativerAnteil_in_Prozent_Female),]
  PrioSubTableAllRelativeOver5 = PrioSubTableAllRelativeOver5[order(PrioSubTableAllRelativeOver5$RelativerAnteil_in_Prozent_Female),]
  
  View(PrioSubTableAllRelative)
  View(PrioSubTableAllRelativeOver5)
  
  View(PrioSubTableAll)
  View(PrioSubTableAllOver5)
  
  write_xlsx(PrioSubTableAll, "C:/Users/Jonas/Desktop/Prio/ExcelTabellen_Priorisierung/nach_Gender/Priorisierungsreihenfolge_Alle.xlsx")
  write_xlsx(PrioSubTableAllOver5, "C:/Users/Jonas/Desktop/Prio/ExcelTabellen_Priorisierung/nach_Gender/Priorisierungsreihenfolge_Alle_over_4.xlsx")    
  
  write_xlsx(PrioSubTableAllRelative, "C:/Users/Jonas/Desktop/Prio/ExcelTabellen_Priorisierung/nach_Gender/Priorisierungsreihenfolge_Alle_Relative.xlsx")
  write_xlsx(PrioSubTableAllRelativeOver5, "C:/Users/Jonas/Desktop/Prio/ExcelTabellen_Priorisierung/nach_Gender/Priorisierungsreihenfolge_Alle_Relative_over_1,4.xlsx")  
  
  
  
  #Relative
  names(PrioSubTableAllRelativeOver5) <- c('Reihenfolge','Female', 'Male','Keine', 'Andere')
  View(PrioSubTableAllRelativeOver5)
  
  tPrioSubTableAllRelativeOver5 <- t(PrioSubTableAllRelativeOver5) # Transpose Data
  View(tPrioSubTableAllRelativeOver5)
  colnames(tPrioSubTableAllRelativeOver5) <- PrioSubTableAllRelativeOver5$Reihenfolge
  tPrioSubTableAllRelativeOver5 <- tPrioSubTableAllRelativeOver5[-c(1), ]
  #tPrioSubTableAllOver5 = tPrioSubTableAllOver5[order(tPrioSubTableAllOver5$`| 1.2 | 1,3,2 | 1,3,2 |`),]
  a2 <- as.matrix(tPrioSubTableAllRelativeOver5)
  #a.num <- as.numeric(a)
  a2 = a2[order(a2[, 8]), ]
  View(a2)
  #is.numeric(a)
  a2 <- apply(a2, 2, as.numeric)
  
  #pal <- colorRampPalette(colors = c("red", "turquoise"))(4)
  #pal <- colorRampPalette(colors = c("green", "blue"))(4)
  #pal <- colorRampPalette(colors = c("red", "blue","green","orange"))
  par(mar=c(3,6.2,1,0)+1.2)
  par(mgp=c(2,0.5,0))
  mp <- barplot( a2, 
                 cex.names=.9,
                 horiz=TRUE,
                 las = 1,
                 beside=TRUE,
                 #col = "#5c00cc",
                 #col = coul,
                 #col = rainbow(4),
                 #col = pal,
                 col = c("green", "red","yellow","dodgerblue"),
                 lwd = 2,
                 xlim=c(0,60),
                 main = "Priorisierung nach Gender (relativ)",
                 xlab = "Prozent")
  legend("right", c("Keine","Weiblich","Andere","M?nnlich"), cex=1.2, fill=c("green", "red","yellow","dodgerblue"),bg='gray84')
  #legend("right", c("Andere","Keine","Weiblich","M?nnlich"), cex=1.2, bty="n", fill=c("yellow", "green","red","dodgerblue"), xpd=FALSE,bg='lightblue')
  xval = seq(0, 60, 10)
  axis(side = 1, at = xval, labels = TRUE, xpd=T, las = 1)
  abline(v=c(seq(10,60,10)))
  mp <- barplot( a2, 
                 cex.names=.9,
                 horiz=TRUE,
                 las = 1,
                 beside=TRUE,
                 #col = "#5c00cc",
                 #col = coul,
                 #col = rainbow(4),
                 #col = pal,
                 col = c("green", "red","yellow","dodgerblue"),
                 lwd = 2,
                 add = TRUE,
                 xlim=c(0,60),
                 main = "Priorisierung nach Gender (relativ)",
                 xlab = "Anzahl")
  
  #Priorisierungsreihenfolge_alle_over_1,4_relativ_mitGitter_klein
  
  
  
#Absolute  
 
  names(PrioSubTableAllOver5) <- c('Reihenfolge','Female', 'Male','Keine', 'Andere')
  View(PrioSubTableAllOver5)
  
  tPrioSubTableAllOver5 <- t(PrioSubTableAllOver5) # Transpose Data
  View(tPrioSubTableAllOver5)
  colnames(tPrioSubTableAllOver5) <- PrioSubTableAllOver5$Reihenfolge
  tPrioSubTableAllOver5 <- tPrioSubTableAllOver5[-c(1), ]
  #tPrioSubTableAllOver5 = tPrioSubTableAllOver5[order(tPrioSubTableAllOver5$`| 1.2 | 1,3,2 | 1,3,2 |`),]
  a <- as.matrix(tPrioSubTableAllOver5)
  #a.num <- as.numeric(a)
  a = a[order(a[, 6]), ]
  View(a)
  #is.numeric(a)
  a <- apply(a, 2, as.numeric)
  
  par(mar=c(3,6.2,1,0)+1.2)
  par(mgp=c(2,0.5,0))
  mp <- barplot( a, 
                 cex.names=.9,
                 horiz=TRUE,
                 las = 1,
                 beside=TRUE,
                 #col = "#5c00cc",
                 #col = coul,
                 #col = rainbow(4),
                 #col = pal,
                 col = c("yellow", "green","red","dodgerblue"),
                 lwd = 2,
                 xlim=c(0,100),
                 main = "Priorisierung nach Gender (absolut)",
                 xlab = "Anzahl")
  legend("right", c("Andere","Keine","Weiblich","M?nnlich"), cex=1.2, fill=c("yellow", "green","red","dodgerblue"),bg='gray84')
  #legend("right", c("Andere","Keine","Weiblich","M?nnlich"), cex=1.2, bty="n", fill=c("yellow", "green","red","dodgerblue"), xpd=FALSE,bg='lightblue')
  xval = seq(0, 100, 10)
  axis(side = 1, at = xval, labels = TRUE, xpd=T, las = 1)
  abline(v=c(seq(10,100,10)))
  mp <- barplot( a, 
                 cex.names=.9,
                 horiz=TRUE,
                 las = 1,
                 beside=TRUE,
                 #col = "#5c00cc",
                 #col = coul,
                 #col = rainbow(4),
                 #col = pal,
                 col = c("yellow", "green","red","dodgerblue"),
                 lwd = 2,
                 add = TRUE,
                 xlim=c(0,100),
                 main = "Priorisierung nach Gender (absolut)",
                 xlab = "Anzahl")

  #Priorisierungsreihenfolge_alle_over_4_mitGitter_klein
  
  
  
  
  
  
  
  
  
  
#________________________________________________________________________________________
# Education  
  
  Data = NULL
  Data <- PriorisierungMitDemo[!(is.na(PriorisierungMitDemo$Education) | PriorisierungMitDemo$Education ==""), ]
  
  DataOther = Data[Data$Education == "Other",]
  DataAnderer = Data[Data$Education == "Anderer",]
  DataKein = Data[Data$Education == "Kein Schulabschluss",]
  DataGrund = Data[Data$Education == "Grund-/Hauptschulabschluss",]
  DataReal = Data[Data$Education == "Realschule (Mittlere Reife)",]
  DataAbi = Data[Data$Education == "Gymnasium (Abitur)",]
  DataAusbildung = Data[Data$Education == "Abgeschlossene Ausbildung",]
  DataFachhochschulabschluss = Data[Data$Education == "Fachhochschulabschluss",]
  DataBachelor = Data[Data$Education == "Bachelor",]
  DataDiplom = Data[Data$Education == "Diplom",]
  DataMaster = Data[Data$Education == "Master",]
  DataPromotion = Data[Data$Education == "Promotion",]
  DataPhD = Data[Data$Education == "PhD",]
  
  View(DataAnderer)
  # Other hat weniger als 2 Datenpunkte und wird nicht eingezeichnet
  dataZw0 = Data[Data$Education == "Anderer" | 
                   Data$Education == "Kein Schulabschluss",]
  
  dataZw1 = Data[Data$Education == "Grund-/Hauptschulabschluss" | 
                   Data$Education == "Realschule (Mittlere Reife)" |
                   Data$Education == "Gymnasium (Abitur)",]
  dataZw2 = Data[Data$Education == "Abgeschlossene Ausbildung" | 
                   Data$Education == "Fachhochschulabschluss" |
                   Data$Education == "Bachelor",]
  dataZw3 = Data[Data$Education == "Diplom" |Data$Education == "Master" | 
                   Data$Education == "Promotion"
                 | Data$Education == "PhD",]
  
  View(Data) # 329
  View(DataPromotion)
  
  
  
# Alle betrachtet (der Personen die bei Education etwas valides angegeben haben)
# NICHT nach Education gruppiert sondern nur ?bersicht bzgl. Reihenfolge/Priorisierungen
 
  Prio = unique(Data$Priorisierung)
  laengePrio = length(Prio)
  laenge = length(Data$Respondent_ID)
  print(laenge)
  print(laengePrio)
  PrioSubTable = NULL
  j <- 1
  while(j < laengePrio + 1){
    PrioSub <- subset(Data, Priorisierung %in% Prio[j])
    Anzahl = length(PrioSub$Respondent_ID)
    Reihenfolge = Prio[j]
    RelativerAnteil_in_Prozent = round((Anzahl/laenge) * 100,2)
    PrioSubTable = rbind(PrioSubTable, data.frame(Reihenfolge, Anzahl, RelativerAnteil_in_Prozent))
    j = j + 1
  }
  print(sum(PrioSubTable$Anzahl))
  PrioSubTableOver5 = PrioSubTable[PrioSubTable$Anzahl >= 3, ]
  PrioSubTable = PrioSubTable[order(PrioSubTable$Anzahl),]
  PrioSubTableOver5 = PrioSubTableOver5[order(PrioSubTableOver5$Anzahl),]
  
  View(PrioSubTable)
  View(PrioSubTableOver5)
  write_xlsx(PrioSubTable, "C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_All.xlsx")
  write_xlsx(PrioSubTableOver5, "C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_All_over_3.xlsx")    
  
  pal <- colorRampPalette(colors = c("red", "turquoise"))(14)
  pal <- colorRampPalette(colors = c("green", "blue"))(14)
  #pal <- colorRampPalette(colors = c("red", "blue","green","orange"))
  par(mar=c(3,6.2,1,0)+1.2)
  par(mgp=c(2,0.5,0))
  mp <- barplot( PrioSubTableOver5$RelativerAnteil_in_Prozent, 
                 names.arg = PrioSubTableOver5$Reihenfolge,
                 cex.names=.9,
                 horiz=TRUE, las = 1, beside=FALSE,
                 #col = rainbow(4),
                 col = pal,
                 #col = c("green", "red","yellow","dodgerblue"),
                 lwd = 2,
                 xlim=c(0,60),
                 main = "Priorisierung (Education-Angaben vorhanden, relativ)",
                 xlab = "Prozent")
  #legend("right", c("Keine","Weiblich","Andere","M?nnlich"), cex=1.2, fill=c("green", "red","yellow","dodgerblue"),bg='gray84')
  #legend("right", c("Andere","Keine","Weiblich","M?nnlich"), cex=1.2, bty="n", fill=c("yellow", "green","red","dodgerblue"), xpd=FALSE,bg='lightblue')
  xval = seq(0, 60, 5)
  axis(side = 1, at = xval, labels = TRUE, xpd=T, las = 1)
  abline(v=c(seq(10,60,10)))
  mp <- barplot( PrioSubTableOver5$RelativerAnteil_in_Prozent, 
                 names.arg = PrioSubTableOver5$Reihenfolge,
                 cex.names=.9,
                 horiz=TRUE,
                 las = 1,
                 beside=FALSE,
                 #col = "#5c00cc",
                 #col = coul,
                 #col = rainbow(4),
                 col = pal,
                 #col = c("green", "red","yellow","dodgerblue"),
                 lwd = 2,
                 add = TRUE,
                 xlim=c(0,60),
                 main = "Priorisierung (Education-Angaben vorhanden, relativ)",
                 xlab = "Anzahl")
  #Priorisierungsreihenfolge_Education_alle_over_3_relativ__klein
  
  
  
  Reihenfolgen_10_y_Achse = unique(PrioSubTableOver5$Reihenfolge)
  View(Reihenfolgen_10_y_Achse) # einheitliche Reihenfolge bzw. Y-Achse f?r die einzelnen
  
  
  
  
  
# Einzeln betrachtet  
  
  DataOther = Data[Data$Education == "Other",]
  DataAnderer = Data[Data$Education == "Anderer",]
  DataKein = Data[Data$Education == "Kein Schulabschluss",]
  DataGrund = Data[Data$Education == "Grund-/Hauptschulabschluss",]
  DataReal = Data[Data$Education == "Realschule (Mittlere Reife)",]
  DataAbi = Data[Data$Education == "Gymnasium (Abitur)",]
  DataAusbildung = Data[Data$Education == "Abgeschlossene Ausbildung",]
  DataFachhochschulabschluss = Data[Data$Education == "Fachhochschulabschluss",]
  DataBachelor = Data[Data$Education == "Bachelor",]
  DataDiplom = Data[Data$Education == "Diplom",]
  DataMaster = Data[Data$Education == "Master",]
  DataPromotion = Data[Data$Education == "Promotion",]
  DataPhD = Data[Data$Education == "PhD",]
  
  
  Prio = Reihenfolgen_10_y_Achse #unique(Data$Priorisierung)
  Prio_all = unique(DataPhD$Priorisierung)
  View(Prio_all)
  laengePrio = length(Prio)
  laengePrio_all = length(Prio_all)
  laenge = length(DataPhD$Respondent_ID)
  print(laenge)
  print(laengePrio)
  PrioSubTable = NULL
  PrioSubTable_all = NULL
  j <- 1
  while(j < laengePrio + 1){
    PrioSub <- subset(DataPhD, Priorisierung %in% Prio[j])
    Anzahl = length(PrioSub$Respondent_ID)
    Reihenfolge = Prio[j]
    RelativerAnteil_in_Prozent = round((Anzahl/laenge) * 100,2)
    PrioSubTable = rbind(PrioSubTable, data.frame(Reihenfolge, Anzahl, RelativerAnteil_in_Prozent))
    j = j + 1
  }
  j <- 1
  while(j < laengePrio_all + 1){
    PrioSub <- subset(DataPhD, Priorisierung %in% Prio_all[j])
    Anzahl = length(PrioSub$Respondent_ID)
    Reihenfolge = Prio_all[j]
    RelativerAnteil_in_Prozent = round((Anzahl/laenge) * 100,2)
    PrioSubTable_all = rbind(PrioSubTable_all, data.frame(Reihenfolge, Anzahl, RelativerAnteil_in_Prozent))
    j = j + 1
  }
  
  print(sum(PrioSubTable$Anzahl))
  PrioSubTableOver5 = PrioSubTable[PrioSubTable$Anzahl >= 3, ]
  PrioSubTableOver5_all = PrioSubTable_all[PrioSubTable_all$Anzahl >= 3, ]
  PrioSubTable_all = PrioSubTable_all[order(PrioSubTable_all$Anzahl),]
  PrioSubTableOver5_all = PrioSubTableOver5_all[order(PrioSubTableOver5_all$Anzahl),]
  View(PrioSubTable_all)
  View(PrioSubTableOver5_all)
  View(PrioSubTable)
  View(PrioSubTableOver5)
  write_xlsx(PrioSubTable, "C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_PhD.xlsx")
  write_xlsx(PrioSubTableOver5, "C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_PhD_over_3.xlsx")
  write_xlsx(PrioSubTable_all, "C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_PhD_all.xlsx")
  write_xlsx(PrioSubTableOver5_all, "C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_PhD_over_3_all.xlsx")    
  
  pal <- colorRampPalette(colors = c("red", "turquoise"))(14)
  pal <- colorRampPalette(colors = c("green", "blue"))(14)
  par(mar=c(3,6.2,1,0)+1.2)
  par(mgp=c(2,0.5,0))
  mp <- barplot( PrioSubTable$RelativerAnteil_in_Prozent, 
                 names.arg = PrioSubTable$Reihenfolge,
                 cex.names=.9,
                 horiz=TRUE, las = 1, beside=FALSE,
                 #col = rainbow(4),
                 col = pal,
                 #col = c("green", "red","yellow","dodgerblue"),
                 lwd = 2,
                 xlim=c(0,80),
                 main = paste("Priorisierung (PhD [",laenge,"], relativ)"),
                 xlab = "Prozent")
  xval = seq(0, 80, 5)
  axis(side = 1, at = xval, labels = TRUE, xpd=T, las = 1)
  abline(v=c(seq(10,80,10)))
  mp <- barplot( PrioSubTable$RelativerAnteil_in_Prozent, 
                 names.arg = PrioSubTable$Reihenfolge,
                 cex.names=.9,
                 horiz=TRUE,
                 las = 1,
                 beside=FALSE,
                 col = pal,
                 #col = c("green", "red","yellow","dodgerblue"),
                 lwd = 2,
                 add = TRUE,
                 xlim=c(0,80),
                 main = paste("Priorisierung (PhD [",laenge,"], relativ)"),
                 xlab = "Anzahl")
  text(PrioSubTable$RelativerAnteil_in_Prozent+2.2, mp, paste(PrioSubTable$RelativerAnteil_in_Prozent, sep=""), col = "red" ,cex=0.8) 
  #Priorisierungsreihenfolge_Education_PhD_y_10_relativ__klein
  
  
  
  
#________________________________________________________________________________
  
# ALLE in 2er Gruppen (zu je 2 Reihenfolgen alle Educationgruppen) mit Legende nach Education
  
  #DataOther <- read_excel("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_Other.xlsx")
  #DataAnderer <- read_excel("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_Andere.xlsx")
  DataKein <- read_excel("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_Kein.xlsx")
  DataGrund <- read_excel("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_Grund.xlsx")
  DataReal <- read_excel("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_Real.xlsx")
  DataAbi <- read_excel("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_Abi.xlsx")
  DataAusbildung <- read_excel("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_Ausbildung.xlsx")
  DataFachhochschulabschluss <- read_excel("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_Fachhochschulabschluss.xlsx")
  DataBachelor <- read_excel("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_Bachelor.xlsx")
  DataDiplom <- read_excel("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_Diplom.xlsx")
  DataMaster <- read_excel("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_Master.xlsx")
  DataPromotion <- read_excel("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_Promotion.xlsx")
  DataPhD <- read_excel("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/einzeln/Priorisierungsreihenfolge_PhD.xlsx")
  
  
  Data = NULL
  Data = DataKein
  colnames(Data)[3] = "Kein_Abschluss"
  Data$Anzahl = NULL
  Data$Grund_Hauptschulabschluss = DataGrund$RelativerAnteil_in_Prozent
  Data$Realschulabschluss = DataReal$RelativerAnteil_in_Prozent
  Data$Abitur = DataAbi$RelativerAnteil_in_Prozent
  Data$Ausbildung = DataAusbildung$RelativerAnteil_in_Prozent
  Data$Fachhochschulabschluss = DataFachhochschulabschluss$RelativerAnteil_in_Prozent
  Data$Bachelor = DataBachelor$RelativerAnteil_in_Prozent
  Data$Diplom = DataDiplom$RelativerAnteil_in_Prozent
  Data$Master = DataMaster$RelativerAnteil_in_Prozent
  Data$Promotion = DataPromotion$RelativerAnteil_in_Prozent
  Data$PhD = DataPhD$RelativerAnteil_in_Prozent
  View(Data)
  write_xlsx(Data, "C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Education/alle/Priorisierungsreihenfolge_alle_Gruppen_1_14_relativ_klein.xlsx")
  
  df1_2 = Data[1:2,]
  df3_4 = Data[3:4,]
  df5_6 = Data[5:6,]
  df7_8 = Data[7:8,]
  df9_10 = Data[9:10,]
  df11_12 = Data[11:12,]
  df13_14 = Data[13:14,]
  View(df11_12)
  
  tdf1_2 <- t(df1_2) # Transpose Data
  View(tdf1_2)
  colnames(tdf1_2) <- df1_2$Reihenfolge
  tdf1_2 <- tdf1_2[-c(1), ]
  a2 <- as.matrix(tdf1_2)
  View(a2)
  a2 <- apply(a2, 2, as.numeric)
  
  #pal <- colorRampPalette(colors = c("red", "turquoise"))(11)
  #pal <- colorRampPalette(colors = c("green", "blue"))(11)
  #pal <- colorRampPalette(colors = c("red", "blue","green","orange"))
  par(mar=c(3,6.2,1,0)+1.2)
  par(mgp=c(2,0.5,0))
  mp <- barplot( a2, 
                 cex.names=.9,
                 horiz=TRUE,
                 las = 1,
                 beside=TRUE,
                 col = rainbow(11),
                 #col = heat.colors(11),
                 #col = topo.colors(11),
                 #col = pal,
                 lwd = 2,
                 xlim=c(0,80),
                 main = "Priorisierung nach Education (relativ)",
                 xlab = "Prozent")
  #legend("right", c("Kein Schulabschluss","Grund-/Hauptschulabschluss","Realschule (Mittlere Reife)","Gymnasium (Abitur)", "Abgeschlossene Ausbildung", "Fachhochschulabschluss", 
  #                      "Bachelor", "Diplom", "Master", "Promotion", "PhD"), 
  #       cex=1.2, fill=rainbow(11),bg='gray84')
  legend("bottomright", legend = rev(rownames(tdf1_2)), ncol = 1,cex=0.72, fill=rev(rainbow(11)),bg='gray84')
  xval = seq(0, 80, 10)
  axis(side = 1, at = xval, labels = TRUE, xpd=T, las = 1)
  abline(v=c(seq(10,80,10)))
  mp <- barplot( a2, 
                 cex.names=.9,
                 horiz=TRUE,
                 las = 1,
                 beside=TRUE,
                 col = rainbow(11),
                 #col = pal,
                 #col = c("green", "red","yellow","dodgerblue"),
                 lwd = 2,
                 add = TRUE,
                 xlim=c(0,80),
                 main = "Priorisierung nach Education (relativ)",
                 xlab = "Prozent")
  #Priorisierungsreihenfolge_alle_Gruppen_1_2_relativ_klein
  
  
#__________________________________________________________________________________  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #________________________________________________________________________________________
  # Alter  
  
  Data = NULL
  Data <- PriorisierungMitDemo[!(is.na(PriorisierungMitDemo$Alter) | PriorisierungMitDemo$Alter ==""), ]
  

  View(Data) # 606
  
  Data0_10 = Data[Data$Alter >= 0 & Data$Alter <= 10,]
  Data11_20 = Data[Data$Alter >= 11 & Data$Alter <= 20,]
  Data21_30 = Data[Data$Alter >= 21 & Data$Alter <= 30,]
  Data31_40 = Data[Data$Alter >= 31 & Data$Alter <= 40,]
  Data41_50 = Data[Data$Alter >= 41 & Data$Alter <= 50,]
  Data51_60 = Data[Data$Alter >= 51 & Data$Alter <= 60,]
  Data61_70 = Data[Data$Alter >= 61 & Data$Alter <= 70,]
  Data71_80 = Data[Data$Alter >= 71 & Data$Alter <= 80,]
  Data81_90 = Data[Data$Alter >= 81 & Data$Alter <= 90,]
  Data91_100 = Data[Data$Alter >= 91 & Data$Alter <= 100,]
  
  View(Data0_10)
  View(Data11_20)
  View(Data21_30)
  View(Data31_40)
  View(Data41_50)
  View(Data51_60)
  View(Data61_70)
  View(Data71_80)
  View(Data81_90)
  View(Data91_100)
  
  print(length(Data0_10$Respondent_ID)) # 248 (alle 0)
  print(length(Data11_20$Respondent_ID)) # 24
  print(length(Data21_30$Respondent_ID)) # 156
  print(length(Data31_40$Respondent_ID)) # 78
  print(length(Data41_50$Respondent_ID)) # 34
  print(length(Data51_60$Respondent_ID)) # 46
  print(length(Data61_70$Respondent_ID)) # 9
  print(length(Data71_80$Respondent_ID)) # 2
  print(length(Data81_90$Respondent_ID)) # 1
  print(length(Data91_100$Respondent_ID)) # 2
  # 352 (ohne Alter 0_10)
  
  
  # Alle betrachtet (der Personen die bei Alter etwas valides angegeben haben)
  # NICHT nach Alter gruppiert sondern nur ?bersicht bzgl. Reihenfolge/Priorisierungen
  
  Prio = unique(Data$Priorisierung)
  laengePrio = length(Prio)
  laenge = length(Data$Respondent_ID)
  print(laenge)
  print(laengePrio)
  PrioSubTable = NULL
  j <- 1
  while(j < laengePrio + 1){
    PrioSub <- subset(Data, Priorisierung %in% Prio[j])
    Anzahl = length(PrioSub$Respondent_ID)
    Reihenfolge = Prio[j]
    RelativerAnteil_in_Prozent = round((Anzahl/laenge) * 100,2)
    PrioSubTable = rbind(PrioSubTable, data.frame(Reihenfolge, Anzahl, RelativerAnteil_in_Prozent))
    j = j + 1
  }
  print(sum(PrioSubTable$Anzahl))
  PrioSubTableOver5 = PrioSubTable[PrioSubTable$Anzahl >= 5, ]
  PrioSubTable = PrioSubTable[order(PrioSubTable$Anzahl),]
  PrioSubTableOver5 = PrioSubTableOver5[order(PrioSubTableOver5$Anzahl),]
  
  View(PrioSubTable)
  View(PrioSubTableOver5)
  write_xlsx(PrioSubTable, "C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Alter/alle/Priorisierungsreihenfolge_All.xlsx")
  write_xlsx(PrioSubTableOver5, "C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Alter/alle/Priorisierungsreihenfolge_All_over_3.xlsx")    
  
  pal <- colorRampPalette(colors = c("red", "turquoise"))(14)
  pal <- colorRampPalette(colors = c("green", "blue"))(14)
  #h.ramp <- heat.colors(12, alpha=h.alpha[i])
  #pal <- colorRampPalette(colors = c("red", "blue","green","orange"))
  par(mar=c(3,6.2,1,0)+1.2)
  par(mgp=c(2,0.5,0))
  mp <- barplot( PrioSubTableOver5$RelativerAnteil_in_Prozent, 
                 names.arg = PrioSubTableOver5$Reihenfolge,
                 cex.names=.9,
                 horiz=TRUE, las = 1, beside=FALSE,
                 #col = rainbow(4),
                 col = pal,
                 #col = c("green", "red","yellow","dodgerblue"),
                 lwd = 2,
                 xlim=c(0,50),
                 main = "Priorisierung (Alter-Angaben vorhanden, relativ)",
                 xlab = "Prozent")
  #legend("right", c("Keine","Weiblich","Andere","M?nnlich"), cex=1.2, fill=c("green", "red","yellow","dodgerblue"),bg='gray84')
  #legend("right", c("Andere","Keine","Weiblich","M?nnlich"), cex=1.2, bty="n", fill=c("yellow", "green","red","dodgerblue"), xpd=FALSE,bg='lightblue')
  xval = seq(0, 50, 5)
  axis(side = 1, at = xval, labels = TRUE, xpd=T, las = 1)
  abline(v=c(seq(10,50,10)))
  mp <- barplot( PrioSubTableOver5$RelativerAnteil_in_Prozent, 
                 names.arg = PrioSubTableOver5$Reihenfolge,
                 cex.names=.9,
                 horiz=TRUE,
                 las = 1,
                 beside=FALSE,
                 #col = "#5c00cc",
                 #col = coul,
                 col = rev(heat.colors(14)), #rainbow(4),
                 #col = pal,
                 #col = c("green", "red","yellow","dodgerblue"),
                 lwd = 2,
                 add = TRUE,
                 xlim=c(0,50),
                 main = "Priorisierung (Alter-Angaben vorhanden, relativ)",
                 xlab = "Anzahl")
  text(PrioSubTableOver5$RelativerAnteil_in_Prozent+1.8, mp, paste(PrioSubTableOver5$RelativerAnteil_in_Prozent, sep=""), col = "red" ,cex=0.8) 
  #Priorisierungsreihenfolge_Alter_alle_over_5_relativ_heat_klein

  
  PrioSubTableOver10 = PrioSubTable[PrioSubTable$Anzahl >= 10, ]
  PrioSubTableOver10 = PrioSubTableOver10[order(PrioSubTableOver10$Anzahl),]
  View(PrioSubTableOver10)
  
  Reihenfolgen_9_y_Achse = unique(PrioSubTableOver10$Reihenfolge)
  View(Reihenfolgen_9_y_Achse) # einheitliche Reihenfolge bzw. Y-Achse f?r die einzelnen
  
  nameAlter.list <- list("_0_10_", "_11_20_", "_21_30_", "_31_40_","_41_50_"
                         , "_51_60_", "_61_70_", "_71_80_", "_81_90_", "_91_100_")
  nameAlterForTable.list <- list("_0_10", "_11_20", "_21_30", "_31_40","_41_50"
                         , "_51_60", "_61_70", "_71_80", "_81_90", "_91_100")
  print(nameAlter.list[1])
  
  View(Data0_10)
  View(Data11_20)
  View(Data21_30)
  View(Data31_40)
  View(Data41_50)
  View(Data51_60)
  View(Data61_70)
  View(Data71_80)
  View(Data81_90)
  View(Data91_100)
  
  my.list <- list(Data0_10, Data11_20, Data21_30, Data31_40, Data41_50, Data51_60, Data61_70
                  , Data71_80, Data81_90, Data91_100)
  View(my.list[[1]])
  
  
  Prio = Reihenfolgen_9_y_Achse 
  laengePrio = length(Prio)
  print(length(my.list))
  
  
  DataZw_0_10 <- read_excel("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Alter/einzeln/Priorisierungsreihenfolge_0_10_.xlsx")
  
  Data = NULL
  Data = DataZw_0_10
  View(Data)
  colnames(Data)[3] = "Alter_0_10"   # "Anteil_in_Prozent_0_10"
  Data$Anzahl = NULL
  
  absolutList <- c()
  
d <- 1  
while (d < length(my.list) + 1) {
  print(my.list[[d]])
 
  Prio_all = unique(my.list[[d]]$Priorisierung)
  #View(Prio_all)
  
  laengePrio_all = length(Prio_all)
  laenge = length(my.list[[d]]$Respondent_ID)
  print(laenge)
  absolutList <- c(absolutList, laenge)
  print(laengePrio)
  PrioSubTable = NULL
  PrioSubTable_all = NULL
  j <- 1
  while(j < laengePrio + 1){
    PrioSub <- subset(my.list[[d]], Priorisierung %in% Prio[j])
    Anzahl = length(PrioSub$Respondent_ID)
    Reihenfolge = Prio[j]
    RelativerAnteil_in_Prozent = round((Anzahl/laenge) * 100,2)
    PrioSubTable = rbind(PrioSubTable, data.frame(Reihenfolge, Anzahl, RelativerAnteil_in_Prozent))
    j = j + 1
  }
  j <- 1
  while(j < laengePrio_all + 1){
    PrioSub <- subset(my.list[[d]], Priorisierung %in% Prio_all[j])
    Anzahl = length(PrioSub$Respondent_ID)
    Reihenfolge = Prio_all[j]
    RelativerAnteil_in_Prozent = round((Anzahl/laenge) * 100,2)
    PrioSubTable_all = rbind(PrioSubTable_all, data.frame(Reihenfolge, Anzahl, RelativerAnteil_in_Prozent))
    j = j + 1
  }
  
  print(sum(PrioSubTable$Anzahl))
  PrioSubTableOver5 = PrioSubTable[PrioSubTable$Anzahl >= 5, ]
  PrioSubTableOver5_all = PrioSubTable_all[PrioSubTable_all$Anzahl >= 5, ]
  PrioSubTable_all = PrioSubTable_all[order(PrioSubTable_all$Anzahl),]
  PrioSubTableOver5_all = PrioSubTableOver5_all[order(PrioSubTableOver5_all$Anzahl),]
  #View(PrioSubTable_all)
  #View(PrioSubTableOver5_all)
  #View(PrioSubTable)
  #View(PrioSubTableOver5)
  
  Data[, paste("Alter", nameAlterForTable.list[[d]], sep = "") ] = PrioSubTable$RelativerAnteil_in_Prozent
  
  #write_xlsx(PrioSubTable, paste("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Alter/einzeln/Priorisierungsreihenfolge", nameAlter.list[d] ,".xlsx", sep = ""))
  #write_xlsx(PrioSubTableOver5, paste("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Alter/einzeln/Priorisierungsreihenfolge", nameAlter.list[d] ,"over_5.xlsx", sep = ""))
  #write_xlsx(PrioSubTable_all, paste("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Alter/einzeln/Priorisierungsreihenfolge", nameAlter.list[d] ,"all.xlsx", sep = ""))
  #write_xlsx(PrioSubTableOver5_all, paste("C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Alter/einzeln/Priorisierungsreihenfolge", nameAlter.list[d] ,"over_5_all.xlsx", sep = ""))  
  
  
  d = d + 1
}   
# Absolute Werte unten dazugepackt
write_xlsx(Data, "C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Alter/alle/Priorisierungsreihenfolge_realtiv_0_100_otherLabel.xlsx")
View(Data)
Reihenfolge = "Absolute Anzahl"
Alter_0_10 = absolutList[1] #Anteil_in_Prozent_11_20
Alter_11_20 = absolutList[2]
Alter_21_30 = absolutList[3]
Alter_31_40 = absolutList[4]
Alter_41_50 = absolutList[5]  
Alter_51_60 = absolutList[6]
Alter_61_70 = absolutList[7]
Alter_71_80 = absolutList[8]
Alter_81_90 = absolutList[9]
Alter_91_100 = absolutList[10]

#Add absolute Number
Data = rbind(Data, data.frame(Reihenfolge,Alter_0_10, Alter_11_20 , Alter_21_30,
                              Alter_31_40, Alter_41_50, Alter_51_60,
                              Alter_61_70, Alter_71_80, Alter_81_90, Alter_91_100 ) )
write_xlsx(Data, "C:/Users/Jonas/Desktop/Prio/Gruppierung_Reihenfolgen/nach_Alter/alle/Priorisierungsreihenfolge_realtiv_0_100_mit_absolut_otherLabel.xlsx")




  
#____________________________________HIER_WEITER_!!!!__________________________________________________________  
######################################################################################################






  
  df1_2 = Data[1:2,]
  df3_4 = Data[3:4,]
  df5_6 = Data[5:6,]
  df7_8 = Data[7:8,]
  df9_9 = Data[9:9,]
  #df11_12 = Data[11:12,]
  #df13_14 = Data[13:14,]
  View(df9_9)
  
  tdf9_9 <- t(df9_9) # Transpose Data
  View(tdf9_9)
  colnames(tdf9_9) <- df9_9$Reihenfolge
  tdf9_9 <- tdf9_9[-c(1), ]
  a2 <- as.matrix(tdf9_9)
  View(a2)
  a2 <- apply(a2, 2, as.numeric)
  
  #pal <- colorRampPalette(colors = c("red", "turquoise"))(11)
  #pal <- colorRampPalette(colors = c("green", "blue"))(11)
  #pal <- colorRampPalette(colors = c("red", "blue","green","orange"))
  par(mar=c(3,6.2,1,0)+1.2)
  par(mgp=c(2,0.5,0))
  mp <- barplot( a2, 
                 cex.names=.9,
                 horiz=TRUE,
                 las = 1,
                 beside=TRUE,
                 col = rainbow(10),
                 #col = heat.colors(11),
                 #col = topo.colors(11),
                 #col = pal,
                 lwd = 2,
                 xlim=c(0,100),
                 main = "Priorisierung nach Alter (relativ)",
                 xlab = "Prozent")
  #legend("right", c("Kein Schulabschluss","Grund-/Hauptschulabschluss","Realschule (Mittlere Reife)","Gymnasium (Abitur)", "Abgeschlossene Ausbildung", "Fachhochschulabschluss", 
  #                      "Bachelor", "Diplom", "Master", "Promotion", "PhD"), 
  #       cex=1.2, fill=rainbow(11),bg='gray84')
  legend("bottomright", legend = rev(rownames(tdf1_2)), ncol = 1,cex=0.72, fill=rev(rainbow(10)),bg='gray84')
  xval = seq(0, 100, 10)
  axis(side = 1, at = xval, labels = TRUE, xpd=T, las = 1)
  abline(v=c(seq(0,100,10)))
  mp <- barplot( a2, 
                 cex.names=.9,
                 horiz=TRUE,
                 las = 1,
                 beside=TRUE,
                 col = rainbow(10),
                 #col = pal,
                 #col = c("green", "red","yellow","dodgerblue"),
                 lwd = 2,
                 add = TRUE,
                 xlim=c(0,100),
                 main = "Priorisierung nach Alter (relativ)",
                 xlab = "Prozent")
  #Priorisierungsreihenfolge_alle_Gruppen_Alter_9_9_relativ_klein
  
  
  
  
  
  
  
  
  
  
  
  
  
  # zeitlicher Befragungsverlauf
  
  Data = NULL
  Data <- BearbeitungszeitOOO_1_3[!(is.na(BearbeitungszeitOOO_1_3$Gender) | BearbeitungszeitOOO_1_3$Gender ==""), ]
  
  demografische_Daten_alle <- read_excel("C:/Users/Jonas/Desktop/Relevanzwahrnehmung_demografische_Daten_Date_chenged.xlsx")
  demografische_Daten_alle$Gender <- revalue(Data$Gender, c("Male"="M?nnlich"))
  demografische_Daten_alle$Gender <- revalue(Data$Gender, c("Female"="Weiblich"))
  demografische_Daten_alle$Gender <- revalue(Data$Gender, c("No answer"="Keine Antwort"))
  View(demografische_Daten_alle)
  
  datums_daten = unique(as.Date(demografische_Daten_alle$`Date_started (deprecated, scheint fehlerhaft und viel zu fr?h)`))
  print(length(datums_daten))
  View(datums_daten)
  
  #zw = demografische_Daten_alle[as.Date(demografische_Daten_alle$`Date_started (deprecated, scheint fehlerhaft und viel zu fr?h)`) == datums_daten[1], ]
  #View(zw)
  #anzahl = length(zw$Respondent_ID)
  #print(anzahl)
  
  date_anzahl = NULL
  f <- 1
  while (f < length(datums_daten) + 1) {
    
    zw = demografische_Daten_alle[as.Date(demografische_Daten_alle$`Date_started (deprecated, scheint fehlerhaft und viel zu fr?h)`) == datums_daten[f], ]
    Anzahl = length(zw$Respondent_ID)
    print(Anzahl)
    Datum = datums_daten[f]
    date_anzahl = rbind(date_anzahl, data.frame(Datum, Anzahl))
    f = f + 1
  }
  View(date_anzahl)
  
  write_xlsx(date_anzahl, "C:/Users/Jonas/Desktop/Prio/zeitlicher_Befragungsablauf.xlsx")
  
  
# Nur die Angaben ber?pcksichtigen, die bei Gender nicht NA haben (also ohne Fake Sessions) 
  
  demografische_Daten_alle_valide <- read_excel("C:/Users/Jonas/Desktop/Relevanzwahrnehmung_demografische_Daten_Date_chenged.xlsx")
  demografische_Daten_alle_valide$Gender <- revalue(demografische_Daten_alle_valide$Gender, c("Male"="M?nnlich"))
  demografische_Daten_alle_valide$Gender <- revalue(demografische_Daten_alle_valide$Gender, c("Female"="Weiblich"))
  demografische_Daten_alle_valide$Gender <- revalue(demografische_Daten_alle_valide$Gender, c("No answer"="Keine Antwort"))
  View(demografische_Daten_alle_valide)
  
  demografische_Daten_alle_valide <- demografische_Daten_alle_valide[!(is.na(demografische_Daten_alle_valide$Gender) | demografische_Daten_alle_valide$Gender ==""), ]
  datums_daten_valide = unique(as.Date(demografische_Daten_alle_valide$`Date_started (deprecated, scheint fehlerhaft und viel zu fr?h)`))
  
  print(length(datums_daten_valide))
  View(datums_daten_valide)
  
  date_anzahl_valid_data = NULL
  f <- 1
  while (f < length(datums_daten_valide) + 1) {
    
    zw = demografische_Daten_alle[as.Date(demografische_Daten_alle$`Date_started (deprecated, scheint fehlerhaft und viel zu fr?h)`) == datums_daten_valide[f], ]
    Anzahl = length(zw$Respondent_ID)
    print(Anzahl)
    Datum = datums_daten_valide[f]
    date_anzahl_valid_data = rbind(date_anzahl_valid_data, data.frame(Datum, Anzahl))
    f = f + 1
  }
  View(date_anzahl_valid_data)
  
  
  write_xlsx(date_anzahl_valid_data, "C:/Users/Jonas/Desktop/Prio/zeitlicher_Befragungsablauf_ohne_Fake_Sessions.xlsx")
  
  
  #plot(date_anzahl_valid_data$Anzahl~date_anzahl_valid_data$Datum , type="b" , lwd=3 , col=rgb(0.1,0.7,0.1,0.8) , ylab="value of ..." , xlab="date" , bty="l" , pch=20 , cex=2)
  #abline(h=seq(0,100,10) , col="grey", lwd=0.8)
  #date_anzahl_valid_data %>%
  #  tail(124) %>%
  #  ggplot( aes(x=Datum, y=Anzahl)) +
  #  geom_line( color="grey") +
  #  geom_point(shape=21, color="black", fill="#69b3a2", size=1) +
  #  theme_ipsum() +
  #  ggtitle("Evolution of bitcoin price")
  # date_anzahl_valid_data %>%
  #  tail(124) %>%
  #  ggplot( aes(x=Datum, y=Anzahl)) +
  #  geom_line() +
  #  geom_point()
  # plot(date_anzahl_valid_data$Anzahl~as.Date(date_anzahl_valid_data$Datum,"%d/%m/%Y %H:%M"),type="l",
  #     xlab="Time", ylab="Concentration (ppb)",
  #     main="Time trend of Oxides of Nitrogen")
  
  
  ggplot(date_anzahl, aes(x=Datum, y=Anzahl)) +
    geom_area( fill="#69b3a2", alpha=0.4) +
    geom_line(color="#69b3a2", size=0.8) +
    geom_point(size=1.8, color="#69b3a2") +
    theme_ipsum() +
    ggtitle("Zeitlicher Befragungsverlauf (Anzahl der Befragungsantworten pro Tag)") +
    theme(plot.title = element_text(size = 10, face = "bold")) +
    labs(y="Anzahl Befragungsantworten", x = "Datum") +
    #theme(axis.text.x = element_text(size = 16, face = "bold")) +
    #theme(axis.title.x = element_text(size = 20, face = "bold")) +
    #theme(axis.title.y = element_text(size = 20, face = "bold")) +
  #scale_x_date(date_breaks = "2 month", limits = as.Date(c("2019-02-01","2019-12-17"))) 
    #scale_x_date(date_breaks = "2 month",date_labels = "%b (%Y)", limits = as.Date(c("2019-02-01","2019-12-17"))) 
    scale_x_date(breaks = seq(as.Date("2019-02-01"), as.Date("2019-12-17"), by="2 months"), date_labels = "%b\n%Y")

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Plot   BearbeitungszeitOOO_1_3_Gender_Density_klein_filled_x10
  ggplot2.density(data=Data, xName='Bearbeitungszeit_1_3', fill ='Gender' , groupName='Gender',
                  legendPosition="top")
  
  
  ggplot2.density(data=Data, xName='Bearbeitungszeit_1_3', groupName='Gender',
                  legendPosition="top",
                  alpha=0.5, fillGroupDensity=TRUE )+
    
    theme(legend.justification=c(1,0),
          legend.key.size = unit(1, "cm"),
          legend.position=c(0.98, 0.45))+
    #scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210, 220,230,240,250))
    scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140,160,180,200,220,240,260))
  
  
  # Density plots with mean lines
  ggplot2.density(data=kpData, xName='Bearbeitungszeit_1_3', groupName='Gender',
                  legendPosition="top",addMeanLine=TRUE) +
    scale_colour_discrete(name = "Gender (Mean-Value/Median)", 
                          labels = c(paste("M?nnlich",mean_Male,"/", median_Male), paste("Weiblich",mean_Female,"/",median_Female), 
                                     paste("Keine Antwort",mean_Keine,"/",median_Keine),paste("Andere",mean_Andere,"/",median_Andere))) +
    theme(legend.justification=c(1,0),
          legend.key.size = unit(1, "cm"),
          legend.position=c(0.98, 0.45))+
    #scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210, 220,230,240,250))
    
    scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140,160,180,200,220,240,260))
  
  
  write_xlsx(BearbeitungszeitOOO_1_3, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_BearbeitungszeitOOO_1_3_Mit_Gender_Education.xlsx")    
  write_xlsx(Data, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_BearbeitungszeitOOO_1_3_Mit_Gender_Only_without_NA.xlsx")
  
  #_______________________________________________________________________________________
  
  
  
  
  
  
  
  
  
  Prio21_231_231 = unique(Priorisierungsverteilung$Priorisierung)
  laengePrio21_231_231 = length(Prio21_231_231)
  print(laengePrio21_231_231)
  PrioSubTable = NULL
  j <- 1
  while(j < laengePrio21_231_231 +1){
    PrioSub <- subset(Priorisierungsverteilung, Priorisierung %in% Prio21_231_231[j])
    Anzahl = length(PrioSub$Respondent_ID)
    Reihenfolge = Prio21_231_231[j]
    PrioSubTable = rbind(PrioSubTable, data.frame(Reihenfolge, Anzahl))
    j = j + 1
  }
  View(PrioSubTable)
  print(sum(PrioSubTable$Anzahl))
  
  PrioSubTableOver5 = PrioSubTable[PrioSubTable$Anzahl >= 5, ]
  View(PrioSubTableOver5)
  
  PrioSubTable = PrioSubTable[order(PrioSubTable$Anzahl),]
  
  #write_xlsx(PrioSubTableOver5, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungs_Reihenfolge_Anzahl_ueber_5_1_3.xlsx")
  #write_xlsx(PrioSubTableOver5, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungs_Reihenfolge_Anzahl_ueber_5_1_3_schoener.xlsx")
  #write_xlsx(PrioSubTable, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungs_Reihenfolge_alle_1_3_schoener.xlsx")    
  #write_xlsx(PrioSubTableOver5, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungs_Reihenfolge_Anzahl_ueber_5_1_3_schoener_ordered.xlsx")
  #write_xlsx(PrioSubTable, "C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Priorisierungs_Reihenfolge_alle_1_3_schoener_ordered.xlsx")    
  
  PrioSubTableOver5 = PrioSubTableOver5[order(PrioSubTableOver5$Anzahl),]
  
  pal <- colorRampPalette(colors = c("lightgreen", "lightgreen"))(14)
  pal <- colorRampPalette(colors = c("lightblue", "blue"))(14)
  par(mar=c(3,6,1,0)+1.4)
  par(mgp=c(2,0.5,0))
  prioplot <- barplot( PrioSubTableOver5$Anzahl,
                       names.arg = PrioSubTableOver5$Reihenfolge,
                       cex.names=.9, horiz=TRUE, las = 1, beside=FALSE,
                       #col = "#5c00cc",
                       #col = coul,
                       col = pal,lwd = 3,xlim=c(0,300),
                       main = "Priorisierungen/Gew?hlte Reihenfolgen (absolut)",xlab = "Anzahl",)
  text(PrioSubTableOver5$Anzahl+8,prioplot,  paste(PrioSubTableOver5$Anzahl, sep=""), col = "red" ,cex=1.0) 
  #Priorisierungsverteilung_alle_Lightgreen_mit_Number_klein_less_margin
  #Priorisierungsverteilung_alle_blau_Lightblau_mit_Number_klein
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#_____________________________________________________________________________________________


# Logik auf Daten
# my_data$MeineSpalte <- ifelse(my_data$Bach == 13, "dreizehn", "andereZahl")
# Addieren von Werte in neue Spalte
# my_data$Summe <- my_data$Mozart + my_data$Beethoven + my_data$Bach

# Datentypen der Spalten

dateBegin <- demografische_Daten$`Date_started (deprecated, scheint fehlerhaft und viel zu früh)`
print(dateBegin[1])
View(dateBegin)
View(demografische_Daten$`Date_started (deprecated, scheint fehlerhaft und viel zu früh)`)

demografische_Daten$`Date_started (deprecated, scheint fehlerhaft und viel zu früh)` <- as.POSIXct(demografische_Daten$`Date_started (deprecated, scheint fehlerhaft und viel zu früh)`, format = '%d%b%Y:%H:%M:%S')

cl = length(demografische_Daten$`Date_started (deprecated, scheint fehlerhaft und viel zu früh)`)
print(cl)

h <- 1
while (h < cl + 1) {
  xd = demografische_Daten$`Date_started (deprecated, scheint fehlerhaft und viel zu früh)`[h]
  c <- hour(xd)
  hour(xd) <- c + 1
  print(xd)
  demografische_Daten$`Date_started (deprecated, scheint fehlerhaft und viel zu früh)`[h] = xd
  h <- h + 1 
}

h <- 429
while (h < 957) {
  xd = demografische_Daten$`Date_started (deprecated, scheint fehlerhaft und viel zu früh)`[h]
  c <- hour(xd)
  hour(xd) <- c + 1
  print(xd)
  demografische_Daten$`Date_started (deprecated, scheint fehlerhaft und viel zu früh)`[h] = xd
  h <- h + 1 
}

View(demografische_Daten$`Date_started (deprecated, scheint fehlerhaft und viel zu früh)`)

write_xlsx(demografische_Daten, "C:/Users/Jonas/Desktop/Relevanzwahrnehmung_demografische_Daten_Date_chenged.xlsx")



demografische_Daten$Bearbeitungszeit = (demografische_Daten$`Date_last_action (deprecated, scheint eher den Start des jeweiligen Teilnehmers anzuzeigen)` - demografische_Daten$`Date_started (deprecated, scheint fehlerhaft und viel zu früh)`) / 60

names(demografische_Daten)[10] = "Bearbeitungszeit (in min.)"


gesamteBearbeitungszeit <- sum(demografische_Daten$`Bearbeitungszeit (in min.)`)
print(gesamteBearbeitungszeit)

durchschnittlicheBearbeitungszeit = gesamteBearbeitungszeit / (cl)
print(durchschnittlicheBearbeitungszeit)

medianBearbeitungszeit = median(demografische_Daten$`Bearbeitungszeit (in min.)`)
maxBearbeitungszeit = max(demografische_Daten$`Bearbeitungszeit (in min.)`)
print(medianBearbeitungszeit)
print(maxBearbeitungszeit)



d = NULL
Bearbeitungszeit <- 0
while(Bearbeitungszeit < 7) {
  print(Bearbeitungszeit)
  alterColumnZw <- demografische_Daten[demografische_Daten$`Bearbeitungszeit (in min.)` == Bearbeitungszeit,]
  alterColumnZwAlter <- alterColumnZw$`Bearbeitungszeit (in min.)`
  Anzahl <- length(alterColumnZwAlter)
  print(Anzahl)
  d = rbind(d, data.frame(Bearbeitungszeit, Anzahl))
  Bearbeitungszeit <- Bearbeitungszeit + 1
}

View(d)

write_xlsx(d, "C:/Users/Jonas/Desktop/Bearbeitungszeitverteilung.xlsx")

df2 <- data_frame(name = d$Bearbeitungszeit, count = d$Anzahl)
names <- df2$name
print(length(names))
#lab <- rep(NA, length(names))
#lab[seq(1, length(names), by=5)] <- names[seq(1, length(names), by=5)]

p2<-barplot(df2$count, 
            #col = rainbow(10), 
            #col = "#5c00cc",
            col = coul,
            #col = rainbow(15),
            ylim=c(0,750), names.arg = names, 
            space=1, axes=F, 
            main = "Bearbeitungszeitverteilung",
            xlab = "Bearbeitungszeit",
            ylab = "Anzahl")
xval = seq(0, 750, 50)
#yval = seq(0, 120, 10)
axis(side = 2, at = xval, labels = FALSE, xpd=T)
axis(side = 2, at = xval, tick = FALSE, labels = xval, xpd=T)
mtext("(in Minuten)", side=1, line=4)
#text(p2, df2$count+22 , paste(df2$count, sep=""), col = "red" ,cex=0.8) 


#Bearbeitungszeitverteilung bunt mit Zahlen klein




#_________________________________________________________________________________________
#                   Altmetric Bekannt?
#


altmetric <- demografische_Daten[demografische_Daten$`Knew Altmetric Badges before (agree = yes)` == "agree",]
altmetric <- altmetric[!(is.na(altmetric$`Knew Altmetric Badges before (agree = yes)`) | altmetric$`Knew Altmetric Badges before (agree = yes)`==""), ]
View(altmetric)
altmetricColumn <- altmetric$`Knew Altmetric Badges before (agree = yes)`
View(altmetricColumn)
Anzahl <- length(altmetricColumn)
print(Anzahl) # 65 (mit NA bei Gender)

#altmetricCategories <- unique(demografische_Daten$`Knew Altmetric Badges before (agree = yes)`) 
#print(altmetricCategories)

altmetric <- altmetric[!(is.na(altmetric$Gender) | altmetric$Gender==""), ]
altmetricUndMale <- altmetric[altmetric$Gender == "Male" | altmetric$Gender == "Männlich" ,]
altmetricUndFemale <- altmetric[altmetric$Gender == "Female" | altmetric$Gender == "Weiblich" ,]
altmetricUndOther <- altmetric[altmetric$Gender != "Female" & altmetric$Gender != "Weiblich" &
                                 altmetric$Gender != "Male" & altmetric$Gender != "Männlich" ,]
View(altmetricUndMale)
View(altmetricUndFemale)
View(altmetricUndOther)

altmetricUndMaleColumn <- altmetricUndMale$`Knew Altmetric Badges before (agree = yes)`
altmetricUndFemaleColumn <- altmetricUndFemale$`Knew Altmetric Badges before (agree = yes)`
altmetricUndOtherColumn <- altmetricUndOther$`Knew Altmetric Badges before (agree = yes)`
AnzahlMale <- length(altmetricUndMaleColumn)
AnzahlFemale <- length(altmetricUndFemaleColumn)
AnzahlOther <- length(altmetricUndOtherColumn)
print(AnzahlMale) # 21
print(AnzahlFemale) # 29
print(AnzahlOther) # 10



# Barplot

altmetric <- demografische_Daten[demografische_Daten$`Knew Altmetric Badges before (agree = yes)` == "agree",]
altmetric <- altmetric[!(is.na(altmetric$`Knew Altmetric Badges before (agree = yes)`) | altmetric$`Knew Altmetric Badges before (agree = yes)`==""), ]

View(altmetric)

educationCategories2 <- unique(demografische_Daten$Education) 
educationCategories2 = NULL
educationCategories2[1] = "Other"
educationCategories2[2] = "Anderer"
educationCategories2[3] = "Kein Schulabschluss"
educationCategories2[4] = "Grund-/Hauptschulabschluss"
educationCategories2[5] = "Realschule (Mittlere Reife)"
educationCategories2[6] = "Gymnasium (Abitur)"
educationCategories2[7] = "Abgeschlossene Ausbildung"
educationCategories2[8] = "Fachhochschulabschluss"
educationCategories2[9] = "Bachelor"
educationCategories2[10] = "Diplom"
educationCategories2[11] = "Master"
educationCategories2[12] = "Promotion"
educationCategories2[13] = "PhD"

numberOfeducationCategories2 <- length(educationCategories2)
bildungsabschlussAnzahl2 <- NULL
altmetric <- altmetric[!(is.na(altmetric$Education) | altmetric$Education==""), ]
i <- 1
while (i < numberOfeducationCategories2 + 1) {
  print(educationCategories2[i])
  Bildungsabschluss <- educationCategories2[i]
  educationColumnZw <- altmetric[altmetric$Education == educationCategories2[i],]
  
  educationColumnZwEducation <- educationColumnZw$Education
  Anzahl <- length(educationColumnZwEducation)
  print(Anzahl)
  bildungsabschlussAnzahl2 = rbind(bildungsabschlussAnzahl2, data.frame(Bildungsabschluss, Anzahl))
  i <- i + 1
}
View(bildungsabschlussAnzahl2)

write_xlsx(bildungsabschlussAnzahl2, "C:/Users/Jonas/Desktop/AltmetricBekanntEducationVerteilung.xlsx")

par(mar=c(3,9,2,1)+1.2)
barplot(bildungsabschlussAnzahl2$Anzahl, 
        names.arg = educationCategories2,
        cex.names=.8,
        horiz=TRUE,
        las = 1,
        #col = "#5c00cc",
        #col = coul,
        col = rainbow(16),
        lwd = 3,
        xlim=c(0,14),
        main = "Altmetric Bekanntheit nach Bildungsabschluss",
        xlab = "Anzahl")

bildungsabschlussAnzahl2 <- NULL
bildungsabschlussAnzahl2 <- NULL








#_____________Wissenschaftlich gearbeitet________________________________________________________


wissGearbeitet <- demografische_Daten[demografische_Daten$`Published scientifically before (agree = yes)` == "agree",]
wissGearbeitet <- wissGearbeitet[!(is.na(wissGearbeitet$`Published scientifically before (agree = yes)`) | wissGearbeitet$`Published scientifically before (agree = yes)`==""), ]
View(wissGearbeitet)
wissGearbeitetColumn <- wissGearbeitet$`Published scientifically before (agree = yes)`
View(wissGearbeitetColumn)
Anzahl <- length(wissGearbeitetColumn)
print(Anzahl) # 144 (mit den NA bei Gender)

#wissGearbeitetCategories <- unique(demografische_Daten$`Published scientifically before (agree = yes)`) 
#print(wissGearbeitetCategories)

wissGearbeitet <- wissGearbeitet[!(is.na(wissGearbeitet$Gender) | wissGearbeitet$Gender==""), ]
wissGearbeitetUndMale <- wissGearbeitet[wissGearbeitet$Gender == "Male" | wissGearbeitet$Gender == "Männlich" ,]
wissGearbeitetUndFemale <- wissGearbeitet[wissGearbeitet$Gender == "Female" | wissGearbeitet$Gender == "Weiblich" ,]
wissGearbeitetUndOther <- wissGearbeitet[wissGearbeitet$Gender != "Female" & wissGearbeitet$Gender != "Weiblich" &
                                           wissGearbeitet$Gender != "Male" & wissGearbeitet$Gender != "Männlich" ,]
View(wissGearbeitetUndMale)
View(wissGearbeitetUndFemale)
View(wissGearbeitetUndOther)

wissGearbeitetUndMaleColumn <- wissGearbeitetUndMale$`Published scientifically before (agree = yes)`
wissGearbeitetUndFemaleColumn <- wissGearbeitetUndFemale$`Published scientifically before (agree = yes)`
wissGearbeitetUndOtherColumn <- wissGearbeitetUndOther$`Published scientifically before (agree = yes)`
AnzahlMale <- length(wissGearbeitetUndMaleColumn)
AnzahlFemale <- length(wissGearbeitetUndFemaleColumn)
AnzahlOther <- length(wissGearbeitetUndOtherColumn)
print(AnzahlMale) # 60
print(AnzahlFemale) # 64
print(AnzahlOther) # 13




# Barplot

wissGearbeitet <- demografische_Daten[demografische_Daten$`Published scientifically before (agree = yes)` == "agree",]
wissGearbeitet <- wissGearbeitet[!(is.na(wissGearbeitet$`Published scientifically before (agree = yes)`) | wissGearbeitet$`Published scientifically before (agree = yes)`==""), ]

View(wissGearbeitet)

numberOfeducationCategories2 <- length(educationCategories2)
bildungsabschlussAnzahl2 <- NULL
wissGearbeitet <- wissGearbeitet[!(is.na(wissGearbeitet$Education) | wissGearbeitet$Education==""), ]
i <- 1
while (i < numberOfeducationCategories2 + 1) {
  print(educationCategories2[i])
  Bildungsabschluss <- educationCategories2[i]
  educationColumnZw <- wissGearbeitet[wissGearbeitet$Education == educationCategories2[i],]
  
  educationColumnZwEducation <- educationColumnZw$Education
  Anzahl <- length(educationColumnZwEducation)
  print(Anzahl)
  bildungsabschlussAnzahl2 = rbind(bildungsabschlussAnzahl2, data.frame(Bildungsabschluss, Anzahl))
  i <- i + 1
}
View(bildungsabschlussAnzahl2)
print(sum(bildungsabschlussAnzahl2$Anzahl)) # 126

write_xlsx(bildungsabschlussAnzahl2, "C:/Users/Jonas/Desktop/BereisWissenschaftlichGearbeitetEducationVerteilung.xlsx")

par(mar=c(3,9,2,1)+1.2)
barplot(bildungsabschlussAnzahl2$Anzahl, 
        names.arg = educationCategories2,
        cex.names=.8,
        horiz=TRUE,
        las = 1,
        #col = "#5c00cc",
        col = coul,
        #col = rainbow(15),
        lwd = 3,
        xlim=c(0,45),
        main = "Bereits wiss. gearbeitet (nach Bildungsabschluss)",
        xlab = "Anzahl")
axis(1, at = seq(0, 45, 5), las = 1)

bildungsabschlussAnzahl2 <- NULL





#___________Altmetric und wiss. gearbeitet?_______________________
#
#
altmetricUndWissGearbeitet <- demografische_Daten[demografische_Daten$`Knew Altmetric Badges before (agree = yes)` == "agree" & demografische_Daten$`Published scientifically before (agree = yes)` == "agree",]
View(altmetricUndWissGearbeitet)
altmetricUndWissGearbeitet <- altmetricUndWissGearbeitet[!(is.na(altmetricUndWissGearbeitet$`Knew Altmetric Badges before (agree = yes)`) | altmetricUndWissGearbeitet$`Knew Altmetric Badges before (agree = yes)`=="" | is.na(altmetricUndWissGearbeitet$`Published scientifically before (agree = yes)`) | altmetricUndWissGearbeitet$`Published scientifically before (agree = yes)`==""), ]
View(altmetricUndWissGearbeitet)
altmetricUndWissGearbeitetColumn <- altmetricUndWissGearbeitet$`Knew Altmetric Badges before (agree = yes)`
View(altmetricUndWissGearbeitetColumn)
Anzahl <- length(altmetricUndWissGearbeitetColumn)
print(Anzahl) # 41


wissGearbeitet <- altmetricUndWissGearbeitet[!(is.na(altmetricUndWissGearbeitet$Gender) | altmetricUndWissGearbeitet$Gender==""), ]
wissGearbeitetUndMale <- wissGearbeitet[wissGearbeitet$Gender == "Male" | wissGearbeitet$Gender == "Männlich" ,]
wissGearbeitetUndFemale <- wissGearbeitet[wissGearbeitet$Gender == "Female" | wissGearbeitet$Gender == "Weiblich" ,]
wissGearbeitetUndOther <- wissGearbeitet[wissGearbeitet$Gender != "Female" & wissGearbeitet$Gender != "Weiblich" &
                                           wissGearbeitet$Gender != "Male" & wissGearbeitet$Gender != "Männlich" ,]
View(wissGearbeitetUndMale)
View(wissGearbeitetUndFemale)
View(wissGearbeitetUndOther)

wissGearbeitetUndMaleColumn <- wissGearbeitetUndMale$`Published scientifically before (agree = yes)`
wissGearbeitetUndFemaleColumn <- wissGearbeitetUndFemale$`Published scientifically before (agree = yes)`
wissGearbeitetUndOtherColumn <- wissGearbeitetUndOther$`Published scientifically before (agree = yes)`
AnzahlMale <- length(wissGearbeitetUndMaleColumn)
AnzahlFemale <- length(wissGearbeitetUndFemaleColumn)
AnzahlOther <- length(wissGearbeitetUndOtherColumn)
print(AnzahlMale) # 15
print(AnzahlFemale) # 17
print(AnzahlOther) # 6




# Barplot

altmetricUndWissGearbeitet <- demografische_Daten[demografische_Daten$`Knew Altmetric Badges before (agree = yes)` == "agree" & demografische_Daten$`Published scientifically before (agree = yes)` == "agree",]
altmetricUndWissGearbeitet <- altmetricUndWissGearbeitet[!(is.na(altmetricUndWissGearbeitet$`Knew Altmetric Badges before (agree = yes)`) | altmetricUndWissGearbeitet$`Knew Altmetric Badges before (agree = yes)`=="" | is.na(altmetricUndWissGearbeitet$`Published scientifically before (agree = yes)`) | altmetricUndWissGearbeitet$`Published scientifically before (agree = yes)`==""), ]

View(altmetricUndWissGearbeitet)

numberOfeducationCategories2 <- length(educationCategories2)
bildungsabschlussAnzahl2 <- NULL
altmetricUndWissGearbeitet <- altmetricUndWissGearbeitet[!(is.na(altmetricUndWissGearbeitet$Education) | altmetricUndWissGearbeitet$Education==""), ]
View(altmetricUndWissGearbeitet)
i <- 1
while (i < numberOfeducationCategories2 + 1) {
  print(educationCategories2[i])
  Bildungsabschluss <- educationCategories2[i]
  educationColumnZw <- altmetricUndWissGearbeitet[altmetricUndWissGearbeitet$Education == educationCategories2[i],]
  
  educationColumnZwEducation <- educationColumnZw$Education
  Anzahl <- length(educationColumnZwEducation)
  print(Anzahl)
  bildungsabschlussAnzahl2 = rbind(bildungsabschlussAnzahl2, data.frame(Bildungsabschluss, Anzahl))
  i <- i + 1
}
View(bildungsabschlussAnzahl2)
print(sum(bildungsabschlussAnzahl2$Anzahl)) # 126

write_xlsx(bildungsabschlussAnzahl2, "C:/Users/Jonas/Desktop/BereisWissenschaftlichGearbeitetUndAltmetricBekanntEducationVerteilung.xlsx")

par(mar=c(3,9,2,1)+1.2)
barplot(bildungsabschlussAnzahl2$Anzahl, 
        names.arg = educationCategories2,
        cex.names=.8,
        horiz=TRUE,
        las = 1,
        #col = "#5c00cc",
        #col = coul,
        #col = rainbow(15),
        lwd = 3,
        xlim=c(0,14),
        main = "Wiss. gearbeitet & Altmetric bekannt (nach Bildungsabschluss)",
        xlab = "Anzahl")
axis(1, at = seq(0, 16, 2), las = 1)

bildungsabschlussAnzahl2 <- NULL










#______________________________________________________________________________________________
# Gender
#
View(demografische_Daten)


str(demografische_Daten)
View(demografische_Daten)

# males <- demografische_Daten[demografische_Daten$Gender == "Male" || demografische_Daten$Gender == "Männlich" ,]
males <- demografische_Daten[demografische_Daten$Gender == "Male" | demografische_Daten$Gender == "Männlich" ,]
females <- demografische_Daten[demografische_Daten$Gender == "Weiblich" | demografische_Daten$Gender == "Female" ,]
#males <- na.omit(mutate_all(males, ~ifelse(. %in% c("N/A", "null", ""),  NA, .)))
males <- males[!(is.na(males$Gender) | males$Gender==""), ]
females <- females[!(is.na(females$Gender) | females$Gender==""), ]

naGender <- demografische_Daten %>% filter_all(any_vars(is.na(Gender))) 

#otherGender <- demografische_Daten[demografische_Daten$Gender=="Männlich", ]
View(naGender)

otherGender <- demografische_Daten[demografische_Daten$Gender != "Weiblich" & demografische_Daten$Gender != "Männlich"
                                   & demografische_Daten$Gender != "Female" & demografische_Daten$Gender != "Male" ,]
#View(otherGender)
otherGender <- otherGender[!(is.na(otherGender$Gender) | otherGender$Gender==""), ]

View(otherGender)
View(males)
View(females)

otherGenderColumn <- otherGender$Gender
otherCount = length(otherGenderColumn)
print(otherCount)

naGenderColumn <- naGender$Gender
naCount = length(naGenderColumn)
print(naCount)

femalesGenderColumn <- females$Gender
femalesCount = length(femalesGenderColumn)
print(femalesCount)

malesGenderColumn <- males$Gender
malesCount = length(malesGenderColumn)
print(malesCount)

write_xlsx(males, "C:/Users/Jonas/Desktop/Gender_Results/males.xlsx")
write_xlsx(females, "C:/Users/Jonas/Desktop/Gender_Results/females.xlsx")
write_xlsx(naGender, "C:/Users/Jonas/Desktop/Gender_Results/na.xlsx")
write_xlsx(otherGender, "C:/Users/Jonas/Desktop/Gender_Results/other.xlsx")

genderName <- c('Male', 'Female', 'Others', 'NA')
genderCount <- c(malesCount, femalesCount, otherCount, naCount)
genders.data <- data.frame(genderName, genderCount)
str(genders.data)
View(genders.data)

# subset <- t(data.frame(genders.data$genderName, genders.data$genderCount))
# barplot(subset, legend = c("Rtime", "Btime"), names.arg=genders.data$Input, log="y", beside=TRUE)
# ggplot2.barplot(data=genders.data, xName="time", yName='total_bill')
#p <- ggplot(genders.data, aes(genderName, genderCount))
#p +geom_bar(stat = "identity")


df <- data_frame(name = genders.data$genderName, count = genders.data$genderCount)
p<-barplot(df$count, col = rainbow(10), ylim=c(0,700), names.arg = df$name, space=1, axes=F, main = "Verteilung nach Gender",
           xlab = "Gender",
           ylab = "Anzahl")
xval = seq(0, 700, 50)
axis(side = 2, at = xval, labels = FALSE, xpd=T)
axis(side = 2, at = xval, tick = FALSE, labels = xval, xpd=T)

genderBar <- genders.data$genderCount

barplot(genderBar, 
        col = "#5c00cc",
        lwd = 4,
        main = "Verteilung nach Gender",
        xlab = "Gender",
        ylab = "Anzahl")





#_______________________________________________________________________________________________
# Alter
#

View(demografische_Daten)

#alter <- demografische_Daten$Birthyear
alter <- demografische_Daten[!(is.na(demografische_Daten$Birthyear) | demografische_Daten$Birthyear==0 | demografische_Daten$Birthyear==""), ]
alter$Birthyear <- 2019 - alter$Birthyear
View(alter)

alterColumn <- alter$Birthyear # data_frame(alter$Birthyear)
anzahlAlterAngaben = length(alterColumn)
summeAlterAllerPersonen = sum(alterColumn) 
print(summeAlterAllerPersonen) # 12947
print(anzahlAlterAngaben) # 359
durchschnittsalter = summeAlterAllerPersonen/anzahlAlterAngaben
print(durchschnittsalter) # 36,06407

write_xlsx(alter, "C:/Users/Jonas/Desktop/Alter_Results/Alter.xlsx")
write_xlsx(alterColumn, "C:/Users/Jonas/Desktop/Alter_Results/AlterSpalte.xlsx")


d = NULL
Alter <- 0
while(Alter < 120) {
  print(Alter)
  alterColumnZw <- alter[alter$Birthyear == Alter,]
  alterColumnZwAlter <- alterColumnZw$Birthyear
  Anzahl <- length(alterColumnZwAlter)
  print(Anzahl)
  d = rbind(d, data.frame(Alter, Anzahl))
  Alter <- Alter + 1
}

View(d)

write_xlsx(d, "C:/Users/Jonas/Desktop/Alter_Results/Altersverteilung.xlsx")

df2 <- data_frame(name = d$Alter, count = d$Anzahl)
names <- df2$name
print(length(names))
lab <- rep(NA, length(names))
lab[seq(1, length(names), by=5)] <- names[seq(1, length(names), by=5)]

p2<-barplot(df2$count, col = rainbow(120), ylim=c(0,20), names.arg = lab, space=1, axes=F, main = "Altersverteilung",
            xlab = "Alter",
            ylab = "Anzahl")
xval = seq(0, 20, 5)
yval = seq(0, 120, 10)
axis(side = 2, at = xval, labels = FALSE, xpd=T)
axis(side = 2, at = xval, tick = FALSE, labels = xval, xpd=T)

View(alterColumn)


View(demografische_Daten)





#__________________________________________________________________________________________________
# Bildungsabschluss
#
educationCategories <- unique(demografische_Daten$Education) 
print(educationCategories)
educationCategories[1] = "NA"
educationCategories[2] = "Other"
educationCategories[3] = "Anderer"
educationCategories[4] = "Kein Schulabschluss"
educationCategories[5] = "Grund-/Hauptschulabschluss"
educationCategories[6] = "Realschule (Mittlere Reife)"
educationCategories[7] = "Gymnasium (Abitur)"
educationCategories[8] = "Abgeschlossene Ausbildung"
educationCategories[9] = "Fachhochschulabschluss"
educationCategories[10] = "Bachelor"
educationCategories[11] = "Diplom"
educationCategories[12] = "Master"
educationCategories[13] = "Promotion"
educationCategories[14] = "PhD"

print(educationCategories)
numberOfeducationCategories <- length(educationCategories)
View(educationCategories)
print(numberOfeducationCategories)

naEducation <- demografische_Daten %>% filter_all(any_vars(is.na(Education))) 
View(naEducation)
#otherGender[!(is.na(otherGender$Gender) | otherGender$Gender==""), ]

education <- demografische_Daten[!(is.na(demografische_Daten$Education) | demografische_Daten$Education==""), ]
View(education)


bildungsabschlussAnzahl = NULL

naEducationColumn <- naEducation$Education
Anzahl <- length(naEducationColumn)
Bildungsabschluss <- educationCategories[1]
print(naEducationAnzahl)
bildungsabschlussAnzahl = rbind(bildungsabschlussAnzahl, data.frame(Bildungsabschluss, Anzahl))


i <- 2
print(educationCategories[2])
while (i < numberOfeducationCategories + 1) {
  print(educationCategories[i])
  Bildungsabschluss <- educationCategories[i]
  educationColumnZw <- education[education$Education == educationCategories[i],]
  educationColumnZwEducation <- educationColumnZw$Education
  Anzahl <- length(educationColumnZwEducation)
  print(Anzahl)
  bildungsabschlussAnzahl = rbind(bildungsabschlussAnzahl, data.frame(Bildungsabschluss, Anzahl))
  
  i <- i + 1
}
View(bildungsabschlussAnzahl)
print(bildungsabschlussAnzahl)

write_xlsx(education, "C:/Users/Jonas/Desktop/Bildungsabschluss_Results/richtig/BildungsabschlusstabelleOhneNA.xlsx")
write_xlsx(bildungsabschlussAnzahl, "C:/Users/Jonas/Desktop/Bildungsabschluss_Results/richtig/Bildungsabschlussverteilung.xlsx")




par(mar=c(3,9,2,1)+1.2)
barplot(bildungsabschlussAnzahl$Anzahl, 
        names.arg = educationCategories,
        #names.arg = bildungsabschlussAnzahl2$Bildungsabschluss,
        cex.names=.8,
        horiz=TRUE,
        las = 1,
        #col = "#5c00cc",
        col = coul,
        #col = rainbow(15),
        lwd = 3,
        xlim=c(0,800),
        main = "Verteilung nach höchstem Bidlungsabschluss",
        xlab = "Anzahl")
#ylab = "Anzahl")
#mtext("Top axis", side=1, line=3)








# OHNE NA
#____________________________

coul <- brewer.pal(5, "Set2") 

educationCategories2 <- unique(demografische_Daten$Education) 
educationCategories2 = NULL
print(educationCategories)
educationCategories2[1] = "Other"
educationCategories2[2] = "Anderer"
educationCategories2[3] = "Kein Schulabschluss"
educationCategories2[4] = "Grund-/Hauptschulabschluss"
educationCategories2[5] = "Realschule (Mittlere Reife)"
educationCategories2[6] = "Gymnasium (Abitur)"
educationCategories2[7] = "Abgeschlossene Ausbildung"
educationCategories2[8] = "Fachhochschulabschluss"
educationCategories2[9] = "Bachelor"
educationCategories2[10] = "Diplom"
educationCategories2[11] = "Master"
educationCategories2[12] = "Promotion"
educationCategories2[13] = "PhD"


numberOfeducationCategories2 <- length(educationCategories2)
bildungsabschlussAnzahl2 <- NULL

i <- 1
print(educationCategories2[2])
while (i < numberOfeducationCategories2 + 1) {
  print(educationCategories2[i])
  Bildungsabschluss <- educationCategories2[i]
  educationColumnZw <- education[education$Education == educationCategories2[i],]
  educationColumnZwEducation <- educationColumnZw$Education
  Anzahl <- length(educationColumnZwEducation)
  print(Anzahl)
  bildungsabschlussAnzahl2 = rbind(bildungsabschlussAnzahl2, data.frame(Bildungsabschluss, Anzahl))
  i <- i + 1
}
View(bildungsabschlussAnzahl2)
View(educationCategories2)


par(mar=c(3,11,2,1)+1.2)
xx <- barplot(bildungsabschlussAnzahl2$Anzahl, 
              names.arg = educationCategories2,
              cex.names=1,
              horiz=TRUE,
              las = 1,
              #col = "#5c00cc",
              col = coul,
              #col = rainbow(15),
              lwd = 2,
              xlim=c(0,90),
              main = "Verteilung nach höchstem Bidlungsabschluss",
              xlab = "Anzahl")
#mtext("Top axis", side=1, line=3)
text(bildungsabschlussAnzahl2$Anzahl+2, xx , paste(bildungsabschlussAnzahl2$Anzahl, sep=""), col = "red" ,cex=0.8) 
axis(1, at = seq(0, 90, 10), las = 1)



#plot <- data_frame(name = bildungsabschlussAnzahl$Bildungsabschluss, count = bildungsabschlussAnzahl$Anzahl)
#names <- plot$name
#print(length(names))
#lab <- rep(NA, length(names))
#lab[seq(1, length(names), by=5)] <- names[seq(1, length(names), by=5)]

#p2<-barplot(plot$count, col = rainbow(14), ylim=c(0,800), names.arg = names, space=1, axes=F, main = "Bildungsabschlussverteilung",xlab = "Höchster Bildungsabschluss", ylab = "Anzahl")
#xval = seq(0, 800, 100)
#yval = seq(0, 120, 10)
#axis(side = 2, at = xval, labels = FALSE, xpd=T)
#axis(side = 2, at = xval, tick = FALSE, labels = xval, xpd=T)



