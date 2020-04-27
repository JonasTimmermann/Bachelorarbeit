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


demografische_Daten <- read_excel("C:/Users/Jonas/Desktop/Relevanzwahrnehmung_demografische_Daten.xlsx")

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



















#_______________________________________________________________________________________________

germanyGesamtCaseZahl = sum(germany$cases)

germanyMaxCaseZahl = max(germany$cases)

germ <- germany[4:8,]
germCases <- germ$cases
View(germCases)

casesPerDayGermany <- germany$cases
View(casesPerDayGermany)
barplot(casesPerDayGermany, 
        col = "#5c00cc",
        lwd = 4,
        main = "Corona-Fallzahlen pro Tag",
        xlab = "Tag",
        ylab = "Anzahl")

View(germanyGesamtCaseZahl)
View(germanyMaxCaseZahl)

x = length(casesPerDayGermany) # nrow
print(x)
#i = 0
#for(i in c:x){
#  x = x - 1
#  print(x)}
germany$totalCases <- germany$cases 
View(germany)
i <- 1
while(i < x) {
  print(i)
  germ <- germany[1:x,]
  germCases <- germ$cases[i:x]
  sumOfGerms <- sum(germCases)
  germany$totalCases[i] = sumOfGerms 
  print(sumOfGerms)
  i <- i+1
}
View(germany)

germany$totalDeath <- NULL

germanyTotalCases <- germany$totalCases
View (germanyTotalCases)
germanyTotalCasesFlip <- germany[nrow(germany):1,]
View (germanyTotalCasesFlip)
head(germany)

write_xlsx(germany, "C:/Users/Jonas/Desktop/COVID-19-germany.xlsx")
write_xlsx(germanyTotalCasesFlip, "C:/Users/Jonas/Desktop/COVID-19-germany-flipped.xlsx")

write_xlsx(data, "data/cereal-yield-1990-to-1995.xlsx")
# germany$totalDeath <- germany$cases 





# Gibt Zeile zurück
bachWithOver <- my_data[my_data$Month == "2009-09",]
# Gibt spalte 2 zurück
bachWithOver <- my_data[2]

# gibt alle zeilen zurück wo Mozart >= 12
bachWithOver <- my_data[my_data$Mozart >= 12,]

# Addiert die Werte von den bach/Mozart Spalten
BachAndMozartAddiert <- bachWithOver$Mozart + bachWithOver$Bach

# Extrahiert die 2 Spalten von der ganzen Tabelle
onlyBachMozart <- data.frame(bachWithOver$Mozart,bachWithOver$Bach)

view(onlyBachMozart)

# Umbenennung der Spalten
colnames(onlyBachMozart) <- c("Mozart", "Bach")

# col=Farbe, lwd=Punktgröße
plot(onlyBachMozart,
     col = "#5c00cc",
     lwd = 4,
     main = "Mozart-Bachberhältnis",
     xlab = "MozartZahl",
     ylab = "BachZahl")

view(onlyBachMozart)
view(BachAndMozartAddiert)
view(bachWithOver)

# gibt tabelle wieder mit allen Reihen bei denen in Spalte "Bach" == 12 steht (Filtern nach Reihen geht hinter dem Komma)
bachWithOver <- my_data[my_data$Bach == 12,]

view(bachWithOver)


head(my_data) 
summary(my_data)  
plot(my_data)

plot(my_data$Mozart)  # Cat x quant


