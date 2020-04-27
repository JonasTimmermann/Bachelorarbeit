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
install.packages("aod")
library(aod)
library(ggplot2)
install.packages("chron")
library(chron)




mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
View(mydata)


summary(mydata)
sapply(mydata, sd) # Standartabweichung

xtabs(~admit + rank, data = mydata) # Gesamt-Anzahl der Einträge für jeweiligen Rank

mydata$rank <- factor(mydata$rank) # Convert rank to categorical variable

mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial") # Logistische Regression anwenden
View(mylogit)
summary(mylogit)
confint(mylogit) # Konfidenz Intervalle (Log-Likelihood)

confint.default(mylogit) # CIs using standard errors
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6) # overall effect of rank

l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)

exp(coef(mylogit))
exp(cbind(OR = coef(mylogit), confint(mylogit))) # Gibt die gewünschten Regressions Infos (anschaulicher)




## Prediction ##
newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
newdata1 # view data frame

newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1  # shows predicted probability (ranP) being acceptet by the Uni (Rank 1 is most likely [52%])



# Further Prediction with various gre, gpa
newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                              4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))

newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

## view first few rows of final dataset
head(newdata3)
View(newdata3)

# Plot the "Prediction probabilities" (Strich = probabilities, Flächen = Konfidenzintervall)
ggplot(newdata3, aes(x = gre, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = 0.2) +
  geom_line(aes(colour = rank),
                                                                                                                      size = 1)


# Freiheitsgrade
with(mylogit, null.deviance - deviance) # chi-square of 41.46    # model with predictors fits significantly better than a model with just an intercept (i.e., a null model). The test statistic is the difference between the residual deviance for the model with predictors and the null model. 
with(mylogit, df.null - df.residual) # 5 degrees of freedom      # number of predictor variables
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) #p-value of less than 0.001
# chi-square of 41.46 with 5 degrees of freedom and an associated 
# p-value of less than 0.001 tells us that our model as a whole fits significantly better 
# than an empty model.
logLik(mylogit) # To see the model’s log likelihood








### Tabellen für logistische Regressionsanalyse ###


demografische_Daten <- read_excel("C:/Users/Jonas/Desktop/Relevanzwahrnehmung_demografische_Daten_Date_chenged.xlsx")
prioriseriungs_Daten <- read_excel("C:/Users/Jonas/Desktop/Relevanzwahrnehmung_Priorisierung.xlsx")
prioriseriungs_Daten_mit_Zeiten <- read_excel("C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_Mit_Time_Intervall.xlsx")
View(prioriseriungs_Daten_mit_Zeiten) #2068
View(demografische_Daten)
View(prioriseriungs_Daten)

priorisierung_beantwortet_1_3 <- read_excel("C:/Users/Jonas/Desktop/Prio/Priorisierungsdaten_BearbeitungszeitOOO_1_3.xlsx")

ersteFrageSpalte = prioriseriungs_Daten[prioriseriungs_Daten$Question_No == 1,]
zweiteFrageSpalte = prioriseriungs_Daten[prioriseriungs_Daten$Question_No == 2,]
dritteFrageSpalte = prioriseriungs_Daten[prioriseriungs_Daten$Question_No == 3,]

#View(priorisierung_beantwortet_1_3)
#kpp = unique(priorisierung_beantwortet_1_3$Respondent_ID)
#View(kpp)

demoDatenDerPriorisierungen <- subset(demografische_Daten, Respondent_ID %in% priorisierung_beantwortet_1_3$Respondent_ID)
View(demoDatenDerPriorisierungen)

demoDatenDerPriorisierungen$Question1 = demoDatenDerPriorisierungen$Respondent_ID
demoDatenDerPriorisierungen$Question2 = demoDatenDerPriorisierungen$Respondent_ID
demoDatenDerPriorisierungen$Question3 = demoDatenDerPriorisierungen$Respondent_ID
View(demoDatenDerPriorisierungen)

i <- 23
while (i < 1075){
  demoDatenDerPriorisierungen$Question1[demoDatenDerPriorisierungen$Respondent_ID == i] <- ersteFrageSpalte$PubIDs_Order_Chosen[ersteFrageSpalte$Respondent_ID == i]
  demoDatenDerPriorisierungen$Question2[demoDatenDerPriorisierungen$Respondent_ID == i] <- zweiteFrageSpalte$PubIDs_Order_Chosen[zweiteFrageSpalte$Respondent_ID == i]
  demoDatenDerPriorisierungen$Question3[demoDatenDerPriorisierungen$Respondent_ID == i] <- dritteFrageSpalte$PubIDs_Order_Chosen[dritteFrageSpalte$Respondent_ID == i]

  i = i + 1
}

View(demoDatenDerPriorisierungen)


demoDatenDerPriorisierungen_ohne_NA <- demoDatenDerPriorisierungen[!(is.na(demoDatenDerPriorisierungen$Gender) | demoDatenDerPriorisierungen$Gender ==""), ]
demoDatenDerPriorisierungen_ohne_NA <- demoDatenDerPriorisierungen_ohne_NA[!(is.na(demoDatenDerPriorisierungen_ohne_NA$Education) | demoDatenDerPriorisierungen_ohne_NA$Education ==""), ]
View(demoDatenDerPriorisierungen_ohne_NA) # 325

demoDatenDerPriorisierungen_ohne_NA <- demoDatenDerPriorisierungen_ohne_NA[!(is.na(demoDatenDerPriorisierungen_ohne_NA$Birthyear) | demoDatenDerPriorisierungen_ohne_NA$Birthyear == 0), ]
View(demoDatenDerPriorisierungen_ohne_NA) # 312

demoDatenDerPriorisierungen_ohne_NA$Birthyear = 2019 - demoDatenDerPriorisierungen_ohne_NA$Birthyear
names(demoDatenDerPriorisierungen_ohne_NA)[6] <- "Alter"

write_xlsx(demoDatenDerPriorisierungen_ohne_NA, "C:/Users/Jonas/Desktop/Prio/logistische_Regressionsanalyse/demografische_UND_Priorisierungen_ohne_NA.xlsx")
write_xlsx(demoDatenDerPriorisierungen, "C:/Users/Jonas/Desktop/Prio/logistische_Regressionsanalyse/demografische_UND_Priorisierungen_mit_NA.xlsx")

























View(BearbeitungszeitOOO_1_3)

Beantwortet_1_3 <- subset(prioriseriungs_Daten_mit_Zeiten, Respondent_ID %in% BearbeitungszeitOOO_1_3$Respondent_ID)
View(Beantwortet_1_3) # 1815, 605

Beantwortet_1_3_1 = Beantwortet_1_3[Beantwortet_1_3$Question_No == 1,]
View(Beantwortet_1_3_1) #605

Beantwortet_1_3_2 = Beantwortet_1_3[Beantwortet_1_3$Question_No == 2,]
View(Beantwortet_1_3_2) #605

Beantwortet_1_3_3 = Beantwortet_1_3[Beantwortet_1_3$Question_No == 3,]
View(Beantwortet_1_3_3) #605







kp <- 0
i <- minRespondend_Id
while (i < maxRespondend_Id + 1){
  
  zeilenZw = prioriseriungs_Daten[prioriseriungs_Daten$Respondent_ID == i,]
  len = length(zeilenZw$Respondent_ID)
  spa = zeilenZw$`Timestamp (DD-MM-YY)`
  
  spalteZw_1 = zeilenZw[zeilenZw$Question_No == 1,]
  anzahl_1 <- length(spalteZw_1$Question_No)
  

  



