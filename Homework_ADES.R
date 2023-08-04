library(rmf)

#Esercizio 1
table(wvs$v208)
mean(wvs$v208,na.rm=TRUE)
median(wvs$v08,na.rm=TRUE)
x <- c(1:10)
n <- c(910,54,18,7,2,4,2,1,2)
var(wvs$v208,na.rm = TRUE)*((sum(n)-1)/sum(n))
sd(wvs$v208,na.rm = TRUE)*sqrt((sum(n)-1)/sum(n))

#Esercizio 2
table(wvs$v208)
table(wvs$v255)
by(wvs$v208,wvs$v255,mean,na.rm=TRUE)

#Esercizio 3
boxplot(wvs$v255~wvs$v208,xlab="beatwife",ylab="size")

#Esercizio 4
x <- wvs$v118[complete.cases(wvs$v118,wvs$v120)]
#x <- wvs$v118[!is.na(wvs$v118)&!is.na(wvs$v120)] 
cov(wvs$v118,wvs$v120,use="complete.obs")*(length(x)-1)/length(x)
cor(wvs$v118,wvs$v120,use="complete.obs")

#Esercizio 5
dado <- 1:6
n <- 0
nrep <- 100000
for(i in 1:nrep){
  x <- max(sample(dado,5,replace = TRUE))
  if(x>=7){
    n <- n+1
  }
}
n/nrep

#Esercizio 6
n <- 258
p <- 0.06
n*p
pbinom(n*p,n,p,lower.tail = FALSE)
pbinom(18,n,p,lower.tail = FALSE)

#Esercizio 7
mi <- 5.9
sigma <- 5.1
x <- 15.3
z <- (x-mi)/sigma
pnorm(z)-pnorm(-1)

#Esercizio 8
set.seed(231312); x<- sample(-3:5,size=300,replace=TRUE); set.seed(NULL)
#a)
quartile(x)
frequenze(x,cumul = TRUE)
x

#Esercizio 9
#vettori dei contagiati
contagiatiM = c(rep(5,444638),rep(15,667351),rep(25,697747),rep(35,675651),rep(45,791996),rep (55,778178),rep(65,469786),rep(75,305004),rep(85,163360),rep(95,32024)) 
(length(contagiatiM)) #devono essere 5025735
contagiatiF = c(rep(5,413787),rep(15,644080),rep(25,694093),rep(35,743312),rep(45,903908),rep (55,827714),rep(65,466019),rep(75,309141),rep(85,229827),rep(95,93094)) 
(length(contagiatiF)) #devono essere 5324975
contagiatiT = c(rep(5,444638+413787),rep(15,667351+644080),rep(25,697747+694093),rep(35,675651+743312),rep(45,791996+903908),rep(55,778178+827714),rep(65,469786+466019),rep(75,305004+309141),rep(85,163360+229827),rep(95,32024+93094)) 
length(contagiatiT) #devono essere 10350710
#vettori dei morti
mortiM = c(rep(5,7),rep(15,15),rep(25,61),rep(35,222),rep(45,948),rep(55,3808),rep(65,10785) ,rep(75,24300),rep(85,31274),rep(95,9870))
(length(mortiM)) #devono essere 81290
mortiF = c(rep(5,10),rep(15,13),rep(25,34),rep(35,128),rep(45,429),rep(55,1535),rep(65,4278) ,rep(75,11978),rep(85,26480),rep(95,18144))
(length(mortiF)) #devono essere 63029
mortiT = c(rep(5,7+10),rep(15,15+13),rep(25,61+34),rep(35,222+128),rep(45,948+429),rep(55,3808+1535),rep(65,10785+4278),rep(75,24300+11978),rep(85,31274+26480),rep(95,9870+18144))
(length(mortiT)) #devono essere 144319
#per avere tutti i grafici in un'unica schermata per comodit? par(mfrow=c(2,3))
#istogramma per la variabile "et? al contagio" istogramma(contagiatiM,da=0,a=100,nclassi=10, stampa=FALSE, nome="Et? al contagio - Maschi")
istogramma(contagiatiF,da=0,a=100,nclassi=10, stampa=FALSE, nome="Et? al contagio - Femmine")
istogramma(contagiatiT,da=0,a=100,nclassi=10, stampa=FALSE, nome="Et? al contagio - Totali")
#istogramma per la variabile et? al decesso" istogramma(mortiM,da=0,a=100,nclassi=10, stampa=FALSE, nome="Et? al decesso - Maschi")
istogramma(mortiF,da=0,a=100,nclassi=10, stampa=FALSE, nome="Et? al decesso - Femmine")
istogramma(mortiT,da=0,a=100,nclassi=10, stampa=FALSE, nome="Et? al decesso - Totali")
#Osservando i grafici ? possibile notare come le classi d'et? maggiormente contagiate sono quelle centrali, mentre gli anziani hanno probabilit? maggiore di morire, se incontrano il virus.
#chiudo la finestra multipla dei grafici par(mfrow=c(1,1))
#media, mediana e sd per i maschi per la variabile "et? al contagio" (miCM = media(contagiatiM))
frequenze(contagiatiM,cumul=TRUE) #la classe mediana ? 40-49 (medCM = 40+((0.5-0.49453204)/(0.65212014-0.49453204))*10) (sqmCM = sd(contagiatiM, na.rm=TRUE))
#media, mediana e sd per i maschi per la variabile "et? al decesso" (miMM = media(mortiM))
frequenze(mortiM,cumul=TRUE) #la classe mediana ? 70-79 (medMM = 70+((0.5-0.1949317)/(0.4938615-0.1949317))*10) (sqmMM = sd(mortiM, na.rm=TRUE))
#media, mediana e sd per le femmine per la variabile "et? al contagio" (miCF = media(contagiatiF))
frequenze(contagiatiF,cumul=TRUE) #la classe mediana ? 40-49 (medCF = 40+((0.5-0.46859788)/(0.63834666-0.46859788))*10) (sqmCF = sd(contagiatiF, na.rm=TRUE))
#media, mediana e sd per le femmine per la variabile "et? al decesso" (miMF = media(mortiF))
frequenze(mortiF,cumul=TRUE) #la classe mediana ? 80-89
(medMF = 80+((0.5-0.2920084406)/(0.7121325104-0.2920084406))*10) (sqmMF = sd(mortiF, na.rm=TRUE))
#media, mediana e sd per la totalit? dei dati per la variabile "et? al contagio" (miCT = media(contagiatiT))
frequenze(contagiatiT,cumul=TRUE) #la classe mediana ? 40-49
(medCT = 40+((0.5-0.48119008)/(0.64503430-0.48119008))*10)
(sqmCT = sd(contagiatiT, na.rm=TRUE))
#media, mediana e sd per la totalit? dei dati per la variabile "et? al decesso" (miMT = media(mortiT))
frequenze(mortiT,cumul=TRUE) #la classe mediana ? 80-89
(medMT = 80+((0.5-0.4057054165)/(0.8058883446-0.4057054165))*10) (sqmMT = sd(mortiT, na.rm=TRUE))
#riassunto per medie e mediane per la variabile "et? al contagio" e "et? al decesso" MisuraDiSintesi=c("media - maschi","mediana - maschi","media - femmine","mediana - femmine","media - totale","mediana - totale") Et?AlContagio=round(c(miCM,medCM,miCF,medCF,miCT,medCT),4) Et?AlDecesso=round(c(miMM,medMM,miMF,medMF,miMT,medMT),4) cbind(MisuraDiSintesi,Et?AlContagio,Et?AlDecesso)
#Come si evince dal prospetto di sintesi sopra riportato media e mediana, per lavariabile "et? al contagio" sono molto simili, a riprova del fatto che assume una forma tendenzialmente campanulare con una asimmetria destra
#Discorso simile anche per la variabile "et? al decesso", che presenta anche'essa media e mediana appartenti alla stessa classe. L'istogramma evidenzia per? un'asimmetria destra pronunciata, mettendo in luce che con un numero basso di contagiati, in proporzione alle altri classi, gli anziani hanno un tasso di mortalit? nettamente pi? elevato
#Perch? l'et? mediana al contagio ottenuta eseguendo lo script (41.15) ? diversa da quella fornita dall'Istituto Superiore di Sanit? (47)? Questa discrepanza tra dati ? dovuta al fatto che l'ISS utilizza i dati grezzi per calcolare le misure di sintesi, mentre lo script sopra eseguito fa riferimento a una distribuzione per classi (minore contenuto informativo). Entrambi i dati ricadono infatti nella classe mediana 40-49.
