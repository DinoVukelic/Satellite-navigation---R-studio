###############################74 DAN#################################

#1) UCITAVANJE PODATAKA
baza <- read.csv("baza za 74.csv", header=FALSE)

# 2) DEFINIRANJE VARIJABLI
vrijeme <- baza$V1
lat <- baza$V2
long <- baza$V3
h <- baza$V4
x <- baza$V5
y <- baza$V6
z <- baza$V7


#3) PRORACUN ODSTUPANJA I PRETVARANJE DOBIVENIH VRIJEDNOSTI U METRE
#Izracunavanje odstupanja od referentnog polozaja (GRAZ)

op_GRAZ<-c(15.4933333,47.0669444,538.3) #Referentni polozaj_GRAZ
Dlat<-(lat-op_GRAZ[2]) #Dlat - odstupanje geografske sirine
Dlong<-(long-op_GRAZ[1]) #Dlong - odstupanje geografske duzine
Dh<-(h-op_GRAZ[3]) #Dh - odstupanje nadmorske visine

#Pretvorba lucnih vrijednosti u metre
cos_op_GRAZ<-(cos(op_GRAZ[2]*pi/180))
Dlatm<-(Dlat*60*1852) #Odstupanje geografske sirine u metrima
Dlongm<-(Dlong*60*1852*cos_op_GRAZ) #Odstupanje geografske duzine u metrima

# 4) KONTROLNO ISCRTAVANJE PODATAKA
plot(vrijeme,Dlat, type="l", col="red", main="Prikaz vremenskog niza za odstupanje geografske sirine", xlab="Vrijeme", ylab="Geografska sirina")
plot(vrijeme, Dlong, type="l", col="green", main="Prikaz vremenskog niza za odstupanje geografske duzine", xlab="Vrijeme", ylab="Geografska duzina")
plot(vrijeme, Dh, type="l", col="blue", main="Prikaz vremenskog niza za odstupanje nadmorske visine", xlab="Vrijeme", ylab="Nadmorska visina")

plot(vrijeme,x, type="l", col="yellow", main="Prikaz vremenskog niza za X komponentu geomagnetskog polja", xlab="Vrijeme", ylab="X komponenta")
plot(vrijeme, y, type="l", col="purple", main="Prikaz vremenskog niza za Y komponentu geomagnetskog polja", xlab="Vrijeme", ylab="X komponenta")
plot(vrijeme, z, type="l", col="orange", main="Prikaz vremenskog niza za Z komponentu gemagnetskog polja", xlab="Vrijeme", ylab="Z komponenta")


#6) GRAFICKO PRIKAZIVANJE PRORACUNATIH I DOBIVENIH VRIJEDNOSTI 

#a) Vremenski niz
par(mar=c(3, 5, 2, 5) + 0.1)

plot(vrijeme,Dlatm, type="l", lwd=2, col="yellow", ylab="latitude deviation (m)", main="Horizontal positioning error GRAZ", cex.lab=1, cex.axis=1, cex.main=1)
grid()
par(new=TRUE)
plot(vrijeme,Dlongm,type="l",lwd=2, col="green",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4,labels=TRUE, cex.axis=1, cex.lab=1)
mtext("longitude deviation (m)", side=4, line=2.3, cex=1)
legend("top", bg="transparent",lwd=3, legend=c("lat","lon"), col=c("yellow", "green"), cex=0.5,ncol=2, lty=c(1,1), pch=c(0,1),pt.cex=c(0,0))


# b) Histogrami

hist(Dlatm, col="red", xlab="latitude deviations (m)", main="Histogram of latitude error")
hist(Dlongm, col="green", xlab="longitude deviations (m)", main="Histogram of longitude error")
hist(Dh, col="blue", xlab="vertical deviations (m)", main="Histogram of vertical error")

hist(x, col="yellow", xlab="X komponenta", main="Ucestalost X komponente")
hist(Dlongm, col="purple", xlab="Y komponenta", main="Ucestalost Y komponente")
hist(Dh, col="orange", xlab="Z komponenta", main="Ucestalost Z komponente")

# c) Boxplotovi ili kutijasti dijagrami

boxplot(Dlatm, grid=TRUE, col="red", main="Prikaz distribucije odstupanja polozaja u geografskoj sirini")
grid()
par(new=TRUE)
boxplot(Dlatm, grid=TRUE, col="red")

boxplot(Dlongm, col="green", main="Prikaz distribucije odstupanja polozaja u geografskoj duzini")
grid()
par(new=TRUE)
boxplot(Dlongm, col="green")

boxplot(Dh, col="blue", main="Prikaz distribucije odstupanja polozaja u nadmorskoj visini")
grid()
par(new=TRUE)
boxplot(Dh, col="blue")

boxplot(x, col="yellow", main="Prikaz distribucije X komponente")

boxplot(y, col="purple", main="Prikaz distribucije y komponente")

boxplot(z, col="orange", main="Prikaz distribucije z komponente")

# d) Scatter plot
plot(Dlatm, Dlongm)

#7)STATISTICKA ANALIZA ODSTUPANJA (PODATKOVNA) ZA IGS STANICE

summary(Dlatm)
summary(Dlongm)
summary(Dh)
sd(Dlatm)
sd(Dlongm)
sd(Dh)
var(Dlatm)
var(Dlongm)
var(Dh)
range(Dlatm)
range(Dlongm)
range(Dh)

#8) STATISTICKA ANALIZA X, Y I Z GEOMAGNETSKOG POLJA
summary(x)
summary(y)
summary(z)
sd(x)
sd(y)
sd(z)
var(x)
var(y)
var(z)
range(x)
range(y)
range(z)
##########################ZAVRSETAK#############################################