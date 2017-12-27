getwd()
read.table("bddwip.txt")->bdd
head(bdd)
names(bdd)
colnames(bdd)

write.table(bdnc, "bdnc.txt")
acm1<-dudi.acm(bdnc)
3
?dudi.acm
acm1
inertia.dudi(acm1)
scatter(acm1)
acm1$li
s.label(acm1$li)
plot(hclust(dist(acm1$li)))
cut5<- cutree(hclust(dist(acm1$li)), k=5)
cut5
s.class(acm1$li, factor(cut5))
pam(acm1$li, k=4)->pam4
library(cluster)
pam4
s.class(acm1$li, factor(pam4$clustering))
chisq.test(pam4$clustering, bdnc[,2])

##### SUITE


# Faire une classification des mares en groupe et si possible avoir une idÃ©e du nombre optial de groupes pour bien les sÃ©parer.
# On ne va conserver que les variables des fiches, puis les croiser avec les donnÃ©es de vÃ©gÃ©tation et physico.
# Quand on fait une ACM (=afc sur donnÃ©es qualitatives), ce qui pÃ¨se dans l'analyse c'est le nombre de modalitÃ©s de cette variable.
bdd$TYPE
summary(bdd$STATUT)
bdd->bddw #bddw = bdd version travaille
head(bddw[,1:6])
head(bddw[,1:6])
colnames(bddw[1:9])
colnames(bddw)
bddw[,-(2:9)]->bddw

summary(bddw[,3])
bddw[which(bddw[,2]=="patatoide"),2]<-"complexe"
bddw[which(bddw[,2]=="triangle"),2]<-"carre/rectangle"
bddw[,2]<- droplevels(bddw[,2])

summary(bddw[,3])
hist(bddw[,3], breaks=100)
bddw[,3]-> crs
crs[crs<=30]<-15
crs[crs>15&crs<=50]<-30
crs[crs>50]<-100
hist(bddw[,3], breaks=100)
bddw[,3]<-crs

summary(bddw[,4])
hist(bddw[,4], breaks=100)
bddw[,4]-> crs
crs[crs<=10]<-5
crs[crs>5&crs<=30]<-20
crs[crs>20]<-100
hist(crs, breaks=100)
bddw[,4]<-crs
hist(bddw[,4], breaks=100)

bddw[,5]->crs
summary(crs)
crs[crs=="carrieres"]<-"carriere"
crs[crs=="bois_resineux"]<-"bois_feuillus"
crs[crs=="prairie_humide"]<-"prairie_mesophile"
crs<- droplevels(crs)
bddw[,5]<-crs
summary(bddw[,5])

summary(bddw[,12])
hist(bddw[,12], breaks=100)
bddw[,12]-> crs
crs[crs<=5]<-2
crs[crs>2&crs<=25]<-15
crs[crs>15&crs<=200]<-100
crs[crs>100]<-1000
hist(crs, breaks=100)
bddw[,12]<-crs
hist(bddw[,12], breaks=100)

summary(bddw[,13])
hist(bddw[,13], breaks=100)
bddw[,13]-> crs
crs[crs<=30]<-15
crs[crs>30&crs<=65]<-50
crs[crs>65]<-100
hist(crs, breaks=100)
bddw[,13]<-crs
hist(bddw[,13], breaks=100)

summary(bddw[,14])
hist(bddw[,14], breaks=100)
bddw[,14]-> crs
crs[crs<=30]<-15
crs[crs>30&crs<=65]<-50
crs[crs>65]<-100
hist(crs, breaks=100)
bddw[,14]<-crs
hist(bddw[,14], breaks=100)

summary(bddw[,15])
hist(bddw[,15], breaks=100)
bddw[,15]-> crs
crs[crs<=18]<-10
crs[crs>10&crs<=55]<-30
crs[crs>55]<-60
hist(crs, breaks=100)
bddw[,15]<-crs
hist(bddw[,15], breaks=100)

summary(bddw[,18])
hist(bddw[,18], breaks=100)
bddw[,18]-> crs
crs[crs<=20]<-10
crs[crs>20&crs<=55]<-40
crs[crs>55]<-100
hist(crs, breaks=100)
bddw[,18]<-crs
hist(bddw[,18], breaks=100)

summary(bddw[,19])
hist(bddw[,19], breaks=100)
bddw[,19]-> crs
crs[crs<=20]<-10
crs[crs>20&crs<=55]<-40
crs[crs>55]<-100
hist(crs, breaks=100)
bddw[,x]<-crs
hist(bddw[,x], breaks=100)

summary(bddw[,20])
colnames(bddw)
hist(bddw[,20], breaks=100)
bddw[,20]-> crs
crs[crs<=10]<-5
crs[crs>10&crs<=55]<-30
crs[crs>55]<-75
hist(crs, breaks=100)
bddw[,20]<-crs
hist(bddw[,20], breaks=100)


summary(bddw[,21])
colnames(bddw)
hist(bddw[,21], breaks=100)
bddw[,21]-> crs
crs[crs<=10]<-5
crs[crs>10&crs<=55]<-30
crs[crs>55]<-75
hist(crs, breaks=100)
bddw[,21]<-crs
hist(bddw[,21], breaks=100)

summary(bddw[,22])
colnames(bddw)
hist(bddw[,22], breaks=100)
bddw[,22]-> crs
crs[crs<=10]<-5
crs[crs>10&crs<=55]<-30
crs[crs>55]<-75
hist(crs, breaks=100)
bddw[,22]<-crs
hist(bddw[,22], breaks=100)

summary(bddw[,23])
colnames(bddw)
hist(bddw[,23], breaks=100)
bddw[,23]-> crs
crs[crs<=10]<-5
crs[crs>10&crs<=55]<-30
crs[crs>55]<-75
hist(crs, breaks=100)
bddw[,23]<-crs
hist(bddw[,23], breaks=100)

summary(bddw[,24])
colnames(bddw)
hist(bddw[,24], breaks=100)
bddw[,24]-> crs
crs[crs<=10]<-5
crs[crs>10&crs<=55]<-30
crs[crs>55]<-75
hist(crs, breaks=100)
bddw[,24]<-crs
hist(bddw[,24], breaks=100)

summary(bddw[,25])
colnames(bddw)
hist(bddw[,25], breaks=100)
bddw[,25]-> crs
crs[crs<=10]<-5
crs[crs>10&crs<=55]<-30
crs[crs>55]<-75
hist(crs, breaks=100)
bddw[,25]<-crs
hist(bddw[,25], breaks=100)

summary(bddw[,26])
colnames(bddw)
hist(bddw[,26], breaks=100)
bddw[,26]-> crs
crs[crs<=10]<-5
crs[crs>10&crs<=55]<-30
crs[crs>55]<-75
hist(crs, breaks=100)
bddw[,26]<-crs
hist(bddw[,26], breaks=100)

summary(bddw[,27])
colnames(bddw)
hist(bddw[,27], breaks=100)
bddw[,27]-> crs
crs[crs<=20]<-10
crs[crs>10&crs<=50]<-30
crs[crs>50&crs<=80]<-65
crs[crs>80]<-90
hist(crs, breaks=100)
bddw[,27]<-crs
hist(bddw[,27], breaks=100)

bddw[,28]->crs
summary(crs)
hist(bddw[,28], breaks=100)
crs[crs>=4]<-4
hist(crs, breaks=100)
bddw[,28]<-crs
hist(bddw[,28], breaks=100)

head(bddw[,1:6])
head(bddw[,])
colnames(bddw[33:39])
colnames(bddw)
bddw[,-(35)]->bddw
colnames(bddw)

summary(bddw[,30])
bddw[,30]->crs
summary(crs)
crs[crs=="Crapaud_calamite"]<-"oui"
crs<- droplevels(crs)
bddw[,30]<-crs
summary(bddw[,30])

summary(bddw[,32])
bddw[,32]->crs
summary(crs)
crs[crs=="Balsamine_de_l_Himalaya"]<-"oui"
crs[crs=="ragondin"]<-"oui"
crs<- droplevels(crs)
bddw[,32]<-crs
summary(bddw[,32])

summary(bddw[,34])
bddw[,34]->crs
summary(crs)
crs[crs=="argile"]<-"naturel"
crs[crs=="bac"]<-"bache"
crs[crs=="bâche"]<-"bache"
crs[crs=="bentonite"]<-"bache"
crs[crs=="beton"]<-"bache"
crs[crs=="carrière"]<-"bache"
crs[crs=="coque"]<-"bache"
crs[crs=="pavee"]<-"bache"
crs[crs=="pierre"]<-"bache"
crs<- droplevels(crs)
bddw[,34]<-crs
summary(bddw[,34])

levels(crs)[levels(crs)=="bache"] <- "artificiel"
summary(bddw[,5])
levels(bddw[,5])[levels(bddw[,5])=="tourbière"] <- "tourbiere"

write.table(bddw, "BDD mare 34.txt")

#### test 1

x=1
x=x+1
levels(bddw[,x])
hist(bddw[,x], breaks=100)
summary(bddw[,x])

colnames(bddw)
hist(bdd[,27], breaks=100)
summary(bdd[,27])

colnames(bdd)

#### ACM
write.table(bddw, "bddw20exclus.txt")
bddw[,1:19]->bdnc
bdnc
library(ade4)
#pour une acm il faut qu'il y ai que des facteurs, et qualitatifs
acm1<-dudi.acm(bdnc)
?dudi.acm
x<-1
x=x+1
bdnc[,x]<-as.factor(bdnc[,x])
x=x+1
bdnc[,x]<-as.factor(bdnc[,x])
x=x+1
bdnc[,x]<-as.factor(bdnc[,x])
x=x+1
bdnc[,x]<-as.factor(bdnc[,x])
x=x+1
bdnc[,x]<-as.factor(bdnc[,x])
x=x+1
bdnc[,x]<-as.factor(bdnc[,x])
x=x+1
bdnc[,x]<-as.factor(bdnc[,x])
x=x+1
bdnc[,x]<-as.factor(bdnc[,x])
x=x+1
bdnc[,x]<-as.factor(bdnc[,x])
x=x+1
bdnc[,x]<-as.factor(bdnc[,x])
x=x+1
bdnc[,x]<-as.factor(bdnc[,x])
x=x+1
bdnc[,x]<-as.factor(bdnc[,x])
x=x+1
bdnc[,x]<-as.factor(bdnc[,x])
x=x+1
bdnc[,x]<-as.factor(bdnc[,x])
x=x+1
bdnc[,x]<-as.factor(bdnc[,x])
is.factor(bdnc)
colnames

write.table(bdnc, "bdnc.txt")
acm1<-dudi.acm(bdnc)
3
?dudi.acm
acm1
inertia.dudi(acm1)
scatter(acm1)
acm1$li
s.label(acm1$li)
plot(hclust(dist(acm1$li)))
cut5<- cutree(hclust(dist(acm1$li)), k=5)
cut5
s.class(acm1$li, factor(cut5))
pam(acm1$li, k=4)->pam4
library(cluster)
pam4
s.class(acm1$li, factor(pam4$clustering))
chisq.test(pam4$clustering, bdnc[,2])

### ACM 34 !

bddw->bddw34OK
x=1
x=x+1
bddw[,x]<-as.factor(bddw[,x])
?dudi.acm
acm1<-dudi.acm(bddw)
2
inertia.dudi(acm1)
scatter(acm1)
acm1$li
s.label(acm1$li, clab=0.7)
plot(hclust(dist(acm1$li)))
library(cluster)
pam(acm1$li, k=2)->pam2
pam(acm1$li, k=3)->pam3
pam(acm1$li, k=4)->pam4
pam(acm1$li, k=5)->pam5
pam(acm1$li, k=6)->pam6
pam2
plot(pam2) #0.34, 0 négatifs
plot(pam3) #0.37, 2 négatifs
plot(pam4) #0.36, 0 négatifs
plot(pam5) #0.36, 2 mini négatifs
plot(pam6) #0.37, 2 négatifs

cut2<- cutree(hclust(dist(acm1$li)), k=2)
cut2
cut3<- cutree(hclust(dist(acm1$li)), k=3)
cut3
cut4<- cutree(hclust(dist(acm1$li)), k=4)
cut4
cut5<- cutree(hclust(dist(acm1$li)), k=5)
cut5
cut6<- cutree(hclust(dist(acm1$li)), k=6)
cut6
s.class(acm1$li, factor(cut4))
s.class(acm1$li, factor(pam2$clustering))
s.class(acm1$li, factor(pam3$clustering))
s.class(acm1$li, factor(pam4$clustering))
s.class(acm1$li, factor(pam5$clustering))
s.class(acm1$li, factor(pam6$clustering))
chisq.test(pam4$clustering, bddw[,4])
?pam

pam4$id.med
pam5$id.med
##PAM4
bddw[223,]
bddw[11,]
bddw[518,]
bddw[122,]

##PAM 5
bddw[488,]
bddw[19,]
bddw[436,]
bddw[577,]
bddw[446,]
bddw[c(488,19,436,577,446),]->bddwpam5
library(compare)
summary(compare(bddw[223,],bddw[11,]))
?compare

### mares extremes
bddw[c(452,479,490,459),]->bddwextremes

### ACM 34 !

bddw->bddw34OK
x=1
x=x+1
bddw[,x]<-as.factor(bddw[,x])
?dudi.acm
acm1<-dudi.acm(bddw)
2
inertia.dudi(acm1)
scatter(acm1)
acm1$li
s.label(acm1$li, clab=0.7)
plot(hclust(dist(acm1$li)))
library(cluster)
pam(acm1$li, k=2)->pam2
pam(acm1$li, k=3)->pam3
pam(acm1$li, k=4)->pam4
pam(acm1$li, k=5)->pam5
pam(acm1$li, k=6)->pam6
pam2
plot(pam2) #0.34, 0 négatifs
plot(pam3) #0.37, 2 négatifs
plot(pam4) #0.36, 0 négatifs
plot(pam5) #0.36, 2 mini négatifs
plot(pam6) #0.37, 2 négatifs

cut2<- cutree(hclust(dist(acm1$li)), k=2)
cut2
cut3<- cutree(hclust(dist(acm1$li)), k=3)
cut3
cut4<- cutree(hclust(dist(acm1$li)), k=4)
cut4
cut5<- cutree(hclust(dist(acm1$li)), k=5)
cut5
cut6<- cutree(hclust(dist(acm1$li)), k=6)
cut6
s.class(acm1$li, factor(cut4))
s.class(acm1$li, factor(pam2$clustering))
s.class(acm1$li, factor(pam3$clustering))
s.class(acm1$li, factor(pam4$clustering))
s.class(acm1$li, factor(pam5$clustering))
s.class(acm1$li, factor(pam6$clustering))
chisq.test(pam4$clustering, bddw[,4])
?pam

pam4$id.med
pam5$id.med
##PAM4
bddw[223,]
bddw[11,]
bddw[518,]
bddw[122,]

##PAM 5
bddw[488,]
bddw[19,]
bddw[436,]
bddw[577,]
bddw[446,]
bddw[c(488,19,436,577,446),]->bddwpam5
library(compare)
summary(compare(bddw[223,],bddw[11,]))
?compare

###ACM 33 SANS ACCES

summary(bddw)
colnames(bddw)

acm33<-dudi.acm(bddw33)
2
inertia.dudi(acm33)
scatter(acm33)
acm1$li
s.label(acm33$li, clab=0.7)
plot(hclust(dist(acm33$li)))
library(cluster)
pam(acm33$li, k=2)->pam332
pam(acm33$li, k=3)->pam333
pam(acm33$li, k=4)->pam334
pam(acm33$li, k=5)->pam335
pam(acm33$li, k=6)->pam336
pam(acm33$li, k=7)->pam337
pam2
plot(pam332) #0.35, 0 négatifs
plot(pam333) #0.36, 1 négatifs
plot(pam334) #0.36, 1 négatifs
plot(pam335) #0.37, 0 négatifs
plot(pam336) #0.38, 0 négatifs
plot(pam337) #0.36, 0 négatifs


s.class(acm1$li, factor(pam2$clustering))
s.class(acm1$li, factor(pam3$clustering))
s.class(acm1$li, factor(pam4$clustering))
s.class(acm1$li, factor(pam5$clustering))
s.class(acm1$li, factor(pam336$clustering))
chisq.test(pam336$clustering, bddw[,4])
?pam
pam336$id.med
bddw33[c(625,407,525,4,30,446),]->bddwpam336

###ACM 33 AVEC OUI NON POTENTIEL

bddw33->bddw33potentiel
bddw33potentiel[,32]->crs
summary(crs)
crs[crs=="oui_à_priori"]<-"oui_certain"
crs<- droplevels(crs)
bddw33potentiel[,32]<-crs
summary(bddw33potentiel[,32])

library(ade4)
acm33potentiel<-dudi.acm(bddw33potentiel)
2
inertia.dudi(acm33potentiel)
scatter(acm33potentiel)
acm1$li
s.label(acm33potentiel$li, clab=0.7)
plot(hclust(dist(acm33potentiel$li)))
library(cluster)
pam(acm33potentiel$li, k=2)->pam33potentiel2
pam(acm33potentiel$li, k=3)->pam33potentiel3
pam(acm33potentiel$li, k=4)->pam33potentiel4
pam(acm33potentiel$li, k=5)->pam33potentiel5
pam(acm33potentiel$li, k=6)->pam33potentiel6
pam(acm33potentiel$li, k=7)->pam33potentiel7
pam2
plot(pam33potentiel2) #0.35, 1 négatifs
plot(pam33potentiel3) #0.37, 1 négatifs
plot(pam33potentiel4) #0.37, 2 négatifs
plot(pam33potentiel5) #0.36, 3 négatifs
plot(pam33potentiel6) #0.38, 2 négatifs
plot(pam33potentiel7) #0.35, 1 négatifs

s.class(acm1$li, factor(pam2$clustering))
s.class(acm1$li, factor(pam3$clustering))
s.class(acm1$li, factor(pam4$clustering))
s.class(acm1$li, factor(pam5$clustering))
s.class(acm1$li, factor(pam336$clustering))
chisq.test(pam336$clustering, bddw[,4])
?pam
pam336$id.med
bddw33[c(625,407,525,4,30,446),]->bddwpam336
colnames(bddw33potentiel)
summary(bddw33$CURAGE)

###ACM 32 SANS CURAGE + POTENTIEL OUI/NON

bddw33potentiel->bddw32
bddw33potentiel[,-15]->bddw32
colnames(bddw32)

library(ade4)
acm32<-dudi.acm(bddw32)
2
acm32
inertia.dudi(acm32)
scatter(acm32)
acm1$li
s.label(acm32$li, clab=0.7)
plot(hclust(dist(acm33potentiel$li)))
library(cluster)
pam(acm32$li, k=2)->pam322
pam(acm32$li, k=3)->pam323
pam(acm32$li, k=4)->pam324
pam(acm32$li, k=5)->pam325
pam(acm32$li, k=6)->pam326
pam(acm32$li, k=7)->pam327
pam2
plot(pam322) #0.35, 1 négatifs
plot(pam323) #0.37, 1 négatifs
plot(pam324) #0.37, 2 négatifs
plot(pam325) #0.36, 3 négatifs
plot(pam326) #0.38, 2 négatifs
plot(pam327) #0.35, 4 négatifs

s.class(acm1$li, factor(pam2$clustering))
s.class(acm1$li, factor(pam3$clustering))
s.class(acm1$li, factor(pam4$clustering))
s.class(acm1$li, factor(pam5$clustering))
s.class(acm1$li, factor(pam326$clustering))
chisq.test(pam336$clustering, bddw[,4])
?pam
pam336$id.med
pam326$id.med
pam324$id.med
bddw32[c(364,258,481,393),]->bddwpam324

colnames(bddw33potentiel)
summary(bddw33$CURAGE)