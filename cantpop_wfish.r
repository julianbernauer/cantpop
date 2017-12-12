##################################################################
#QTA cantonal branches SVP and analysis of SVP vote using SELECTS# 
##################################################################

#Julian Bernauer (MZES Mannheim) and Anna Storz (IPW Bern)
#December 12, 2017

setwd("...") 

#Using tidyverse, quanteda and JAGS  

library(tidyverse)
library(quanteda)
library(readtext)
library(R2jags)
library(arm)

###############
#text data svp# 
###############

#Nur ANSI?! could be specified... 
svp_txt <- readtext("ansi/*.txt")

svp_corpus <- corpus(svp_txt)

summary(svp_corpus)

#word matrix, removing stopwords and stemming
svp_dfm_stem <- dfm(svp_corpus, remove = c(stopwords("german"),"fuer","dass","svp","nidwalden","obwalden","1/4","?","fÃ£","bÃ£","kÃ£","mÃ£","claire","luca"), remove_punct = TRUE, stem=TRUE)
#attributes(svp_dfm_stem)

topfeatures(svp_dfm_stem, 1000)

#dictionary
svp_dict <- dictionary(list(
  elites = c("Elite*"),
  politicians = c("Politiker*"),
  corruption = c("korrupt*"),
  responsibility = c("*verantwortung*"),
  credibility = c("glaubwürdig*"),
  # we = c("wir","uns*"),
  # them = ("ihr"), 
  sovereignty = c("Souverän*"),
  independence = c("Unabhäng*"),
  neutrality = c("Neutral*"),
  europe = c("EU","Europa*","europäisch*"),
  #uno = c("Uno"),
  freedom = c("Freiheit*"),  
  people = c("Volk*","Schweizer*"),
  citizens = c("Bürger*","Einbürger*"),
  taxpayers = c("Steuerzahler"),
  community = c("Gemeinschaft*"),
  language = c("*sprach*"),
  democracy = c("*demokrat*") ))

head(dfm(svp_corpus, dictionary = svp_dict))

#dictionary word matrix, no stemming/stopword removal needed?!  
svp_dfm_dict <- dfm(svp_corpus, dictionary = svp_dict)

topfeatures(svp_dfm_dict, 20)

summary(svp_dfm_dict)

#word cloud 
postscript("fig5_wordcloud.eps", width = 600, height = 600, horizontal=FALSE)
set.seed(2)
textplot_wordcloud(svp_dfm_dict, min.freq = 1, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))
dev.off()

###
#Wordfish model 
###

#14 schaffhausen, 24 zÃ¼rich 
fish_svp_dict <- textmodel(svp_dfm_dict, model = "wordfish", dir = c(14,24)) 

summary(fish_svp_dict)

krzel <- c("Argovia","Inner Rhodes","Basle-City","Basle-Country","Berne","Fribourg","Geneva","Grisons","Jura","Lucerne","Neuchâtel","Nidwald","Obwald","Schaffhausen",
           "SWITZERLAND","Schwyz","Solothurn","St. Gall","Ticino","Thurgovia","Uri","Valais","Vaud","Zurich","Zug")

krzel_acr <- c("AG","AI","BS","BL","BE","FR","GE","GR","JU","LU","NE","NW","OW","SH",
           "CH","SZ","SO","SG","TI","TG","UR","VL","VD","ZH","ZG")

#Cantonal positions 
#postscript("fig1_fishdict.eps", width = 400, height = 400)
textplot_scale1d(fish_svp_dict, doclabels = krzel)
#dev.off()

#word weights and intercepts 
postscript("fig4_wordsdict.eps", width = 600, height = 600, horizontal=FALSE)
textplot_scale1d(fish_svp_dict, margin = "features", 
                 highlighted = c("community","elites","people","democracy","corruption","europe"), 
                 highlighted_color = "black")
dev.off

###
#wordfish all words 
###
fish_svp <- textmodel(svp_dfm_stem, model = "wordfish", dir = c(14,24)) 

#summary(fish_svp)

#word cloud 
#set.seed(100)
#textplot_wordcloud(svp_dfm_stem, min.freq = 20, random.order = FALSE,
#                   rot.per = .25, 
#                   colors = RColorBrewer::brewer.pal(8,"Dark2"))

#Cantonal positions 
#postscript("fig2_fishall.eps", width = 400, height = 400)
textplot_scale1d(fish_svp, doclabels = krzel)
#dev.off()


#word weights and intercepts 
postscript("fig3_wordsall.eps", width = 600, height = 600, horizontal=FALSE)
textplot_scale1d(fish_svp, margin = "features", 
                 highlighted = c("eliten","volk","demokrati","unabhängig","korrupt","ausländer",
                                 "masseneinwanderung","asyl","einbruch","kriminell"), 
                 highlighted_color = "black")
#"tiger",
dev.off()


#Nebeneinander
theta_all <- as.numeric(fish_svp@theta)
sdtheta_a <- as.numeric(fish_svp@se.theta)
theta_dict <- as.numeric(fish_svp_dict@theta)
sdtheta_d <- as.numeric(fish_svp_dict@se.theta)
fishis <- cbind.data.frame(theta_all,theta_dict,sdtheta_a,sdtheta_d,krzel,krzel_acr) 
fishis <- as.tibble(fishis[order(theta_all),])

#textplot_scale1d(fishis$theta_dict, doclabels = fishis$krzel_acr)
#plot(fishis$theta_dict)

var.names <- fishis$krzel

m1 <- fishis$theta_all
s1 <- fishis$sdtheta_a
m2 <- fishis$theta_dict
s2 <- fishis$sdtheta_d

pic <- data.frame(var.names,m1,m2,s1,s2)
pic.sort <- pic[order(m1) , ]
pic.sort

diff <- .25


postscript("fig1_wordfishes.eps", width = 600, height = 600, horizontal=FALSE)

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(pic.sort$m1, y.axis, type = "n", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-3,3), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(-3,3, by = .5), label = seq(-3,3, by = .5), cex.axis=.9)
axis(2, at = y.axis, label = var.names, las = 1, tick = T, font=1, cex.axis=.8)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")

points(pic.sort$m1, y.axis+diff, type = "p", pch = 19, cex=1)
segments(pic.sort$m1-qnorm(.975)*pic.sort$s1, y.axis+diff, pic.sort$m1+qnorm(.975)*pic.sort$s1, y.axis+diff, lwd =  1.5)
points(pic.sort$m2, y.axis-diff, type = "p", pch = 1, cex=1)
segments(pic.sort$m2-qnorm(.975)*pic.sort$s2, y.axis-diff, pic.sort$m2+qnorm(.975)*pic.sort$s2, y.axis-diff, lwd =  1.5)

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .9 
segments(left.side,25,left.side,1) 
segments(left.side,25,left.side+.1,25) 
segments(left.side,1,left.side+.1,1)
text(.7, 13, "Estimated document positions (thetas)", srt = 90, cex=1)

dev.off()



###
#comparison dictionary / all words  

fish_svp@theta 
fish_svp_dict@theta 
cor(fish_svp@theta,fish_svp_dict@theta)

postscript("fig2_fishcomp.eps", width = 800, height = 800, horizontal=FALSE)
plot(fish_svp@theta,fish_svp_dict@theta,type="n",ylab="Wordfish with Dictionary",xlab="Wordfish with all Words")
abline(reg <- lm(fish_svp_dict@theta~fish_svp@theta))
text(fish_svp@theta,fish_svp_dict@theta,krzel_acr)
dev.off()


#For later use, adding CH on position 26 (Schweiz for Glarus) 
#Schweiz already on 15 for AR
popsvp_dict <- c(fish_svp_dict@theta,fish_svp_dict@theta[15])
popsvp_all <- c(fish_svp@theta,fish_svp@theta[15])


###
#Selects 
### 

#Prepared Selects data
load("selects_cantpop_panel.RData")


#order of wordfish analysis 
selects <- selects_c[order(selects_c$cantid),]

attach(selects)

N <- length(svp)
NK <- 26

#daten - switch here (popsvp_dict or popsvp_all) to use either wordfish thetas with dictionary or all words -> default dictionary, all words as robustness (measurement in text, model in appendix)
svp.data <- list(N=N, NK=NK, cantid=cantid, svp=svp, pop=pop, age=age, fem=fem, interest=interest, income=income, edu=edu,popsvp=
                   #popsvp_all)
                   popsvp_dict)

#Logit multilevel 

svp.model <- "model{

for(i in 1:N){
svp[i] ~ dbern(p.svp[i])
p.svp[i] <- 1/(1+exp(-z[i]))
z[i] <- a[cantid[i]] + b_pop[cantid[i]]*pop[i] + b_age*age[i] + b_fem*fem[i] + b_interest*interest[i] + b_income*income[i] + b_edu*edu[i] 
}

#b_pop ~ dnorm(0, .0001)
b_age ~ dnorm(0, .0001)
b_fem ~ dnorm(0, .0001)
b_interest ~ dnorm(0, .0001)
b_income ~ dnorm(0, .0001)
b_edu ~ dnorm(0, .0001)

#Cantons
for(j in 1:NK){
a[j] ~ dnorm(mu1[j],tau1)
mu1[j] <- a1 + sigma_popsvp1*popsvp[j]
}

a1 ~ dnorm(0, .0001)
sigma_popsvp1 ~ dnorm(0, .0001)
tau1 <- pow(sigma1, -2)
sigma1 ~ dunif(0,100)

#slopes pop
for(j in 1:NK){
b_pop[j] ~ dnorm(mu_pop[j],tau_pop)
mu_pop[j] <- a_pop + sigma_popsvp2*popsvp[j]
}

a_pop ~ dnorm(0, .0001)
sigma_popsvp2 ~ dnorm(0, .0001)
tau_pop <- pow(sigma_pop, -2)
sigma_pop ~ dunif(0,100)

}"


#JAGS
write(svp.model, file="svp.model.jags")

svp.svpameters <- c("a", "a1", "sigma1", "tau1", "b_pop", "b_age", "b_fem", "b_interest", "b_income", "b_edu", "a_pop", "sigma_popsvp1", "sigma_popsvp2", "sigma_pop")

#tau = variance

jags.svp <- jags.model(file="svp.model.jags", data = svp.data, n.chains = 3, n.adapt = 100)

sampleshelp <- coda.samples(jags.svp, svp.svpameters, n.iter=100, thin=1)

samplesburn <- coda.samples(jags.svp, svp.svpameters, n.iter=1800, thin=18)

samples <- coda.samples(jags.svp, svp.svpameters,  n.iter=2000, thin=20)

#plot(sampleshelp, ask=TRUE) 

#plot(samples, ask=TRUE) 

#summary(samples)

kette <- as.matrix(samples)

#intercept
ba <- kette[,"a1"] 
mba <- mean(ba)
sba <- sd(ba)

#bpop <- kette[,"b_pop"] 
#mpop <- mean(bpop)
#spop <- sd(bpop)

bage <- kette[,"b_age"] 
mage <- mean(bage)
sage <- sd(bage)

bfem <- kette[,"b_fem"] 
mfem <- mean(bfem)
sfem <- sd(bfem)

binterest <- kette[,"b_interest"] 
minterest <- mean(binterest)
sinterest <- sd(binterest)

bincome <- kette[,"b_income"] 
mincome <- mean(bincome)
sincome <- sd(bincome)

bedu <- kette[,"b_edu"] 
medu <- mean(bedu)
sedu <- sd(bedu)

bcant <- kette[,"sigma1"] 
mcant <- mean(bcant)
scant <- sd(bcant)

bapop <- kette[,"a_pop"] 
mapop <- mean(bapop)
sapop <- sd(bapop)

bpopsvp1 <- kette[,"sigma_popsvp1"]
mpopsvp1 <- mean(bpopsvp1)
spopsvp1 <- sd(bpopsvp1)

bpopsvp2 <- kette[,"sigma_popsvp2"]
mpopsvp2 <- mean(bpopsvp2)
spopsvp2 <- sd(bpopsvp2)

bpopsd <- kette[,"sigma_pop"]
mpopsd <- mean(bpopsd)
spopsd <- sd(bpopsd)

m <- c(mba,mapop,
       mage,mfem,minterest,mincome,medu,mcant,mpopsd,mpopsvp1,mpopsvp2)
s <- c(sba,sapop,
       sage,sfem,sinterest,sincome,sedu,scant,spopsd,spopsvp1,spopsvp2)


vlabels <- c("Mean","Pop. (m.)",
             "Age","Female","Interest","Income","Education","SD cant.","SD pop.","Pos. SVP","Pos.*pop.") 


#Fig 
postscript("fig7_coeffsd.eps", width = 400, height = 400, horizontal=FALSE)
#postscript("fig6_coeffsa.eps", width = 400, height = 400, horizontal=FALSE)
coefplot(m,s,varnames=vlabels,vertical=FALSE,main="",ylim=c(-1,1))
dev.off()

postscript("fig8_interaction.eps", width = 800, height = 800, horizontal=FALSE)
#Interaction 
par(mar=c(3,4,0,1) + 0.1)    
par(oma=c(1,1,1,0)+.1)
par(xaxs = "i") 
par(mfrow=c(1,1))
curve(mapop + mpopsvp2*x, from=-1.8,
      to=2.6,ylim=c(0,1), ylab="",  xlab="", axes=F, col="white")
axis(2, at=seq(0, 1, by=.2))
axis(1, at=seq(-1.8, 2.6, by=.2))
axis(2, at = .5, label = "Effect of populist attitudes on the SVP vote", las = 0, tick=F, outer=F, cex.axis=1, line=2)
axis(1, at = .4, label = "Contextual degree of populism", las = 0, tick=F, outer=F, cex.axis=1, line=2)
#axis(1, at = .4, label = "Contextual SVP heterogeneity", las = 0, tick=F, outer=F, cex.axis=1, line=2)
for (i in 100:300){
  curve(bapop[i] + bpopsvp2[i]*x, from=-1.8, to=2.6,add=T,
        col="grey", lwd=1, lty=3)
  #lty=2 or 3
}
abline(h=0)
curve(mapop + mpopsvp2*x, from=-1.8,
      to=2.6,ylim=c(0,1), ylab="", xlab="", col="black", lwd=2, add=T)
box()
dev.off()


