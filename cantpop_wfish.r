##################################################################
#QTA cantonal branches SVP and analysis of SVP vote using SELECTS# 
##################################################################

#Julian Bernauer (MZES Mannheim) and Anna Storz (IPW Bern)
#October 3, 2017

setwd("U:/Forschung MZES/QTA Paper")

#austin package with wordfish 
library(austin)


#######################################
#Wordfish: Reduced matrix with stemming 
svp_d <- wfm("svp_d_red_stem.csv") 

fish_svp <- wordfish(svp_d, dir=c(7, 4), control=list(tol=1e-06, sigma=3, startparams=NULL), verbose=TRUE)

summary(fish_svp)

#Plot of manifesto positions 
plot(fish_svp, truevals=NULL, level=0.95, pch=20)

#Word weights and frequencies 
plot(fish_svp$beta,fish_svp$psi,type="n",xlab="Word discriminatory power (weight)",ylab="Word frequency")
text(fish_svp$beta,fish_svp$psi,fish_svp$words,cex=.8)

