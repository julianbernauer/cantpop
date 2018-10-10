############################################################################
#QTA cantonal branches SVP and analysis of populist attitudes using SELECTS# 
############################################################################

# Julian Bernauer (MZES Mannheim) and Anna Storz (IPW Bern)
# Oktober 2018

setwd("...")

library(tidyverse)
library(quanteda)
library(readtext)
library(arm)
library(sp)
library(rgdal)

###############
#text data svp# 
###############

# manifesto corpus 
svp_txt <- readtext("ansi/*.txt")
svp_corpus <- corpus(svp_txt)
summary(svp_corpus)

# dictionary

svp_dict <- dictionary(list(
  people = c("volk*","schweizer*"),
  commun = c("*bürger*","*einbürger*","steuerzahler*","*gemeinschaft*","*sprach*"),
  elites = c("*elite*","*politiker*","*establishm*","*herrsch*"),
  corrupt = c("*korrupt*","*propagand*","*täusch*","*betrüg*","*verrat*","*scham*","*schäm*","*skandal*","*wahrheit*","*fair*","*unehrlich*","*lüge*"),
  integrity = c("*verantwortung*","glaubwürdig*"),
  independ = c("*souver*","*unabh*","*neutral*","freiheit"),
  directdem = c("*direkt*","*demokrat*","*referend*","*volksabst*","*volksinitiativ*","*undemokrat*"),
  delib = c("*konsens*","*kompromiss*") 
))

head(dfm(svp_corpus, dictionary = svp_dict))

# dictionary word matrix, no stemming/stopword removal needed  
svp_dfm_dict <- dfm(svp_corpus, dictionary = svp_dict)
topfeatures(svp_dfm_dict, 20)
summary(svp_dfm_dict)

# word cloud 
postscript("fig1_cloud.eps", width = 600, height = 600, horizontal=FALSE)
set.seed(2)
textplot_wordcloud(svp_dfm_dict, rotation=.1,color="black")
dev.off()

###
#Wordfish model 

# 13 schaffhausen, 23 zurich 
fish_svp_dict <- textmodel_wordfish(svp_dfm_dict, dir = c(13,23)) 
summary(fish_svp_dict)

krzel <- c("Argovia","Basle-City","Basle-Country","Berne","Fribourg","Geneva","Grisons","Jura*","Lucerne","Neuchâtel*","Nidwald*","Obwald*","Schaffhausen*","SWITZERLAND","Schwyz","Solothurn","St. Gall","Ticino*","Thurgovia","Uri*","Valais","Vaud","Zurich","Zug*")

krzel_acr <- c("AG","BS","BL","BE","FR","GE","GR","JU","LU","NE","NW","OW","SH","CH","SZ","SO","SG","TI","TG","UR","VL","VD","ZH","ZG")

# cantonal positions 
postscript("fig2_fish.eps", width = 600, height = 600, horizontal=FALSE)
textplot_scale1d(fish_svp_dict, doclabels = krzel)
dev.off()

# word weights and intercepts 
postscript("fig3_words.eps", width = 600, height = 600, horizontal=FALSE)
textplot_scale1d(fish_svp_dict, margin = "features",
                 highlighted = c("people","commun","elites","corrupt","integrity","independ","europe","directdem","delib"), 
                 highlighted_color = "black")
dev.off()

# for later use, adding CH on position 24 (for Glarus) 

#popsvp_dict <- c(fish_svp_dict$theta[1:13],fish_svp_dict$theta[15:24],fish_svp_dict$theta[14])
popsvp_dict <- c(fish_svp_dict@theta[1:13],fish_svp_dict@theta[15:24],fish_svp_dict@theta[14])


###
#Selects 
# prepared Selects data

load("selects_cantpop_panel.RData")

# cantid2 without AI / AR 
selects_c$cantid2 <- NA
selects_c$cantid2[selects_c$canton=="Zürich"] <- 22
selects_c$cantid2[selects_c$canton=="Bern"] <- 4
selects_c$cantid2[selects_c$canton=="Luzern"] <- 9
selects_c$cantid2[selects_c$canton=="Uri"] <- 19
selects_c$cantid2[selects_c$canton=="Schwyz"] <- 14
selects_c$cantid2[selects_c$canton=="Obwalden"] <- 12
selects_c$cantid2[selects_c$canton=="Nidwalden"] <- 11
selects_c$cantid2[selects_c$canton=="Glarus"] <- 24
selects_c$cantid2[selects_c$canton=="Zug"] <- 23
selects_c$cantid2[selects_c$canton=="Fribourg"] <- 5
selects_c$cantid2[selects_c$canton=="Solothurn"] <- 15
selects_c$cantid2[selects_c$canton=="Basel-Stadt"] <- 3
selects_c$cantid2[selects_c$canton=="Basel-Landschaft"] <- 2
selects_c$cantid2[selects_c$canton=="Schaffhausen"] <- 13
# selects$cantid[selects$canton=="Appenzell Ausserrhoden"] <- 15
# selects$cantid[selects$canton=="Appenzell Innerrhoden"] <- 2
selects_c$cantid2[selects_c$canton=="St. Gallen"] <- 16
selects_c$cantid2[selects_c$canton=="Graubünden"] <- 7
selects_c$cantid2[selects_c$canton=="Aargau"] <- 1
selects_c$cantid2[selects_c$canton=="Thurgau"] <- 18
selects_c$cantid2[selects_c$canton=="Ticino"] <- 17
selects_c$cantid2[selects_c$canton=="Vaud"] <- 21
selects_c$cantid2[selects_c$canton=="Valais"] <- 20
selects_c$cantid2[selects_c$canton=="Neuchâtel"] <- 10
selects_c$cantid2[selects_c$canton=="Genève"] <- 6
selects_c$cantid2[selects_c$canton=="Jura"] <- 8
summary(selects_c$cantid2)
tabulate(selects_c$cantid2)

# deselect AI / AR 
selects2 <- selects_c[!is.na(selects_c$cantid2),]

# order of wordfish analysis 
selects <- selects_c[order(selects_c$cantid),]
selects2 <- selects2[order(selects2$cantid2),]

# only SVP voters 
summary(selects2$svp)
selects2_svp <- selects2[selects2$svp==1,]

# for maps 
selects_svp <- selects[selects$svp==1,]


###
# analyiss 

# BDP
bdp <- c(1,1,1,1,1,1,1,0,1,0,0,0,1,1,1,1,1,0,0,1,1,1,0,1)
bdp_new <- c(1,0,1,0,1,1,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,1,0)

# German speaking  
d <- c(1,1,1,1,0,0,1,0,1,0,1,1,1,1,1,1,0,1,1,0,0,1,1,1)

# explain party positions by voter positions 

reg <- lm(popsvp_s ~ popagg_s + bdp + pop_agg_n$x + popagg_s*bdp + popagg_s*pop_agg_n$x + pop_agg_n$x*bdp + popagg_s*bdp*pop_agg_n$x)
summary(reg) 
coefplot(reg)

vlabels <- c("Constant","Pop. SVP","BDP","Ind. N","Pop. x BDP","Pop. x N","N x BDP","P. x N x B.") 

# fig 
postscript("fig6_linreg.eps", width = 400, height = 400, horizontal=FALSE)
coefplot(reg,varnames=vlabels,vertical=FALSE,main="",ylim=c(-2,2))
dev.off()


###
# maps 

# from Ruedin: https://druedin.com/2015/11/21/a-modified-shapefile-for-plotting-swiss-cantons/

setwd("...")

x <- readOGR(dsn = ".", layer = "ch-cantons")

#popmap <- as.numeric(fish_svp_dict$theta)
popmap <- as.numeric(fish_svp_dict@theta)

x$popmap <- c(popmap[1],NA,NA,popmap[3],popmap[2],popmap[4],popmap[5],popmap[6],
              popmap[14],popmap[7],popmap[8],popmap[9],popmap[10],
              popmap[11],popmap[12],popmap[17],popmap[13],popmap[15],
              popmap[16],popmap[19],popmap[18],popmap[20],popmap[21],popmap[22],
              popmap[23],popmap[24])

x$popmap = (x$popmap-mean(x$popmap,na.rm=T))/sd(x$popmap,na.rm=T)

popmapind <- aggregate(selects_svp$pop, by=list(selects_svp$cantid), FUN=mean)
popmapind <- popmapind$x

x$popmapind <- c(popmapind[1],NA,NA,popmapind[4],popmapind[3],
                 popmapind[5],popmapind[6],popmapind[7],popmapind[26],popmapind[8],
                 popmapind[9],popmapind[10],popmapind[11],
                 popmapind[12],popmapind[13],popmapind[18],popmapind[14],
                 popmapind[16],popmapind[17],popmapind[20],popmapind[19],
                 popmapind[21],popmapind[22],popmapind[23],popmapind[24],
                 popmapind[25])

x$popmapind = (x$popmapind-mean(x$popmapind,na.rm=T))/sd(x$popmapind,na.rm=T)

postscript("fig4a_mapcant.eps", width = 800, height = 800, horizontal=FALSE)
col <- rev(gray.colors(24))
spplot(x, "popmap" , col.regions=col, main="", colorkey=T)
spplot(x, "popmap" , col.regions=col, main="", colorkey=F)
dev.off()

postscript("fig4b_mapind.eps", width = 800, height = 800, horizontal=FALSE)
col <- rev(gray.colors(24))
spplot(x, "popmapind" , col.regions=col, main="", colorkey=T)
spplot(x, "popmapind" , col.regions=col, main="", colorkey=F)
dev.off()

# individual and contextual 
krzel_acr2 <- c("AG","NA","NA","BL","BS","BE","FR","GE","GL","GR","JU","LU","NE",
    "NW","OW","SG","SH","SZ","SO","TG","TI","UR","VS","VD","ZH","ZG")

postscript("fig5_cantind.eps", width = 800, height = 800, horizontal=FALSE)
plot(x$popmap,x$popmapind,type="n",ylab="Populist Attitudes",
     xlab="Contextual Populism")
abline(reg <- lm(x$popmapind~x$popmap),lty=2)
abline(reg <- lm(no_obw_ind~no_obw_con))
text(x$popmap,x$popmapind,krzel_acr2)
dev.off()

no_obw_con <- c(x$popmap[1:14],x$popmap[16:24])
no_obw_ind <- c(x$popmapind[1:14],x$popmapind[16:24])

cor.test(x$popmap,x$popmapind,na.rm=TRUE)
cor.test(no_obw_con,no_obw_ind,na.rm=TRUE)

plot(no_obw_con,no_obw_ind)
abline(reg <- lm(no_obw_ind~no_obw_con))

bdp_map <- c(0,1,1,0,0,0,0,0,0,0,1,0,1,
         1,1,0,1,0,0,0,1,1,0,0,0,1)

cor.test(x$popmap,bdp_map,na.rm=TRUE)
