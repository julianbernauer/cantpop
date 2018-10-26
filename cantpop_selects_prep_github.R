############################################################################
#QTA cantonal branches SVP and analysis of populist attitudes using SELECTS# 
############################################################################

# Julian Bernauer (MZES Mannheim) and Anna Storz (IPW Bern)
# Oktober 2018

# Preparation Selects data
# Source: FORS, https://forsbase.unil.ch/datasets/dataset-public-detail/15433/1328

setwd("...")

# Selects panel 
selects <- read.csv("selects15_panel.csv",header=TRUE,sep=",")

# Two populism items
#Most politicians do not care about the people
summary(selects$W4_CSES_pop1)
#Politicians are the main problem in Switzerland
summary(selects$W4_CSES_pop2)

selects$pop1 <- NA 
selects$pop1[selects$W4_CSES_pop1=="Strongly disagree"] <- -2
selects$pop1[selects$W4_CSES_pop1=="Rather disagree"] <- -1
selects$pop1[selects$W4_CSES_pop1=="Neither nor"] <- 0
selects$pop1[selects$W4_CSES_pop1=="Rather agree"] <- 1
selects$pop1[selects$W4_CSES_pop1=="Strongly agree"] <- 2

selects$pop2 <- NA 
selects$pop2[selects$W4_CSES_pop2=="Strongly disagree"] <- -2
selects$pop2[selects$W4_CSES_pop2=="Rather disagree"] <- -1
selects$pop2[selects$W4_CSES_pop2=="Neither nor"] <- 0
selects$pop2[selects$W4_CSES_pop2=="Rather agree"] <- 1
selects$pop2[selects$W4_CSES_pop2=="Strongly agree"] <- 2

selects$pop <- (selects$pop1+selects$pop2)/2
summary(selects$pop1)
summary(selects$pop2)
summary(selects$pop)

# SVP vote 
selects$svp <- NA 
selects$svp[selects$f10200r=="Voted"] <- 0
selects$svp[selects$f10300main7=="SVP/UDC - Swiss People's Party"] <- 1
summary(selects$svp)

# Controls
selects$alter <- selects$age
selects$alter[selects$age=="No answer"] <- NA
summary(selects$alter)
selects$age <- as.numeric(as.character(selects$alter))

selects$fem <- NA
selects$fem[selects$sex=="Female"] <- 1
selects$fem[selects$sex=="Male"] <- 0

selects$interest <- NA
selects$interest[selects$f10100=="Not interested at all"] <- 1
selects$interest[selects$f10100=="Rather not interested"] <- 2
selects$interest[selects$f10100=="Rather interested"] <- 3
selects$interest[selects$f10100=="Very interested"] <- 4

# Income 
summary(selects$f28910)
selects$income <- NA
selects$income[selects$f28910=="Less than 2000 CHF"] <- 1
selects$income[selects$f28910=="2'001-3'000 CHF"] <- 2
selects$income[selects$f28910=="3'001-4'000 CHF"] <- 3
selects$income[selects$f28910=="4'001-5'000 CHF"] <- 4
selects$income[selects$f28910=="5'001-6'000 CHF"] <- 5
selects$income[selects$f28910=="6'001-7'000 CHF"] <- 6
selects$income[selects$f28910=="7'001-8'000 CHF"] <- 7
selects$income[selects$f28910=="8'001-9'000 CHF"] <- 8
selects$income[selects$f28910=="9'001-10'000 CHF"] <- 9
selects$income[selects$f28910=="10'001-11'000 CHF"] <- 10
selects$income[selects$f28910=="11'001 - 12'000 CHF"] <- 11
selects$income[selects$f28910=="12'001-13'000 CHF"] <- 12
selects$income[selects$f28910=="13'001-14'000 CHF"] <- 13
selects$income[selects$f28910=="14'001-15'000 CHF"] <- 14
selects$income[selects$f28910=="Over 15'000 CHF"] <- 15
summary(selects$income)

# Education 
selects$edu <- NA
selects$edu[selects$f21310=="Basic vocational training"] <- 0                                     
selects$edu[selects$f21310=="Diploma school"] <- 0 
selects$edu[selects$f21310=="High school, school preparing for the baccalaureate"] <- 0                
selects$edu[selects$f21310=="Higher vocational college: technical, economics, social work, etc."] <- 1
selects$edu[selects$f21310=="Higher vocational education with master diploma"] <- 1
# "No answer"                                                         
selects$edu[selects$f21310=="No education"] <- 0
# "Other"                                                             
selects$edu[selects$f21310=="Primary School"] <- 0                                                      
selects$edu[selects$f21310=="Secondary School"] <- 0                                                    
selects$edu[selects$f21310=="Secondary school vocational diploma"] <- 0                                 
selects$edu[selects$f21310=="Trading school"] <- 0                                                     
selects$edu[selects$f21310=="University of applied sciences, pedagogical university"] <- 1              
selects$edu[selects$f21310=="University, Federal institute of technology"] <- 1                        
selects$edu[selects$f21310=="Vocational training, apprenticeship"] <- 0


# Cantonal IDs
# Matching order of manifestos 

selects$cantid <- NA
selects$cantid[selects$canton=="Zürich"] <- 24
selects$cantid[selects$canton=="Bern"] <- 5
selects$cantid[selects$canton=="Luzern"] <- 10
selects$cantid[selects$canton=="Uri"] <- 21
selects$cantid[selects$canton=="Schwyz"] <- 16
selects$cantid[selects$canton=="Obwalden"] <- 13
selects$cantid[selects$canton=="Nidwalden"] <- 12
selects$cantid[selects$canton=="Glarus"] <- 26
selects$cantid[selects$canton=="Zug"] <- 25
selects$cantid[selects$canton=="Fribourg"] <- 6
selects$cantid[selects$canton=="Solothurn"] <- 17
selects$cantid[selects$canton=="Basel-Stadt"] <- 4
selects$cantid[selects$canton=="Basel-Landschaft"] <- 3
selects$cantid[selects$canton=="Schaffhausen"] <- 14
selects$cantid[selects$canton=="Appenzell Ausserrhoden"] <- 15
selects$cantid[selects$canton=="Appenzell Innerrhoden"] <- 2
selects$cantid[selects$canton=="St. Gallen"] <- 18
selects$cantid[selects$canton=="Graubünden"] <- 8
selects$cantid[selects$canton=="Aargau"] <- 1
selects$cantid[selects$canton=="Thurgau"] <- 20
selects$cantid[selects$canton=="Ticino"] <- 19
selects$cantid[selects$canton=="Vaud"] <- 23
selects$cantid[selects$canton=="Valais"] <- 22
selects$cantid[selects$canton=="Neuchâtel"] <- 11
selects$cantid[selects$canton=="Genève"] <- 7
selects$cantid[selects$canton=="Jura"] <- 9
summary(selects$cantid)
tabulate(selects$cantid)


# Exclude missings 
selects_c <- selects[!is.na(selects$svp)&!is.na(selects$pop)&!is.na(selects$canton),]
attach(selects_c)
summary(svp)
length(svp)
# N = 4112 

# Export for "cantopo_wfish.r" 
save(selects_c,file="selects_cantpop_panel.Rdata")

