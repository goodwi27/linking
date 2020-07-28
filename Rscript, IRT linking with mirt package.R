# install mirt and catR packages if necessary
#update.packages()
#install.packages()


#### simulated data ####

# MIRT with sim data converged after 102 EM iterations

library(catR)

set.seed(42)
# generate item parameters (k = 350)
simitembank<-genDichoMatrix(items = 350, model = "2PL")

# simulate item responses
simres<-genPattern(th=Theta,it=simitembank)
colnames(simres)<-seq(001,350)

library(mirt)
mirtsim <- mirt(simres,1); mirtsim # MIRT with simulated responses
summary(mirtsim)






#### MIRT analyses ####


# exploratory IRTs
#mirt_model_EIRT <- mirt(data_PAcommonscale[,2:325],2); summary(mirt_model_EIRT) # 2 factors
#mirt_model2_EIRT <- mirt(data_PAcommonscale[,2:325],3); summary(mirt_model2_EIRT) # 3 factors


#### 2PL MIRT analyses ####

#mirt_model_2PL <- mirt(data_PAcommonscale[,2:325],1); summary(mirt_model_2PL) # 1-factor model
# graded or 2PL itemtype is default

# USE GRADED response model



# Items come from 5 tests: 110 items on Test A, 27 on Test B, 116 on Test C, 78 on Test D, and 10 on Test E


# load package
library(mirt)


#run these before running MIRT
removeEmptyRows<-list(removeEmptyRows=TRUE) # needed for mirt "technical" argument to remove responses only containing NAs

itemtypes_dichopoly <- as.character(c(rep("2PL",253), rep("graded",19), rep("2PL",2), "graded", rep("2PL",8), rep("graded",47), rep("2PL",6), rep("graded",5))) # save vector of text indicating whether items should be scored as 2PL or graded (contains 341 items)

options(max.print=350) # increase the number of displayed output rows on screen


# IRT model won't converge with real item response data
mirt_model_1 <- mirt(data_PAcommonscale[,c(2:165,173:349)],1,itemtype=itemtypes_dichopoly,technical=removeEmptyRows); mirt_model_1 # save MIRT model, 1st argument: data, 2nd argument: assume 1 factor, 3rd argument: specify item types (scored 2PL or GRM) loaded earlier, 4th argument: removeEmptyRows list loaded earlier (specifies extra technical information)


summary(mirt_model_1)
MDIFF(mirt_model_1)
MDISC(mirt_model_1)



# model run omitting worst discriminating item (item # 247)
#mirt_model_2 <- mirt(data_PAcommonscale[,c(2:165,173:248,250:349)],1,itemtype=itemtypes_dichopoly[c(1:247,249:341)],technical=removeEmptyRows); mirt_model_2
#MDISC(mirt_model_2)


# model run omitting two worst discriminating items and also omitting last 7 with fewer responsesL = DOES converge
mirt_model_graded <- mirt(data_PAcommonscale[,c(2:154,156:165,173:255,257:349)],1,itemtype=itemtypes_dichopoly[c(1:153,155:247,249:341)],technical=removeEmptyRows); mirt_model_graded
summary(mirt_model_graded)


itemdiscrims<-as.data.frame(MDISC(mirt_model_graded))
itemdiffs<-as.data.frame(MDIFF(mirt_model_graded))
itemdiffsanddiscrims<-cbind(itemdiffs,itemdiscrims)

write.csv(itemdiffsanddiscrims, "itemdiffsanddiscrims.csv", row.names = TRUE)







Theta<-as.matrix(seq(-4,4, by = .1)) # simulated examinee ability levels





#### predicted raw sum score on each subtest ####


filenames<-colnames(data_PAcommonscale[,c(2:154,156:165,173:255,257:349)]) # save text vector of names of items

# probability trace value extractions for all items
for (i in 111:339) {
  assign(paste("probtrace_", filenames[i], sep = ""), probtrace(extract.item(mirt_model_graded,i),Theta))
}



# sum probability of P.1 for each dichotomous item, sum by subtest, at each theta (ability level)

TestBsummed<-probtrace_item_1 + probtrace_item_2 + probtrace_item_3 + probtrace_item_4 + probtrace_item_5 + probtrace_item_6 + probtrace_item_7 + probtrace_item_8 + probtrace_item_9 + probtrace_item_10 + probtrace_item_11 + probtrace_item_12 + probtrace_item_13 + probtrace_item_14 + probtrace_item_15 + probtrace_item_16 + probtrace_item_17 + probtrace_item_18 + probtrace_item_19 + probtrace_item_20 + probtrace_item_21 + probtrace_item_22 + probtrace_item_23 + probtrace_item_24 + probtrace_item_25 + probtrace_item_26 + probtrace_item_27  # 27 items













