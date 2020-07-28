#### sample size determination to identify .5 DIF on one item, 1000 simulations ####

# 540 items exposures for 720 examinees in Group A, and 200 examinees in Group B taking all items, for 80 items


# install catR,stringr,imputeMulti packages if needed

# load R packages
library("catR")
library("stringr")
library("imputeMulti")

set.seed(42)

# set item parameters
a <- round(runif(80,-3,3), 2)
b <- round(runif(80,-1,1), 2)
# or can use "c(2.00, -2.00, ...)" with 80 of your own values for "a" and "b" above. a and b should have the same number of elements (ensure 80 values, if using k=80 items as below)
itembank <- cbind(a, b, c=0, d=1)
itembank <- as.matrix(itembank) # form item matrix

#OR#

# generate item difficulties (k = 80)
itembank<-genDichoMatrix(items = 80, model = "1PL")


# make another copy of the item bank to edit
itembankspecial<-itembank


# simulate .5 item difficulty difference
itembankspecial[1,2]<-itembankspecial[1,2]+.5  # simulate itembankspecial item 1 being .5 more difficult than itembank item 1
# or
#itembankspecial[1,2]<-itembankspecial[1,2]-.5  # sim item 1 being .5 less difficult


filenames<-sprintf("SIFILE%d.txt", 1:1000) # generate a list of filenames with sequential numbers at end




# ONLY RUN THE NEXT LINES IF YOU WISH TO generate 1000 simulated data samples, outputting 1000 text files

for (i in 1:1000) {  
# simulate number of examinees in Group A with normal distribution # CHANGE NUMBER AFTER rnorm
thetas<-rnorm(720)
res<-as.matrix(genPattern(thetas,itembank))

# replace some values per column with NAs (25% currently)
createNAs_25 <- function (x, pctNA = 0.25) {
  n <- nrow(x)
  p <- ncol(x)
  NAloc <- rep(FALSE, n * p)
  NAloc[sample.int(n * p, floor(n * p * pctNA))] <- TRUE
  x[matrix(NAloc, nrow = n, ncol = p)] <- NA
  return(x)
}
res<-createNAs_25(res)
res<-data.frame("A",res)

# simulate number of examinees in Group B
thetasspecial<-rnorm(200)
resspecial<-as.matrix(genPattern(thetasspecial,itembankspecial))


# resspecial<-createNAs_25(resspecial) #  add NAs to resspecial


# for Group B, simulate everyone seeing every item
resspecial<-data.frame("B",resspecial)


names(res)[names(res)=="X.A."] <- "dif"
names(resspecial)[names(resspecial)=="X.B."] <- "dif"
resps<-rbind(res,resspecial)

# combine res and resspecial to get responses into one data frame called "resps"

resps <- sapply(resps, as.character)

resps[is.na(resps)] <- " "

resps_IDs_added <- cbind("ID" = sprintf("%04d", 1:nrow(resps)), resps)   # adds 4-digit number identifier for person ID
resp_strings<-apply(resps_IDs_added, 1, paste, collapse="") # puts each person's responses into one string for WINSTEPS


write.table(resp_strings, file=filenames[i], row.names=FALSE, col.names=FALSE, quote=FALSE)

}





# move SIFILES to directory where Winsteps batch file will be run




# run WINSTEPS in Batch Mode ; also need specification file that tells WINSTEPS how to read the SIFILES




# move TFILEs to their own folder location, and set working directory to this location







# read text output into R


# install data.table,tidyr,dplyr packages if needed


library(data.table)

list_of_files <- list.files(path = ".", recursive = TRUE,
                            pattern = "\\.txt$", full.names = TRUE)

# read all the files and create a FileName column to store filenames
DT <- rbindlist(sapply(list_of_files, fread, simplify = FALSE),
                 use.names = TRUE, idcol = "FileName" )

DT<-DT[,c(1,3)]

DT<-DT[grep("^A ", DT$V2), ]

DT$V2 <- gsub('\\s+', ' ', DT$V2)


library(tidyr)

DT<-separate(DT, V2, into=c("GroupA", "ObsExpAvgGroupA", "DifMeasureGroupA", "DifSEGroupA", "GroupB", "ObsExpAvgGroupB", "DifMeasureGroupB", "DifSEGroupB", "DifContrast", "JointSE", "RW_t", "RW_df", "RW_prob", "MH_ChiSq", "MH_Prob", "Cumlor", "Slices", "ItemNo", "ItemName"), sep = " ", remove = TRUE, convert = FALSE, extra = "warn", fill = "warn")

DT$RW_prob <- as.numeric(DT$RW_prob)

library(dplyr)
datacheck<-select(filter(DT, DT$ItemName == "IT001" & DT$RW_prob < .05))


# filename convention: simulation__w_x__y_z__DIF.csv contains DIF results of "w" item exposures from "x" persons in Group A, and "y" item exposures from "z" persons in Group B

write.csv(DT, "simulation__540r_720__200r_200__DIF.csv", row.names = FALSE)

