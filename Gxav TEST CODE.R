
#####################################################################
# Load packages
#####################################################################

library(SOAR) # to handle memory
library(Matrix) # to handle matrix
library(Hmisc) ; library(MASS) # for descriptive analysis
library(RTextTools) # to stem the words
library(tau) # for n-grams
library(tm) # to remove duplicated blanks and punctuation marks
library(clim.pact) # to convert to lower case

Objects()

#####################################################################
#####################################################################
# Some useful function
#####################################################################
#####################################################################

#####################################################################
# Function to group predictions per essay set
#####################################################################

Group_t<-function(E) {
  out<-rep(0,nrow(test))
  for (i in 1:10) out[test$EssaySet==i]<-E[[i]]
  out
}
Store(Group_t)

#####################################################################
# Function to check if predictions are consistent with 5 folds cv predictions
#####################################################################

graph3<-function(s_t,s) {
  par(mfrow=c(4,3))
  for (i in 1:10) {
    if (!is.na(s_t[test$EssaySet==i][1])) { 
      plot(density(s_t[test$EssaySet==i]),main=paste("set",i))
      lines(density(s[training$EssaySet==i]),col=2) }
  }
}
Store(graph3)

#####################################################################
#####################################################################
# Read file
#####################################################################
#####################################################################

test<-read.csv('private_leaderboard.tsv', header=TRUE, 
	sep = "\t", fileEncoding="windows-1252",quote="")
str(test)
str(training)

#####################################################################
# convert EssayText_t to character format
#####################################################################

EssayText_t<-test[,3] ; EssayText_t<-as.character(EssayText_t) ; Store(EssayText_t)
test<-test[,-3] ; Store(test)

#####################################################################
# add score1.. with NAs
#####################################################################

test$Score1<-NA ; test$Score2<-NA ; test$ScoreA<-NA ; 
test$isgood<-NA ; Store(test)

#####################################################################
# Add type and grade
#####################################################################

test$type<-rep(1,nrow(test)) ; test$type[test$EssaySet %in% c(5:6)]<-2 ; test$type<-factor(test$type)
test$grade<-rep(10,nrow(test)) ; test$grade[test$EssaySet %in% c(10)]<-8 ; test$grade<-factor(test$grade)
test$subject<-rep("science",nrow(test)) ; test$subject[test$EssaySet %in% c(3,4)]<-"ELA"
test$subject[test$EssaySet %in% c(5,6)]<-"biology" ; test$subject[test$EssaySet %in% c(7:9)]<-"english"
test$subject<-factor(test$subject)

#####################################################################
# number of characters
#####################################################################

test$NCHAR<-sapply(EssayText_t,nchar)
# check if consistent with training
par(mfrow=c(1,1)) ; plot(density(training$NCHAR)) ; lines(density(test$NCHAR),col=2)

#####################################################################
# n-grams
#####################################################################

L1G_t<-list()
for (i in 1:nrow(test)) L1G_t[[i]]<-textcnt(EssayText_t[i], method="string", n=1L)
Store(L1G_t)

L2G_t<-list()
for (i in 1:nrow(test)) L2G_t[[i]]<-textcnt(EssayText_t[i], method="string", n=2L)
Store(L2G_t)

L3G_t<-list()
for (i in 1:nrow(test)) L3G_t[[i]]<-textcnt(EssayText_t[i], method="string", n=3L)
Store(L3G_t)

L4G_t<-list()
for (i in 1:nrow(test)) L4G_t[[i]]<-textcnt(EssayText_t[i], method="string", n=4L)
Store(L4G_t)

L5G_t<-list()
for (i in 1:nrow(test)) L5G_t[[i]]<-textcnt(EssayText_t[i], method="string", n=5L)
Store(L5G_t)

#####################################################################
# count number of words
#####################################################################

test$NW<-sapply(L1G_t,sum)
# check if consistent with training
par(mfrow=c(1,1)) ; plot(density(training$NW)) ; lines(density(test$NW),col=2)

#####################################################################
#stats on words length
#####################################################################

tmp<-t(sapply(L1G_t, function(x) quantile(sapply(rep(names(x),x),nchar),c(0.75,0.9,0.95,0.99))))
test$Q90<-tmp[,2] ; plot(density(training$Q90)) ; lines(density(test$Q90),col=2)
test$Q95<-tmp[,3] ; plot(density(training$Q95)) ; lines(density(test$Q95),col=2)
test$Q99<-tmp[,4] ; plot(density(training$Q99)) ; lines(density(test$Q99),col=2)

#####################################################################
#stem words
#####################################################################

STEM1G_t = lapply(L1G_t,function(x) wordStem(names(x))) ; Store(STEM1G_t)
STEM2G_t = lapply(L2G_t,function(x) wordStem(names(x))) ; Store(STEM2G_t)
STEM3G_t = lapply(L3G_t,function(x) wordStem(names(x))) ; Store(STEM3G_t)
STEM4G_t = lapply(L4G_t,function(x) wordStem(names(x))) ; Store(STEM4G_t)

STEM_t<-list()
for (i in 1:nrow(test)) STEM_t[[i]]<-c(STEM1G_t[[i]],STEM2G_t[[i]],STEM3G_t[[i]],STEM4G_t[[i]])
Store(STEM_t)
     
#####################################################################
#% of precise verbs
#####################################################################

Prec_verbs<-read.csv("precise_verbs.csv",header=FALSE)
Prec_verbs = sapply(as.character(Prec_verbs[,1]),function(x) wordStem(x))
NPrV<-sapply(STEM1G_t,function(x) sum((x %in%  Prec_verbs)==TRUE))
NPrV2<-sapply(STEM2G_t,function(x) sum((x %in%  Prec_verbs)==TRUE))
test$PPrV<-(NPrV+NPrV2)/test$NW
# check if consistent with training
par(mfrow=c(1,1)) ; plot(density(training$PPrV)) ; lines(density(test$PPrV),col=2)

#####################################################################
#% of transition words
#####################################################################

Trans_words<-read.csv("transition_words.csv",header=FALSE)
Trans_words<-as.character(Trans_words[,1])
NTransW<-sapply(L1G_t,function(x) sum((names(x) %in%  Trans_words)==TRUE))
NTransW2<-sapply(L2G_t,function(x) sum((names(x) %in%  Trans_words)==TRUE))
NTransW3<-sapply(L3G_t,function(x) sum((names(x) %in%  Trans_words)==TRUE))
NTransW4<-sapply(L4G_t,function(x) sum((names(x) %in%  Trans_words)==TRUE))
test$PTransW<-(NTransW+NTransW2+NTransW3+NTransW4)/test$NW
# check if consistent with training
par(mfrow=c(1,1)) ; plot(density(training$PTransW)) ; lines(density(test$PTransW),col=2)

#####################################################################
#% of misspelling
#####################################################################

NErr<-sapply(L1G_t,function(x) sum((names(x) %in%  VOCAB)==FALSE))
test$PErr<-NErr/test$NW
# check if consistent with training
par(mfrow=c(1,1)) ; plot(density(training$PErr)) ; lines(density(test$PErr),col=2)

Store(test)

#####################################################################
#some data work to ensure some consistency accross EssayTexts
#####################################################################

tmp<-EssayText_t
for (i in 1:nrow(test)) tmp[i]<-gsub("I '","I'",tmp[i])
for (i in 1:nrow(test)) tmp[i]<-gsub(" 's ","'s ",tmp[i])
for (i in 1:nrow(test)) tmp[i]<-gsub("`","'",tmp[i])
for (i in 1:nrow(test)) tmp[i]<-gsub("''","\"",tmp[i])
for (i in 1:nrow(test)) tmp[i]<-gsub("' ","\" ",tmp[i])
for (i in 1:nrow(test)) tmp[i]<-gsub(" '"," \"",tmp[i])
for (i in 1:nrow(test)) tmp[i]<-gsub("\"\"","\"",tmp[i])
EssayText_t<-tmp
Store(EssayText_t)

#####################################################################
#presence of QUOTE
#####################################################################

busted<-strsplit(EssayText_t,split="\"",fixed=TRUE)
test$QuotM<-(sapply(busted,length)-1)
# check if consistent with training
par(mfrow=c(1,1)) ; plot(density(training$QuotM)) ; lines(density(test$QuotM),col=2)

#####################################################################
#presence of  )
#####################################################################

busted<-strsplit(EssayText_t,split=")",fixed=TRUE)
test$BK<-(sapply(busted,length)-1)
# check if consistent with training
par(mfrow=c(1,1)) ; plot(density(training$BK)) ; lines(density(test$BK),col=2)

#####################################################################
#presence of  ,
#####################################################################

busted<-strsplit(EssayText_t,split=",",fixed=TRUE)
test$CM<-(sapply(busted,length)-1)/test$NW
# check if consistent with training
par(mfrow=c(1,1)) ; plot(density(training$CM)) ; lines(density(test$CM),col=2)

#####################################################################
#presence of 's
#####################################################################

busted<-strsplit(EssayText_t,split="'s",fixed=TRUE)
test$POS<-(sapply(busted,length)-1)
# check if consistent with training
par(mfrow=c(1,1)) ; plot(density(training$POS)) ; lines(density(test$POS),col=2)
Store(test)

#####################################################################
#####################################################################
# split test per EssaySet
#####################################################################
#####################################################################

update_split_t<-function() {
  test.E1<-test[test$EssaySet==1,] ; Store(test.E1)
  test.E2<-test[test$EssaySet==2,] ; Store(test.E2)
  test.E3<-test[test$EssaySet==3,] ; Store(test.E3)
  test.E4<-test[test$EssaySet==4,] ; Store(test.E4)
  test.E5<-test[test$EssaySet==5,] ; Store(test.E5)
  test.E6<-test[test$EssaySet==6,] ; Store(test.E6)
  test.E7<-test[test$EssaySet==7,] ; Store(test.E7)
  test.E8<-test[test$EssaySet==8,] ; Store(test.E8)
  test.E9<-test[test$EssaySet==9,] ; Store(test.E9)
  test.E10<-test[test$EssaySet==10,] ; Store(test.E10)
}
Store(update_split_t)
update_split_t()

#####################################################################
#####################################################################
# Produce document-term matrix
#####################################################################
#####################################################################

#####################################################################
# keep stemmed words as in training set
#####################################################################

tmp<-data.frame(table(unlist(STEM)))
tmp<-as.character(tmp[tmp[,2]>20,1])

STEMsh_t<-STEM_t
for (i in 1:nrow(test)) {
  STEMsh_t[[i]]<-STEM_t[[i]][STEM_t[[i]] %in% tmp]
	if (i%%100==0) print(i)}
Store(STEMsh_t)

#####################################################################
# create features matrix
#####################################################################

Feat_mat_t<-NULL
for (i in 1:nrow(test)) {
  if (length(STEMsh_t[[i]])!=0) Feat_mat_t=  rbind(Feat_mat_t,data.frame(row_id=i,W=STEMsh_t[[i]]))
	if (i%%100==0) print(i)}

#####################################################################
# assign number to words (token) as in training set
#####################################################################

tmp<-Feat_mat[,c(2,3)] ; tmp$W<-as.character(tmp$W)
tmp<-unique(tmp)
tmp2<-Feat_mat_t ; tmp2$W<-as.character(tmp2$W)
Feat_mat_t<-merge(tmp2,tmp)
Store(Feat_mat_t)

#####################################################################
# incidence matrix
#####################################################################

bin_mat_t<-as.matrix(sparseMatrix(Feat_mat_t$row_id,Feat_mat_t$token,
x=rep(1,nrow(Feat_mat_t)))) ; 
if (ncol(bin_mat)>ncol(bin_mat_t)) 
  for (j in (ncol(bin_mat_t)+1):ncol(bin_mat)) bin_mat_t<-cbind(bin_mat_t,0)
Store(bin_mat_t)

#####################################################################
# split incidence matrix per EssaySet
#####################################################################

Bin_mat_t.E1<-bin_mat_t[test$EssaySet==1,] ; Store(Bin_mat_t.E1)
Bin_mat_t.E2<-bin_mat_t[test$EssaySet==2,] ; Store(Bin_mat_t.E2)
Bin_mat_t.E3<-bin_mat_t[test$EssaySet==3,] ; Store(Bin_mat_t.E3)
Bin_mat_t.E4<-bin_mat_t[test$EssaySet==4,] ; Store(Bin_mat_t.E4)
Bin_mat_t.E5<-bin_mat_t[test$EssaySet==5,] ; Store(Bin_mat_t.E5)
Bin_mat_t.E6<-bin_mat_t[test$EssaySet==6,] ; Store(Bin_mat_t.E6)
Bin_mat_t.E7<-bin_mat_t[test$EssaySet==7,] ; Store(Bin_mat_t.E7)
Bin_mat_t.E8<-bin_mat_t[test$EssaySet==8,] ; Store(Bin_mat_t.E8)
Bin_mat_t.E9<-bin_mat_t[test$EssaySet==9,] ; Store(Bin_mat_t.E9)
Bin_mat_t.E10<-bin_mat_t[test$EssaySet==10,] ; Store(Bin_mat_t.E10)

#####################################################################
# reduce dimension (new matrix start with small letter !)
#####################################################################

bin_mat_t.E1<-Bin_mat_t.E1[,-which(colSums(Bin_mat.E1)<5)] ; Store(bin_mat_t.E1)
bin_mat_t.E2<-Bin_mat_t.E2[,-which(colSums(Bin_mat.E2)<5)] ; Store(bin_mat_t.E2)
bin_mat_t.E3<-Bin_mat_t.E3[,-which(colSums(Bin_mat.E3)<5)] ; Store(bin_mat_t.E3)
bin_mat_t.E4<-Bin_mat_t.E4[,-which(colSums(Bin_mat.E4)<5)] ; Store(bin_mat_t.E4)
bin_mat_t.E5<-Bin_mat_t.E5[,-which(colSums(Bin_mat.E5)<5)] ; Store(bin_mat_t.E5)
bin_mat_t.E6<-Bin_mat_t.E6[,-which(colSums(Bin_mat.E6)<5)] ; Store(bin_mat_t.E6)
bin_mat_t.E7<-Bin_mat_t.E7[,-which(colSums(Bin_mat.E7)<5)] ; Store(bin_mat_t.E7)
bin_mat_t.E8<-Bin_mat_t.E8[,-which(colSums(Bin_mat.E8)<5)] ; Store(bin_mat_t.E8)
bin_mat_t.E9<-Bin_mat_t.E9[,-which(colSums(Bin_mat.E9)<5)] ; Store(bin_mat_t.E9)
bin_mat_t.E10<-Bin_mat_t.E10[,-which(colSums(Bin_mat.E10)<5)] ; Store(bin_mat_t.E10)

#####################################################################
#####################################################################
# Ridit transformation
#####################################################################
#####################################################################

Ridit_mat_t.E1<-RIDIT_mat(bin_mat.E1,bin_mat_t.E1) ; Store(Ridit_mat_t.E1)
Ridit_mat_t.E2<-RIDIT_mat(bin_mat.E2,bin_mat_t.E2) ; Store(Ridit_mat_t.E2)
Ridit_mat_t.E3<-RIDIT_mat(bin_mat.E3,bin_mat_t.E3) ; Store(Ridit_mat_t.E3)
Ridit_mat_t.E4<-RIDIT_mat(bin_mat.E4,bin_mat_t.E4) ; Store(Ridit_mat_t.E4)
Ridit_mat_t.E5<-RIDIT_mat(bin_mat.E5,bin_mat_t.E5) ; Store(Ridit_mat_t.E5)
Ridit_mat_t.E6<-RIDIT_mat(bin_mat.E6,bin_mat_t.E6) ; Store(Ridit_mat_t.E6)
Ridit_mat_t.E7<-RIDIT_mat(bin_mat.E7,bin_mat_t.E7) ; Store(Ridit_mat_t.E7)
Ridit_mat_t.E8<-RIDIT_mat(bin_mat.E8,bin_mat_t.E8) ; Store(Ridit_mat_t.E8)
Ridit_mat_t.E9<-RIDIT_mat(bin_mat.E9,bin_mat_t.E9) ; Store(Ridit_mat_t.E9)
Ridit_mat_t.E10<-RIDIT_mat(bin_mat.E10,bin_mat_t.E10) ; Store(Ridit_mat_t.E10)

#####################################################################
#####################################################################
# Produce another document-term matrix in 2 steps
# stemming is done before producing n-grams 
# and n-grams are produced on a selection of stemmed 1-grams
#####################################################################
#####################################################################

#####################################################################
# transform EssayText
#####################################################################

EssayText_t2<-EssayText_t
EssayText_t2<-sapply(EssayText_t, function(x) gsub("n't"," not",x))
EssayText_t2<-sapply(EssayText_t, function(x) gsub("cannot","can not",x))
EssayText_t2<-sapply(EssayText_t2, stripWhitespace)
EssayText_t2<-sapply(EssayText_t2, tolower)
EssayText_t2<-sapply(EssayText_t2, removePunctuation)

#####################################################################
# stem text
#####################################################################

STEM.1_t<-stem_text(EssayText_t2) ; Store(STEM.1_t)

#####################################################################
# Produce document-term matrix Bin_mat.1.Ex
#####################################################################

#####################################################################
# keep stemmed words as in test set
#####################################################################

tmp<-data.frame(table(unlist(STEM.1)))
tmp<-as.character(tmp[tmp[,2]>5,1])

STEMsh.1_t<-STEM.1_t
for (i in 1:nrow(test)) {
  STEMsh.1_t[[i]]<-STEM.1_t[[i]][STEM.1_t[[i]] %in% tmp]
  if (i%%100==0) print(i)}
Store(STEMsh.1_t)

#####################################################################
# create features matrix
#####################################################################

Feat_mat.1_t<-NULL
for (i in 1:nrow(test)) {
  if (length(STEMsh.1_t[[i]])!=0) Feat_mat.1_t=  rbind(Feat_mat.1_t,data.frame(row_id=i,W=STEMsh.1_t[[i]]))
  if (i%%100==0) print(i)}

#####################################################################
# assign number to words (token) as in training set
#####################################################################

tmp<-Feat_mat.1[,c(2,3)] ; tmp$W<-as.character(tmp$W)
tmp<-unique(tmp)
tmp2<-Feat_mat.1_t ; tmp2$W<-as.character(tmp2$W)
Feat_mat.1_t<-merge(tmp2,tmp)
Store(Feat_mat.1_t)

#####################################################################
# incidence matrix
#####################################################################

bin_mat.1_t<-as.matrix(sparseMatrix(Feat_mat.1_t$row_id,Feat_mat.1_t$token,
                                    x=rep(1,nrow(Feat_mat.1_t)))) 
if (ncol(bin_mat.1)>ncol(bin_mat.1_t)) 
  for (j in (ncol(bin_mat.1_t)+1):ncol(bin_mat.1)) bin_mat.1_t<-cbind(bin_mat.1_t,0)
Store(bin_mat.1_t)


#####################################################################
# split incidence matrix per EssaySet
#####################################################################

Bin_mat.1_t.E1<-bin_mat.1_t[test$EssaySet==1,] ; Store(Bin_mat.1_t.E1)
Bin_mat.1_t.E2<-bin_mat.1_t[test$EssaySet==2,] ; Store(Bin_mat.1_t.E2)
Bin_mat.1_t.E3<-bin_mat.1_t[test$EssaySet==3,] ; Store(Bin_mat.1_t.E3)
Bin_mat.1_t.E4<-bin_mat.1_t[test$EssaySet==4,] ; Store(Bin_mat.1_t.E4)
Bin_mat.1_t.E5<-bin_mat.1_t[test$EssaySet==5,] ; Store(Bin_mat.1_t.E5)
Bin_mat.1_t.E6<-bin_mat.1_t[test$EssaySet==6,] ; Store(Bin_mat.1_t.E6)
Bin_mat.1_t.E7<-bin_mat.1_t[test$EssaySet==7,] ; Store(Bin_mat.1_t.E7)
Bin_mat.1_t.E8<-bin_mat.1_t[test$EssaySet==8,] ; Store(Bin_mat.1_t.E8)
Bin_mat.1_t.E9<-bin_mat.1_t[test$EssaySet==9,] ; Store(Bin_mat.1_t.E9)
Bin_mat.1_t.E10<-bin_mat.1_t[test$EssaySet==10,] ; Store(Bin_mat.1_t.E10)

#####################################################################
#####################################################################
# Ridit transformation
#####################################################################
#####################################################################

Ridit_mat.1_t.E1<-RIDIT_mat(Bin_mat.1.E1,Bin_mat.1_t.E1) ; Store(Ridit_mat.1_t.E1)
Ridit_mat.1_t.E2<-RIDIT_mat(Bin_mat.1.E2,Bin_mat.1_t.E2) ; Store(Ridit_mat.1_t.E2)
Ridit_mat.1_t.E3<-RIDIT_mat(Bin_mat.1.E3,Bin_mat.1_t.E3) ; Store(Ridit_mat.1_t.E3)
Ridit_mat.1_t.E4<-RIDIT_mat(Bin_mat.1.E4,Bin_mat.1_t.E4) ; Store(Ridit_mat.1_t.E4)
Ridit_mat.1_t.E5<-RIDIT_mat(Bin_mat.1.E5,Bin_mat.1_t.E5) ; Store(Ridit_mat.1_t.E5)
Ridit_mat.1_t.E6<-RIDIT_mat(Bin_mat.1.E6,Bin_mat.1_t.E6) ; Store(Ridit_mat.1_t.E6)
Ridit_mat.1_t.E7<-RIDIT_mat(Bin_mat.1.E7,Bin_mat.1_t.E7) ; Store(Ridit_mat.1_t.E7)
Ridit_mat.1_t.E8<-RIDIT_mat(Bin_mat.1.E8,Bin_mat.1_t.E8) ; Store(Ridit_mat.1_t.E8)
Ridit_mat.1_t.E9<-RIDIT_mat(Bin_mat.1.E9,Bin_mat.1_t.E9) ; Store(Ridit_mat.1_t.E9)
Ridit_mat.1_t.E10<-RIDIT_mat(Bin_mat.1.E10,Bin_mat.1_t.E10) ; Store(Ridit_mat.1_t.E10)

#####################################################################
#####################################################################
# Predict GNET to reduce dimension
#####################################################################
#####################################################################

library(glmnet)

GNET1R.1_t.E1<-predict(GNET1R.1.E1[[1]][[6]],Ridit_mat.1_t.E1,type="response", s="lambda.min")[,1]
GNET1R.1_t.E2<-predict(GNET1R.1.E2[[1]][[6]],Ridit_mat.1_t.E2,type="response", s="lambda.min")[,1]
GNET1R.1_t.E3<-predict(GNET1R.1.E3[[1]][[6]],Ridit_mat.1_t.E3,type="response", s="lambda.min")[,1]
GNET1R.1_t.E4<-predict(GNET1R.1.E4[[1]][[6]],Ridit_mat.1_t.E4,type="response", s="lambda.min")[,1]
GNET1R.1_t.E5<-predict(GNET1R.1.E5[[1]][[6]],Ridit_mat.1_t.E5,type="response", s="lambda.min")[,1]
GNET1R.1_t.E6<-predict(GNET1R.1.E6[[1]][[6]],Ridit_mat.1_t.E6,type="response", s="lambda.min")[,1]
GNET1R.1_t.E7<-predict(GNET1R.1.E7[[1]][[6]],Ridit_mat.1_t.E7,type="response", s="lambda.min")[,1]
GNET1R.1_t.E8<-predict(GNET1R.1.E8[[1]][[6]],Ridit_mat.1_t.E8,type="response", s="lambda.min")[,1]
GNET1R.1_t.E9<-predict(GNET1R.1.E9[[1]][[6]],Ridit_mat.1_t.E9,type="response", s="lambda.min")[,1]
GNET1R.1_t.E10<-predict(GNET1R.1.E10[[1]][[6]],Ridit_mat.1_t.E10,type="response", s="lambda.min")[,1]

GNET1R.1_t<-Group_t(list(GNET1R.1_t.E1,GNET1R.1_t.E2,GNET1R.1_t.E3,GNET1R.1_t.E4,GNET1R.1_t.E5,
                         GNET1R.1_t.E6,GNET1R.1_t.E7,GNET1R.1_t.E8,GNET1R.1_t.E9,GNET1R.1_t.E10)) ; Store(GNET1R.1_t)

#to check if ok
graph3(GNET1R.1_t,GNET1R.1)

#####################################################################
#function to stem text
#####################################################################

stem_n_grams_t<-function(set,word_list) {
  tmp<-stem_remove_text(EssayText_t2[test$EssaySet==set],word_list[[6]])
  out<-list()
  for (i in 1:length(tmp)) {
    out[[i]]<-names(c(textcnt(tmp[i], method="string", n=1L),textcnt(tmp[i], method="string", n=2L),
                      textcnt(tmp[i], method="string", n=3L),textcnt(tmp[i], method="string", n=4L),
                      textcnt(tmp[i], method="string", n=5L)))
  }    
  out
} 

STEM_t.E1<-stem_n_grams_t(1,useful_words.E1) ; Store(STEM_t.E1)
STEM_t.E2<-stem_n_grams_t(2,useful_words.E2) ; Store(STEM_t.E2)
STEM_t.E3<-stem_n_grams_t(3,useful_words.E3) ; Store(STEM_t.E3)
STEM_t.E4<-stem_n_grams_t(4,useful_words.E4) ; Store(STEM_t.E4)
STEM_t.E5<-stem_n_grams_t(5,useful_words.E5) ; Store(STEM_t.E5)
STEM_t.E6<-stem_n_grams_t(6,useful_words.E6) ; Store(STEM_t.E6)
STEM_t.E7<-stem_n_grams_t(7,useful_words.E7) ; Store(STEM_t.E7)
STEM_t.E8<-stem_n_grams_t(8,useful_words.E8) ; Store(STEM_t.E8)
STEM_t.E9<-stem_n_grams_t(9,useful_words.E9) ; Store(STEM_t.E9)
STEM_t.E10<-stem_n_grams_t(10,useful_words.E10) ; Store(STEM_t.E10)

#####################################################################
#####################################################################
# Produce document-term matrix bin_mat.2.Ex
#####################################################################
#####################################################################

#####################################################################
# remove unfrequent stemmed words (less than 5 times used)
#####################################################################

shorten_t<-function(x) {
  out<-list()
  tmp<-data.frame(table(unlist(x)))
  tmp<-as.character(tmp[tmp[,2]>5,1])
  for (i in 1:length(x)) 
    out[[i]]<-x[[i]][x[[i]] %in% tmp]
  out
}

STEMsh_t.E1<-shorten_t(STEM_t.E1) ; Store(STEMsh_t.E1)
STEMsh_t.E2<-shorten_t(STEM_t.E2) ; Store(STEMsh_t.E2)
STEMsh_t.E3<-shorten_t(STEM_t.E3) ; Store(STEMsh_t.E3)
STEMsh_t.E4<-shorten_t(STEM_t.E4) ; Store(STEMsh_t.E4)
STEMsh_t.E5<-shorten_t(STEM_t.E5) ; Store(STEMsh_t.E5)
STEMsh_t.E6<-shorten_t(STEM_t.E6) ; Store(STEMsh_t.E6)
STEMsh_t.E7<-shorten_t(STEM_t.E7) ; Store(STEMsh_t.E7)
STEMsh_t.E8<-shorten_t(STEM_t.E8) ; Store(STEMsh_t.E8)
STEMsh_t.E9<-shorten_t(STEM_t.E9) ; Store(STEMsh_t.E9)
STEMsh_t.E10<-shorten_t(STEM_t.E10) ; Store(STEMsh_t.E10)

#####################################################################
# create features matrix
#####################################################################

Create_Feat_mat_t<-function(x) {
  out<-data.frame(row_id=1,W=x[[1]])
  for (i in 2:length(x)) {
    if (length(x[[i]])!=0) out=  rbind(out,data.frame(row_id=i,W=x[[i]]))  
  }
  out
}

Feat_mat_t.E1<-Create_Feat_mat_t(STEMsh_t.E1) ; Store(Feat_mat_t.E1)
Feat_mat_t.E2<-Create_Feat_mat_t(STEMsh_t.E2) ; Store(Feat_mat_t.E2)
Feat_mat_t.E3<-Create_Feat_mat_t(STEMsh_t.E3) ; Store(Feat_mat_t.E3)
Feat_mat_t.E4<-Create_Feat_mat_t(STEMsh_t.E4) ; Store(Feat_mat_t.E4)
Feat_mat_t.E5<-Create_Feat_mat_t(STEMsh_t.E5) ; Store(Feat_mat_t.E5)
Feat_mat_t.E6<-Create_Feat_mat_t(STEMsh_t.E6) ; Store(Feat_mat_t.E6)
Feat_mat_t.E7<-Create_Feat_mat_t(STEMsh_t.E7) ; Store(Feat_mat_t.E7)
Feat_mat_t.E8<-Create_Feat_mat_t(STEMsh_t.E8) ; Store(Feat_mat_t.E8)
Feat_mat_t.E9<-Create_Feat_mat_t(STEMsh_t.E9) ; Store(Feat_mat_t.E9)
Feat_mat_t.E10<-Create_Feat_mat_t(STEMsh_t.E10) ; Store(Feat_mat_t.E10)

#####################################################################
# assign number to words (token) as in training set
#####################################################################

Ass_token_t<-function(x1,x2) {
  tmp<-x2[,c(2,3)] ; tmp$W<-as.character(tmp$W)
  tmp<-unique(tmp)
  tmp2<-x1 ; tmp2$W<-as.character(tmp2$W)
  merge(tmp2,tmp)
}

Feat_mat_t.E1<-Ass_token_t(Feat_mat_t.E1,Feat_mat.E1[[6]]) ; Store(Feat_mat_t.E1)
Feat_mat_t.E2<-Ass_token_t(Feat_mat_t.E2,Feat_mat.E2[[6]]) ; Store(Feat_mat_t.E2)
Feat_mat_t.E3<-Ass_token_t(Feat_mat_t.E3,Feat_mat.E3[[6]]) ; Store(Feat_mat_t.E3)
Feat_mat_t.E4<-Ass_token_t(Feat_mat_t.E4,Feat_mat.E4[[6]]) ; Store(Feat_mat_t.E4)
Feat_mat_t.E5<-Ass_token_t(Feat_mat_t.E5,Feat_mat.E5[[6]]) ; Store(Feat_mat_t.E5)
Feat_mat_t.E6<-Ass_token_t(Feat_mat_t.E6,Feat_mat.E6[[6]]) ; Store(Feat_mat_t.E6)
Feat_mat_t.E7<-Ass_token_t(Feat_mat_t.E7,Feat_mat.E7[[6]]) ; Store(Feat_mat_t.E7)
Feat_mat_t.E8<-Ass_token_t(Feat_mat_t.E8,Feat_mat.E8[[6]]) ; Store(Feat_mat_t.E8)
Feat_mat_t.E9<-Ass_token_t(Feat_mat_t.E9,Feat_mat.E9[[6]]) ; Store(Feat_mat_t.E9)
Feat_mat_t.E10<-Ass_token_t(Feat_mat_t.E10,Feat_mat.E10[[6]]) ; Store(Feat_mat_t.E10)

#####################################################################
# incidence matrix
#####################################################################

Inc_mat_t<-function(x,tr_ncol) {
  out<-as.matrix(sparseMatrix(x$row_id,x$token,x=rep(1,nrow(x))))  
  if (tr_ncol>ncol(out)) 
    for (j in (ncol(out)+1):tr_ncol) out<-cbind(out,0)
  out
}

bin_mat.2_t.E1<-Inc_mat_t(Feat_mat_t.E1,ncol(bin_mat.2.E1[[6]])) ; Store(bin_mat.2_t.E1)
bin_mat.2_t.E2<-Inc_mat_t(Feat_mat_t.E2,ncol(bin_mat.2.E2[[6]])) ; Store(bin_mat.2_t.E2)
bin_mat.2_t.E3<-Inc_mat_t(Feat_mat_t.E3,ncol(bin_mat.2.E3[[6]])) ; Store(bin_mat.2_t.E3)
bin_mat.2_t.E4<-Inc_mat_t(Feat_mat_t.E4,ncol(bin_mat.2.E4[[6]])) ; Store(bin_mat.2_t.E4)
bin_mat.2_t.E5<-Inc_mat_t(Feat_mat_t.E5,ncol(bin_mat.2.E5[[6]])) ; Store(bin_mat.2_t.E5)
bin_mat.2_t.E6<-Inc_mat_t(Feat_mat_t.E6,ncol(bin_mat.2.E6[[6]])) ; Store(bin_mat.2_t.E6)
bin_mat.2_t.E7<-Inc_mat_t(Feat_mat_t.E7,ncol(bin_mat.2.E7[[6]])) ; Store(bin_mat.2_t.E7)
bin_mat.2_t.E8<-Inc_mat_t(Feat_mat_t.E8,ncol(bin_mat.2.E8[[6]])) ; Store(bin_mat.2_t.E8)
bin_mat.2_t.E9<-Inc_mat_t(Feat_mat_t.E9,ncol(bin_mat.2.E9[[6]])) ; Store(bin_mat.2_t.E9)
bin_mat.2_t.E10<-Inc_mat_t(Feat_mat_t.E10,ncol(bin_mat.2.E10[[6]])) ; Store(bin_mat.2_t.E10)

#####################################################################
#####################################################################
# Ridit transformation
#####################################################################
#####################################################################

Ridit_mat.2_t.E1<-RIDIT_mat(bin_mat.2.E1[[6]],bin_mat.2_t.E1) ; Store(Ridit_mat.2_t.E1)
Ridit_mat.2_t.E2<-RIDIT_mat(bin_mat.2.E2[[6]],bin_mat.2_t.E2) ; Store(Ridit_mat.2_t.E2)
Ridit_mat.2_t.E3<-RIDIT_mat(bin_mat.2.E3[[6]],bin_mat.2_t.E3) ; Store(Ridit_mat.2_t.E3)
Ridit_mat.2_t.E4<-RIDIT_mat(bin_mat.2.E4[[6]],bin_mat.2_t.E4) ; Store(Ridit_mat.2_t.E4)
Ridit_mat.2_t.E5<-RIDIT_mat(bin_mat.2.E5[[6]],bin_mat.2_t.E5) ; Store(Ridit_mat.2_t.E5)
Ridit_mat.2_t.E6<-RIDIT_mat(bin_mat.2.E6[[6]],bin_mat.2_t.E6) ; Store(Ridit_mat.2_t.E6)
Ridit_mat.2_t.E7<-RIDIT_mat(bin_mat.2.E7[[6]],bin_mat.2_t.E7) ; Store(Ridit_mat.2_t.E7)
Ridit_mat.2_t.E8<-RIDIT_mat(bin_mat.2.E8[[6]],bin_mat.2_t.E8) ; Store(Ridit_mat.2_t.E8)
Ridit_mat.2_t.E9<-RIDIT_mat(bin_mat.2.E9[[6]],bin_mat.2_t.E9) ; Store(Ridit_mat.2_t.E9)
Ridit_mat.2_t.E10<-RIDIT_mat(bin_mat.2.E10[[6]],bin_mat.2_t.E10) ; Store(Ridit_mat.2_t.E10)

#####################################################################
#####################################################################
# SCALE MATRIX
#####################################################################
#####################################################################

#####################################################################
# scale incidence matrix with delta tfidf
#####################################################################

# 1rst set of bin_mat
# based on dummy "isgood"
#####################################################################

scal_mat0<-function(mat,metrics,type) {
  sweep(mat,MARGIN=2,metrics[[6]][[type]],'*')
}
Store(scal_mat0)

scal_mat1_t.E1<-scal_mat0(bin_mat_t.E1,metrics.E1,1) ; Store(scal_mat1_t.E1)
scal_mat1_t.E2<-scal_mat0(bin_mat_t.E2,metrics.E2,1) ; Store(scal_mat1_t.E2)
scal_mat1_t.E3<-scal_mat0(bin_mat_t.E3,metrics.E3,1) ; Store(scal_mat1_t.E3)
scal_mat1_t.E4<-scal_mat0(bin_mat_t.E4,metrics.E4,1) ; Store(scal_mat1_t.E4)
scal_mat1_t.E5<-scal_mat0(bin_mat_t.E5,metrics.E5,1) ; Store(scal_mat1_t.E5)
scal_mat1_t.E6<-scal_mat0(bin_mat_t.E6,metrics.E6,1) ; Store(scal_mat1_t.E6)
scal_mat1_t.E7<-scal_mat0(bin_mat_t.E7,metrics.E7,1) ; Store(scal_mat1_t.E7)
scal_mat1_t.E8<-scal_mat0(bin_mat_t.E8,metrics.E8,1) ; Store(scal_mat1_t.E8)
scal_mat1_t.E9<-scal_mat0(bin_mat_t.E9,metrics.E9,1) ; Store(scal_mat1_t.E9)
scal_mat1_t.E10<-scal_mat0(bin_mat_t.E10,metrics.E10,1) ; Store(scal_mat1_t.E10)

# 1rst set of bin_mat
# based on dummy "is.bad"
#####################################################################

scal.IB_mat1_t.E1<-scal_mat0(bin_mat_t.E1,metrics.IB.E1,1) ; Store(scal.IB_mat1_t.E1)
scal.IB_mat1_t.E2<-scal_mat0(bin_mat_t.E2,metrics.IB.E2,1) ; Store(scal.IB_mat1_t.E2)
scal.IB_mat1_t.E3<-scal_mat0(bin_mat_t.E3,metrics.IB.E3,1) ; Store(scal.IB_mat1_t.E3)
scal.IB_mat1_t.E4<-scal_mat0(bin_mat_t.E4,metrics.IB.E4,1) ; Store(scal.IB_mat1_t.E4)
scal.IB_mat1_t.E5<-scal_mat0(bin_mat_t.E5,metrics.IB.E5,1) ; Store(scal.IB_mat1_t.E5)
scal.IB_mat1_t.E6<-scal_mat0(bin_mat_t.E6,metrics.IB.E6,1) ; Store(scal.IB_mat1_t.E6)
scal.IB_mat1_t.E7<-scal_mat0(bin_mat_t.E7,metrics.IB.E7,1) ; Store(scal.IB_mat1_t.E7)
scal.IB_mat1_t.E8<-scal_mat0(bin_mat_t.E8,metrics.IB.E8,1) ; Store(scal.IB_mat1_t.E8)
scal.IB_mat1_t.E9<-scal_mat0(bin_mat_t.E9,metrics.IB.E9,1) ; Store(scal.IB_mat1_t.E9)
scal.IB_mat1_t.E10<-scal_mat0(bin_mat_t.E10,metrics.IB.E10,1) ; Store(scal.IB_mat1_t.E10)

# 2nd set of bin_mat
# based on dummy "isgood"
#####################################################################

scal_mat1.2_t.E1<-scal_mat0(bin_mat.2_t.E1,metrics.2.E1,1) ; Store(scal_mat1.2_t.E1)
scal_mat1.2_t.E2<-scal_mat0(bin_mat.2_t.E2,metrics.2.E2,1) ; Store(scal_mat1.2_t.E2)
scal_mat1.2_t.E3<-scal_mat0(bin_mat.2_t.E3,metrics.2.E3,1) ; Store(scal_mat1.2_t.E3)
scal_mat1.2_t.E4<-scal_mat0(bin_mat.2_t.E4,metrics.2.E4,1) ; Store(scal_mat1.2_t.E4)
scal_mat1.2_t.E5<-scal_mat0(bin_mat.2_t.E5,metrics.2.E5,1) ; Store(scal_mat1.2_t.E5)
scal_mat1.2_t.E6<-scal_mat0(bin_mat.2_t.E6,metrics.2.E6,1) ; Store(scal_mat1.2_t.E6)
scal_mat1.2_t.E7<-scal_mat0(bin_mat.2_t.E7,metrics.2.E7,1) ; Store(scal_mat1.2_t.E7)
scal_mat1.2_t.E8<-scal_mat0(bin_mat.2_t.E8,metrics.2.E8,1) ; Store(scal_mat1.2_t.E8)
scal_mat1.2_t.E9<-scal_mat0(bin_mat.2_t.E9,metrics.2.E9,1) ; Store(scal_mat1.2_t.E9)
scal_mat1.2_t.E10<-scal_mat0(bin_mat.2_t.E10,metrics.2.E10,1) ; Store(scal_mat1.2_t.E10)

# 2nd set of bin_mat
# based on dummy "isbad"
#####################################################################

scal.IB_mat1.2_t.E1<-scal_mat0(bin_mat.2_t.E1,metrics.IB.2.E1,1) ; Store(scal.IB_mat1.2_t.E1)
scal.IB_mat1.2_t.E2<-scal_mat0(bin_mat.2_t.E2,metrics.IB.2.E2,1) ; Store(scal.IB_mat1.2_t.E2)
scal.IB_mat1.2_t.E3<-scal_mat0(bin_mat.2_t.E3,metrics.IB.2.E3,1) ; Store(scal.IB_mat1.2_t.E3)
scal.IB_mat1.2_t.E4<-scal_mat0(bin_mat.2_t.E4,metrics.IB.2.E4,1) ; Store(scal.IB_mat1.2_t.E4)
scal.IB_mat1.2_t.E5<-scal_mat0(bin_mat.2_t.E5,metrics.IB.2.E5,1) ; Store(scal.IB_mat1.2_t.E5)
scal.IB_mat1.2_t.E6<-scal_mat0(bin_mat.2_t.E6,metrics.IB.2.E6,1) ; Store(scal.IB_mat1.2_t.E6)
scal.IB_mat1.2_t.E7<-scal_mat0(bin_mat.2_t.E7,metrics.IB.2.E7,1) ; Store(scal.IB_mat1.2_t.E7)
scal.IB_mat1.2_t.E8<-scal_mat0(bin_mat.2_t.E8,metrics.IB.2.E8,1) ; Store(scal.IB_mat1.2_t.E8)
scal.IB_mat1.2_t.E9<-scal_mat0(bin_mat.2_t.E9,metrics.IB.2.E9,1) ; Store(scal.IB_mat1.2_t.E9)
scal.IB_mat1.2_t.E10<-scal_mat0(bin_mat.2_t.E10,metrics.IB.2.E10,1) ; Store(scal.IB_mat1.2_t.E10)

#####################################################################
# scale incidence matrix with bns
#####################################################################

# 1rst set of bin_mat
# based on dummy "isgood"
#####################################################################

scal_mat3_t.E1<-scal_mat0(bin_mat_t.E1,metrics.E1,3) ; Store(scal_mat3_t.E1)
scal_mat3_t.E2<-scal_mat0(bin_mat_t.E2,metrics.E2,3) ; Store(scal_mat3_t.E2)
scal_mat3_t.E3<-scal_mat0(bin_mat_t.E3,metrics.E3,3) ; Store(scal_mat3_t.E3)
scal_mat3_t.E4<-scal_mat0(bin_mat_t.E4,metrics.E4,3) ; Store(scal_mat3_t.E4)
scal_mat3_t.E5<-scal_mat0(bin_mat_t.E5,metrics.E5,3) ; Store(scal_mat3_t.E5)
scal_mat3_t.E6<-scal_mat0(bin_mat_t.E6,metrics.E6,3) ; Store(scal_mat3_t.E6)
scal_mat3_t.E7<-scal_mat0(bin_mat_t.E7,metrics.E7,3) ; Store(scal_mat3_t.E7)
scal_mat3_t.E8<-scal_mat0(bin_mat_t.E8,metrics.E8,3) ; Store(scal_mat3_t.E8)
scal_mat3_t.E9<-scal_mat0(bin_mat_t.E9,metrics.E9,3) ; Store(scal_mat3_t.E9)
scal_mat3_t.E10<-scal_mat0(bin_mat_t.E10,metrics.E10,3) ; Store(scal_mat3_t.E10)

# 1rst set of bin_mat
# based on dummy "isbad"
#####################################################################

scal.IB_mat3_t.E1<-scal_mat0(bin_mat_t.E1,metrics.IB.E1,3) ; Store(scal.IB_mat3_t.E1)
scal.IB_mat3_t.E2<-scal_mat0(bin_mat_t.E2,metrics.IB.E2,3) ; Store(scal.IB_mat3_t.E2)
scal.IB_mat3_t.E3<-scal_mat0(bin_mat_t.E3,metrics.IB.E3,3) ; Store(scal.IB_mat3_t.E3)
scal.IB_mat3_t.E4<-scal_mat0(bin_mat_t.E4,metrics.IB.E4,3) ; Store(scal.IB_mat3_t.E4)
scal.IB_mat3_t.E5<-scal_mat0(bin_mat_t.E5,metrics.IB.E5,3) ; Store(scal.IB_mat3_t.E5)
scal.IB_mat3_t.E6<-scal_mat0(bin_mat_t.E6,metrics.IB.E6,3) ; Store(scal.IB_mat3_t.E6)
scal.IB_mat3_t.E7<-scal_mat0(bin_mat_t.E7,metrics.IB.E7,3) ; Store(scal.IB_mat3_t.E7)
scal.IB_mat3_t.E8<-scal_mat0(bin_mat_t.E8,metrics.IB.E8,3) ; Store(scal.IB_mat3_t.E8)
scal.IB_mat3_t.E9<-scal_mat0(bin_mat_t.E9,metrics.IB.E9,3) ; Store(scal.IB_mat3_t.E9)
scal.IB_mat3_t.E10<-scal_mat0(bin_mat_t.E10,metrics.IB.E10,3) ; Store(scal.IB_mat3_t.E10)

# 2nd set of bin_mat
# based on dummy "isgood"
#####################################################################

scal_mat3.2_t.E1<-scal_mat0(bin_mat.2_t.E1,metrics.2.E1,3) ; Store(scal_mat3.2_t.E1)
scal_mat3.2_t.E2<-scal_mat0(bin_mat.2_t.E2,metrics.2.E2,3) ; Store(scal_mat3.2_t.E2)
scal_mat3.2_t.E3<-scal_mat0(bin_mat.2_t.E3,metrics.2.E3,3) ; Store(scal_mat3.2_t.E3)
scal_mat3.2_t.E4<-scal_mat0(bin_mat.2_t.E4,metrics.2.E4,3) ; Store(scal_mat3.2_t.E4)
scal_mat3.2_t.E5<-scal_mat0(bin_mat.2_t.E5,metrics.2.E5,3) ; Store(scal_mat3.2_t.E5)
scal_mat3.2_t.E6<-scal_mat0(bin_mat.2_t.E6,metrics.2.E6,3) ; Store(scal_mat3.2_t.E6)
scal_mat3.2_t.E7<-scal_mat0(bin_mat.2_t.E7,metrics.2.E7,3) ; Store(scal_mat3.2_t.E7)
scal_mat3.2_t.E8<-scal_mat0(bin_mat.2_t.E8,metrics.2.E8,3) ; Store(scal_mat3.2_t.E8)
scal_mat3.2_t.E9<-scal_mat0(bin_mat.2_t.E9,metrics.2.E9,3) ; Store(scal_mat3.2_t.E9)
scal_mat3.2_t.E10<-scal_mat0(bin_mat.2_t.E10,metrics.2.E10,3) ; Store(scal_mat3.2_t.E10)

# 2nd set of bin_mat
# based on dummy "isbad"
#####################################################################

scal.IB_mat3.2_t.E1<-scal_mat0(bin_mat.2_t.E1,metrics.IB.2.E1,3) ; Store(scal.IB_mat3.2_t.E1)
scal.IB_mat3.2_t.E2<-scal_mat0(bin_mat.2_t.E2,metrics.IB.2.E2,3) ; Store(scal.IB_mat3.2_t.E2)
scal.IB_mat3.2_t.E3<-scal_mat0(bin_mat.2_t.E3,metrics.IB.2.E3,3) ; Store(scal.IB_mat3.2_t.E3)
scal.IB_mat3.2_t.E4<-scal_mat0(bin_mat.2_t.E4,metrics.IB.2.E4,3) ; Store(scal.IB_mat3.2_t.E4)
scal.IB_mat3.2_t.E5<-scal_mat0(bin_mat.2_t.E5,metrics.IB.2.E5,3) ; Store(scal.IB_mat3.2_t.E5)
scal.IB_mat3.2_t.E6<-scal_mat0(bin_mat.2_t.E6,metrics.IB.2.E6,3) ; Store(scal.IB_mat3.2_t.E6)
scal.IB_mat3.2_t.E7<-scal_mat0(bin_mat.2_t.E7,metrics.IB.2.E7,3) ; Store(scal.IB_mat3.2_t.E7)
scal.IB_mat3.2_t.E8<-scal_mat0(bin_mat.2_t.E8,metrics.IB.2.E8,3) ; Store(scal.IB_mat3.2_t.E8)
scal.IB_mat3.2_t.E9<-scal_mat0(bin_mat.2_t.E9,metrics.IB.2.E9,3) ; Store(scal.IB_mat3.2_t.E9)
scal.IB_mat3.2_t.E10<-scal_mat0(bin_mat.2_t.E10,metrics.IB.2.E10,3) ; Store(scal.IB_mat3.2_t.E10)


#####################################################################
#####################################################################
# predict GNET (standardize=FALSE) on 
# 2 sets of bin_mat after Ridit transformation
#####################################################################
#####################################################################

# 1rst set of bin_mat (transformed with ridit coding) 
#####################################################################

library(glmnet)

GNET1R_t.E1<-predict(GNET1R.E1[[1]][[6]],Ridit_mat_t.E1,type="response", s="lambda.min")[,1]
GNET1R_t.E2<-predict(GNET1R.E2[[1]][[6]],Ridit_mat_t.E2,type="response", s="lambda.min")[,1]
GNET1R_t.E3<-predict(GNET1R.E3[[1]][[6]],Ridit_mat_t.E3,type="response", s="lambda.min")[,1]
GNET1R_t.E4<-predict(GNET1R.E4[[1]][[6]],Ridit_mat_t.E4,type="response", s="lambda.min")[,1]
GNET1R_t.E5<-predict(GNET1R.E5[[1]][[6]],Ridit_mat_t.E5,type="response", s="lambda.min")[,1]
GNET1R_t.E6<-predict(GNET1R.E6[[1]][[6]],Ridit_mat_t.E6,type="response", s="lambda.min")[,1]
GNET1R_t.E7<-predict(GNET1R.E7[[1]][[6]],Ridit_mat_t.E7,type="response", s="lambda.min")[,1]
GNET1R_t.E8<-predict(GNET1R.E8[[1]][[6]],Ridit_mat_t.E8,type="response", s="lambda.min")[,1]
GNET1R_t.E9<-predict(GNET1R.E9[[1]][[6]],Ridit_mat_t.E9,type="response", s="lambda.min")[,1]
GNET1R_t.E10<-predict(GNET1R.E10[[1]][[6]],Ridit_mat_t.E10,type="response", s="lambda.min")[,1]

GNET1R_t<-Group_t(list(GNET1R_t.E1,GNET1R_t.E2,GNET1R_t.E3,GNET1R_t.E4,GNET1R_t.E5,
                       GNET1R_t.E6,GNET1R_t.E7,GNET1R_t.E8,GNET1R_t.E9,GNET1R_t.E10)) ; Store(GNET1R_t)

#to check if ok
graph3(GNET1R_t,GNET1R)

# 2nd set of bin_mat (transformed with ridit coding) 
#####################################################################

GNET1R.2_t.E1<-predict(GNET1R.2.E1[[1]][[6]],Ridit_mat.2_t.E1,type="response", s="lambda.min")[,1]
GNET1R.2_t.E2<-predict(GNET1R.2.E2[[1]][[6]],Ridit_mat.2_t.E2,type="response", s="lambda.min")[,1]
GNET1R.2_t.E3<-predict(GNET1R.2.E3[[1]][[6]],Ridit_mat.2_t.E3,type="response", s="lambda.min")[,1]
GNET1R.2_t.E4<-predict(GNET1R.2.E4[[1]][[6]],Ridit_mat.2_t.E4,type="response", s="lambda.min")[,1]
GNET1R.2_t.E5<-predict(GNET1R.2.E5[[1]][[6]],Ridit_mat.2_t.E5,type="response", s="lambda.min")[,1]
GNET1R.2_t.E6<-predict(GNET1R.2.E6[[1]][[6]],Ridit_mat.2_t.E6,type="response", s="lambda.min")[,1]
GNET1R.2_t.E7<-predict(GNET1R.2.E7[[1]][[6]],Ridit_mat.2_t.E7,type="response", s="lambda.min")[,1]
GNET1R.2_t.E8<-predict(GNET1R.2.E8[[1]][[6]],Ridit_mat.2_t.E8,type="response", s="lambda.min")[,1]
GNET1R.2_t.E9<-predict(GNET1R.2.E9[[1]][[6]],Ridit_mat.2_t.E9,type="response", s="lambda.min")[,1]
GNET1R.2_t.E10<-predict(GNET1R.2.E10[[1]][[6]],Ridit_mat.2_t.E10,type="response", s="lambda.min")[,1]

GNET1R.2_t<-Group_t(list(GNET1R.2_t.E1,GNET1R.2_t.E2,GNET1R.2_t.E3,GNET1R.2_t.E4,GNET1R.2_t.E5,
                         GNET1R.2_t.E6,GNET1R.2_t.E7,GNET1R.2_t.E8,GNET1R.2_t.E9,GNET1R.2_t.E10)) ; Store(GNET1R.2_t)

#to check if ok
graph3(GNET1R.2_t,GNET1R.2)

#####################################################################
#####################################################################
# PREDICT SVM
# on 2 sets of bin mat
# scaled with bns using "isgood" or "isbadd"
# with linear and radial kernels
# regression, multinomial and binary classification
#####################################################################
#####################################################################

#####################################################################
# predict SVM on incidence matrix scaled with bns
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

SVM3_t.E1<-predict(SVM3.E1[[1]][[6]],scal_mat3_t.E1) ; Store(SVM3_t.E1)
SVM3_t.E2<-predict(SVM3.E2[[1]][[6]],scal_mat3_t.E2) ; Store(SVM3_t.E2)
SVM3_t.E3<-predict(SVM3.E3[[1]][[6]],scal_mat3_t.E3) ; Store(SVM3_t.E3)
SVM3_t.E4<-predict(SVM3.E4[[1]][[6]],scal_mat3_t.E4) ; Store(SVM3_t.E4)
SVM3_t.E5<-predict(SVM3.E5[[1]][[6]],scal_mat3_t.E5) ; Store(SVM3_t.E5)
SVM3_t.E6<-predict(SVM3.E6[[1]][[6]],scal_mat3_t.E6) ; Store(SVM3_t.E6)
SVM3_t.E7<-predict(SVM3.E7[[1]][[6]],scal_mat3_t.E7) ; Store(SVM3_t.E7)
SVM3_t.E8<-predict(SVM3.E8[[1]][[6]],scal_mat3_t.E8) ; Store(SVM3_t.E8)
SVM3_t.E9<-predict(SVM3.E9[[1]][[6]],scal_mat3_t.E9) ; Store(SVM3_t.E9)
SVM3_t.E10<-predict(SVM3.E10[[1]][[6]],scal_mat3_t.E10) ; Store(SVM3_t.E10)

SVM3_t<-Group_t(list(SVM3_t.E1,SVM3_t.E2,SVM3_t.E3,SVM3_t.E4,SVM3_t.E5,
                     SVM3_t.E6,SVM3_t.E7,SVM3_t.E8,SVM3_t.E9,SVM3_t.E10)) ; Store(SVM3_t)

#to check if ok
graph3(SVM3_t,SVM3)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

SVM3.IB_t.E1<-predict(SVM3.IB.E1[[1]][[6]],scal.IB_mat3_t.E1) ; Store(SVM3.IB_t.E1)
SVM3.IB_t.E2<-predict(SVM3.IB.E2[[1]][[6]],scal.IB_mat3_t.E2) ; Store(SVM3.IB_t.E2)
SVM3.IB_t.E3<-predict(SVM3.IB.E3[[1]][[6]],scal.IB_mat3_t.E3) ; Store(SVM3.IB_t.E3)
SVM3.IB_t.E4<-predict(SVM3.IB.E4[[1]][[6]],scal.IB_mat3_t.E4) ; Store(SVM3.IB_t.E4)
SVM3.IB_t.E5<-predict(SVM3.IB.E5[[1]][[6]],scal.IB_mat3_t.E5) ; Store(SVM3.IB_t.E5)
SVM3.IB_t.E6<-predict(SVM3.IB.E6[[1]][[6]],scal.IB_mat3_t.E6) ; Store(SVM3.IB_t.E6)
SVM3.IB_t.E7<-predict(SVM3.IB.E7[[1]][[6]],scal.IB_mat3_t.E7) ; Store(SVM3.IB_t.E7)
SVM3.IB_t.E8<-predict(SVM3.IB.E8[[1]][[6]],scal.IB_mat3_t.E8) ; Store(SVM3.IB_t.E8)
SVM3.IB_t.E9<-predict(SVM3.IB.E9[[1]][[6]],scal.IB_mat3_t.E9) ; Store(SVM3.IB_t.E9)
SVM3.IB_t.E10<-predict(SVM3.IB.E10[[1]][[6]],scal.IB_mat3_t.E10) ; Store(SVM3.IB_t.E10)

SVM3.IB_t<-Group_t(list(SVM3.IB_t.E1,SVM3.IB_t.E2,SVM3.IB_t.E3,SVM3.IB_t.E4,SVM3.IB_t.E5,
                        SVM3.IB_t.E6,SVM3.IB_t.E7,SVM3.IB_t.E8,SVM3.IB_t.E9,SVM3.IB_t.E10)) ; Store(SVM3.IB_t)

#to check if ok
graph3(SVM3.IB_t,SVM3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

SVM3.2_t.E1<-predict(SVM3.2.E1[[1]][[6]],scal_mat3.2_t.E1) ; Store(SVM3.2_t.E1)
SVM3.2_t.E2<-predict(SVM3.2.E2[[1]][[6]],scal_mat3.2_t.E2) ; Store(SVM3.2_t.E2)
SVM3.2_t.E3<-predict(SVM3.2.E3[[1]][[6]],scal_mat3.2_t.E3) ; Store(SVM3.2_t.E3)
SVM3.2_t.E4<-predict(SVM3.2.E4[[1]][[6]],scal_mat3.2_t.E4) ; Store(SVM3.2_t.E4)
SVM3.2_t.E5<-predict(SVM3.2.E5[[1]][[6]],scal_mat3.2_t.E5) ; Store(SVM3.2_t.E5)
SVM3.2_t.E6<-predict(SVM3.2.E6[[1]][[6]],scal_mat3.2_t.E6) ; Store(SVM3.2_t.E6)
SVM3.2_t.E7<-predict(SVM3.2.E7[[1]][[6]],scal_mat3.2_t.E7) ; Store(SVM3.2_t.E7)
SVM3.2_t.E8<-predict(SVM3.2.E8[[1]][[6]],scal_mat3.2_t.E8) ; Store(SVM3.2_t.E8)
SVM3.2_t.E9<-predict(SVM3.2.E9[[1]][[6]],scal_mat3.2_t.E9) ; Store(SVM3.2_t.E9)
SVM3.2_t.E10<-predict(SVM3.2.E10[[1]][[6]],scal_mat3.2_t.E10) ; Store(SVM3.2_t.E10)

SVM3.2_t<-Group_t(list(SVM3.2_t.E1,SVM3.2_t.E2,SVM3.2_t.E3,SVM3.2_t.E4,SVM3.2_t.E5,
                       SVM3.2_t.E6,SVM3.2_t.E7,SVM3.2_t.E8,SVM3.2_t.E9,SVM3.2_t.E10)) ; Store(SVM3.2_t)

#to check if ok
graph3(SVM3.2_t,SVM3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

SVM3.IB.2_t.E1<-predict(SVM3.IB.2.E1[[1]][[6]],scal.IB_mat3.2_t.E1) ; Store(SVM3.IB.2_t.E1)
SVM3.IB.2_t.E2<-predict(SVM3.IB.2.E2[[1]][[6]],scal.IB_mat3.2_t.E2) ; Store(SVM3.IB.2_t.E2)
SVM3.IB.2_t.E3<-predict(SVM3.IB.2.E3[[1]][[6]],scal.IB_mat3.2_t.E3) ; Store(SVM3.IB.2_t.E3)
SVM3.IB.2_t.E4<-predict(SVM3.IB.2.E4[[1]][[6]],scal.IB_mat3.2_t.E4) ; Store(SVM3.IB.2_t.E4)
SVM3.IB.2_t.E5<-predict(SVM3.IB.2.E5[[1]][[6]],scal.IB_mat3.2_t.E5) ; Store(SVM3.IB.2_t.E5)
SVM3.IB.2_t.E6<-predict(SVM3.IB.2.E6[[1]][[6]],scal.IB_mat3.2_t.E6) ; Store(SVM3.IB.2_t.E6)
SVM3.IB.2_t.E7<-predict(SVM3.IB.2.E7[[1]][[6]],scal.IB_mat3.2_t.E7) ; Store(SVM3.IB.2_t.E7)
SVM3.IB.2_t.E8<-predict(SVM3.IB.2.E8[[1]][[6]],scal.IB_mat3.2_t.E8) ; Store(SVM3.IB.2_t.E8)
SVM3.IB.2_t.E9<-predict(SVM3.IB.2.E9[[1]][[6]],scal.IB_mat3.2_t.E9) ; Store(SVM3.IB.2_t.E9)
SVM3.IB.2_t.E10<-predict(SVM3.IB.2.E10[[1]][[6]],scal.IB_mat3.2_t.E10) ; Store(SVM3.IB.2_t.E10)

SVM3.IB.2_t<-Group_t(list(SVM3.IB.2_t.E1,SVM3.IB.2_t.E2,SVM3.IB.2_t.E3,SVM3.IB.2_t.E4,SVM3.IB.2_t.E5,
                          SVM3.IB.2_t.E6,SVM3.IB.2_t.E7,SVM3.IB.2_t.E8,SVM3.IB.2_t.E9,SVM3.IB.2_t.E10)) ; Store(SVM3.IB.2_t)

#to check if ok
graph3(SVM3.IB.2_t,SVM3.IB.2)

#####################################################################
# PREDICT SVM with radial kernel on scaled matrix
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

RAD2_t.E1<-predict(RAD2.E1[[1]][[6]],scal_mat3_t.E1) ; Store(RAD2_t.E1)
RAD2_t.E2<-predict(RAD2.E2[[1]][[6]],scal_mat3_t.E2) ; Store(RAD2_t.E2)
RAD2_t.E3<-predict(RAD2.E3[[1]][[6]],scal_mat3_t.E3) ; Store(RAD2_t.E3)
RAD2_t.E4<-predict(RAD2.E4[[1]][[6]],scal_mat3_t.E4) ; Store(RAD2_t.E4)
RAD2_t.E5<-predict(RAD2.E5[[1]][[6]],scal_mat3_t.E5) ; Store(RAD2_t.E5)
RAD2_t.E6<-predict(RAD2.E6[[1]][[6]],scal_mat3_t.E6) ; Store(RAD2_t.E6)
RAD2_t.E7<-predict(RAD2.E7[[1]][[6]],scal_mat3_t.E7) ; Store(RAD2_t.E7)
RAD2_t.E8<-predict(RAD2.E8[[1]][[6]],scal_mat3_t.E8) ; Store(RAD2_t.E8)
RAD2_t.E9<-predict(RAD2.E9[[1]][[6]],scal_mat3_t.E9) ; Store(RAD2_t.E9)
RAD2_t.E10<-predict(RAD2.E10[[1]][[6]],scal_mat3_t.E10) ; Store(RAD2_t.E10)

RAD2_t<-Group_t(list(RAD2_t.E1,RAD2_t.E2,RAD2_t.E3,RAD2_t.E4,RAD2_t.E5,
                     RAD2_t.E6,RAD2_t.E7,RAD2_t.E8,RAD2_t.E9,RAD2_t.E10)) ; Store(RAD2_t)

#to check if ok
graph3(RAD2_t,RAD2)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

RAD2.IB_t.E1<-predict(RAD2.IB.E1[[1]][[6]],scal.IB_mat3_t.E1) ; Store(RAD2.IB_t.E1)
RAD2.IB_t.E2<-predict(RAD2.IB.E2[[1]][[6]],scal.IB_mat3_t.E2) ; Store(RAD2.IB_t.E2)
RAD2.IB_t.E3<-predict(RAD2.IB.E3[[1]][[6]],scal.IB_mat3_t.E3) ; Store(RAD2.IB_t.E3)
RAD2.IB_t.E4<-predict(RAD2.IB.E4[[1]][[6]],scal.IB_mat3_t.E4) ; Store(RAD2.IB_t.E4)
RAD2.IB_t.E5<-predict(RAD2.IB.E5[[1]][[6]],scal.IB_mat3_t.E5) ; Store(RAD2.IB_t.E5)
RAD2.IB_t.E6<-predict(RAD2.IB.E6[[1]][[6]],scal.IB_mat3_t.E6) ; Store(RAD2.IB_t.E6)
RAD2.IB_t.E7<-predict(RAD2.IB.E7[[1]][[6]],scal.IB_mat3_t.E7) ; Store(RAD2.IB_t.E7)
RAD2.IB_t.E8<-predict(RAD2.IB.E8[[1]][[6]],scal.IB_mat3_t.E8) ; Store(RAD2.IB_t.E8)
RAD2.IB_t.E9<-predict(RAD2.IB.E9[[1]][[6]],scal.IB_mat3_t.E9) ; Store(RAD2.IB_t.E9)
RAD2.IB_t.E10<-predict(RAD2.IB.E10[[1]][[6]],scal.IB_mat3_t.E10) ; Store(RAD2.IB_t.E10)

RAD2.IB_t<-Group_t(list(RAD2.IB_t.E1,RAD2.IB_t.E2,RAD2.IB_t.E3,RAD2.IB_t.E4,RAD2.IB_t.E5,
                        RAD2.IB_t.E6,RAD2.IB_t.E7,RAD2.IB_t.E8,RAD2.IB_t.E9,RAD2.IB_t.E10)) ; Store(RAD2.IB_t)

#to check if ok
graph3(RAD2.IB_t,RAD2.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

RAD2.2_t.E1<-predict(RAD2.2.E1[[1]][[6]],scal_mat3.2_t.E1) ; Store(RAD2.2_t.E1)
RAD2.2_t.E2<-predict(RAD2.2.E2[[1]][[6]],scal_mat3.2_t.E2) ; Store(RAD2.2_t.E2)
RAD2.2_t.E3<-predict(RAD2.2.E3[[1]][[6]],scal_mat3.2_t.E3) ; Store(RAD2.2_t.E3)
RAD2.2_t.E4<-predict(RAD2.2.E4[[1]][[6]],scal_mat3.2_t.E4) ; Store(RAD2.2_t.E4)
RAD2.2_t.E5<-predict(RAD2.2.E5[[1]][[6]],scal_mat3.2_t.E5) ; Store(RAD2.2_t.E5)
RAD2.2_t.E6<-predict(RAD2.2.E6[[1]][[6]],scal_mat3.2_t.E6) ; Store(RAD2.2_t.E6)
RAD2.2_t.E7<-predict(RAD2.2.E7[[1]][[6]],scal_mat3.2_t.E7) ; Store(RAD2.2_t.E7)
RAD2.2_t.E8<-predict(RAD2.2.E8[[1]][[6]],scal_mat3.2_t.E8) ; Store(RAD2.2_t.E8)
RAD2.2_t.E9<-predict(RAD2.2.E9[[1]][[6]],scal_mat3.2_t.E9) ; Store(RAD2.2_t.E9)
RAD2.2_t.E10<-predict(RAD2.2.E10[[1]][[6]],scal_mat3.2_t.E10) ; Store(RAD2.2_t.E10)

RAD2.2_t<-Group_t(list(RAD2.2_t.E1,RAD2.2_t.E2,RAD2.2_t.E3,RAD2.2_t.E4,RAD2.2_t.E5,
                       RAD2.2_t.E6,RAD2.2_t.E7,RAD2.2_t.E8,RAD2.2_t.E9,RAD2.2_t.E10)) ; Store(RAD2.2_t)

#to check if ok
graph3(RAD2.2_t,RAD2.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

RAD2.IB.2_t.E1<-predict(RAD2.IB.2.E1[[1]][[6]],scal.IB_mat3.2_t.E1) ; Store(RAD2.IB.2_t.E1)
RAD2.IB.2_t.E2<-predict(RAD2.IB.2.E2[[1]][[6]],scal.IB_mat3.2_t.E2) ; Store(RAD2.IB.2_t.E2)
RAD2.IB.2_t.E3<-predict(RAD2.IB.2.E3[[1]][[6]],scal.IB_mat3.2_t.E3) ; Store(RAD2.IB.2_t.E3)
RAD2.IB.2_t.E4<-predict(RAD2.IB.2.E4[[1]][[6]],scal.IB_mat3.2_t.E4) ; Store(RAD2.IB.2_t.E4)
RAD2.IB.2_t.E5<-predict(RAD2.IB.2.E5[[1]][[6]],scal.IB_mat3.2_t.E5) ; Store(RAD2.IB.2_t.E5)
RAD2.IB.2_t.E6<-predict(RAD2.IB.2.E6[[1]][[6]],scal.IB_mat3.2_t.E6) ; Store(RAD2.IB.2_t.E6)
RAD2.IB.2_t.E7<-predict(RAD2.IB.2.E7[[1]][[6]],scal.IB_mat3.2_t.E7) ; Store(RAD2.IB.2_t.E7)
RAD2.IB.2_t.E8<-predict(RAD2.IB.2.E8[[1]][[6]],scal.IB_mat3.2_t.E8) ; Store(RAD2.IB.2_t.E8)
RAD2.IB.2_t.E9<-predict(RAD2.IB.2.E9[[1]][[6]],scal.IB_mat3.2_t.E9) ; Store(RAD2.IB.2_t.E9)
RAD2.IB.2_t.E10<-predict(RAD2.IB.2.E10[[1]][[6]],scal.IB_mat3.2_t.E10) ; Store(RAD2.IB.2_t.E10)

RAD2.IB.2_t<-Group_t(list(RAD2.IB.2_t.E1,RAD2.IB.2_t.E2,RAD2.IB.2_t.E3,RAD2.IB.2_t.E4,RAD2.IB.2_t.E5,
                          RAD2.IB.2_t.E6,RAD2.IB.2_t.E7,RAD2.IB.2_t.E8,RAD2.IB.2_t.E9,RAD2.IB.2_t.E10)) ; Store(RAD2.IB.2_t)

#to check if ok
graph3(RAD2.IB.2_t,RAD2.IB.2)

#####################################################################
# PREDICT SVM to model binary response good/poor essays
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

SVMc3b_t.E1<-attr(predict(SVMc3b.E1[[1]][[6]],scal_mat3_t.E1,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b_t.E1)
SVMc3b_t.E2<-attr(predict(SVMc3b.E2[[1]][[6]],scal_mat3_t.E2,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b_t.E2)
SVMc3b_t.E3<-attr(predict(SVMc3b.E3[[1]][[6]],scal_mat3_t.E3,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b_t.E3)
SVMc3b_t.E4<-attr(predict(SVMc3b.E4[[1]][[6]],scal_mat3_t.E4,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b_t.E4)
SVMc3b_t.E5<-attr(predict(SVMc3b.E5[[1]][[6]],scal_mat3_t.E5,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b_t.E5)
SVMc3b_t.E6<-attr(predict(SVMc3b.E6[[1]][[6]],scal_mat3_t.E6,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b_t.E6)
SVMc3b_t.E7<-attr(predict(SVMc3b.E7[[1]][[6]],scal_mat3_t.E7,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b_t.E7)
SVMc3b_t.E8<-attr(predict(SVMc3b.E8[[1]][[6]],scal_mat3_t.E8,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b_t.E8)
SVMc3b_t.E9<-attr(predict(SVMc3b.E9[[1]][[6]],scal_mat3_t.E9,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b_t.E9)
SVMc3b_t.E10<-attr(predict(SVMc3b.E10[[1]][[6]],scal_mat3_t.E10,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b_t.E10)

SVMc3b_t<-Group_t(list(SVMc3b_t.E1,SVMc3b_t.E2,SVMc3b_t.E3,SVMc3b_t.E4,SVMc3b_t.E5,
                       SVMc3b_t.E6,SVMc3b_t.E7,SVMc3b_t.E8,SVMc3b_t.E9,SVMc3b_t.E10)) ; Store(SVMc3b_t)

#to check if ok
graph3(SVMc3b_t,SVMc3b)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

SVMc3.IB_t.E1<-attr(predict(SVMc3.IB.E1[[1]][[6]],scal.IB_mat3_t.E1,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.IB_t.E1)
SVMc3.IB_t.E2<-attr(predict(SVMc3.IB.E2[[1]][[6]],scal.IB_mat3_t.E2,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.IB_t.E2)
SVMc3.IB_t.E3<-attr(predict(SVMc3.IB.E3[[1]][[6]],scal.IB_mat3_t.E3,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.IB_t.E3)
SVMc3.IB_t.E4<-attr(predict(SVMc3.IB.E4[[1]][[6]],scal.IB_mat3_t.E4,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.IB_t.E4)
SVMc3.IB_t.E5<-attr(predict(SVMc3.IB.E5[[1]][[6]],scal.IB_mat3_t.E5,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.IB_t.E5)
SVMc3.IB_t.E6<-attr(predict(SVMc3.IB.E6[[1]][[6]],scal.IB_mat3_t.E6,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.IB_t.E6)
SVMc3.IB_t.E7<-attr(predict(SVMc3.IB.E7[[1]][[6]],scal.IB_mat3_t.E7,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.IB_t.E7)
SVMc3.IB_t.E8<-attr(predict(SVMc3.IB.E8[[1]][[6]],scal.IB_mat3_t.E8,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.IB_t.E8)
SVMc3.IB_t.E9<-attr(predict(SVMc3.IB.E9[[1]][[6]],scal.IB_mat3_t.E9,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.IB_t.E9)
SVMc3.IB_t.E10<-attr(predict(SVMc3.IB.E10[[1]][[6]],scal.IB_mat3_t.E10,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.IB_t.E10)

SVMc3.IB_t<-Group_t(list(SVMc3.IB_t.E1,SVMc3.IB_t.E2,SVMc3.IB_t.E3,SVMc3.IB_t.E4,SVMc3.IB_t.E5,
                         SVMc3.IB_t.E6,SVMc3.IB_t.E7,SVMc3.IB_t.E8,SVMc3.IB_t.E9,SVMc3.IB_t.E10)) ; Store(SVMc3.IB_t)

#to check if ok
graph3(SVMc3.IB_t,SVMc3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

SVMc3.2_t.E1<-attr(predict(SVMc3.2.E1[[1]][[6]],scal_mat3.2_t.E1,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.2_t.E1)
SVMc3.2_t.E2<-attr(predict(SVMc3.2.E2[[1]][[6]],scal_mat3.2_t.E2,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.2_t.E2)
SVMc3.2_t.E3<-attr(predict(SVMc3.2.E3[[1]][[6]],scal_mat3.2_t.E3,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.2_t.E3)
SVMc3.2_t.E4<-attr(predict(SVMc3.2.E4[[1]][[6]],scal_mat3.2_t.E4,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.2_t.E4)
SVMc3.2_t.E5<-attr(predict(SVMc3.2.E5[[1]][[6]],scal_mat3.2_t.E5,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.2_t.E5)
SVMc3.2_t.E6<-attr(predict(SVMc3.2.E6[[1]][[6]],scal_mat3.2_t.E6,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.2_t.E6)
SVMc3.2_t.E7<-attr(predict(SVMc3.2.E7[[1]][[6]],scal_mat3.2_t.E7,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.2_t.E7)
SVMc3.2_t.E8<-attr(predict(SVMc3.2.E8[[1]][[6]],scal_mat3.2_t.E8,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.2_t.E8)
SVMc3.2_t.E9<-attr(predict(SVMc3.2.E9[[1]][[6]],scal_mat3.2_t.E9,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.2_t.E9)
SVMc3.2_t.E10<-attr(predict(SVMc3.2.E10[[1]][[6]],scal_mat3.2_t.E10,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3.2_t.E10)

SVMc3.2_t<-Group_t(list(SVMc3.2_t.E1,SVMc3.2_t.E2,SVMc3.2_t.E3,SVMc3.2_t.E4,SVMc3.2_t.E5,
                        SVMc3.2_t.E6,SVMc3.2_t.E7,SVMc3.2_t.E8,SVMc3.2_t.E9,SVMc3.2_t.E10)) ; Store(SVMc3.2_t)

#to check if ok
graph3(SVMc3.2_t,SVMc3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

SVMc3b.IB.2_t.E1<-attr(predict(SVMc3b.IB.2.E1[[1]][[6]],scal.IB_mat3.2_t.E1,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b.IB.2_t.E1)
SVMc3b.IB.2_t.E2<-attr(predict(SVMc3b.IB.2.E2[[1]][[6]],scal.IB_mat3.2_t.E2,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b.IB.2_t.E2)
SVMc3b.IB.2_t.E3<-attr(predict(SVMc3b.IB.2.E3[[1]][[6]],scal.IB_mat3.2_t.E3,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b.IB.2_t.E3)
SVMc3b.IB.2_t.E4<-attr(predict(SVMc3b.IB.2.E4[[1]][[6]],scal.IB_mat3.2_t.E4,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b.IB.2_t.E4)
SVMc3b.IB.2_t.E5<-attr(predict(SVMc3b.IB.2.E5[[1]][[6]],scal.IB_mat3.2_t.E5,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b.IB.2_t.E5)
SVMc3b.IB.2_t.E6<-attr(predict(SVMc3b.IB.2.E6[[1]][[6]],scal.IB_mat3.2_t.E6,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b.IB.2_t.E6)
SVMc3b.IB.2_t.E7<-attr(predict(SVMc3b.IB.2.E7[[1]][[6]],scal.IB_mat3.2_t.E7,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b.IB.2_t.E7)
SVMc3b.IB.2_t.E8<-attr(predict(SVMc3b.IB.2.E8[[1]][[6]],scal.IB_mat3.2_t.E8,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b.IB.2_t.E8)
SVMc3b.IB.2_t.E9<-attr(predict(SVMc3b.IB.2.E9[[1]][[6]],scal.IB_mat3.2_t.E9,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b.IB.2_t.E9)
SVMc3b.IB.2_t.E10<-attr(predict(SVMc3b.IB.2.E10[[1]][[6]],scal.IB_mat3.2_t.E10,probability =TRUE),"probabilities")[,"1"] ; Store(SVMc3b.IB.2_t.E10)

SVMc3b.IB.2_t<-Group_t(list(SVMc3b.IB.2_t.E1,SVMc3b.IB.2_t.E2,SVMc3b.IB.2_t.E3,SVMc3b.IB.2_t.E4,SVMc3b.IB.2_t.E5,
                            SVMc3b.IB.2_t.E6,SVMc3b.IB.2_t.E7,SVMc3b.IB.2_t.E8,SVMc3b.IB.2_t.E9,SVMc3b.IB.2_t.E10)) ; Store(SVMc3b.IB.2_t)

#to check if ok
graph3(SVMc3b.IB.2_t,SVMc3b.IB.2)

#####################################################################
# PREDICT SVM to model binary response good/poor essays
# with radial kernel on scaled matrix
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

RADc2_t.E1<-attr(predict(RADc2.E1[[1]][[6]],scal_mat3_t.E1,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2_t.E1)
RADc2_t.E2<-attr(predict(RADc2.E2[[1]][[6]],scal_mat3_t.E2,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2_t.E2)
RADc2_t.E3<-attr(predict(RADc2.E3[[1]][[6]],scal_mat3_t.E3,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2_t.E3)
RADc2_t.E4<-attr(predict(RADc2.E4[[1]][[6]],scal_mat3_t.E4,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2_t.E4)
RADc2_t.E5<-attr(predict(RADc2.E5[[1]][[6]],scal_mat3_t.E5,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2_t.E5)
RADc2_t.E6<-attr(predict(RADc2.E6[[1]][[6]],scal_mat3_t.E6,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2_t.E6)
RADc2_t.E7<-attr(predict(RADc2.E7[[1]][[6]],scal_mat3_t.E7,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2_t.E7)
RADc2_t.E8<-attr(predict(RADc2.E8[[1]][[6]],scal_mat3_t.E8,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2_t.E8)
RADc2_t.E9<-attr(predict(RADc2.E9[[1]][[6]],scal_mat3_t.E9,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2_t.E9)
RADc2_t.E10<-attr(predict(RADc2.E10[[1]][[6]],scal_mat3_t.E10,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2_t.E10)

RADc2_t<-Group_t(list(RADc2_t.E1,RADc2_t.E2,RADc2_t.E3,RADc2_t.E4,RADc2_t.E5,
                      RADc2_t.E6,RADc2_t.E7,RADc2_t.E8,RADc2_t.E9,RADc2_t.E10)) ; Store(RADc2_t)

#to check if ok
graph3(RADc2_t,RADc2)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

RADc2.IB_t.E1<-attr(predict(RADc2.IB.E1[[1]][[6]],scal.IB_mat3_t.E1,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB_t.E1)
RADc2.IB_t.E2<-attr(predict(RADc2.IB.E2[[1]][[6]],scal.IB_mat3_t.E2,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB_t.E2)
RADc2.IB_t.E3<-attr(predict(RADc2.IB.E3[[1]][[6]],scal.IB_mat3_t.E3,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB_t.E3)
RADc2.IB_t.E4<-attr(predict(RADc2.IB.E4[[1]][[6]],scal.IB_mat3_t.E4,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB_t.E4)
RADc2.IB_t.E5<-attr(predict(RADc2.IB.E5[[1]][[6]],scal.IB_mat3_t.E5,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB_t.E5)
RADc2.IB_t.E6<-attr(predict(RADc2.IB.E6[[1]][[6]],scal.IB_mat3_t.E6,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB_t.E6)
RADc2.IB_t.E7<-attr(predict(RADc2.IB.E7[[1]][[6]],scal.IB_mat3_t.E7,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB_t.E7)
RADc2.IB_t.E8<-attr(predict(RADc2.IB.E8[[1]][[6]],scal.IB_mat3_t.E8,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB_t.E8)
RADc2.IB_t.E9<-attr(predict(RADc2.IB.E9[[1]][[6]],scal.IB_mat3_t.E9,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB_t.E9)
RADc2.IB_t.E10<-attr(predict(RADc2.IB.E10[[1]][[6]],scal.IB_mat3_t.E10,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB_t.E10)

RADc2.IB_t<-Group_t(list(RADc2.IB_t.E1,RADc2.IB_t.E2,RADc2.IB_t.E3,RADc2.IB_t.E4,RADc2.IB_t.E5,
                         RADc2.IB_t.E6,RADc2.IB_t.E7,RADc2.IB_t.E8,RADc2.IB_t.E9,RADc2.IB_t.E10)) ; Store(RADc2.IB_t)

#to check if ok
graph3(RADc2.IB_t,RADc2.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

RADc2.2_t.E1<-attr(predict(RADc2.2.E1[[1]][[6]],scal_mat3.2_t.E1,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.2_t.E1)
RADc2.2_t.E2<-attr(predict(RADc2.2.E2[[1]][[6]],scal_mat3.2_t.E2,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.2_t.E2)
RADc2.2_t.E3<-attr(predict(RADc2.2.E3[[1]][[6]],scal_mat3.2_t.E3,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.2_t.E3)
RADc2.2_t.E4<-attr(predict(RADc2.2.E4[[1]][[6]],scal_mat3.2_t.E4,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.2_t.E4)
RADc2.2_t.E5<-attr(predict(RADc2.2.E5[[1]][[6]],scal_mat3.2_t.E5,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.2_t.E5)
RADc2.2_t.E6<-attr(predict(RADc2.2.E6[[1]][[6]],scal_mat3.2_t.E6,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.2_t.E6)
RADc2.2_t.E7<-attr(predict(RADc2.2.E7[[1]][[6]],scal_mat3.2_t.E7,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.2_t.E7)
RADc2.2_t.E8<-attr(predict(RADc2.2.E8[[1]][[6]],scal_mat3.2_t.E8,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.2_t.E8)
RADc2.2_t.E9<-attr(predict(RADc2.2.E9[[1]][[6]],scal_mat3.2_t.E9,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.2_t.E9)
RADc2.2_t.E10<-attr(predict(RADc2.2.E10[[1]][[6]],scal_mat3.2_t.E10,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.2_t.E10)

RADc2.2_t<-Group_t(list(RADc2.2_t.E1,RADc2.2_t.E2,RADc2.2_t.E3,RADc2.2_t.E4,RADc2.2_t.E5,
                        RADc2.2_t.E6,RADc2.2_t.E7,RADc2.2_t.E8,RADc2.2_t.E9,RADc2.2_t.E10)) ; Store(RADc2.2_t)

#to check if ok
graph3(RADc2.2_t,RADc2.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

RADc2.IB.2_t.E1<-attr(predict(RADc2.IB.2.E1[[1]][[6]],scal.IB_mat3.2_t.E1,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB.2_t.E1)
RADc2.IB.2_t.E2<-attr(predict(RADc2.IB.2.E2[[1]][[6]],scal.IB_mat3.2_t.E2,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB.2_t.E2)
RADc2.IB.2_t.E3<-attr(predict(RADc2.IB.2.E3[[1]][[6]],scal.IB_mat3.2_t.E3,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB.2_t.E3)
RADc2.IB.2_t.E4<-attr(predict(RADc2.IB.2.E4[[1]][[6]],scal.IB_mat3.2_t.E4,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB.2_t.E4)
RADc2.IB.2_t.E5<-attr(predict(RADc2.IB.2.E5[[1]][[6]],scal.IB_mat3.2_t.E5,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB.2_t.E5)
RADc2.IB.2_t.E6<-attr(predict(RADc2.IB.2.E6[[1]][[6]],scal.IB_mat3.2_t.E6,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB.2_t.E6)
RADc2.IB.2_t.E7<-attr(predict(RADc2.IB.2.E7[[1]][[6]],scal.IB_mat3.2_t.E7,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB.2_t.E7)
RADc2.IB.2_t.E8<-attr(predict(RADc2.IB.2.E8[[1]][[6]],scal.IB_mat3.2_t.E8,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB.2_t.E8)
RADc2.IB.2_t.E9<-attr(predict(RADc2.IB.2.E9[[1]][[6]],scal.IB_mat3.2_t.E9,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB.2_t.E9)
RADc2.IB.2_t.E10<-attr(predict(RADc2.IB.2.E10[[1]][[6]],scal.IB_mat3.2_t.E10,probability =TRUE),"probabilities")[,"1"] ; Store(RADc2.IB.2_t.E10)

RADc2.IB.2_t<-Group_t(list(RADc2.IB.2_t.E1,RADc2.IB.2_t.E2,RADc2.IB.2_t.E3,RADc2.IB.2_t.E4,RADc2.IB.2_t.E5,
                           RADc2.IB.2_t.E6,RADc2.IB.2_t.E7,RADc2.IB.2_t.E8,RADc2.IB.2_t.E9,RADc2.IB.2_t.E10)) ; Store(RADc2.IB.2_t)

#to check if ok
graph3(RADc2.IB.2_t,RADc2.IB.2)

#####################################################################
# PREDICT Multinomial SVM
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

SVMm3_t.E1<-predict(SVMm3.E1[[1]][[6]],scal_mat3_t.E1) ; Store(SVMm3_t.E1)
SVMm3_t.E2<-predict(SVMm3.E2[[1]][[6]],scal_mat3_t.E2) ; Store(SVMm3_t.E2)
SVMm3_t.E3<-predict(SVMm3.E3[[1]][[6]],scal_mat3_t.E3) ; Store(SVMm3_t.E3)
SVMm3_t.E4<-predict(SVMm3.E4[[1]][[6]],scal_mat3_t.E4) ; Store(SVMm3_t.E4)
SVMm3_t.E5<-predict(SVMm3.E5[[1]][[6]],scal_mat3_t.E5) ; Store(SVMm3_t.E5)
SVMm3_t.E6<-predict(SVMm3.E6[[1]][[6]],scal_mat3_t.E6) ; Store(SVMm3_t.E6)
SVMm3_t.E7<-predict(SVMm3.E7[[1]][[6]],scal_mat3_t.E7) ; Store(SVMm3_t.E7)
SVMm3_t.E8<-predict(SVMm3.E8[[1]][[6]],scal_mat3_t.E8) ; Store(SVMm3_t.E8)
SVMm3_t.E9<-predict(SVMm3.E9[[1]][[6]],scal_mat3_t.E9) ; Store(SVMm3_t.E9)
SVMm3_t.E10<-predict(SVMm3.E10[[1]][[6]],scal_mat3_t.E10) ; Store(SVMm3_t.E10)

SVMm3_t.E1<-as.numeric(gsub("S","",SVMm3_t.E1)) ; Store(SVMm3_t.E1)
SVMm3_t.E2<-as.numeric(gsub("S","",SVMm3_t.E2)) ; Store(SVMm3_t.E2)
SVMm3_t.E3<-as.numeric(gsub("S","",SVMm3_t.E3)) ; Store(SVMm3_t.E3)
SVMm3_t.E4<-as.numeric(gsub("S","",SVMm3_t.E4)) ; Store(SVMm3_t.E4)
SVMm3_t.E5<-as.numeric(gsub("S","",SVMm3_t.E5)) ; Store(SVMm3_t.E5)
SVMm3_t.E6<-as.numeric(gsub("S","",SVMm3_t.E6)) ; Store(SVMm3_t.E6)
SVMm3_t.E7<-as.numeric(gsub("S","",SVMm3_t.E7)) ; Store(SVMm3_t.E7)
SVMm3_t.E8<-as.numeric(gsub("S","",SVMm3_t.E8)) ; Store(SVMm3_t.E8)
SVMm3_t.E9<-as.numeric(gsub("S","",SVMm3_t.E9)) ; Store(SVMm3_t.E9)
SVMm3_t.E10<-as.numeric(gsub("S","",SVMm3_t.E10)) ; Store(SVMm3_t.E10)

SVMm3_t<-Group_t(list(SVMm3_t.E1,SVMm3_t.E2,SVMm3_t.E3,SVMm3_t.E4,SVMm3_t.E5,
                      SVMm3_t.E6,SVMm3_t.E7,SVMm3_t.E8,SVMm3_t.E9,SVMm3_t.E10)) ; Store(SVMm3_t)

#to check if ok
graph3(SVMm3_t,SVMm3)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

SVMm3.IB_t.E1<-predict(SVMm3.IB.E1[[1]][[6]],scal.IB_mat3_t.E1) ; Store(SVMm3.IB_t.E1)
SVMm3.IB_t.E2<-predict(SVMm3.IB.E2[[1]][[6]],scal.IB_mat3_t.E2) ; Store(SVMm3.IB_t.E2)
SVMm3.IB_t.E3<-predict(SVMm3.IB.E3[[1]][[6]],scal.IB_mat3_t.E3) ; Store(SVMm3.IB_t.E3)
SVMm3.IB_t.E4<-predict(SVMm3.IB.E4[[1]][[6]],scal.IB_mat3_t.E4) ; Store(SVMm3.IB_t.E4)
SVMm3.IB_t.E5<-predict(SVMm3.IB.E5[[1]][[6]],scal.IB_mat3_t.E5) ; Store(SVMm3.IB_t.E5)
SVMm3.IB_t.E6<-predict(SVMm3.IB.E6[[1]][[6]],scal.IB_mat3_t.E6) ; Store(SVMm3.IB_t.E6)
SVMm3.IB_t.E7<-predict(SVMm3.IB.E7[[1]][[6]],scal.IB_mat3_t.E7) ; Store(SVMm3.IB_t.E7)
SVMm3.IB_t.E8<-predict(SVMm3.IB.E8[[1]][[6]],scal.IB_mat3_t.E8) ; Store(SVMm3.IB_t.E8)
SVMm3.IB_t.E9<-predict(SVMm3.IB.E9[[1]][[6]],scal.IB_mat3_t.E9) ; Store(SVMm3.IB_t.E9)
SVMm3.IB_t.E10<-predict(SVMm3.IB.E10[[1]][[6]],scal.IB_mat3_t.E10) ; Store(SVMm3.IB_t.E10)

SVMm3.IB_t.E1<-as.numeric(gsub("S","",SVMm3.IB_t.E1)) ; Store(SVMm3.IB_t.E1)
SVMm3.IB_t.E2<-as.numeric(gsub("S","",SVMm3.IB_t.E2)) ; Store(SVMm3.IB_t.E2)
SVMm3.IB_t.E3<-as.numeric(gsub("S","",SVMm3.IB_t.E3)) ; Store(SVMm3.IB_t.E3)
SVMm3.IB_t.E4<-as.numeric(gsub("S","",SVMm3.IB_t.E4)) ; Store(SVMm3.IB_t.E4)
SVMm3.IB_t.E5<-as.numeric(gsub("S","",SVMm3.IB_t.E5)) ; Store(SVMm3.IB_t.E5)
SVMm3.IB_t.E6<-as.numeric(gsub("S","",SVMm3.IB_t.E6)) ; Store(SVMm3.IB_t.E6)
SVMm3.IB_t.E7<-as.numeric(gsub("S","",SVMm3.IB_t.E7)) ; Store(SVMm3.IB_t.E7)
SVMm3.IB_t.E8<-as.numeric(gsub("S","",SVMm3.IB_t.E8)) ; Store(SVMm3.IB_t.E8)
SVMm3.IB_t.E9<-as.numeric(gsub("S","",SVMm3.IB_t.E9)) ; Store(SVMm3.IB_t.E9)
SVMm3.IB_t.E10<-as.numeric(gsub("S","",SVMm3.IB_t.E10)) ; Store(SVMm3.IB_t.E10)

SVMm3.IB_t<-Group_t(list(SVMm3.IB_t.E1,SVMm3.IB_t.E2,SVMm3.IB_t.E3,SVMm3.IB_t.E4,SVMm3.IB_t.E5,
                         SVMm3.IB_t.E6,SVMm3.IB_t.E7,SVMm3.IB_t.E8,SVMm3.IB_t.E9,SVMm3.IB_t.E10)) ; Store(SVMm3.IB_t)

#to check if ok
graph3(SVMm3.IB_t,SVMm3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

SVMm3.2_t.E1<-predict(SVMm3.2.E1[[1]][[6]],scal_mat3.2_t.E1) ; Store(SVMm3.2_t.E1)
SVMm3.2_t.E2<-predict(SVMm3.2.E2[[1]][[6]],scal_mat3.2_t.E2) ; Store(SVMm3.2_t.E2)
SVMm3.2_t.E3<-predict(SVMm3.2.E3[[1]][[6]],scal_mat3.2_t.E3) ; Store(SVMm3.2_t.E3)
SVMm3.2_t.E4<-predict(SVMm3.2.E4[[1]][[6]],scal_mat3.2_t.E4) ; Store(SVMm3.2_t.E4)
SVMm3.2_t.E5<-predict(SVMm3.2.E5[[1]][[6]],scal_mat3.2_t.E5) ; Store(SVMm3.2_t.E5)
SVMm3.2_t.E6<-predict(SVMm3.2.E6[[1]][[6]],scal_mat3.2_t.E6) ; Store(SVMm3.2_t.E6)
SVMm3.2_t.E7<-predict(SVMm3.2.E7[[1]][[6]],scal_mat3.2_t.E7) ; Store(SVMm3.2_t.E7)
SVMm3.2_t.E8<-predict(SVMm3.2.E8[[1]][[6]],scal_mat3.2_t.E8) ; Store(SVMm3.2_t.E8)
SVMm3.2_t.E9<-predict(SVMm3.2.E9[[1]][[6]],scal_mat3.2_t.E9) ; Store(SVMm3.2_t.E9)
SVMm3.2_t.E10<-predict(SVMm3.2.E10[[1]][[6]],scal_mat3.2_t.E10) ; Store(SVMm3.2_t.E10)

SVMm3.2_t.E1<-as.numeric(gsub("S","",SVMm3.2_t.E1)) ; Store(SVMm3.2_t.E1)
SVMm3.2_t.E2<-as.numeric(gsub("S","",SVMm3.2_t.E2)) ; Store(SVMm3.2_t.E2)
SVMm3.2_t.E3<-as.numeric(gsub("S","",SVMm3.2_t.E3)) ; Store(SVMm3.2_t.E3)
SVMm3.2_t.E4<-as.numeric(gsub("S","",SVMm3.2_t.E4)) ; Store(SVMm3.2_t.E4)
SVMm3.2_t.E5<-as.numeric(gsub("S","",SVMm3.2_t.E5)) ; Store(SVMm3.2_t.E5)
SVMm3.2_t.E6<-as.numeric(gsub("S","",SVMm3.2_t.E6)) ; Store(SVMm3.2_t.E6)
SVMm3.2_t.E7<-as.numeric(gsub("S","",SVMm3.2_t.E7)) ; Store(SVMm3.2_t.E7)
SVMm3.2_t.E8<-as.numeric(gsub("S","",SVMm3.2_t.E8)) ; Store(SVMm3.2_t.E8)
SVMm3.2_t.E9<-as.numeric(gsub("S","",SVMm3.2_t.E9)) ; Store(SVMm3.2_t.E9)
SVMm3.2_t.E10<-as.numeric(gsub("S","",SVMm3.2_t.E10)) ; Store(SVMm3.2_t.E10)

SVMm3.2_t<-Group_t(list(SVMm3.2_t.E1,SVMm3.2_t.E2,SVMm3.2_t.E3,SVMm3.2_t.E4,SVMm3.2_t.E5,
                        SVMm3.2_t.E6,SVMm3.2_t.E7,SVMm3.2_t.E8,SVMm3.2_t.E9,SVMm3.2_t.E10)) ; Store(SVMm3.2_t)

#to check if ok
graph3(SVMm3.2_t,SVMm3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

SVMm3.IB.2_t.E1<-predict(SVMm3.IB.2.E1[[1]][[6]],scal.IB_mat3.2_t.E1) ; Store(SVMm3.IB.2_t.E1)
SVMm3.IB.2_t.E2<-predict(SVMm3.IB.2.E2[[1]][[6]],scal.IB_mat3.2_t.E2) ; Store(SVMm3.IB.2_t.E2)
SVMm3.IB.2_t.E3<-predict(SVMm3.IB.2.E3[[1]][[6]],scal.IB_mat3.2_t.E3) ; Store(SVMm3.IB.2_t.E3)
SVMm3.IB.2_t.E4<-predict(SVMm3.IB.2.E4[[1]][[6]],scal.IB_mat3.2_t.E4) ; Store(SVMm3.IB.2_t.E4)
SVMm3.IB.2_t.E5<-predict(SVMm3.IB.2.E5[[1]][[6]],scal.IB_mat3.2_t.E5) ; Store(SVMm3.IB.2_t.E5)
SVMm3.IB.2_t.E6<-predict(SVMm3.IB.2.E6[[1]][[6]],scal.IB_mat3.2_t.E6) ; Store(SVMm3.IB.2_t.E6)
SVMm3.IB.2_t.E7<-predict(SVMm3.IB.2.E7[[1]][[6]],scal.IB_mat3.2_t.E7) ; Store(SVMm3.IB.2_t.E7)
SVMm3.IB.2_t.E8<-predict(SVMm3.IB.2.E8[[1]][[6]],scal.IB_mat3.2_t.E8) ; Store(SVMm3.IB.2_t.E8)
SVMm3.IB.2_t.E9<-predict(SVMm3.IB.2.E9[[1]][[6]],scal.IB_mat3.2_t.E9) ; Store(SVMm3.IB.2_t.E9)
SVMm3.IB.2_t.E10<-predict(SVMm3.IB.2.E10[[1]][[6]],scal.IB_mat3.2_t.E10) ; Store(SVMm3.IB.2_t.E10)

SVMm3.IB.2_t.E1<-as.numeric(gsub("S","",SVMm3.IB.2_t.E1)) ; Store(SVMm3.IB.2_t.E1)
SVMm3.IB.2_t.E2<-as.numeric(gsub("S","",SVMm3.IB.2_t.E2)) ; Store(SVMm3.IB.2_t.E2)
SVMm3.IB.2_t.E3<-as.numeric(gsub("S","",SVMm3.IB.2_t.E3)) ; Store(SVMm3.IB.2_t.E3)
SVMm3.IB.2_t.E4<-as.numeric(gsub("S","",SVMm3.IB.2_t.E4)) ; Store(SVMm3.IB.2_t.E4)
SVMm3.IB.2_t.E5<-as.numeric(gsub("S","",SVMm3.IB.2_t.E5)) ; Store(SVMm3.IB.2_t.E5)
SVMm3.IB.2_t.E6<-as.numeric(gsub("S","",SVMm3.IB.2_t.E6)) ; Store(SVMm3.IB.2_t.E6)
SVMm3.IB.2_t.E7<-as.numeric(gsub("S","",SVMm3.IB.2_t.E7)) ; Store(SVMm3.IB.2_t.E7)
SVMm3.IB.2_t.E8<-as.numeric(gsub("S","",SVMm3.IB.2_t.E8)) ; Store(SVMm3.IB.2_t.E8)
SVMm3.IB.2_t.E9<-as.numeric(gsub("S","",SVMm3.IB.2_t.E9)) ; Store(SVMm3.IB.2_t.E9)
SVMm3.IB.2_t.E10<-as.numeric(gsub("S","",SVMm3.IB.2_t.E10)) ; Store(SVMm3.IB.2_t.E10)

SVMm3.IB.2_t<-Group_t(list(SVMm3.IB.2_t.E1,SVMm3.IB.2_t.E2,SVMm3.IB.2_t.E3,SVMm3.IB.2_t.E4,SVMm3.IB.2_t.E5,
                           SVMm3.IB.2_t.E6,SVMm3.IB.2_t.E7,SVMm3.IB.2_t.E8,SVMm3.IB.2_t.E9,SVMm3.IB.2_t.E10)) ; Store(SVMm3.IB.2_t)

#to check if ok
graph3(SVMm3.IB.2_t,SVMm3.IB.2)

#####################################################################
# TRAIN SVM multinomial
# with radial kernel on scaled matrix
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

RADm2_t.E1<-predict(RADm2.E1[[1]][[6]],scal_mat3_t.E1) ; Store(RADm2_t.E1)
RADm2_t.E2<-predict(RADm2.E2[[1]][[6]],scal_mat3_t.E2) ; Store(RADm2_t.E2)
RADm2_t.E3<-predict(RADm2.E3[[1]][[6]],scal_mat3_t.E3) ; Store(RADm2_t.E3)
RADm2_t.E4<-predict(RADm2.E4[[1]][[6]],scal_mat3_t.E4) ; Store(RADm2_t.E4)
RADm2_t.E5<-predict(RADm2.E5[[1]][[6]],scal_mat3_t.E5) ; Store(RADm2_t.E5)
RADm2_t.E6<-predict(RADm2.E6[[1]][[6]],scal_mat3_t.E6) ; Store(RADm2_t.E6)
RADm2_t.E7<-predict(RADm2.E7[[1]][[6]],scal_mat3_t.E7) ; Store(RADm2_t.E7)
RADm2_t.E8<-predict(RADm2.E8[[1]][[6]],scal_mat3_t.E8) ; Store(RADm2_t.E8)
RADm2_t.E9<-predict(RADm2.E9[[1]][[6]],scal_mat3_t.E9) ; Store(RADm2_t.E9)
RADm2_t.E10<-predict(RADm2.E10[[1]][[6]],scal_mat3_t.E10) ; Store(RADm2_t.E10)

RADm2_t.E1<-as.numeric(gsub("S","",RADm2_t.E1)) ; Store(RADm2_t.E1)
RADm2_t.E2<-as.numeric(gsub("S","",RADm2_t.E2)) ; Store(RADm2_t.E2)
RADm2_t.E3<-as.numeric(gsub("S","",RADm2_t.E3)) ; Store(RADm2_t.E3)
RADm2_t.E4<-as.numeric(gsub("S","",RADm2_t.E4)) ; Store(RADm2_t.E4)
RADm2_t.E5<-as.numeric(gsub("S","",RADm2_t.E5)) ; Store(RADm2_t.E5)
RADm2_t.E6<-as.numeric(gsub("S","",RADm2_t.E6)) ; Store(RADm2_t.E6)
RADm2_t.E7<-as.numeric(gsub("S","",RADm2_t.E7)) ; Store(RADm2_t.E7)
RADm2_t.E8<-as.numeric(gsub("S","",RADm2_t.E8)) ; Store(RADm2_t.E8)
RADm2_t.E9<-as.numeric(gsub("S","",RADm2_t.E9)) ; Store(RADm2_t.E9)
RADm2_t.E10<-as.numeric(gsub("S","",RADm2_t.E10)) ; Store(RADm2_t.E10)

RADm2_t<-Group_t(list(RADm2_t.E1,RADm2_t.E2,RADm2_t.E3,RADm2_t.E4,RADm2_t.E5,
                      RADm2_t.E6,RADm2_t.E7,RADm2_t.E8,RADm2_t.E9,RADm2_t.E10)) ; Store(RADm2_t)

#to check if ok
graph3(RADm2_t,RADm2)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

RADm2.IB_t.E1<-predict(RADm2.IB.E1[[1]][[6]],scal.IB_mat3_t.E1) ; Store(RADm2.IB_t.E1)
RADm2.IB_t.E2<-predict(RADm2.IB.E2[[1]][[6]],scal.IB_mat3_t.E2) ; Store(RADm2.IB_t.E2)
RADm2.IB_t.E3<-predict(RADm2.IB.E3[[1]][[6]],scal.IB_mat3_t.E3) ; Store(RADm2.IB_t.E3)
RADm2.IB_t.E4<-predict(RADm2.IB.E4[[1]][[6]],scal.IB_mat3_t.E4) ; Store(RADm2.IB_t.E4)
RADm2.IB_t.E5<-predict(RADm2.IB.E5[[1]][[6]],scal.IB_mat3_t.E5) ; Store(RADm2.IB_t.E5)
RADm2.IB_t.E6<-predict(RADm2.IB.E6[[1]][[6]],scal.IB_mat3_t.E6) ; Store(RADm2.IB_t.E6)
RADm2.IB_t.E7<-predict(RADm2.IB.E7[[1]][[6]],scal.IB_mat3_t.E7) ; Store(RADm2.IB_t.E7)
RADm2.IB_t.E8<-predict(RADm2.IB.E8[[1]][[6]],scal.IB_mat3_t.E8) ; Store(RADm2.IB_t.E8)
RADm2.IB_t.E9<-predict(RADm2.IB.E9[[1]][[6]],scal.IB_mat3_t.E9) ; Store(RADm2.IB_t.E9)
RADm2.IB_t.E10<-predict(RADm2.IB.E10[[1]][[6]],scal.IB_mat3_t.E10) ; Store(RADm2.IB_t.E10)

RADm2.IB_t.E1<-as.numeric(gsub("S","",RADm2.IB_t.E1)) ; Store(RADm2.IB_t.E1)
RADm2.IB_t.E2<-as.numeric(gsub("S","",RADm2.IB_t.E2)) ; Store(RADm2.IB_t.E2)
RADm2.IB_t.E3<-as.numeric(gsub("S","",RADm2.IB_t.E3)) ; Store(RADm2.IB_t.E3)
RADm2.IB_t.E4<-as.numeric(gsub("S","",RADm2.IB_t.E4)) ; Store(RADm2.IB_t.E4)
RADm2.IB_t.E5<-as.numeric(gsub("S","",RADm2.IB_t.E5)) ; Store(RADm2.IB_t.E5)
RADm2.IB_t.E6<-as.numeric(gsub("S","",RADm2.IB_t.E6)) ; Store(RADm2.IB_t.E6)
RADm2.IB_t.E7<-as.numeric(gsub("S","",RADm2.IB_t.E7)) ; Store(RADm2.IB_t.E7)
RADm2.IB_t.E8<-as.numeric(gsub("S","",RADm2.IB_t.E8)) ; Store(RADm2.IB_t.E8)
RADm2.IB_t.E9<-as.numeric(gsub("S","",RADm2.IB_t.E9)) ; Store(RADm2.IB_t.E9)
RADm2.IB_t.E10<-as.numeric(gsub("S","",RADm2.IB_t.E10)) ; Store(RADm2.IB_t.E10)

RADm2.IB_t<-Group_t(list(RADm2.IB_t.E1,RADm2.IB_t.E2,RADm2.IB_t.E3,RADm2.IB_t.E4,RADm2.IB_t.E5,
                         RADm2.IB_t.E6,RADm2.IB_t.E7,RADm2.IB_t.E8,RADm2.IB_t.E9,RADm2.IB_t.E10)) ; Store(RADm2.IB_t)

#to check if ok
graph3(RADm2.IB_t,RADm2.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

RADm2.2_t.E1<-predict(RADm2.2.E1[[1]][[6]],scal_mat3.2_t.E1) ; Store(RADm2.2_t.E1)
RADm2.2_t.E2<-predict(RADm2.2.E2[[1]][[6]],scal_mat3.2_t.E2) ; Store(RADm2.2_t.E2)
RADm2.2_t.E3<-predict(RADm2.2.E3[[1]][[6]],scal_mat3.2_t.E3) ; Store(RADm2.2_t.E3)
RADm2.2_t.E4<-predict(RADm2.2.E4[[1]][[6]],scal_mat3.2_t.E4) ; Store(RADm2.2_t.E4)
RADm2.2_t.E5<-predict(RADm2.2.E5[[1]][[6]],scal_mat3.2_t.E5) ; Store(RADm2.2_t.E5)
RADm2.2_t.E6<-predict(RADm2.2.E6[[1]][[6]],scal_mat3.2_t.E6) ; Store(RADm2.2_t.E6)
RADm2.2_t.E7<-predict(RADm2.2.E7[[1]][[6]],scal_mat3.2_t.E7) ; Store(RADm2.2_t.E7)
RADm2.2_t.E8<-predict(RADm2.2.E8[[1]][[6]],scal_mat3.2_t.E8) ; Store(RADm2.2_t.E8)
RADm2.2_t.E9<-predict(RADm2.2.E9[[1]][[6]],scal_mat3.2_t.E9) ; Store(RADm2.2_t.E9)
RADm2.2_t.E10<-predict(RADm2.2.E10[[1]][[6]],scal_mat3.2_t.E10) ; Store(RADm2.2_t.E10)

RADm2.2_t.E1<-as.numeric(gsub("S","",RADm2.2_t.E1)) ; Store(RADm2.2_t.E1)
RADm2.2_t.E2<-as.numeric(gsub("S","",RADm2.2_t.E2)) ; Store(RADm2.2_t.E2)
RADm2.2_t.E3<-as.numeric(gsub("S","",RADm2.2_t.E3)) ; Store(RADm2.2_t.E3)
RADm2.2_t.E4<-as.numeric(gsub("S","",RADm2.2_t.E4)) ; Store(RADm2.2_t.E4)
RADm2.2_t.E5<-as.numeric(gsub("S","",RADm2.2_t.E5)) ; Store(RADm2.2_t.E5)
RADm2.2_t.E6<-as.numeric(gsub("S","",RADm2.2_t.E6)) ; Store(RADm2.2_t.E6)
RADm2.2_t.E7<-as.numeric(gsub("S","",RADm2.2_t.E7)) ; Store(RADm2.2_t.E7)
RADm2.2_t.E8<-as.numeric(gsub("S","",RADm2.2_t.E8)) ; Store(RADm2.2_t.E8)
RADm2.2_t.E9<-as.numeric(gsub("S","",RADm2.2_t.E9)) ; Store(RADm2.2_t.E9)
RADm2.2_t.E10<-as.numeric(gsub("S","",RADm2.2_t.E10)) ; Store(RADm2.2_t.E10)

RADm2.2_t<-Group_t(list(RADm2.2_t.E1,RADm2.2_t.E2,RADm2.2_t.E3,RADm2.2_t.E4,RADm2.2_t.E5,
                        RADm2.2_t.E6,RADm2.2_t.E7,RADm2.2_t.E8,RADm2.2_t.E9,RADm2.2_t.E10)) ; Store(RADm2.2_t)

#to check if ok
graph3(RADm2.2_t,RADm2.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

RADm2.IB.2_t.E1<-predict(RADm2.IB.2.E1[[1]][[6]],scal.IB_mat3.2_t.E1) ; Store(RADm2.IB.2_t.E1)
RADm2.IB.2_t.E2<-predict(RADm2.IB.2.E2[[1]][[6]],scal.IB_mat3.2_t.E2) ; Store(RADm2.IB.2_t.E2)
RADm2.IB.2_t.E3<-predict(RADm2.IB.2.E3[[1]][[6]],scal.IB_mat3.2_t.E3) ; Store(RADm2.IB.2_t.E3)
RADm2.IB.2_t.E4<-predict(RADm2.IB.2.E4[[1]][[6]],scal.IB_mat3.2_t.E4) ; Store(RADm2.IB.2_t.E4)
RADm2.IB.2_t.E5<-predict(RADm2.IB.2.E5[[1]][[6]],scal.IB_mat3.2_t.E5) ; Store(RADm2.IB.2_t.E5)
RADm2.IB.2_t.E6<-predict(RADm2.IB.2.E6[[1]][[6]],scal.IB_mat3.2_t.E6) ; Store(RADm2.IB.2_t.E6)
RADm2.IB.2_t.E7<-predict(RADm2.IB.2.E7[[1]][[6]],scal.IB_mat3.2_t.E7) ; Store(RADm2.IB.2_t.E7)
RADm2.IB.2_t.E8<-predict(RADm2.IB.2.E8[[1]][[6]],scal.IB_mat3.2_t.E8) ; Store(RADm2.IB.2_t.E8)
RADm2.IB.2_t.E9<-predict(RADm2.IB.2.E9[[1]][[6]],scal.IB_mat3.2_t.E9) ; Store(RADm2.IB.2_t.E9)
RADm2.IB.2_t.E10<-predict(RADm2.IB.2.E10[[1]][[6]],scal.IB_mat3.2_t.E10) ; Store(RADm2.IB.2_t.E10)

RADm2.IB.2_t.E1<-as.numeric(gsub("S","",RADm2.IB.2_t.E1)) ; Store(RADm2.IB.2_t.E1)
RADm2.IB.2_t.E2<-as.numeric(gsub("S","",RADm2.IB.2_t.E2)) ; Store(RADm2.IB.2_t.E2)
RADm2.IB.2_t.E3<-as.numeric(gsub("S","",RADm2.IB.2_t.E3)) ; Store(RADm2.IB.2_t.E3)
RADm2.IB.2_t.E4<-as.numeric(gsub("S","",RADm2.IB.2_t.E4)) ; Store(RADm2.IB.2_t.E4)
RADm2.IB.2_t.E5<-as.numeric(gsub("S","",RADm2.IB.2_t.E5)) ; Store(RADm2.IB.2_t.E5)
RADm2.IB.2_t.E6<-as.numeric(gsub("S","",RADm2.IB.2_t.E6)) ; Store(RADm2.IB.2_t.E6)
RADm2.IB.2_t.E7<-as.numeric(gsub("S","",RADm2.IB.2_t.E7)) ; Store(RADm2.IB.2_t.E7)
RADm2.IB.2_t.E8<-as.numeric(gsub("S","",RADm2.IB.2_t.E8)) ; Store(RADm2.IB.2_t.E8)
RADm2.IB.2_t.E9<-as.numeric(gsub("S","",RADm2.IB.2_t.E9)) ; Store(RADm2.IB.2_t.E9)
RADm2.IB.2_t.E10<-as.numeric(gsub("S","",RADm2.IB.2_t.E10)) ; Store(RADm2.IB.2_t.E10)

RADm2.IB.2_t<-Group_t(list(RADm2.IB.2_t.E1,RADm2.IB.2_t.E2,RADm2.IB.2_t.E3,RADm2.IB.2_t.E4,RADm2.IB.2_t.E5,
                           RADm2.IB.2_t.E6,RADm2.IB.2_t.E7,RADm2.IB.2_t.E8,RADm2.IB.2_t.E9,RADm2.IB.2_t.E10)) ; Store(RADm2.IB.2_t)

#to check if ok
graph3(RADm2.IB.2_t,RADm2.IB.2)

#####################################################################
#####################################################################
# PREDICT SVM with radial kernel on ridit scores
#####################################################################
#####################################################################

# function to produce reduced matrix and predict
#####################################################################

Pred_RAD<-function(data,metrics,M,fit,newdata) {
  data<-as.matrix(data)
  WHICH<-which(metrics[[6]][[3]]>M)
  pc.cr<-princomp(data[,WHICH],centre=FALSE,scale=FALSE)
  tmp3<-cumsum(pc.cr$sdev^2)/sum(pc.cr$sdev^2)
  DPCAnew<-as.matrix(predict(pc.cr,newdata=as.matrix(newdata)[,WHICH])[,1:min(which(tmp3>0.98))])
  predict(fit,DPCAnew)
}
Store(Pred_RAD)

# predict
#####################################################################

library(e1071)

RAD_t.E1<-Pred_RAD(Ridit_mat.E1,metrics.E1,0.25, RAD.E1[[1]][[6]],Ridit_mat_t.E1) ; Store(RAD_t.E1)
RAD_t.E2<-Pred_RAD(Ridit_mat.E2,metrics.E2,0.25, RAD.E2[[1]][[6]],Ridit_mat_t.E2) ; Store(RAD_t.E2)
RAD_t.E3<-Pred_RAD(Ridit_mat.E3,metrics.E3,0.25, RAD.E3[[1]][[6]],Ridit_mat_t.E3) ; Store(RAD_t.E3)
RAD_t.E4<-Pred_RAD(Ridit_mat.E4,metrics.E4,0.4, RAD.E4[[1]][[6]],Ridit_mat_t.E4) ; Store(RAD_t.E4)
RAD_t.E5<-Pred_RAD(Ridit_mat.E5,metrics.E5,0.25, RAD.E5[[1]][[6]],Ridit_mat_t.E5) ; Store(RAD_t.E5)
RAD_t.E6<-Pred_RAD(Ridit_mat.E6,metrics.E6,0.25, RAD.E6[[1]][[6]],Ridit_mat_t.E6) ; Store(RAD_t.E6)
RAD_t.E7<-Pred_RAD(Ridit_mat.E7,metrics.E7,0.25, RAD.E7[[1]][[6]],Ridit_mat_t.E7) ; Store(RAD_t.E7)
RAD_t.E8<-Pred_RAD(Ridit_mat.E8,metrics.E8,0.8, RAD.E8[[1]][[6]],Ridit_mat_t.E8) ; Store(RAD_t.E8)
RAD_t.E9<-Pred_RAD(Ridit_mat.E9,metrics.E9,0.3, RAD.E9[[1]][[6]],Ridit_mat_t.E9) ; Store(RAD_t.E9)
RAD_t.E10<-Pred_RAD(Ridit_mat.E10,metrics.E10,0.25, RAD.E10[[1]][[6]],Ridit_mat_t.E10) ; Store(RAD_t.E10)

RAD_t<-Group_t(list(RAD_t.E1,RAD_t.E2,RAD_t.E3,RAD_t.E4,RAD_t.E5,
                    RAD_t.E6,RAD_t.E7,RAD_t.E8,RAD_t.E9,RAD_t.E10)) ; Store(RAD_t)

#to check if ok
graph3(RAD_t,RAD)

#####################################################################
#####################################################################
# PREDICT Glmnet on scaled matrix
# scaled with delta tfidf and bns
# using isgood and isbad
# regression and multinomial
#####################################################################
#####################################################################

#####################################################################
# PREDICT Glmnet on scaled matrix with delta tfidf
#####################################################################

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

GNETns1_t.E1<-predict(GNETns1.E1[[1]][[6]],scal_mat1_t.E1,type="response", s="lambda.min")[,1]
GNETns1_t.E2<-predict(GNETns1.E2[[1]][[6]],scal_mat1_t.E2,type="response", s="lambda.min")[,1]
GNETns1_t.E3<-predict(GNETns1.E3[[1]][[6]],scal_mat1_t.E3,type="response", s="lambda.min")[,1]
GNETns1_t.E4<-predict(GNETns1.E4[[1]][[6]],scal_mat1_t.E4,type="response", s="lambda.min")[,1]
GNETns1_t.E5<-predict(GNETns1.E5[[1]][[6]],scal_mat1_t.E5,type="response", s="lambda.min")[,1]
GNETns1_t.E6<-predict(GNETns1.E6[[1]][[6]],scal_mat1_t.E6,type="response", s="lambda.min")[,1]
GNETns1_t.E7<-predict(GNETns1.E7[[1]][[6]],scal_mat1_t.E7,type="response", s="lambda.min")[,1]
GNETns1_t.E8<-predict(GNETns1.E8[[1]][[6]],scal_mat1_t.E8,type="response", s="lambda.min")[,1]
GNETns1_t.E9<-predict(GNETns1.E9[[1]][[6]],scal_mat1_t.E9,type="response", s="lambda.min")[,1]
GNETns1_t.E10<-predict(GNETns1.E10[[1]][[6]],scal_mat1_t.E10,type="response", s="lambda.min")[,1]

GNETns1_t<-Group_t(list(GNETns1_t.E1,GNETns1_t.E2,GNETns1_t.E3,GNETns1_t.E4,GNETns1_t.E5,
                        GNETns1_t.E6,GNETns1_t.E7,GNETns1_t.E8,GNETns1_t.E9,GNETns1_t.E10)) ; Store(GNETns1_t)

#to check if ok
graph3(GNETns1_t,GNETns1)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

GNETns1.IB_t.E1<-predict(GNETns1.IB.E1[[1]][[6]],scal.IB_mat1_t.E1,type="response", s="lambda.min")[,1]
GNETns1.IB_t.E2<-predict(GNETns1.IB.E2[[1]][[6]],scal.IB_mat1_t.E2,type="response", s="lambda.min")[,1]
GNETns1.IB_t.E3<-predict(GNETns1.IB.E3[[1]][[6]],scal.IB_mat1_t.E3,type="response", s="lambda.min")[,1]
GNETns1.IB_t.E4<-predict(GNETns1.IB.E4[[1]][[6]],scal.IB_mat1_t.E4,type="response", s="lambda.min")[,1]
GNETns1.IB_t.E5<-predict(GNETns1.IB.E5[[1]][[6]],scal.IB_mat1_t.E5,type="response", s="lambda.min")[,1]
GNETns1.IB_t.E6<-predict(GNETns1.IB.E6[[1]][[6]],scal.IB_mat1_t.E6,type="response", s="lambda.min")[,1]
GNETns1.IB_t.E7<-predict(GNETns1.IB.E7[[1]][[6]],scal.IB_mat1_t.E7,type="response", s="lambda.min")[,1]
GNETns1.IB_t.E8<-predict(GNETns1.IB.E8[[1]][[6]],scal.IB_mat1_t.E8,type="response", s="lambda.min")[,1]
GNETns1.IB_t.E9<-predict(GNETns1.IB.E9[[1]][[6]],scal.IB_mat1_t.E9,type="response", s="lambda.min")[,1]
GNETns1.IB_t.E10<-predict(GNETns1.IB.E10[[1]][[6]],scal.IB_mat1_t.E10,type="response", s="lambda.min")[,1]

GNETns1.IB_t<-Group_t(list(GNETns1.IB_t.E1,GNETns1.IB_t.E2,GNETns1.IB_t.E3,GNETns1.IB_t.E4,GNETns1.IB_t.E5,
                           GNETns1.IB_t.E6,GNETns1.IB_t.E7,GNETns1.IB_t.E8,GNETns1.IB_t.E9,GNETns1.IB_t.E10)) ; Store(GNETns1.IB_t)

#to check if ok
graph3(GNETns1.IB_t,GNETns1.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

GNETns1.2_t.E1<-predict(GNETns1.2.E1[[1]][[6]],scal_mat1.2_t.E1,type="response", s="lambda.min")[,1]
GNETns1.2_t.E2<-predict(GNETns1.2.E2[[1]][[6]],scal_mat1.2_t.E2,type="response", s="lambda.min")[,1]
GNETns1.2_t.E3<-predict(GNETns1.2.E3[[1]][[6]],scal_mat1.2_t.E3,type="response", s="lambda.min")[,1]
GNETns1.2_t.E4<-predict(GNETns1.2.E4[[1]][[6]],scal_mat1.2_t.E4,type="response", s="lambda.min")[,1]
GNETns1.2_t.E5<-predict(GNETns1.2.E5[[1]][[6]],scal_mat1.2_t.E5,type="response", s="lambda.min")[,1]
GNETns1.2_t.E6<-predict(GNETns1.2.E6[[1]][[6]],scal_mat1.2_t.E6,type="response", s="lambda.min")[,1]
GNETns1.2_t.E7<-predict(GNETns1.2.E7[[1]][[6]],scal_mat1.2_t.E7,type="response", s="lambda.min")[,1]
GNETns1.2_t.E8<-predict(GNETns1.2.E8[[1]][[6]],scal_mat1.2_t.E8,type="response", s="lambda.min")[,1]
GNETns1.2_t.E9<-predict(GNETns1.2.E9[[1]][[6]],scal_mat1.2_t.E9,type="response", s="lambda.min")[,1]
GNETns1.2_t.E10<-predict(GNETns1.2.E10[[1]][[6]],scal_mat1.2_t.E10,type="response", s="lambda.min")[,1]

GNETns1.2_t<-Group_t(list(GNETns1.2_t.E1,GNETns1.2_t.E2,GNETns1.2_t.E3,GNETns1.2_t.E4,GNETns1.2_t.E5,
                          GNETns1.2_t.E6,GNETns1.2_t.E7,GNETns1.2_t.E8,GNETns1.2_t.E9,GNETns1.2_t.E10)) ; Store(GNETns1.2_t)

#to check if ok
graph3(GNETns1.2_t,GNETns1.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

GNETns1.IB.2_t.E1<-predict(GNETns1.IB.2.E1[[1]][[6]],scal.IB_mat1.2_t.E1,type="response", s="lambda.min")[,1]
GNETns1.IB.2_t.E2<-predict(GNETns1.IB.2.E2[[1]][[6]],scal.IB_mat1.2_t.E2,type="response", s="lambda.min")[,1]
GNETns1.IB.2_t.E3<-predict(GNETns1.IB.2.E3[[1]][[6]],scal.IB_mat1.2_t.E3,type="response", s="lambda.min")[,1]
GNETns1.IB.2_t.E4<-predict(GNETns1.IB.2.E4[[1]][[6]],scal.IB_mat1.2_t.E4,type="response", s="lambda.min")[,1]
GNETns1.IB.2_t.E5<-predict(GNETns1.IB.2.E5[[1]][[6]],scal.IB_mat1.2_t.E5,type="response", s="lambda.min")[,1]
GNETns1.IB.2_t.E6<-predict(GNETns1.IB.2.E6[[1]][[6]],scal.IB_mat1.2_t.E6,type="response", s="lambda.min")[,1]
GNETns1.IB.2_t.E7<-predict(GNETns1.IB.2.E7[[1]][[6]],scal.IB_mat1.2_t.E7,type="response", s="lambda.min")[,1]
GNETns1.IB.2_t.E8<-predict(GNETns1.IB.2.E8[[1]][[6]],scal.IB_mat1.2_t.E8,type="response", s="lambda.min")[,1]
GNETns1.IB.2_t.E9<-predict(GNETns1.IB.2.E9[[1]][[6]],scal.IB_mat1.2_t.E9,type="response", s="lambda.min")[,1]
GNETns1.IB.2_t.E10<-predict(GNETns1.IB.2.E10[[1]][[6]],scal.IB_mat1.2_t.E10,type="response", s="lambda.min")[,1]

GNETns1.IB.2_t<-Group_t(list(GNETns1.IB.2_t.E1,GNETns1.IB.2_t.E2,GNETns1.IB.2_t.E3,GNETns1.IB.2_t.E4,GNETns1.IB.2_t.E5,
                             GNETns1.IB.2_t.E6,GNETns1.IB.2_t.E7,GNETns1.IB.2_t.E8,GNETns1.IB.2_t.E9,GNETns1.IB.2_t.E10)) ; Store(GNETns1.IB.2_t)

#to check if ok
graph3(GNETns1.IB.2_t,GNETns1.IB.2)

#####################################################################
# PREDICT Glmnet on scaled matrix with bns
#####################################################################

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

GNETns3_t.E1<-predict(GNETns3.E1[[1]][[6]],scal_mat3_t.E1,type="response", s="lambda.min")[,1]
GNETns3_t.E2<-predict(GNETns3.E2[[1]][[6]],scal_mat3_t.E2,type="response", s="lambda.min")[,1]
GNETns3_t.E3<-predict(GNETns3.E3[[1]][[6]],scal_mat3_t.E3,type="response", s="lambda.min")[,1]
GNETns3_t.E4<-predict(GNETns3.E4[[1]][[6]],scal_mat3_t.E4,type="response", s="lambda.min")[,1]
GNETns3_t.E5<-predict(GNETns3.E5[[1]][[6]],scal_mat3_t.E5,type="response", s="lambda.min")[,1]
GNETns3_t.E6<-predict(GNETns3.E6[[1]][[6]],scal_mat3_t.E6,type="response", s="lambda.min")[,1]
GNETns3_t.E7<-predict(GNETns3.E7[[1]][[6]],scal_mat3_t.E7,type="response", s="lambda.min")[,1]
GNETns3_t.E8<-predict(GNETns3.E8[[1]][[6]],scal_mat3_t.E8,type="response", s="lambda.min")[,1]
GNETns3_t.E9<-predict(GNETns3.E9[[1]][[6]],scal_mat3_t.E9,type="response", s="lambda.min")[,1]
GNETns3_t.E10<-predict(GNETns3.E10[[1]][[6]],scal_mat3_t.E10,type="response", s="lambda.min")[,1]

GNETns3_t<-Group_t(list(GNETns3_t.E1,GNETns3_t.E2,GNETns3_t.E3,GNETns3_t.E4,GNETns3_t.E5,
                        GNETns3_t.E6,GNETns3_t.E7,GNETns3_t.E8,GNETns3_t.E9,GNETns3_t.E10)) ; Store(GNETns3_t)

#to check if ok
graph3(GNETns3_t,GNETns3)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

GNETns3.IB_t.E1<-predict(GNETns3.IB.E1[[1]][[6]],scal.IB_mat3_t.E1,type="response", s="lambda.min")[,1]
GNETns3.IB_t.E2<-predict(GNETns3.IB.E2[[1]][[6]],scal.IB_mat3_t.E2,type="response", s="lambda.min")[,1]
GNETns3.IB_t.E3<-predict(GNETns3.IB.E3[[1]][[6]],scal.IB_mat3_t.E3,type="response", s="lambda.min")[,1]
GNETns3.IB_t.E4<-predict(GNETns3.IB.E4[[1]][[6]],scal.IB_mat3_t.E4,type="response", s="lambda.min")[,1]
GNETns3.IB_t.E5<-predict(GNETns3.IB.E5[[1]][[6]],scal.IB_mat3_t.E5,type="response", s="lambda.min")[,1]
GNETns3.IB_t.E6<-predict(GNETns3.IB.E6[[1]][[6]],scal.IB_mat3_t.E6,type="response", s="lambda.min")[,1]
GNETns3.IB_t.E7<-predict(GNETns3.IB.E7[[1]][[6]],scal.IB_mat3_t.E7,type="response", s="lambda.min")[,1]
GNETns3.IB_t.E8<-predict(GNETns3.IB.E8[[1]][[6]],scal.IB_mat3_t.E8,type="response", s="lambda.min")[,1]
GNETns3.IB_t.E9<-predict(GNETns3.IB.E9[[1]][[6]],scal.IB_mat3_t.E9,type="response", s="lambda.min")[,1]
GNETns3.IB_t.E10<-predict(GNETns3.IB.E10[[1]][[6]],scal.IB_mat3_t.E10,type="response", s="lambda.min")[,1]

GNETns3.IB_t<-Group_t(list(GNETns3.IB_t.E1,GNETns3.IB_t.E2,GNETns3.IB_t.E3,GNETns3.IB_t.E4,GNETns3.IB_t.E5,
                           GNETns3.IB_t.E6,GNETns3.IB_t.E7,GNETns3.IB_t.E8,GNETns3.IB_t.E9,GNETns3.IB_t.E10)) ; Store(GNETns3.IB_t)

#to check if ok
graph3(GNETns3.IB_t,GNETns3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

GNETns3.2_t.E1<-predict(GNETns3.2.E1[[1]][[6]],scal_mat3.2_t.E1,type="response", s="lambda.min")[,1]
GNETns3.2_t.E2<-predict(GNETns3.2.E2[[1]][[6]],scal_mat3.2_t.E2,type="response", s="lambda.min")[,1]
GNETns3.2_t.E3<-predict(GNETns3.2.E3[[1]][[6]],scal_mat3.2_t.E3,type="response", s="lambda.min")[,1]
GNETns3.2_t.E4<-predict(GNETns3.2.E4[[1]][[6]],scal_mat3.2_t.E4,type="response", s="lambda.min")[,1]
GNETns3.2_t.E5<-predict(GNETns3.2.E5[[1]][[6]],scal_mat3.2_t.E5,type="response", s="lambda.min")[,1]
GNETns3.2_t.E6<-predict(GNETns3.2.E6[[1]][[6]],scal_mat3.2_t.E6,type="response", s="lambda.min")[,1]
GNETns3.2_t.E7<-predict(GNETns3.2.E7[[1]][[6]],scal_mat3.2_t.E7,type="response", s="lambda.min")[,1]
GNETns3.2_t.E8<-predict(GNETns3.2.E8[[1]][[6]],scal_mat3.2_t.E8,type="response", s="lambda.min")[,1]
GNETns3.2_t.E9<-predict(GNETns3.2.E9[[1]][[6]],scal_mat3.2_t.E9,type="response", s="lambda.min")[,1]
GNETns3.2_t.E10<-predict(GNETns3.2.E10[[1]][[6]],scal_mat3.2_t.E10,type="response", s="lambda.min")[,1]

GNETns3.2_t<-Group_t(list(GNETns3.2_t.E1,GNETns3.2_t.E2,GNETns3.2_t.E3,GNETns3.2_t.E4,GNETns3.2_t.E5,
                          GNETns3.2_t.E6,GNETns3.2_t.E7,GNETns3.2_t.E8,GNETns3.2_t.E9,GNETns3.2_t.E10)) ; Store(GNETns3.2_t)

#to check if ok
graph3(GNETns3.2_t,GNETns3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

GNETns3.IB.2_t.E1<-predict(GNETns3.IB.2.E1[[1]][[6]],scal.IB_mat3.2_t.E1,type="response", s="lambda.min")[,1]
GNETns3.IB.2_t.E2<-predict(GNETns3.IB.2.E2[[1]][[6]],scal.IB_mat3.2_t.E2,type="response", s="lambda.min")[,1]
GNETns3.IB.2_t.E3<-predict(GNETns3.IB.2.E3[[1]][[6]],scal.IB_mat3.2_t.E3,type="response", s="lambda.min")[,1]
GNETns3.IB.2_t.E4<-predict(GNETns3.IB.2.E4[[1]][[6]],scal.IB_mat3.2_t.E4,type="response", s="lambda.min")[,1]
GNETns3.IB.2_t.E5<-predict(GNETns3.IB.2.E5[[1]][[6]],scal.IB_mat3.2_t.E5,type="response", s="lambda.min")[,1]
GNETns3.IB.2_t.E6<-predict(GNETns3.IB.2.E6[[1]][[6]],scal.IB_mat3.2_t.E6,type="response", s="lambda.min")[,1]
GNETns3.IB.2_t.E7<-predict(GNETns3.IB.2.E7[[1]][[6]],scal.IB_mat3.2_t.E7,type="response", s="lambda.min")[,1]
GNETns3.IB.2_t.E8<-predict(GNETns3.IB.2.E8[[1]][[6]],scal.IB_mat3.2_t.E8,type="response", s="lambda.min")[,1]
GNETns3.IB.2_t.E9<-predict(GNETns3.IB.2.E9[[1]][[6]],scal.IB_mat3.2_t.E9,type="response", s="lambda.min")[,1]
GNETns3.IB.2_t.E10<-predict(GNETns3.IB.2.E10[[1]][[6]],scal.IB_mat3.2_t.E10,type="response", s="lambda.min")[,1]

GNETns3.IB.2_t<-Group_t(list(GNETns3.IB.2_t.E1,GNETns3.IB.2_t.E2,GNETns3.IB.2_t.E3,GNETns3.IB.2_t.E4,GNETns3.IB.2_t.E5,
                             GNETns3.IB.2_t.E6,GNETns3.IB.2_t.E7,GNETns3.IB.2_t.E8,GNETns3.IB.2_t.E9,GNETns3.IB.2_t.E10)) ; Store(GNETns3.IB.2_t)

#to check if ok
graph3(GNETns3.IB.2_t,GNETns3.IB.2)

#####################################################################
# PREDICT multinomial glmnet
#####################################################################

library(glmnetcr)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

GNETcr3_t.E1<-as.numeric(as.character(predict(GNETcr3.E1[[1]][[6]],scal_mat3_t.E1,type="class")$class[,95]))
GNETcr3_t.E2<-as.numeric(as.character(predict(GNETcr3.E2[[1]][[6]],scal_mat3_t.E2,type="class")$class[,95]))
GNETcr3_t.E3<-as.numeric(as.character(predict(GNETcr3.E3[[1]][[6]],scal_mat3_t.E3,type="class")$class[,95]))
GNETcr3_t.E4<-as.numeric(as.character(predict(GNETcr3.E4[[1]][[6]],scal_mat3_t.E4,type="class")$class[,95]))
GNETcr3_t.E5<-as.numeric(as.character(predict(GNETcr3.E5[[1]][[6]],scal_mat3_t.E5,type="class")$class[,95]))
GNETcr3_t.E6<-as.numeric(as.character(predict(GNETcr3.E6[[1]][[6]],scal_mat3_t.E6,type="class")$class[,95]))
GNETcr3_t.E7<-as.numeric(as.character(predict(GNETcr3.E7[[1]][[6]],scal_mat3_t.E7,type="class")$class[,95]))
GNETcr3_t.E8<-as.numeric(as.character(predict(GNETcr3.E8[[1]][[6]],scal_mat3_t.E8,type="class")$class[,95]))
GNETcr3_t.E9<-as.numeric(as.character(predict(GNETcr3.E9[[1]][[6]],scal_mat3_t.E9,type="class")$class[,95]))
GNETcr3_t.E10<-as.numeric(as.character(predict(GNETcr3.E10[[1]][[6]],scal_mat3_t.E10,type="class")$class[,95]))

GNETcr3_t<-Group_t(list(GNETcr3_t.E1,GNETcr3_t.E2,GNETcr3_t.E3,GNETcr3_t.E4,GNETcr3_t.E5,
                        GNETcr3_t.E6,GNETcr3_t.E7,GNETcr3_t.E8,GNETcr3_t.E9,GNETcr3_t.E10)) ; Store(GNETcr3_t)

#to check if ok
graph3(GNETcr3_t,GNETcr3)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

GNETcr3.IB_t.E1<-as.numeric(as.character(predict(GNETcr3.IB.E1[[1]][[6]],scal.IB_mat3_t.E1,type="class")$class[,95]))
GNETcr3.IB_t.E2<-as.numeric(as.character(predict(GNETcr3.IB.E2[[1]][[6]],scal.IB_mat3_t.E2,type="class")$class[,95]))
GNETcr3.IB_t.E3<-as.numeric(as.character(predict(GNETcr3.IB.E3[[1]][[6]],scal.IB_mat3_t.E3,type="class")$class[,95]))
GNETcr3.IB_t.E4<-as.numeric(as.character(predict(GNETcr3.IB.E4[[1]][[6]],scal.IB_mat3_t.E4,type="class")$class[,95]))
GNETcr3.IB_t.E5<-as.numeric(as.character(predict(GNETcr3.IB.E5[[1]][[6]],scal.IB_mat3_t.E5,type="class")$class[,95]))
GNETcr3.IB_t.E6<-as.numeric(as.character(predict(GNETcr3.IB.E6[[1]][[6]],scal.IB_mat3_t.E6,type="class")$class[,95]))
GNETcr3.IB_t.E7<-as.numeric(as.character(predict(GNETcr3.IB.E7[[1]][[6]],scal.IB_mat3_t.E7,type="class")$class[,95]))
GNETcr3.IB_t.E8<-as.numeric(as.character(predict(GNETcr3.IB.E8[[1]][[6]],scal.IB_mat3_t.E8,type="class")$class[,95]))
GNETcr3.IB_t.E9<-as.numeric(as.character(predict(GNETcr3.IB.E9[[1]][[6]],scal.IB_mat3_t.E9,type="class")$class[,95]))
GNETcr3.IB_t.E10<-as.numeric(as.character(predict(GNETcr3.IB.E10[[1]][[6]],scal.IB_mat3_t.E10,type="class")$class[,95]))

GNETcr3.IB_t<-Group_t(list(GNETcr3.IB_t.E1,GNETcr3.IB_t.E2,GNETcr3.IB_t.E3,GNETcr3.IB_t.E4,GNETcr3.IB_t.E5,
                           GNETcr3.IB_t.E6,GNETcr3.IB_t.E7,GNETcr3.IB_t.E8,GNETcr3.IB_t.E9,GNETcr3.IB_t.E10)) ; Store(GNETcr3.IB_t)

#to check if ok
graph3(GNETcr3.IB_t,GNETcr3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

GNETcr3.2_t.E1<-as.numeric(as.character(predict(GNETcr3.2.E1[[1]][[6]],scal_mat3.2_t.E1,type="class")$class[,95]))
GNETcr3.2_t.E2<-as.numeric(as.character(predict(GNETcr3.2.E2[[1]][[6]],scal_mat3.2_t.E2,type="class")$class[,95]))
GNETcr3.2_t.E3<-as.numeric(as.character(predict(GNETcr3.2.E3[[1]][[6]],scal_mat3.2_t.E3,type="class")$class[,95]))
GNETcr3.2_t.E4<-as.numeric(as.character(predict(GNETcr3.2.E4[[1]][[6]],scal_mat3.2_t.E4,type="class")$class[,95]))
GNETcr3.2_t.E5<-as.numeric(as.character(predict(GNETcr3.2.E5[[1]][[6]],scal_mat3.2_t.E5,type="class")$class[,95]))
GNETcr3.2_t.E6<-as.numeric(as.character(predict(GNETcr3.2.E6[[1]][[6]],scal_mat3.2_t.E6,type="class")$class[,95]))
GNETcr3.2_t.E7<-as.numeric(as.character(predict(GNETcr3.2.E7[[1]][[6]],scal_mat3.2_t.E7,type="class")$class[,95]))
GNETcr3.2_t.E8<-as.numeric(as.character(predict(GNETcr3.2.E8[[1]][[6]],scal_mat3.2_t.E8,type="class")$class[,95]))
GNETcr3.2_t.E9<-as.numeric(as.character(predict(GNETcr3.2.E9[[1]][[6]],scal_mat3.2_t.E9,type="class")$class[,95]))
GNETcr3.2_t.E10<-as.numeric(as.character(predict(GNETcr3.2.E10[[1]][[6]],scal_mat3.2_t.E10,type="class")$class[,95]))

GNETcr3.2_t<-Group_t(list(GNETcr3.2_t.E1,GNETcr3.2_t.E2,GNETcr3.2_t.E3,GNETcr3.2_t.E4,GNETcr3.2_t.E5,
                          GNETcr3.2_t.E6,GNETcr3.2_t.E7,GNETcr3.2_t.E8,GNETcr3.2_t.E9,GNETcr3.2_t.E10)) ; Store(GNETcr3.2_t)

#to check if ok
graph3(GNETcr3.2_t,GNETcr3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

GNETcr3.IB.2_t.E1<-as.numeric(as.character(predict(GNETcr3.IB.2.E1[[1]][[6]],scal.IB_mat3.2_t.E1,type="class")$class[,95]))
GNETcr3.IB.2_t.E2<-as.numeric(as.character(predict(GNETcr3.IB.2.E2[[1]][[6]],scal.IB_mat3.2_t.E2,type="class")$class[,95]))
GNETcr3.IB.2_t.E3<-as.numeric(as.character(predict(GNETcr3.IB.2.E3[[1]][[6]],scal.IB_mat3.2_t.E3,type="class")$class[,95]))
GNETcr3.IB.2_t.E4<-as.numeric(as.character(predict(GNETcr3.IB.2.E4[[1]][[6]],scal.IB_mat3.2_t.E4,type="class")$class[,95]))
GNETcr3.IB.2_t.E5<-as.numeric(as.character(predict(GNETcr3.IB.2.E5[[1]][[6]],scal.IB_mat3.2_t.E5,type="class")$class[,95]))
GNETcr3.IB.2_t.E6<-as.numeric(as.character(predict(GNETcr3.IB.2.E6[[1]][[6]],scal.IB_mat3.2_t.E6,type="class")$class[,95]))
GNETcr3.IB.2_t.E7<-as.numeric(as.character(predict(GNETcr3.IB.2.E7[[1]][[6]],scal.IB_mat3.2_t.E7,type="class")$class[,95]))
GNETcr3.IB.2_t.E8<-as.numeric(as.character(predict(GNETcr3.IB.2.E8[[1]][[6]],scal.IB_mat3.2_t.E8,type="class")$class[,95]))
GNETcr3.IB.2_t.E9<-as.numeric(as.character(predict(GNETcr3.IB.2.E9[[1]][[6]],scal.IB_mat3.2_t.E9,type="class")$class[,95]))
GNETcr3.IB.2_t.E10<-as.numeric(as.character(predict(GNETcr3.IB.2.E10[[1]][[6]],scal.IB_mat3.2_t.E10,type="class")$class[,95]))

GNETcr3.IB.2_t<-Group_t(list(GNETcr3.IB.2_t.E1,GNETcr3.IB.2_t.E2,GNETcr3.IB.2_t.E3,GNETcr3.IB.2_t.E4,GNETcr3.IB.2_t.E5,
                             GNETcr3.IB.2_t.E6,GNETcr3.IB.2_t.E7,GNETcr3.IB.2_t.E8,GNETcr3.IB.2_t.E9,GNETcr3.IB.2_t.E10)) ; Store(GNETcr3.IB.2_t)

#to check if ok
graph3(GNETcr3.IB.2_t,GNETcr3.IB.2)

#####################################################################
#####################################################################
# Features Selection using GNET1R
#####################################################################
#####################################################################

DATAG.0<-function(data,fit) {
  VARSelect<-names(which(fit$glmnet.fit$beta[,which(fit$lambda==fit$lambda.min)]!=0))
  VARSelect<-as.numeric(gsub("V","",VARSelect))
  data[,VARSelect]
}
Store(DATAG.0)

library(Matrix)

DATAG1_t.E1<-DATAG.0(bin_mat_t.E1,GNET1R.E1[[1]][[6]]) ; Store(DATAG1_t.E1)
DATAG1_t.E2<-DATAG.0(bin_mat_t.E2,GNET1R.E2[[1]][[6]]) ; Store(DATAG1_t.E2)
DATAG1_t.E3<-DATAG.0(bin_mat_t.E3,GNET1R.E3[[1]][[6]]) ; Store(DATAG1_t.E3)
DATAG1_t.E4<-DATAG.0(bin_mat_t.E4,GNET1R.E4[[1]][[6]]) ; Store(DATAG1_t.E4)
DATAG1_t.E5<-DATAG.0(bin_mat_t.E5,GNET1R.E5[[1]][[6]]) ; Store(DATAG1_t.E5)
DATAG1_t.E6<-DATAG.0(bin_mat_t.E6,GNET1R.E6[[1]][[6]]) ; Store(DATAG1_t.E6)
DATAG1_t.E7<-DATAG.0(bin_mat_t.E7,GNET1R.E7[[1]][[6]]) ; Store(DATAG1_t.E7)
DATAG1_t.E8<-DATAG.0(bin_mat_t.E8,GNET1R.E8[[1]][[6]]) ; Store(DATAG1_t.E8)
DATAG1_t.E9<-DATAG.0(bin_mat_t.E9,GNET1R.E9[[1]][[6]]) ; Store(DATAG1_t.E9)
DATAG1_t.E10<-DATAG.0(bin_mat_t.E10,GNET1R.E10[[1]][[6]]) ; Store(DATAG1_t.E10)

#####################################################################
# combine proxiess with reduced term matrix
#####################################################################

COMBGP.0<-function(datag,proxies) data.frame(proxies,datag) ; Store(COMBGP.0)

comb<-10:21
names(test)[comb]
COMBGP1_t.E1<-COMBGP.0(DATAG1_t.E1,test[test$EssaySet==1,comb]) ; Store(COMBGP1_t.E1)
COMBGP1_t.E2<-COMBGP.0(DATAG1_t.E2,test[test$EssaySet==2,comb]) ; Store(COMBGP1_t.E2)
COMBGP1_t.E3<-COMBGP.0(DATAG1_t.E3,test[test$EssaySet==3,comb]) ; Store(COMBGP1_t.E3)
COMBGP1_t.E4<-COMBGP.0(DATAG1_t.E4,test[test$EssaySet==4,comb]) ; Store(COMBGP1_t.E4)
COMBGP1_t.E5<-COMBGP.0(DATAG1_t.E5,test[test$EssaySet==5,comb]) ; Store(COMBGP1_t.E5)
COMBGP1_t.E6<-COMBGP.0(DATAG1_t.E6,test[test$EssaySet==6,comb]) ; Store(COMBGP1_t.E6)
COMBGP1_t.E7<-COMBGP.0(DATAG1_t.E7,test[test$EssaySet==7,comb]) ; Store(COMBGP1_t.E7)
COMBGP1_t.E8<-COMBGP.0(DATAG1_t.E8,test[test$EssaySet==8,comb]) ; Store(COMBGP1_t.E8)
COMBGP1_t.E9<-COMBGP.0(DATAG1_t.E9,test[test$EssaySet==9,comb]) ; Store(COMBGP1_t.E9)
COMBGP1_t.E10<-COMBGP.0(DATAG1_t.E10,test[test$EssaySet==10,comb]) ; Store(COMBGP1_t.E10)

#####################################################################
#####################################################################
# Predict RF on reduced matrix with proxies
#####################################################################
#####################################################################

library(randomForest)
SEL_RF1_t.E1<-predict(SEL_RF1.E1[[1]][[6]],COMBGP1_t.E1) ; Store(SEL_RF1_t.E1)
SEL_RF1_t.E2<-predict(SEL_RF1.E2[[1]][[6]],COMBGP1_t.E2) ; Store(SEL_RF1_t.E2)
SEL_RF1_t.E3<-predict(SEL_RF1.E3[[1]][[6]],COMBGP1_t.E3) ; Store(SEL_RF1_t.E3)
SEL_RF1_t.E4<-predict(SEL_RF1.E4[[1]][[6]],COMBGP1_t.E4) ; Store(SEL_RF1_t.E4)
SEL_RF1_t.E5<-predict(SEL_RF1.E5[[1]][[6]],COMBGP1_t.E5) ; Store(SEL_RF1_t.E5)
SEL_RF1_t.E6<-predict(SEL_RF1.E6[[1]][[6]],COMBGP1_t.E6) ; Store(SEL_RF1_t.E6)
SEL_RF1_t.E7<-predict(SEL_RF1.E7[[1]][[6]],COMBGP1_t.E7) ; Store(SEL_RF1_t.E7)
SEL_RF1_t.E8<-predict(SEL_RF1.E8[[1]][[6]],COMBGP1_t.E8) ; Store(SEL_RF1_t.E8)
SEL_RF1_t.E9<-predict(SEL_RF1.E9[[1]][[6]],COMBGP1_t.E9) ; Store(SEL_RF1_t.E9)
SEL_RF1_t.E10<-predict(SEL_RF1.E10[[1]][[6]],COMBGP1_t.E10) ; Store(SEL_RF1_t.E10)

SEL_RF1_t<-Group_t(list(SEL_RF1_t.E1,SEL_RF1_t.E2,SEL_RF1_t.E3,SEL_RF1_t.E4,SEL_RF1_t.E5,
                        SEL_RF1_t.E6,SEL_RF1_t.E7,SEL_RF1_t.E8,SEL_RF1_t.E9,SEL_RF1_t.E10)) ; Store(SEL_RF1_t)

#to check if ok
graph3(SEL_RF1_t,SEL_RF1)

#####################################################################
#####################################################################
# Combine proxies with 2nd set of bin_mat
#####################################################################
#####################################################################

comb<-10:21
names(test)[comb]
COMBGP1.2_t.E1<-COMBGP.0(bin_mat.2_t.E1,test[test$EssaySet==1,comb]) ; Store(COMBGP1.2_t.E1)
COMBGP1.2_t.E2<-COMBGP.0(bin_mat.2_t.E2,test[test$EssaySet==2,comb]) ; Store(COMBGP1.2_t.E2)
COMBGP1.2_t.E3<-COMBGP.0(bin_mat.2_t.E3,test[test$EssaySet==3,comb]) ; Store(COMBGP1.2_t.E3)
COMBGP1.2_t.E4<-COMBGP.0(bin_mat.2_t.E4,test[test$EssaySet==4,comb]) ; Store(COMBGP1.2_t.E4)
COMBGP1.2_t.E5<-COMBGP.0(bin_mat.2_t.E5,test[test$EssaySet==5,comb]) ; Store(COMBGP1.2_t.E5)
COMBGP1.2_t.E6<-COMBGP.0(bin_mat.2_t.E6,test[test$EssaySet==6,comb]) ; Store(COMBGP1.2_t.E6)
COMBGP1.2_t.E7<-COMBGP.0(bin_mat.2_t.E7,test[test$EssaySet==7,comb]) ; Store(COMBGP1.2_t.E7)
COMBGP1.2_t.E8<-COMBGP.0(bin_mat.2_t.E8,test[test$EssaySet==8,comb]) ; Store(COMBGP1.2_t.E8)
COMBGP1.2_t.E9<-COMBGP.0(bin_mat.2_t.E9,test[test$EssaySet==9,comb]) ; Store(COMBGP1.2_t.E9)
COMBGP1.2_t.E10<-COMBGP.0(bin_mat.2_t.E10,test[test$EssaySet==10,comb]) ; Store(COMBGP1.2_t.E10)

#####################################################################
#####################################################################
# Train RF on 2nd set of bin_mat with proxies
#####################################################################
#####################################################################

library(randomForest)
SEL_RF1.2_t.E1<-predict(SEL_RF1.2.E1[[1]][[6]],COMBGP1.2_t.E1) ; Store(SEL_RF1.2_t.E1)
SEL_RF1.2_t.E2<-predict(SEL_RF1.2.E2[[1]][[6]],COMBGP1.2_t.E2) ; Store(SEL_RF1.2_t.E2)
SEL_RF1.2_t.E3<-predict(SEL_RF1.2.E3[[1]][[6]],COMBGP1.2_t.E3) ; Store(SEL_RF1.2_t.E3)
SEL_RF1.2_t.E4<-predict(SEL_RF1.2.E4[[1]][[6]],COMBGP1.2_t.E4) ; Store(SEL_RF1.2_t.E4)
SEL_RF1.2_t.E5<-predict(SEL_RF1.2.E5[[1]][[6]],COMBGP1.2_t.E5) ; Store(SEL_RF1.2_t.E5)
SEL_RF1.2_t.E6<-predict(SEL_RF1.2.E6[[1]][[6]],COMBGP1.2_t.E6) ; Store(SEL_RF1.2_t.E6)
SEL_RF1.2_t.E7<-predict(SEL_RF1.2.E7[[1]][[6]],COMBGP1.2_t.E7) ; Store(SEL_RF1.2_t.E7)
SEL_RF1.2_t.E8<-predict(SEL_RF1.2.E8[[1]][[6]],COMBGP1.2_t.E8) ; Store(SEL_RF1.2_t.E8)
SEL_RF1.2_t.E9<-predict(SEL_RF1.2.E9[[1]][[6]],COMBGP1.2_t.E9) ; Store(SEL_RF1.2_t.E9)
SEL_RF1.2_t.E10<-predict(SEL_RF1.2.E10[[1]][[6]],COMBGP1.2_t.E10) ; Store(SEL_RF1.2_t.E10)

SEL_RF1.2_t<-Group_t(list(SEL_RF1.2_t.E1,SEL_RF1.2_t.E2,SEL_RF1.2_t.E3,SEL_RF1.2_t.E4,SEL_RF1.2_t.E5,
                          SEL_RF1.2_t.E6,SEL_RF1.2_t.E7,SEL_RF1.2_t.E8,SEL_RF1.2_t.E9,SEL_RF1.2_t.E10)) ; Store(SEL_RF1.2_t)

#to check if ok
graph3(SEL_RF1.2_t,SEL_RF1.2)

#####################################################################
#####################################################################
# Compute simple averages
#####################################################################
#####################################################################

SEL_RF1.MIX_t<-(SEL_RF1_t+SEL_RF1.2_t)/2 ; Store(SEL_RF1.MIX_t)
GNET1R.MIX_t<-(GNET1R_t+GNET1R.2_t)/2 ; Store(GNET1R.MIX_t)
SVM3.MIX_t<-(SVM3_t+SVM3.2_t+SVM3.IB_t+SVM3.IB.2_t)/4 ; Store(SVM3.MIX_t)
RAD2.MIX_t<-(RAD2_t+RAD2.2_t+RAD2.IB_t+RAD2.IB.2_t)/4 ; Store(RAD2.MIX_t)
SVMc3.MIX_t<-(SVMc3b_t+SVMc3.2_t-SVMc3.IB_t-SVMc3b.IB.2_t)/4 ; Store(SVMc3.MIX_t)
RADc2.MIX_t<-(RADc2_t+RADc2.2_t-RADc2.IB_t-RADc2.IB.2_t)/4 ; Store(RADc2.MIX_t)
SVMm3.MIX_t<-(SVMm3_t+SVMm3.2_t+SVMm3.IB_t+SVMm3.IB.2_t)/4 ; Store(SVMm3.MIX_t)
RADm2.MIX_t<-(RADm2_t+RADm2.2_t+RADm2.IB_t+RADm2.IB.2_t)/4 ; Store(RADm2.MIX_t)
GNETns1.MIX_t<-(GNETns1_t+GNETns1.2_t+GNETns1.IB_t+GNETns1.IB.2_t)/4 ; Store(GNETns1.MIX_t)
GNETns3.MIX_t<-(GNETns3_t+GNETns3.2_t+GNETns3.IB_t+GNETns3.IB.2_t)/4 ; Store(GNETns3.MIX_t)
GNETcr3.MIX_t<-(GNETcr3_t+GNETcr3.2_t+GNETcr3.IB_t+GNETcr3.IB.2_t)/4 ; Store(GNETcr3.MIX_t)

# to check if ok
graph3(SEL_RF1.MIX_t,SEL_RF1.MIX)
graph3(GNET1R.MIX_t,GNET1R.MIX)
graph3(SVM3.MIX_t,SVM3.MIX)
graph3(RAD2.MIX_t,RAD2.MIX)
graph3(SVMc3.MIX_t,SVMc3.MIX)
graph3(RADc2.MIX_t,RADc2.MIX)
graph3(SVMm3.MIX_t,SVMm3.MIX)
graph3(RADm2.MIX_t,RADm2.MIX)
graph3(GNETns1.MIX_t,GNETns1.MIX)
graph3(GNETns3.MIX_t,GNETns3.MIX)
graph3(GNETcr3.MIX_t,GNETcr3.MIX)

#####################################################################
#####################################################################
#####################################################################
# Same but with models trained with score2
#####################################################################
#####################################################################
#####################################################################

#####################################################################
#####################################################################
# predict S2.GNET (standardize=FALSE) on 
# 2 sets of bin_mat after Ridit transformation
#####################################################################
#####################################################################

# 1rst set of bin_mat (transformed with ridit coding) 
#####################################################################

library(glmnet)

S2.GNET1R_t.E1<-predict(S2.GNET1R.E1[[1]][[6]],Ridit_mat_t.E1,type="response", s="lambda.min")[,1]
S2.GNET1R_t.E2<-predict(S2.GNET1R.E2[[1]][[6]],Ridit_mat_t.E2,type="response", s="lambda.min")[,1]
S2.GNET1R_t.E3<-predict(S2.GNET1R.E3[[1]][[6]],Ridit_mat_t.E3,type="response", s="lambda.min")[,1]
S2.GNET1R_t.E4<-predict(S2.GNET1R.E4[[1]][[6]],Ridit_mat_t.E4,type="response", s="lambda.min")[,1]
S2.GNET1R_t.E5<-predict(S2.GNET1R.E5[[1]][[6]],Ridit_mat_t.E5,type="response", s="lambda.min")[,1]
S2.GNET1R_t.E6<-predict(S2.GNET1R.E6[[1]][[6]],Ridit_mat_t.E6,type="response", s="lambda.min")[,1]
S2.GNET1R_t.E7<-predict(S2.GNET1R.E7[[1]][[6]],Ridit_mat_t.E7,type="response", s="lambda.min")[,1]
S2.GNET1R_t.E8<-predict(S2.GNET1R.E8[[1]][[6]],Ridit_mat_t.E8,type="response", s="lambda.min")[,1]
S2.GNET1R_t.E9<-predict(S2.GNET1R.E9[[1]][[6]],Ridit_mat_t.E9,type="response", s="lambda.min")[,1]
S2.GNET1R_t.E10<-predict(S2.GNET1R.E10[[1]][[6]],Ridit_mat_t.E10,type="response", s="lambda.min")[,1]

S2.GNET1R_t<-Group_t(list(S2.GNET1R_t.E1,S2.GNET1R_t.E2,S2.GNET1R_t.E3,S2.GNET1R_t.E4,S2.GNET1R_t.E5,
                          S2.GNET1R_t.E6,S2.GNET1R_t.E7,S2.GNET1R_t.E8,S2.GNET1R_t.E9,S2.GNET1R_t.E10)) ; Store(S2.GNET1R_t)

#to check if ok
graph3(S2.GNET1R_t,S2.GNET1R)

# 2nd set of bin_mat (transformed with ridit coding) 
#####################################################################

S2.GNET1R.2_t.E1<-predict(S2.GNET1R.2.E1[[1]][[6]],Ridit_mat.2_t.E1,type="response", s="lambda.min")[,1]
S2.GNET1R.2_t.E2<-predict(S2.GNET1R.2.E2[[1]][[6]],Ridit_mat.2_t.E2,type="response", s="lambda.min")[,1]
S2.GNET1R.2_t.E3<-predict(S2.GNET1R.2.E3[[1]][[6]],Ridit_mat.2_t.E3,type="response", s="lambda.min")[,1]
S2.GNET1R.2_t.E4<-predict(S2.GNET1R.2.E4[[1]][[6]],Ridit_mat.2_t.E4,type="response", s="lambda.min")[,1]
S2.GNET1R.2_t.E5<-predict(S2.GNET1R.2.E5[[1]][[6]],Ridit_mat.2_t.E5,type="response", s="lambda.min")[,1]
S2.GNET1R.2_t.E6<-predict(S2.GNET1R.2.E6[[1]][[6]],Ridit_mat.2_t.E6,type="response", s="lambda.min")[,1]
S2.GNET1R.2_t.E7<-predict(S2.GNET1R.2.E7[[1]][[6]],Ridit_mat.2_t.E7,type="response", s="lambda.min")[,1]
S2.GNET1R.2_t.E8<-predict(S2.GNET1R.2.E8[[1]][[6]],Ridit_mat.2_t.E8,type="response", s="lambda.min")[,1]
S2.GNET1R.2_t.E9<-predict(S2.GNET1R.2.E9[[1]][[6]],Ridit_mat.2_t.E9,type="response", s="lambda.min")[,1]
S2.GNET1R.2_t.E10<-predict(S2.GNET1R.2.E10[[1]][[6]],Ridit_mat.2_t.E10,type="response", s="lambda.min")[,1]

S2.GNET1R.2_t<-Group_t(list(S2.GNET1R.2_t.E1,S2.GNET1R.2_t.E2,S2.GNET1R.2_t.E3,S2.GNET1R.2_t.E4,S2.GNET1R.2_t.E5,
                            S2.GNET1R.2_t.E6,S2.GNET1R.2_t.E7,S2.GNET1R.2_t.E8,S2.GNET1R.2_t.E9,S2.GNET1R.2_t.E10)) ; Store(S2.GNET1R.2_t)

#to check if ok
graph3(S2.GNET1R.2_t,S2.GNET1R.2)

#####################################################################
#####################################################################
# PREDICT S2.SVM
# on 2 sets of bin mat
# scaled with bns using "isgood" or "isbadd"
# with linear and radial kernels
# regression, multinomial and binary classification
#####################################################################
#####################################################################

#####################################################################
# predict S2.SVM on incidence matrix scaled with bns
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

S2.SVM3_t.E1<-predict(S2.SVM3.E1[[1]][[6]],scal_mat3_t.E1) ; Store(S2.SVM3_t.E1)
S2.SVM3_t.E2<-predict(S2.SVM3.E2[[1]][[6]],scal_mat3_t.E2) ; Store(S2.SVM3_t.E2)
S2.SVM3_t.E3<-predict(S2.SVM3.E3[[1]][[6]],scal_mat3_t.E3) ; Store(S2.SVM3_t.E3)
S2.SVM3_t.E4<-predict(S2.SVM3.E4[[1]][[6]],scal_mat3_t.E4) ; Store(S2.SVM3_t.E4)
S2.SVM3_t.E5<-predict(S2.SVM3.E5[[1]][[6]],scal_mat3_t.E5) ; Store(S2.SVM3_t.E5)
S2.SVM3_t.E6<-predict(S2.SVM3.E6[[1]][[6]],scal_mat3_t.E6) ; Store(S2.SVM3_t.E6)
S2.SVM3_t.E7<-predict(S2.SVM3.E7[[1]][[6]],scal_mat3_t.E7) ; Store(S2.SVM3_t.E7)
S2.SVM3_t.E8<-predict(S2.SVM3.E8[[1]][[6]],scal_mat3_t.E8) ; Store(S2.SVM3_t.E8)
S2.SVM3_t.E9<-predict(S2.SVM3.E9[[1]][[6]],scal_mat3_t.E9) ; Store(S2.SVM3_t.E9)
S2.SVM3_t.E10<-predict(S2.SVM3.E10[[1]][[6]],scal_mat3_t.E10) ; Store(S2.SVM3_t.E10)

S2.SVM3_t<-Group_t(list(S2.SVM3_t.E1,S2.SVM3_t.E2,S2.SVM3_t.E3,S2.SVM3_t.E4,S2.SVM3_t.E5,
                        S2.SVM3_t.E6,S2.SVM3_t.E7,S2.SVM3_t.E8,S2.SVM3_t.E9,S2.SVM3_t.E10)) ; Store(S2.SVM3_t)

#to check if ok
graph3(S2.SVM3_t,S2.SVM3)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.SVM3.IB_t.E1<-predict(S2.SVM3.IB.E1[[1]][[6]],scal.IB_mat3_t.E1) ; Store(S2.SVM3.IB_t.E1)
S2.SVM3.IB_t.E2<-predict(S2.SVM3.IB.E2[[1]][[6]],scal.IB_mat3_t.E2) ; Store(S2.SVM3.IB_t.E2)
S2.SVM3.IB_t.E3<-predict(S2.SVM3.IB.E3[[1]][[6]],scal.IB_mat3_t.E3) ; Store(S2.SVM3.IB_t.E3)
S2.SVM3.IB_t.E4<-predict(S2.SVM3.IB.E4[[1]][[6]],scal.IB_mat3_t.E4) ; Store(S2.SVM3.IB_t.E4)
S2.SVM3.IB_t.E5<-predict(S2.SVM3.IB.E5[[1]][[6]],scal.IB_mat3_t.E5) ; Store(S2.SVM3.IB_t.E5)
S2.SVM3.IB_t.E6<-predict(S2.SVM3.IB.E6[[1]][[6]],scal.IB_mat3_t.E6) ; Store(S2.SVM3.IB_t.E6)
S2.SVM3.IB_t.E7<-predict(S2.SVM3.IB.E7[[1]][[6]],scal.IB_mat3_t.E7) ; Store(S2.SVM3.IB_t.E7)
S2.SVM3.IB_t.E8<-predict(S2.SVM3.IB.E8[[1]][[6]],scal.IB_mat3_t.E8) ; Store(S2.SVM3.IB_t.E8)
S2.SVM3.IB_t.E9<-predict(S2.SVM3.IB.E9[[1]][[6]],scal.IB_mat3_t.E9) ; Store(S2.SVM3.IB_t.E9)
S2.SVM3.IB_t.E10<-predict(S2.SVM3.IB.E10[[1]][[6]],scal.IB_mat3_t.E10) ; Store(S2.SVM3.IB_t.E10)

S2.SVM3.IB_t<-Group_t(list(S2.SVM3.IB_t.E1,S2.SVM3.IB_t.E2,S2.SVM3.IB_t.E3,S2.SVM3.IB_t.E4,S2.SVM3.IB_t.E5,
                           S2.SVM3.IB_t.E6,S2.SVM3.IB_t.E7,S2.SVM3.IB_t.E8,S2.SVM3.IB_t.E9,S2.SVM3.IB_t.E10)) ; Store(S2.SVM3.IB_t)

#to check if ok
graph3(S2.SVM3.IB_t,S2.SVM3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.SVM3.2_t.E1<-predict(S2.SVM3.2.E1[[1]][[6]],scal_mat3.2_t.E1) ; Store(S2.SVM3.2_t.E1)
S2.SVM3.2_t.E2<-predict(S2.SVM3.2.E2[[1]][[6]],scal_mat3.2_t.E2) ; Store(S2.SVM3.2_t.E2)
S2.SVM3.2_t.E3<-predict(S2.SVM3.2.E3[[1]][[6]],scal_mat3.2_t.E3) ; Store(S2.SVM3.2_t.E3)
S2.SVM3.2_t.E4<-predict(S2.SVM3.2.E4[[1]][[6]],scal_mat3.2_t.E4) ; Store(S2.SVM3.2_t.E4)
S2.SVM3.2_t.E5<-predict(S2.SVM3.2.E5[[1]][[6]],scal_mat3.2_t.E5) ; Store(S2.SVM3.2_t.E5)
S2.SVM3.2_t.E6<-predict(S2.SVM3.2.E6[[1]][[6]],scal_mat3.2_t.E6) ; Store(S2.SVM3.2_t.E6)
S2.SVM3.2_t.E7<-predict(S2.SVM3.2.E7[[1]][[6]],scal_mat3.2_t.E7) ; Store(S2.SVM3.2_t.E7)
S2.SVM3.2_t.E8<-predict(S2.SVM3.2.E8[[1]][[6]],scal_mat3.2_t.E8) ; Store(S2.SVM3.2_t.E8)
S2.SVM3.2_t.E9<-predict(S2.SVM3.2.E9[[1]][[6]],scal_mat3.2_t.E9) ; Store(S2.SVM3.2_t.E9)
S2.SVM3.2_t.E10<-predict(S2.SVM3.2.E10[[1]][[6]],scal_mat3.2_t.E10) ; Store(S2.SVM3.2_t.E10)

S2.SVM3.2_t<-Group_t(list(S2.SVM3.2_t.E1,S2.SVM3.2_t.E2,S2.SVM3.2_t.E3,S2.SVM3.2_t.E4,S2.SVM3.2_t.E5,
                          S2.SVM3.2_t.E6,S2.SVM3.2_t.E7,S2.SVM3.2_t.E8,S2.SVM3.2_t.E9,S2.SVM3.2_t.E10)) ; Store(S2.SVM3.2_t)

#to check if ok
graph3(S2.SVM3.2_t,S2.SVM3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.SVM3.IB.2_t.E1<-predict(S2.SVM3.IB.2.E1[[1]][[6]],scal.IB_mat3.2_t.E1) ; Store(S2.SVM3.IB.2_t.E1)
S2.SVM3.IB.2_t.E2<-predict(S2.SVM3.IB.2.E2[[1]][[6]],scal.IB_mat3.2_t.E2) ; Store(S2.SVM3.IB.2_t.E2)
S2.SVM3.IB.2_t.E3<-predict(S2.SVM3.IB.2.E3[[1]][[6]],scal.IB_mat3.2_t.E3) ; Store(S2.SVM3.IB.2_t.E3)
S2.SVM3.IB.2_t.E4<-predict(S2.SVM3.IB.2.E4[[1]][[6]],scal.IB_mat3.2_t.E4) ; Store(S2.SVM3.IB.2_t.E4)
S2.SVM3.IB.2_t.E5<-predict(S2.SVM3.IB.2.E5[[1]][[6]],scal.IB_mat3.2_t.E5) ; Store(S2.SVM3.IB.2_t.E5)
S2.SVM3.IB.2_t.E6<-predict(S2.SVM3.IB.2.E6[[1]][[6]],scal.IB_mat3.2_t.E6) ; Store(S2.SVM3.IB.2_t.E6)
S2.SVM3.IB.2_t.E7<-predict(S2.SVM3.IB.2.E7[[1]][[6]],scal.IB_mat3.2_t.E7) ; Store(S2.SVM3.IB.2_t.E7)
S2.SVM3.IB.2_t.E8<-predict(S2.SVM3.IB.2.E8[[1]][[6]],scal.IB_mat3.2_t.E8) ; Store(S2.SVM3.IB.2_t.E8)
S2.SVM3.IB.2_t.E9<-predict(S2.SVM3.IB.2.E9[[1]][[6]],scal.IB_mat3.2_t.E9) ; Store(S2.SVM3.IB.2_t.E9)
S2.SVM3.IB.2_t.E10<-predict(S2.SVM3.IB.2.E10[[1]][[6]],scal.IB_mat3.2_t.E10) ; Store(S2.SVM3.IB.2_t.E10)

S2.SVM3.IB.2_t<-Group_t(list(S2.SVM3.IB.2_t.E1,S2.SVM3.IB.2_t.E2,S2.SVM3.IB.2_t.E3,S2.SVM3.IB.2_t.E4,S2.SVM3.IB.2_t.E5,
                             S2.SVM3.IB.2_t.E6,S2.SVM3.IB.2_t.E7,S2.SVM3.IB.2_t.E8,S2.SVM3.IB.2_t.E9,S2.SVM3.IB.2_t.E10)) ; Store(S2.SVM3.IB.2_t)

#to check if ok
graph3(S2.SVM3.IB.2_t,S2.SVM3.IB.2)

#####################################################################
# PREDICT S2.SVM with radial kernel on scaled matrix
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

S2.RAD2_t.E1<-predict(S2.RAD2.E1[[1]][[6]],scal_mat3_t.E1) ; Store(S2.RAD2_t.E1)
S2.RAD2_t.E2<-predict(S2.RAD2.E2[[1]][[6]],scal_mat3_t.E2) ; Store(S2.RAD2_t.E2)
S2.RAD2_t.E3<-predict(S2.RAD2.E3[[1]][[6]],scal_mat3_t.E3) ; Store(S2.RAD2_t.E3)
S2.RAD2_t.E4<-predict(S2.RAD2.E4[[1]][[6]],scal_mat3_t.E4) ; Store(S2.RAD2_t.E4)
S2.RAD2_t.E5<-predict(S2.RAD2.E5[[1]][[6]],scal_mat3_t.E5) ; Store(S2.RAD2_t.E5)
S2.RAD2_t.E6<-predict(S2.RAD2.E6[[1]][[6]],scal_mat3_t.E6) ; Store(S2.RAD2_t.E6)
S2.RAD2_t.E7<-predict(S2.RAD2.E7[[1]][[6]],scal_mat3_t.E7) ; Store(S2.RAD2_t.E7)
S2.RAD2_t.E8<-predict(S2.RAD2.E8[[1]][[6]],scal_mat3_t.E8) ; Store(S2.RAD2_t.E8)
S2.RAD2_t.E9<-predict(S2.RAD2.E9[[1]][[6]],scal_mat3_t.E9) ; Store(S2.RAD2_t.E9)
S2.RAD2_t.E10<-predict(S2.RAD2.E10[[1]][[6]],scal_mat3_t.E10) ; Store(S2.RAD2_t.E10)

S2.RAD2_t<-Group_t(list(S2.RAD2_t.E1,S2.RAD2_t.E2,S2.RAD2_t.E3,S2.RAD2_t.E4,S2.RAD2_t.E5,
                        S2.RAD2_t.E6,S2.RAD2_t.E7,S2.RAD2_t.E8,S2.RAD2_t.E9,S2.RAD2_t.E10)) ; Store(S2.RAD2_t)

#to check if ok
graph3(S2.RAD2_t,S2.RAD2)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.RAD2.IB_t.E1<-predict(S2.RAD2.IB.E1[[1]][[6]],scal.IB_mat3_t.E1) ; Store(S2.RAD2.IB_t.E1)
S2.RAD2.IB_t.E2<-predict(S2.RAD2.IB.E2[[1]][[6]],scal.IB_mat3_t.E2) ; Store(S2.RAD2.IB_t.E2)
S2.RAD2.IB_t.E3<-predict(S2.RAD2.IB.E3[[1]][[6]],scal.IB_mat3_t.E3) ; Store(S2.RAD2.IB_t.E3)
S2.RAD2.IB_t.E4<-predict(S2.RAD2.IB.E4[[1]][[6]],scal.IB_mat3_t.E4) ; Store(S2.RAD2.IB_t.E4)
S2.RAD2.IB_t.E5<-predict(S2.RAD2.IB.E5[[1]][[6]],scal.IB_mat3_t.E5) ; Store(S2.RAD2.IB_t.E5)
S2.RAD2.IB_t.E6<-predict(S2.RAD2.IB.E6[[1]][[6]],scal.IB_mat3_t.E6) ; Store(S2.RAD2.IB_t.E6)
S2.RAD2.IB_t.E7<-predict(S2.RAD2.IB.E7[[1]][[6]],scal.IB_mat3_t.E7) ; Store(S2.RAD2.IB_t.E7)
S2.RAD2.IB_t.E8<-predict(S2.RAD2.IB.E8[[1]][[6]],scal.IB_mat3_t.E8) ; Store(S2.RAD2.IB_t.E8)
S2.RAD2.IB_t.E9<-predict(S2.RAD2.IB.E9[[1]][[6]],scal.IB_mat3_t.E9) ; Store(S2.RAD2.IB_t.E9)
S2.RAD2.IB_t.E10<-predict(S2.RAD2.IB.E10[[1]][[6]],scal.IB_mat3_t.E10) ; Store(S2.RAD2.IB_t.E10)

S2.RAD2.IB_t<-Group_t(list(S2.RAD2.IB_t.E1,S2.RAD2.IB_t.E2,S2.RAD2.IB_t.E3,S2.RAD2.IB_t.E4,S2.RAD2.IB_t.E5,
                           S2.RAD2.IB_t.E6,S2.RAD2.IB_t.E7,S2.RAD2.IB_t.E8,S2.RAD2.IB_t.E9,S2.RAD2.IB_t.E10)) ; Store(S2.RAD2.IB_t)

#to check if ok
graph3(S2.RAD2.IB_t,S2.RAD2.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.RAD2.2_t.E1<-predict(S2.RAD2.2.E1[[1]][[6]],scal_mat3.2_t.E1) ; Store(S2.RAD2.2_t.E1)
S2.RAD2.2_t.E2<-predict(S2.RAD2.2.E2[[1]][[6]],scal_mat3.2_t.E2) ; Store(S2.RAD2.2_t.E2)
S2.RAD2.2_t.E3<-predict(S2.RAD2.2.E3[[1]][[6]],scal_mat3.2_t.E3) ; Store(S2.RAD2.2_t.E3)
S2.RAD2.2_t.E4<-predict(S2.RAD2.2.E4[[1]][[6]],scal_mat3.2_t.E4) ; Store(S2.RAD2.2_t.E4)
S2.RAD2.2_t.E5<-predict(S2.RAD2.2.E5[[1]][[6]],scal_mat3.2_t.E5) ; Store(S2.RAD2.2_t.E5)
S2.RAD2.2_t.E6<-predict(S2.RAD2.2.E6[[1]][[6]],scal_mat3.2_t.E6) ; Store(S2.RAD2.2_t.E6)
S2.RAD2.2_t.E7<-predict(S2.RAD2.2.E7[[1]][[6]],scal_mat3.2_t.E7) ; Store(S2.RAD2.2_t.E7)
S2.RAD2.2_t.E8<-predict(S2.RAD2.2.E8[[1]][[6]],scal_mat3.2_t.E8) ; Store(S2.RAD2.2_t.E8)
S2.RAD2.2_t.E9<-predict(S2.RAD2.2.E9[[1]][[6]],scal_mat3.2_t.E9) ; Store(S2.RAD2.2_t.E9)
S2.RAD2.2_t.E10<-predict(S2.RAD2.2.E10[[1]][[6]],scal_mat3.2_t.E10) ; Store(S2.RAD2.2_t.E10)

S2.RAD2.2_t<-Group_t(list(S2.RAD2.2_t.E1,S2.RAD2.2_t.E2,S2.RAD2.2_t.E3,S2.RAD2.2_t.E4,S2.RAD2.2_t.E5,
                          S2.RAD2.2_t.E6,S2.RAD2.2_t.E7,S2.RAD2.2_t.E8,S2.RAD2.2_t.E9,S2.RAD2.2_t.E10)) ; Store(S2.RAD2.2_t)

#to check if ok
graph3(S2.RAD2.2_t,S2.RAD2.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.RAD2.IB.2_t.E1<-predict(S2.RAD2.IB.2.E1[[1]][[6]],scal.IB_mat3.2_t.E1) ; Store(S2.RAD2.IB.2_t.E1)
S2.RAD2.IB.2_t.E2<-predict(S2.RAD2.IB.2.E2[[1]][[6]],scal.IB_mat3.2_t.E2) ; Store(S2.RAD2.IB.2_t.E2)
S2.RAD2.IB.2_t.E3<-predict(S2.RAD2.IB.2.E3[[1]][[6]],scal.IB_mat3.2_t.E3) ; Store(S2.RAD2.IB.2_t.E3)
S2.RAD2.IB.2_t.E4<-predict(S2.RAD2.IB.2.E4[[1]][[6]],scal.IB_mat3.2_t.E4) ; Store(S2.RAD2.IB.2_t.E4)
S2.RAD2.IB.2_t.E5<-predict(S2.RAD2.IB.2.E5[[1]][[6]],scal.IB_mat3.2_t.E5) ; Store(S2.RAD2.IB.2_t.E5)
S2.RAD2.IB.2_t.E6<-predict(S2.RAD2.IB.2.E6[[1]][[6]],scal.IB_mat3.2_t.E6) ; Store(S2.RAD2.IB.2_t.E6)
S2.RAD2.IB.2_t.E7<-predict(S2.RAD2.IB.2.E7[[1]][[6]],scal.IB_mat3.2_t.E7) ; Store(S2.RAD2.IB.2_t.E7)
S2.RAD2.IB.2_t.E8<-predict(S2.RAD2.IB.2.E8[[1]][[6]],scal.IB_mat3.2_t.E8) ; Store(S2.RAD2.IB.2_t.E8)
S2.RAD2.IB.2_t.E9<-predict(S2.RAD2.IB.2.E9[[1]][[6]],scal.IB_mat3.2_t.E9) ; Store(S2.RAD2.IB.2_t.E9)
S2.RAD2.IB.2_t.E10<-predict(S2.RAD2.IB.2.E10[[1]][[6]],scal.IB_mat3.2_t.E10) ; Store(S2.RAD2.IB.2_t.E10)

S2.RAD2.IB.2_t<-Group_t(list(S2.RAD2.IB.2_t.E1,S2.RAD2.IB.2_t.E2,S2.RAD2.IB.2_t.E3,S2.RAD2.IB.2_t.E4,S2.RAD2.IB.2_t.E5,
                             S2.RAD2.IB.2_t.E6,S2.RAD2.IB.2_t.E7,S2.RAD2.IB.2_t.E8,S2.RAD2.IB.2_t.E9,S2.RAD2.IB.2_t.E10)) ; Store(S2.RAD2.IB.2_t)

#to check if ok
graph3(S2.RAD2.IB.2_t,S2.RAD2.IB.2)

#####################################################################
# PREDICT S2.SVM to model binary response good/poor essays
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

S2.SVMc3b_t.E1<-attr(predict(S2.SVMc3b.E1[[1]][[6]],scal_mat3_t.E1,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b_t.E1)
S2.SVMc3b_t.E2<-attr(predict(S2.SVMc3b.E2[[1]][[6]],scal_mat3_t.E2,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b_t.E2)
S2.SVMc3b_t.E3<-attr(predict(S2.SVMc3b.E3[[1]][[6]],scal_mat3_t.E3,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b_t.E3)
S2.SVMc3b_t.E4<-attr(predict(S2.SVMc3b.E4[[1]][[6]],scal_mat3_t.E4,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b_t.E4)
S2.SVMc3b_t.E5<-attr(predict(S2.SVMc3b.E5[[1]][[6]],scal_mat3_t.E5,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b_t.E5)
S2.SVMc3b_t.E6<-attr(predict(S2.SVMc3b.E6[[1]][[6]],scal_mat3_t.E6,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b_t.E6)
S2.SVMc3b_t.E7<-attr(predict(S2.SVMc3b.E7[[1]][[6]],scal_mat3_t.E7,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b_t.E7)
S2.SVMc3b_t.E8<-attr(predict(S2.SVMc3b.E8[[1]][[6]],scal_mat3_t.E8,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b_t.E8)
S2.SVMc3b_t.E9<-attr(predict(S2.SVMc3b.E9[[1]][[6]],scal_mat3_t.E9,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b_t.E9)
S2.SVMc3b_t.E10<-attr(predict(S2.SVMc3b.E10[[1]][[6]],scal_mat3_t.E10,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b_t.E10)

S2.SVMc3b_t<-Group_t(list(S2.SVMc3b_t.E1,S2.SVMc3b_t.E2,S2.SVMc3b_t.E3,S2.SVMc3b_t.E4,S2.SVMc3b_t.E5,
                          S2.SVMc3b_t.E6,S2.SVMc3b_t.E7,S2.SVMc3b_t.E8,S2.SVMc3b_t.E9,S2.SVMc3b_t.E10)) ; Store(S2.SVMc3b_t)

#to check if ok
graph3(S2.SVMc3b_t,S2.SVMc3b)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.SVMc3.IB_t.E1<-attr(predict(S2.SVMc3.IB.E1[[1]][[6]],scal.IB_mat3_t.E1,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.IB_t.E1)
S2.SVMc3.IB_t.E2<-attr(predict(S2.SVMc3.IB.E2[[1]][[6]],scal.IB_mat3_t.E2,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.IB_t.E2)
S2.SVMc3.IB_t.E3<-attr(predict(S2.SVMc3.IB.E3[[1]][[6]],scal.IB_mat3_t.E3,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.IB_t.E3)
S2.SVMc3.IB_t.E4<-attr(predict(S2.SVMc3.IB.E4[[1]][[6]],scal.IB_mat3_t.E4,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.IB_t.E4)
S2.SVMc3.IB_t.E5<-attr(predict(S2.SVMc3.IB.E5[[1]][[6]],scal.IB_mat3_t.E5,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.IB_t.E5)
S2.SVMc3.IB_t.E6<-attr(predict(S2.SVMc3.IB.E6[[1]][[6]],scal.IB_mat3_t.E6,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.IB_t.E6)
S2.SVMc3.IB_t.E7<-attr(predict(S2.SVMc3.IB.E7[[1]][[6]],scal.IB_mat3_t.E7,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.IB_t.E7)
S2.SVMc3.IB_t.E8<-attr(predict(S2.SVMc3.IB.E8[[1]][[6]],scal.IB_mat3_t.E8,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.IB_t.E8)
S2.SVMc3.IB_t.E9<-attr(predict(S2.SVMc3.IB.E9[[1]][[6]],scal.IB_mat3_t.E9,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.IB_t.E9)
S2.SVMc3.IB_t.E10<-attr(predict(S2.SVMc3.IB.E10[[1]][[6]],scal.IB_mat3_t.E10,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.IB_t.E10)

S2.SVMc3.IB_t<-Group_t(list(S2.SVMc3.IB_t.E1,S2.SVMc3.IB_t.E2,S2.SVMc3.IB_t.E3,S2.SVMc3.IB_t.E4,S2.SVMc3.IB_t.E5,
                            S2.SVMc3.IB_t.E6,S2.SVMc3.IB_t.E7,S2.SVMc3.IB_t.E8,S2.SVMc3.IB_t.E9,S2.SVMc3.IB_t.E10)) ; Store(S2.SVMc3.IB_t)

#to check if ok
graph3(S2.SVMc3.IB_t,S2.SVMc3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.SVMc3.2_t.E1<-attr(predict(S2.SVMc3.2.E1[[1]][[6]],scal_mat3.2_t.E1,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.2_t.E1)
S2.SVMc3.2_t.E2<-attr(predict(S2.SVMc3.2.E2[[1]][[6]],scal_mat3.2_t.E2,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.2_t.E2)
S2.SVMc3.2_t.E3<-attr(predict(S2.SVMc3.2.E3[[1]][[6]],scal_mat3.2_t.E3,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.2_t.E3)
S2.SVMc3.2_t.E4<-attr(predict(S2.SVMc3.2.E4[[1]][[6]],scal_mat3.2_t.E4,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.2_t.E4)
S2.SVMc3.2_t.E5<-attr(predict(S2.SVMc3.2.E5[[1]][[6]],scal_mat3.2_t.E5,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.2_t.E5)
S2.SVMc3.2_t.E6<-attr(predict(S2.SVMc3.2.E6[[1]][[6]],scal_mat3.2_t.E6,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.2_t.E6)
S2.SVMc3.2_t.E7<-attr(predict(S2.SVMc3.2.E7[[1]][[6]],scal_mat3.2_t.E7,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.2_t.E7)
S2.SVMc3.2_t.E8<-attr(predict(S2.SVMc3.2.E8[[1]][[6]],scal_mat3.2_t.E8,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.2_t.E8)
S2.SVMc3.2_t.E9<-attr(predict(S2.SVMc3.2.E9[[1]][[6]],scal_mat3.2_t.E9,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.2_t.E9)
S2.SVMc3.2_t.E10<-attr(predict(S2.SVMc3.2.E10[[1]][[6]],scal_mat3.2_t.E10,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3.2_t.E10)

S2.SVMc3.2_t<-Group_t(list(S2.SVMc3.2_t.E1,S2.SVMc3.2_t.E2,S2.SVMc3.2_t.E3,S2.SVMc3.2_t.E4,S2.SVMc3.2_t.E5,
                           S2.SVMc3.2_t.E6,S2.SVMc3.2_t.E7,S2.SVMc3.2_t.E8,S2.SVMc3.2_t.E9,S2.SVMc3.2_t.E10)) ; Store(S2.SVMc3.2_t)

#to check if ok
graph3(S2.SVMc3.2_t,S2.SVMc3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.SVMc3b.IB.2_t.E1<-attr(predict(S2.SVMc3b.IB.2.E1[[1]][[6]],scal.IB_mat3.2_t.E1,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b.IB.2_t.E1)
S2.SVMc3b.IB.2_t.E2<-attr(predict(S2.SVMc3b.IB.2.E2[[1]][[6]],scal.IB_mat3.2_t.E2,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b.IB.2_t.E2)
S2.SVMc3b.IB.2_t.E3<-attr(predict(S2.SVMc3b.IB.2.E3[[1]][[6]],scal.IB_mat3.2_t.E3,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b.IB.2_t.E3)
S2.SVMc3b.IB.2_t.E4<-attr(predict(S2.SVMc3b.IB.2.E4[[1]][[6]],scal.IB_mat3.2_t.E4,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b.IB.2_t.E4)
S2.SVMc3b.IB.2_t.E5<-attr(predict(S2.SVMc3b.IB.2.E5[[1]][[6]],scal.IB_mat3.2_t.E5,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b.IB.2_t.E5)
S2.SVMc3b.IB.2_t.E6<-attr(predict(S2.SVMc3b.IB.2.E6[[1]][[6]],scal.IB_mat3.2_t.E6,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b.IB.2_t.E6)
S2.SVMc3b.IB.2_t.E7<-attr(predict(S2.SVMc3b.IB.2.E7[[1]][[6]],scal.IB_mat3.2_t.E7,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b.IB.2_t.E7)
S2.SVMc3b.IB.2_t.E8<-attr(predict(S2.SVMc3b.IB.2.E8[[1]][[6]],scal.IB_mat3.2_t.E8,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b.IB.2_t.E8)
S2.SVMc3b.IB.2_t.E9<-attr(predict(S2.SVMc3b.IB.2.E9[[1]][[6]],scal.IB_mat3.2_t.E9,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b.IB.2_t.E9)
S2.SVMc3b.IB.2_t.E10<-attr(predict(S2.SVMc3b.IB.2.E10[[1]][[6]],scal.IB_mat3.2_t.E10,probability =TRUE),"probabilities")[,"1"] ; Store(S2.SVMc3b.IB.2_t.E10)

S2.SVMc3b.IB.2_t<-Group_t(list(S2.SVMc3b.IB.2_t.E1,S2.SVMc3b.IB.2_t.E2,S2.SVMc3b.IB.2_t.E3,S2.SVMc3b.IB.2_t.E4,S2.SVMc3b.IB.2_t.E5,
                               S2.SVMc3b.IB.2_t.E6,S2.SVMc3b.IB.2_t.E7,S2.SVMc3b.IB.2_t.E8,S2.SVMc3b.IB.2_t.E9,S2.SVMc3b.IB.2_t.E10)) ; Store(S2.SVMc3b.IB.2_t)

#to check if ok
graph3(S2.SVMc3b.IB.2_t,S2.SVMc3b.IB.2)

#####################################################################
# PREDICT S2.SVM to model binary response good/poor essays
# with radial kernel on scaled matrix
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

S2.RADc2_t.E1<-attr(predict(S2.RADc2.E1[[1]][[6]],scal_mat3_t.E1,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2_t.E1)
S2.RADc2_t.E2<-attr(predict(S2.RADc2.E2[[1]][[6]],scal_mat3_t.E2,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2_t.E2)
S2.RADc2_t.E3<-attr(predict(S2.RADc2.E3[[1]][[6]],scal_mat3_t.E3,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2_t.E3)
S2.RADc2_t.E4<-attr(predict(S2.RADc2.E4[[1]][[6]],scal_mat3_t.E4,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2_t.E4)
S2.RADc2_t.E5<-attr(predict(S2.RADc2.E5[[1]][[6]],scal_mat3_t.E5,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2_t.E5)
S2.RADc2_t.E6<-attr(predict(S2.RADc2.E6[[1]][[6]],scal_mat3_t.E6,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2_t.E6)
S2.RADc2_t.E7<-attr(predict(S2.RADc2.E7[[1]][[6]],scal_mat3_t.E7,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2_t.E7)
S2.RADc2_t.E8<-attr(predict(S2.RADc2.E8[[1]][[6]],scal_mat3_t.E8,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2_t.E8)
S2.RADc2_t.E9<-attr(predict(S2.RADc2.E9[[1]][[6]],scal_mat3_t.E9,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2_t.E9)
S2.RADc2_t.E10<-attr(predict(S2.RADc2.E10[[1]][[6]],scal_mat3_t.E10,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2_t.E10)

S2.RADc2_t<-Group_t(list(S2.RADc2_t.E1,S2.RADc2_t.E2,S2.RADc2_t.E3,S2.RADc2_t.E4,S2.RADc2_t.E5,
                         S2.RADc2_t.E6,S2.RADc2_t.E7,S2.RADc2_t.E8,S2.RADc2_t.E9,S2.RADc2_t.E10)) ; Store(S2.RADc2_t)

#to check if ok
graph3(S2.RADc2_t,S2.RADc2)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.RADc2.IB_t.E1<-attr(predict(S2.RADc2.IB.E1[[1]][[6]],scal.IB_mat3_t.E1,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB_t.E1)
S2.RADc2.IB_t.E2<-attr(predict(S2.RADc2.IB.E2[[1]][[6]],scal.IB_mat3_t.E2,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB_t.E2)
S2.RADc2.IB_t.E3<-attr(predict(S2.RADc2.IB.E3[[1]][[6]],scal.IB_mat3_t.E3,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB_t.E3)
S2.RADc2.IB_t.E4<-attr(predict(S2.RADc2.IB.E4[[1]][[6]],scal.IB_mat3_t.E4,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB_t.E4)
S2.RADc2.IB_t.E5<-attr(predict(S2.RADc2.IB.E5[[1]][[6]],scal.IB_mat3_t.E5,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB_t.E5)
S2.RADc2.IB_t.E6<-attr(predict(S2.RADc2.IB.E6[[1]][[6]],scal.IB_mat3_t.E6,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB_t.E6)
S2.RADc2.IB_t.E7<-attr(predict(S2.RADc2.IB.E7[[1]][[6]],scal.IB_mat3_t.E7,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB_t.E7)
S2.RADc2.IB_t.E8<-attr(predict(S2.RADc2.IB.E8[[1]][[6]],scal.IB_mat3_t.E8,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB_t.E8)
S2.RADc2.IB_t.E9<-attr(predict(S2.RADc2.IB.E9[[1]][[6]],scal.IB_mat3_t.E9,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB_t.E9)
S2.RADc2.IB_t.E10<-attr(predict(S2.RADc2.IB.E10[[1]][[6]],scal.IB_mat3_t.E10,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB_t.E10)

S2.RADc2.IB_t<-Group_t(list(S2.RADc2.IB_t.E1,S2.RADc2.IB_t.E2,S2.RADc2.IB_t.E3,S2.RADc2.IB_t.E4,S2.RADc2.IB_t.E5,
                            S2.RADc2.IB_t.E6,S2.RADc2.IB_t.E7,S2.RADc2.IB_t.E8,S2.RADc2.IB_t.E9,S2.RADc2.IB_t.E10)) ; Store(S2.RADc2.IB_t)

#to check if ok
graph3(S2.RADc2.IB_t,S2.RADc2.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.RADc2.2_t.E1<-attr(predict(S2.RADc2.2.E1[[1]][[6]],scal_mat3.2_t.E1,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.2_t.E1)
S2.RADc2.2_t.E2<-attr(predict(S2.RADc2.2.E2[[1]][[6]],scal_mat3.2_t.E2,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.2_t.E2)
S2.RADc2.2_t.E3<-attr(predict(S2.RADc2.2.E3[[1]][[6]],scal_mat3.2_t.E3,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.2_t.E3)
S2.RADc2.2_t.E4<-attr(predict(S2.RADc2.2.E4[[1]][[6]],scal_mat3.2_t.E4,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.2_t.E4)
S2.RADc2.2_t.E5<-attr(predict(S2.RADc2.2.E5[[1]][[6]],scal_mat3.2_t.E5,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.2_t.E5)
S2.RADc2.2_t.E6<-attr(predict(S2.RADc2.2.E6[[1]][[6]],scal_mat3.2_t.E6,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.2_t.E6)
S2.RADc2.2_t.E7<-attr(predict(S2.RADc2.2.E7[[1]][[6]],scal_mat3.2_t.E7,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.2_t.E7)
S2.RADc2.2_t.E8<-attr(predict(S2.RADc2.2.E8[[1]][[6]],scal_mat3.2_t.E8,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.2_t.E8)
S2.RADc2.2_t.E9<-attr(predict(S2.RADc2.2.E9[[1]][[6]],scal_mat3.2_t.E9,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.2_t.E9)
S2.RADc2.2_t.E10<-attr(predict(S2.RADc2.2.E10[[1]][[6]],scal_mat3.2_t.E10,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.2_t.E10)

S2.RADc2.2_t<-Group_t(list(S2.RADc2.2_t.E1,S2.RADc2.2_t.E2,S2.RADc2.2_t.E3,S2.RADc2.2_t.E4,S2.RADc2.2_t.E5,
                           S2.RADc2.2_t.E6,S2.RADc2.2_t.E7,S2.RADc2.2_t.E8,S2.RADc2.2_t.E9,S2.RADc2.2_t.E10)) ; Store(S2.RADc2.2_t)

#to check if ok
graph3(S2.RADc2.2_t,S2.RADc2.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.RADc2.IB.2_t.E1<-attr(predict(S2.RADc2.IB.2.E1[[1]][[6]],scal.IB_mat3.2_t.E1,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB.2_t.E1)
S2.RADc2.IB.2_t.E2<-attr(predict(S2.RADc2.IB.2.E2[[1]][[6]],scal.IB_mat3.2_t.E2,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB.2_t.E2)
S2.RADc2.IB.2_t.E3<-attr(predict(S2.RADc2.IB.2.E3[[1]][[6]],scal.IB_mat3.2_t.E3,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB.2_t.E3)
S2.RADc2.IB.2_t.E4<-attr(predict(S2.RADc2.IB.2.E4[[1]][[6]],scal.IB_mat3.2_t.E4,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB.2_t.E4)
S2.RADc2.IB.2_t.E5<-attr(predict(S2.RADc2.IB.2.E5[[1]][[6]],scal.IB_mat3.2_t.E5,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB.2_t.E5)
S2.RADc2.IB.2_t.E6<-attr(predict(S2.RADc2.IB.2.E6[[1]][[6]],scal.IB_mat3.2_t.E6,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB.2_t.E6)
S2.RADc2.IB.2_t.E7<-attr(predict(S2.RADc2.IB.2.E7[[1]][[6]],scal.IB_mat3.2_t.E7,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB.2_t.E7)
S2.RADc2.IB.2_t.E8<-attr(predict(S2.RADc2.IB.2.E8[[1]][[6]],scal.IB_mat3.2_t.E8,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB.2_t.E8)
S2.RADc2.IB.2_t.E9<-attr(predict(S2.RADc2.IB.2.E9[[1]][[6]],scal.IB_mat3.2_t.E9,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB.2_t.E9)
S2.RADc2.IB.2_t.E10<-attr(predict(S2.RADc2.IB.2.E10[[1]][[6]],scal.IB_mat3.2_t.E10,probability =TRUE),"probabilities")[,"1"] ; Store(S2.RADc2.IB.2_t.E10)

S2.RADc2.IB.2_t<-Group_t(list(S2.RADc2.IB.2_t.E1,S2.RADc2.IB.2_t.E2,S2.RADc2.IB.2_t.E3,S2.RADc2.IB.2_t.E4,S2.RADc2.IB.2_t.E5,
                              S2.RADc2.IB.2_t.E6,S2.RADc2.IB.2_t.E7,S2.RADc2.IB.2_t.E8,S2.RADc2.IB.2_t.E9,S2.RADc2.IB.2_t.E10)) ; Store(S2.RADc2.IB.2_t)

#to check if ok
graph3(S2.RADc2.IB.2_t,S2.RADc2.IB.2)

#####################################################################
# PREDICT Multinomial S2.SVM
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

S2.SVMm3_t.E1<-predict(S2.SVMm3.E1[[1]][[6]],scal_mat3_t.E1) ; Store(S2.SVMm3_t.E1)
S2.SVMm3_t.E2<-predict(S2.SVMm3.E2[[1]][[6]],scal_mat3_t.E2) ; Store(S2.SVMm3_t.E2)
S2.SVMm3_t.E3<-predict(S2.SVMm3.E3[[1]][[6]],scal_mat3_t.E3) ; Store(S2.SVMm3_t.E3)
S2.SVMm3_t.E4<-predict(S2.SVMm3.E4[[1]][[6]],scal_mat3_t.E4) ; Store(S2.SVMm3_t.E4)
S2.SVMm3_t.E5<-predict(S2.SVMm3.E5[[1]][[6]],scal_mat3_t.E5) ; Store(S2.SVMm3_t.E5)
S2.SVMm3_t.E6<-predict(S2.SVMm3.E6[[1]][[6]],scal_mat3_t.E6) ; Store(S2.SVMm3_t.E6)
S2.SVMm3_t.E7<-predict(S2.SVMm3.E7[[1]][[6]],scal_mat3_t.E7) ; Store(S2.SVMm3_t.E7)
S2.SVMm3_t.E8<-predict(S2.SVMm3.E8[[1]][[6]],scal_mat3_t.E8) ; Store(S2.SVMm3_t.E8)
S2.SVMm3_t.E9<-predict(S2.SVMm3.E9[[1]][[6]],scal_mat3_t.E9) ; Store(S2.SVMm3_t.E9)
S2.SVMm3_t.E10<-predict(S2.SVMm3.E10[[1]][[6]],scal_mat3_t.E10) ; Store(S2.SVMm3_t.E10)

S2.SVMm3_t.E1<-as.numeric(gsub("S","",S2.SVMm3_t.E1)) ; Store(S2.SVMm3_t.E1)
S2.SVMm3_t.E2<-as.numeric(gsub("S","",S2.SVMm3_t.E2)) ; Store(S2.SVMm3_t.E2)
S2.SVMm3_t.E3<-as.numeric(gsub("S","",S2.SVMm3_t.E3)) ; Store(S2.SVMm3_t.E3)
S2.SVMm3_t.E4<-as.numeric(gsub("S","",S2.SVMm3_t.E4)) ; Store(S2.SVMm3_t.E4)
S2.SVMm3_t.E5<-as.numeric(gsub("S","",S2.SVMm3_t.E5)) ; Store(S2.SVMm3_t.E5)
S2.SVMm3_t.E6<-as.numeric(gsub("S","",S2.SVMm3_t.E6)) ; Store(S2.SVMm3_t.E6)
S2.SVMm3_t.E7<-as.numeric(gsub("S","",S2.SVMm3_t.E7)) ; Store(S2.SVMm3_t.E7)
S2.SVMm3_t.E8<-as.numeric(gsub("S","",S2.SVMm3_t.E8)) ; Store(S2.SVMm3_t.E8)
S2.SVMm3_t.E9<-as.numeric(gsub("S","",S2.SVMm3_t.E9)) ; Store(S2.SVMm3_t.E9)
S2.SVMm3_t.E10<-as.numeric(gsub("S","",S2.SVMm3_t.E10)) ; Store(S2.SVMm3_t.E10)

S2.SVMm3_t<-Group_t(list(S2.SVMm3_t.E1,S2.SVMm3_t.E2,S2.SVMm3_t.E3,S2.SVMm3_t.E4,S2.SVMm3_t.E5,
                         S2.SVMm3_t.E6,S2.SVMm3_t.E7,S2.SVMm3_t.E8,S2.SVMm3_t.E9,S2.SVMm3_t.E10)) ; Store(S2.SVMm3_t)

#to check if ok
graph3(S2.SVMm3_t,S2.SVMm3)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.SVMm3.IB_t.E1<-predict(S2.SVMm3.IB.E1[[1]][[6]],scal.IB_mat3_t.E1) ; Store(S2.SVMm3.IB_t.E1)
S2.SVMm3.IB_t.E2<-predict(S2.SVMm3.IB.E2[[1]][[6]],scal.IB_mat3_t.E2) ; Store(S2.SVMm3.IB_t.E2)
S2.SVMm3.IB_t.E3<-predict(S2.SVMm3.IB.E3[[1]][[6]],scal.IB_mat3_t.E3) ; Store(S2.SVMm3.IB_t.E3)
S2.SVMm3.IB_t.E4<-predict(S2.SVMm3.IB.E4[[1]][[6]],scal.IB_mat3_t.E4) ; Store(S2.SVMm3.IB_t.E4)
S2.SVMm3.IB_t.E5<-predict(S2.SVMm3.IB.E5[[1]][[6]],scal.IB_mat3_t.E5) ; Store(S2.SVMm3.IB_t.E5)
S2.SVMm3.IB_t.E6<-predict(S2.SVMm3.IB.E6[[1]][[6]],scal.IB_mat3_t.E6) ; Store(S2.SVMm3.IB_t.E6)
S2.SVMm3.IB_t.E7<-predict(S2.SVMm3.IB.E7[[1]][[6]],scal.IB_mat3_t.E7) ; Store(S2.SVMm3.IB_t.E7)
S2.SVMm3.IB_t.E8<-predict(S2.SVMm3.IB.E8[[1]][[6]],scal.IB_mat3_t.E8) ; Store(S2.SVMm3.IB_t.E8)
S2.SVMm3.IB_t.E9<-predict(S2.SVMm3.IB.E9[[1]][[6]],scal.IB_mat3_t.E9) ; Store(S2.SVMm3.IB_t.E9)
S2.SVMm3.IB_t.E10<-predict(S2.SVMm3.IB.E10[[1]][[6]],scal.IB_mat3_t.E10) ; Store(S2.SVMm3.IB_t.E10)

S2.SVMm3.IB_t.E1<-as.numeric(gsub("S","",S2.SVMm3.IB_t.E1)) ; Store(S2.SVMm3.IB_t.E1)
S2.SVMm3.IB_t.E2<-as.numeric(gsub("S","",S2.SVMm3.IB_t.E2)) ; Store(S2.SVMm3.IB_t.E2)
S2.SVMm3.IB_t.E3<-as.numeric(gsub("S","",S2.SVMm3.IB_t.E3)) ; Store(S2.SVMm3.IB_t.E3)
S2.SVMm3.IB_t.E4<-as.numeric(gsub("S","",S2.SVMm3.IB_t.E4)) ; Store(S2.SVMm3.IB_t.E4)
S2.SVMm3.IB_t.E5<-as.numeric(gsub("S","",S2.SVMm3.IB_t.E5)) ; Store(S2.SVMm3.IB_t.E5)
S2.SVMm3.IB_t.E6<-as.numeric(gsub("S","",S2.SVMm3.IB_t.E6)) ; Store(S2.SVMm3.IB_t.E6)
S2.SVMm3.IB_t.E7<-as.numeric(gsub("S","",S2.SVMm3.IB_t.E7)) ; Store(S2.SVMm3.IB_t.E7)
S2.SVMm3.IB_t.E8<-as.numeric(gsub("S","",S2.SVMm3.IB_t.E8)) ; Store(S2.SVMm3.IB_t.E8)
S2.SVMm3.IB_t.E9<-as.numeric(gsub("S","",S2.SVMm3.IB_t.E9)) ; Store(S2.SVMm3.IB_t.E9)
S2.SVMm3.IB_t.E10<-as.numeric(gsub("S","",S2.SVMm3.IB_t.E10)) ; Store(S2.SVMm3.IB_t.E10)

S2.SVMm3.IB_t<-Group_t(list(S2.SVMm3.IB_t.E1,S2.SVMm3.IB_t.E2,S2.SVMm3.IB_t.E3,S2.SVMm3.IB_t.E4,S2.SVMm3.IB_t.E5,
                            S2.SVMm3.IB_t.E6,S2.SVMm3.IB_t.E7,S2.SVMm3.IB_t.E8,S2.SVMm3.IB_t.E9,S2.SVMm3.IB_t.E10)) ; Store(S2.SVMm3.IB_t)

#to check if ok
graph3(S2.SVMm3.IB_t,S2.SVMm3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.SVMm3.2_t.E1<-predict(S2.SVMm3.2.E1[[1]][[6]],scal_mat3.2_t.E1) ; Store(S2.SVMm3.2_t.E1)
S2.SVMm3.2_t.E2<-predict(S2.SVMm3.2.E2[[1]][[6]],scal_mat3.2_t.E2) ; Store(S2.SVMm3.2_t.E2)
S2.SVMm3.2_t.E3<-predict(S2.SVMm3.2.E3[[1]][[6]],scal_mat3.2_t.E3) ; Store(S2.SVMm3.2_t.E3)
S2.SVMm3.2_t.E4<-predict(S2.SVMm3.2.E4[[1]][[6]],scal_mat3.2_t.E4) ; Store(S2.SVMm3.2_t.E4)
S2.SVMm3.2_t.E5<-predict(S2.SVMm3.2.E5[[1]][[6]],scal_mat3.2_t.E5) ; Store(S2.SVMm3.2_t.E5)
S2.SVMm3.2_t.E6<-predict(S2.SVMm3.2.E6[[1]][[6]],scal_mat3.2_t.E6) ; Store(S2.SVMm3.2_t.E6)
S2.SVMm3.2_t.E7<-predict(S2.SVMm3.2.E7[[1]][[6]],scal_mat3.2_t.E7) ; Store(S2.SVMm3.2_t.E7)
S2.SVMm3.2_t.E8<-predict(S2.SVMm3.2.E8[[1]][[6]],scal_mat3.2_t.E8) ; Store(S2.SVMm3.2_t.E8)
S2.SVMm3.2_t.E9<-predict(S2.SVMm3.2.E9[[1]][[6]],scal_mat3.2_t.E9) ; Store(S2.SVMm3.2_t.E9)
S2.SVMm3.2_t.E10<-predict(S2.SVMm3.2.E10[[1]][[6]],scal_mat3.2_t.E10) ; Store(S2.SVMm3.2_t.E10)

S2.SVMm3.2_t.E1<-as.numeric(gsub("S","",S2.SVMm3.2_t.E1)) ; Store(S2.SVMm3.2_t.E1)
S2.SVMm3.2_t.E2<-as.numeric(gsub("S","",S2.SVMm3.2_t.E2)) ; Store(S2.SVMm3.2_t.E2)
S2.SVMm3.2_t.E3<-as.numeric(gsub("S","",S2.SVMm3.2_t.E3)) ; Store(S2.SVMm3.2_t.E3)
S2.SVMm3.2_t.E4<-as.numeric(gsub("S","",S2.SVMm3.2_t.E4)) ; Store(S2.SVMm3.2_t.E4)
S2.SVMm3.2_t.E5<-as.numeric(gsub("S","",S2.SVMm3.2_t.E5)) ; Store(S2.SVMm3.2_t.E5)
S2.SVMm3.2_t.E6<-as.numeric(gsub("S","",S2.SVMm3.2_t.E6)) ; Store(S2.SVMm3.2_t.E6)
S2.SVMm3.2_t.E7<-as.numeric(gsub("S","",S2.SVMm3.2_t.E7)) ; Store(S2.SVMm3.2_t.E7)
S2.SVMm3.2_t.E8<-as.numeric(gsub("S","",S2.SVMm3.2_t.E8)) ; Store(S2.SVMm3.2_t.E8)
S2.SVMm3.2_t.E9<-as.numeric(gsub("S","",S2.SVMm3.2_t.E9)) ; Store(S2.SVMm3.2_t.E9)
S2.SVMm3.2_t.E10<-as.numeric(gsub("S","",S2.SVMm3.2_t.E10)) ; Store(S2.SVMm3.2_t.E10)

S2.SVMm3.2_t<-Group_t(list(S2.SVMm3.2_t.E1,S2.SVMm3.2_t.E2,S2.SVMm3.2_t.E3,S2.SVMm3.2_t.E4,S2.SVMm3.2_t.E5,
                           S2.SVMm3.2_t.E6,S2.SVMm3.2_t.E7,S2.SVMm3.2_t.E8,S2.SVMm3.2_t.E9,S2.SVMm3.2_t.E10)) ; Store(S2.SVMm3.2_t)

#to check if ok
graph3(S2.SVMm3.2_t,S2.SVMm3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.SVMm3.IB.2_t.E1<-predict(S2.SVMm3.IB.2.E1[[1]][[6]],scal.IB_mat3.2_t.E1) ; Store(S2.SVMm3.IB.2_t.E1)
S2.SVMm3.IB.2_t.E2<-predict(S2.SVMm3.IB.2.E2[[1]][[6]],scal.IB_mat3.2_t.E2) ; Store(S2.SVMm3.IB.2_t.E2)
S2.SVMm3.IB.2_t.E3<-predict(S2.SVMm3.IB.2.E3[[1]][[6]],scal.IB_mat3.2_t.E3) ; Store(S2.SVMm3.IB.2_t.E3)
S2.SVMm3.IB.2_t.E4<-predict(S2.SVMm3.IB.2.E4[[1]][[6]],scal.IB_mat3.2_t.E4) ; Store(S2.SVMm3.IB.2_t.E4)
S2.SVMm3.IB.2_t.E5<-predict(S2.SVMm3.IB.2.E5[[1]][[6]],scal.IB_mat3.2_t.E5) ; Store(S2.SVMm3.IB.2_t.E5)
S2.SVMm3.IB.2_t.E6<-predict(S2.SVMm3.IB.2.E6[[1]][[6]],scal.IB_mat3.2_t.E6) ; Store(S2.SVMm3.IB.2_t.E6)
S2.SVMm3.IB.2_t.E7<-predict(S2.SVMm3.IB.2.E7[[1]][[6]],scal.IB_mat3.2_t.E7) ; Store(S2.SVMm3.IB.2_t.E7)
S2.SVMm3.IB.2_t.E8<-predict(S2.SVMm3.IB.2.E8[[1]][[6]],scal.IB_mat3.2_t.E8) ; Store(S2.SVMm3.IB.2_t.E8)
S2.SVMm3.IB.2_t.E9<-predict(S2.SVMm3.IB.2.E9[[1]][[6]],scal.IB_mat3.2_t.E9) ; Store(S2.SVMm3.IB.2_t.E9)
S2.SVMm3.IB.2_t.E10<-predict(S2.SVMm3.IB.2.E10[[1]][[6]],scal.IB_mat3.2_t.E10) ; Store(S2.SVMm3.IB.2_t.E10)

S2.SVMm3.IB.2_t.E1<-as.numeric(gsub("S","",S2.SVMm3.IB.2_t.E1)) ; Store(S2.SVMm3.IB.2_t.E1)
S2.SVMm3.IB.2_t.E2<-as.numeric(gsub("S","",S2.SVMm3.IB.2_t.E2)) ; Store(S2.SVMm3.IB.2_t.E2)
S2.SVMm3.IB.2_t.E3<-as.numeric(gsub("S","",S2.SVMm3.IB.2_t.E3)) ; Store(S2.SVMm3.IB.2_t.E3)
S2.SVMm3.IB.2_t.E4<-as.numeric(gsub("S","",S2.SVMm3.IB.2_t.E4)) ; Store(S2.SVMm3.IB.2_t.E4)
S2.SVMm3.IB.2_t.E5<-as.numeric(gsub("S","",S2.SVMm3.IB.2_t.E5)) ; Store(S2.SVMm3.IB.2_t.E5)
S2.SVMm3.IB.2_t.E6<-as.numeric(gsub("S","",S2.SVMm3.IB.2_t.E6)) ; Store(S2.SVMm3.IB.2_t.E6)
S2.SVMm3.IB.2_t.E7<-as.numeric(gsub("S","",S2.SVMm3.IB.2_t.E7)) ; Store(S2.SVMm3.IB.2_t.E7)
S2.SVMm3.IB.2_t.E8<-as.numeric(gsub("S","",S2.SVMm3.IB.2_t.E8)) ; Store(S2.SVMm3.IB.2_t.E8)
S2.SVMm3.IB.2_t.E9<-as.numeric(gsub("S","",S2.SVMm3.IB.2_t.E9)) ; Store(S2.SVMm3.IB.2_t.E9)
S2.SVMm3.IB.2_t.E10<-as.numeric(gsub("S","",S2.SVMm3.IB.2_t.E10)) ; Store(S2.SVMm3.IB.2_t.E10)

S2.SVMm3.IB.2_t<-Group_t(list(S2.SVMm3.IB.2_t.E1,S2.SVMm3.IB.2_t.E2,S2.SVMm3.IB.2_t.E3,S2.SVMm3.IB.2_t.E4,S2.SVMm3.IB.2_t.E5,
                              S2.SVMm3.IB.2_t.E6,S2.SVMm3.IB.2_t.E7,S2.SVMm3.IB.2_t.E8,S2.SVMm3.IB.2_t.E9,S2.SVMm3.IB.2_t.E10)) ; Store(S2.SVMm3.IB.2_t)

#to check if ok
graph3(S2.SVMm3.IB.2_t,S2.SVMm3.IB.2)

#####################################################################
# TRAIN S2.SVM multinomial
# with radial kernel on scaled matrix
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

S2.RADm2_t.E1<-predict(S2.RADm2.E1[[1]][[6]],scal_mat3_t.E1) ; Store(S2.RADm2_t.E1)
S2.RADm2_t.E2<-predict(S2.RADm2.E2[[1]][[6]],scal_mat3_t.E2) ; Store(S2.RADm2_t.E2)
S2.RADm2_t.E3<-predict(S2.RADm2.E3[[1]][[6]],scal_mat3_t.E3) ; Store(S2.RADm2_t.E3)
S2.RADm2_t.E4<-predict(S2.RADm2.E4[[1]][[6]],scal_mat3_t.E4) ; Store(S2.RADm2_t.E4)
S2.RADm2_t.E5<-predict(S2.RADm2.E5[[1]][[6]],scal_mat3_t.E5) ; Store(S2.RADm2_t.E5)
S2.RADm2_t.E6<-predict(S2.RADm2.E6[[1]][[6]],scal_mat3_t.E6) ; Store(S2.RADm2_t.E6)
S2.RADm2_t.E7<-predict(S2.RADm2.E7[[1]][[6]],scal_mat3_t.E7) ; Store(S2.RADm2_t.E7)
S2.RADm2_t.E8<-predict(S2.RADm2.E8[[1]][[6]],scal_mat3_t.E8) ; Store(S2.RADm2_t.E8)
S2.RADm2_t.E9<-predict(S2.RADm2.E9[[1]][[6]],scal_mat3_t.E9) ; Store(S2.RADm2_t.E9)
S2.RADm2_t.E10<-predict(S2.RADm2.E10[[1]][[6]],scal_mat3_t.E10) ; Store(S2.RADm2_t.E10)

S2.RADm2_t.E1<-as.numeric(gsub("S","",S2.RADm2_t.E1)) ; Store(S2.RADm2_t.E1)
S2.RADm2_t.E2<-as.numeric(gsub("S","",S2.RADm2_t.E2)) ; Store(S2.RADm2_t.E2)
S2.RADm2_t.E3<-as.numeric(gsub("S","",S2.RADm2_t.E3)) ; Store(S2.RADm2_t.E3)
S2.RADm2_t.E4<-as.numeric(gsub("S","",S2.RADm2_t.E4)) ; Store(S2.RADm2_t.E4)
S2.RADm2_t.E5<-as.numeric(gsub("S","",S2.RADm2_t.E5)) ; Store(S2.RADm2_t.E5)
S2.RADm2_t.E6<-as.numeric(gsub("S","",S2.RADm2_t.E6)) ; Store(S2.RADm2_t.E6)
S2.RADm2_t.E7<-as.numeric(gsub("S","",S2.RADm2_t.E7)) ; Store(S2.RADm2_t.E7)
S2.RADm2_t.E8<-as.numeric(gsub("S","",S2.RADm2_t.E8)) ; Store(S2.RADm2_t.E8)
S2.RADm2_t.E9<-as.numeric(gsub("S","",S2.RADm2_t.E9)) ; Store(S2.RADm2_t.E9)
S2.RADm2_t.E10<-as.numeric(gsub("S","",S2.RADm2_t.E10)) ; Store(S2.RADm2_t.E10)

S2.RADm2_t<-Group_t(list(S2.RADm2_t.E1,S2.RADm2_t.E2,S2.RADm2_t.E3,S2.RADm2_t.E4,S2.RADm2_t.E5,
                         S2.RADm2_t.E6,S2.RADm2_t.E7,S2.RADm2_t.E8,S2.RADm2_t.E9,S2.RADm2_t.E10)) ; Store(S2.RADm2_t)

#to check if ok
graph3(S2.RADm2_t,S2.RADm2)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.RADm2.IB_t.E1<-predict(S2.RADm2.IB.E1[[1]][[6]],scal.IB_mat3_t.E1) ; Store(S2.RADm2.IB_t.E1)
S2.RADm2.IB_t.E2<-predict(S2.RADm2.IB.E2[[1]][[6]],scal.IB_mat3_t.E2) ; Store(S2.RADm2.IB_t.E2)
S2.RADm2.IB_t.E3<-predict(S2.RADm2.IB.E3[[1]][[6]],scal.IB_mat3_t.E3) ; Store(S2.RADm2.IB_t.E3)
S2.RADm2.IB_t.E4<-predict(S2.RADm2.IB.E4[[1]][[6]],scal.IB_mat3_t.E4) ; Store(S2.RADm2.IB_t.E4)
S2.RADm2.IB_t.E5<-predict(S2.RADm2.IB.E5[[1]][[6]],scal.IB_mat3_t.E5) ; Store(S2.RADm2.IB_t.E5)
S2.RADm2.IB_t.E6<-predict(S2.RADm2.IB.E6[[1]][[6]],scal.IB_mat3_t.E6) ; Store(S2.RADm2.IB_t.E6)
S2.RADm2.IB_t.E7<-predict(S2.RADm2.IB.E7[[1]][[6]],scal.IB_mat3_t.E7) ; Store(S2.RADm2.IB_t.E7)
S2.RADm2.IB_t.E8<-predict(S2.RADm2.IB.E8[[1]][[6]],scal.IB_mat3_t.E8) ; Store(S2.RADm2.IB_t.E8)
S2.RADm2.IB_t.E9<-predict(S2.RADm2.IB.E9[[1]][[6]],scal.IB_mat3_t.E9) ; Store(S2.RADm2.IB_t.E9)
S2.RADm2.IB_t.E10<-predict(S2.RADm2.IB.E10[[1]][[6]],scal.IB_mat3_t.E10) ; Store(S2.RADm2.IB_t.E10)

S2.RADm2.IB_t.E1<-as.numeric(gsub("S","",S2.RADm2.IB_t.E1)) ; Store(S2.RADm2.IB_t.E1)
S2.RADm2.IB_t.E2<-as.numeric(gsub("S","",S2.RADm2.IB_t.E2)) ; Store(S2.RADm2.IB_t.E2)
S2.RADm2.IB_t.E3<-as.numeric(gsub("S","",S2.RADm2.IB_t.E3)) ; Store(S2.RADm2.IB_t.E3)
S2.RADm2.IB_t.E4<-as.numeric(gsub("S","",S2.RADm2.IB_t.E4)) ; Store(S2.RADm2.IB_t.E4)
S2.RADm2.IB_t.E5<-as.numeric(gsub("S","",S2.RADm2.IB_t.E5)) ; Store(S2.RADm2.IB_t.E5)
S2.RADm2.IB_t.E6<-as.numeric(gsub("S","",S2.RADm2.IB_t.E6)) ; Store(S2.RADm2.IB_t.E6)
S2.RADm2.IB_t.E7<-as.numeric(gsub("S","",S2.RADm2.IB_t.E7)) ; Store(S2.RADm2.IB_t.E7)
S2.RADm2.IB_t.E8<-as.numeric(gsub("S","",S2.RADm2.IB_t.E8)) ; Store(S2.RADm2.IB_t.E8)
S2.RADm2.IB_t.E9<-as.numeric(gsub("S","",S2.RADm2.IB_t.E9)) ; Store(S2.RADm2.IB_t.E9)
S2.RADm2.IB_t.E10<-as.numeric(gsub("S","",S2.RADm2.IB_t.E10)) ; Store(S2.RADm2.IB_t.E10)

S2.RADm2.IB_t<-Group_t(list(S2.RADm2.IB_t.E1,S2.RADm2.IB_t.E2,S2.RADm2.IB_t.E3,S2.RADm2.IB_t.E4,S2.RADm2.IB_t.E5,
                            S2.RADm2.IB_t.E6,S2.RADm2.IB_t.E7,S2.RADm2.IB_t.E8,S2.RADm2.IB_t.E9,S2.RADm2.IB_t.E10)) ; Store(S2.RADm2.IB_t)

#to check if ok
graph3(S2.RADm2.IB_t,S2.RADm2.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.RADm2.2_t.E1<-predict(S2.RADm2.2.E1[[1]][[6]],scal_mat3.2_t.E1) ; Store(S2.RADm2.2_t.E1)
S2.RADm2.2_t.E2<-predict(S2.RADm2.2.E2[[1]][[6]],scal_mat3.2_t.E2) ; Store(S2.RADm2.2_t.E2)
S2.RADm2.2_t.E3<-predict(S2.RADm2.2.E3[[1]][[6]],scal_mat3.2_t.E3) ; Store(S2.RADm2.2_t.E3)
S2.RADm2.2_t.E4<-predict(S2.RADm2.2.E4[[1]][[6]],scal_mat3.2_t.E4) ; Store(S2.RADm2.2_t.E4)
S2.RADm2.2_t.E5<-predict(S2.RADm2.2.E5[[1]][[6]],scal_mat3.2_t.E5) ; Store(S2.RADm2.2_t.E5)
S2.RADm2.2_t.E6<-predict(S2.RADm2.2.E6[[1]][[6]],scal_mat3.2_t.E6) ; Store(S2.RADm2.2_t.E6)
S2.RADm2.2_t.E7<-predict(S2.RADm2.2.E7[[1]][[6]],scal_mat3.2_t.E7) ; Store(S2.RADm2.2_t.E7)
S2.RADm2.2_t.E8<-predict(S2.RADm2.2.E8[[1]][[6]],scal_mat3.2_t.E8) ; Store(S2.RADm2.2_t.E8)
S2.RADm2.2_t.E9<-predict(S2.RADm2.2.E9[[1]][[6]],scal_mat3.2_t.E9) ; Store(S2.RADm2.2_t.E9)
S2.RADm2.2_t.E10<-predict(S2.RADm2.2.E10[[1]][[6]],scal_mat3.2_t.E10) ; Store(S2.RADm2.2_t.E10)

S2.RADm2.2_t.E1<-as.numeric(gsub("S","",S2.RADm2.2_t.E1)) ; Store(S2.RADm2.2_t.E1)
S2.RADm2.2_t.E2<-as.numeric(gsub("S","",S2.RADm2.2_t.E2)) ; Store(S2.RADm2.2_t.E2)
S2.RADm2.2_t.E3<-as.numeric(gsub("S","",S2.RADm2.2_t.E3)) ; Store(S2.RADm2.2_t.E3)
S2.RADm2.2_t.E4<-as.numeric(gsub("S","",S2.RADm2.2_t.E4)) ; Store(S2.RADm2.2_t.E4)
S2.RADm2.2_t.E5<-as.numeric(gsub("S","",S2.RADm2.2_t.E5)) ; Store(S2.RADm2.2_t.E5)
S2.RADm2.2_t.E6<-as.numeric(gsub("S","",S2.RADm2.2_t.E6)) ; Store(S2.RADm2.2_t.E6)
S2.RADm2.2_t.E7<-as.numeric(gsub("S","",S2.RADm2.2_t.E7)) ; Store(S2.RADm2.2_t.E7)
S2.RADm2.2_t.E8<-as.numeric(gsub("S","",S2.RADm2.2_t.E8)) ; Store(S2.RADm2.2_t.E8)
S2.RADm2.2_t.E9<-as.numeric(gsub("S","",S2.RADm2.2_t.E9)) ; Store(S2.RADm2.2_t.E9)
S2.RADm2.2_t.E10<-as.numeric(gsub("S","",S2.RADm2.2_t.E10)) ; Store(S2.RADm2.2_t.E10)

S2.RADm2.2_t<-Group_t(list(S2.RADm2.2_t.E1,S2.RADm2.2_t.E2,S2.RADm2.2_t.E3,S2.RADm2.2_t.E4,S2.RADm2.2_t.E5,
                           S2.RADm2.2_t.E6,S2.RADm2.2_t.E7,S2.RADm2.2_t.E8,S2.RADm2.2_t.E9,S2.RADm2.2_t.E10)) ; Store(S2.RADm2.2_t)

#to check if ok
graph3(S2.RADm2.2_t,S2.RADm2.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.RADm2.IB.2_t.E1<-predict(S2.RADm2.IB.2.E1[[1]][[6]],scal.IB_mat3.2_t.E1) ; Store(S2.RADm2.IB.2_t.E1)
S2.RADm2.IB.2_t.E2<-predict(S2.RADm2.IB.2.E2[[1]][[6]],scal.IB_mat3.2_t.E2) ; Store(S2.RADm2.IB.2_t.E2)
S2.RADm2.IB.2_t.E3<-predict(S2.RADm2.IB.2.E3[[1]][[6]],scal.IB_mat3.2_t.E3) ; Store(S2.RADm2.IB.2_t.E3)
S2.RADm2.IB.2_t.E4<-predict(S2.RADm2.IB.2.E4[[1]][[6]],scal.IB_mat3.2_t.E4) ; Store(S2.RADm2.IB.2_t.E4)
S2.RADm2.IB.2_t.E5<-predict(S2.RADm2.IB.2.E5[[1]][[6]],scal.IB_mat3.2_t.E5) ; Store(S2.RADm2.IB.2_t.E5)
S2.RADm2.IB.2_t.E6<-predict(S2.RADm2.IB.2.E6[[1]][[6]],scal.IB_mat3.2_t.E6) ; Store(S2.RADm2.IB.2_t.E6)
S2.RADm2.IB.2_t.E7<-predict(S2.RADm2.IB.2.E7[[1]][[6]],scal.IB_mat3.2_t.E7) ; Store(S2.RADm2.IB.2_t.E7)
S2.RADm2.IB.2_t.E8<-predict(S2.RADm2.IB.2.E8[[1]][[6]],scal.IB_mat3.2_t.E8) ; Store(S2.RADm2.IB.2_t.E8)
S2.RADm2.IB.2_t.E9<-predict(S2.RADm2.IB.2.E9[[1]][[6]],scal.IB_mat3.2_t.E9) ; Store(S2.RADm2.IB.2_t.E9)
S2.RADm2.IB.2_t.E10<-predict(S2.RADm2.IB.2.E10[[1]][[6]],scal.IB_mat3.2_t.E10) ; Store(S2.RADm2.IB.2_t.E10)

S2.RADm2.IB.2_t.E1<-as.numeric(gsub("S","",S2.RADm2.IB.2_t.E1)) ; Store(S2.RADm2.IB.2_t.E1)
S2.RADm2.IB.2_t.E2<-as.numeric(gsub("S","",S2.RADm2.IB.2_t.E2)) ; Store(S2.RADm2.IB.2_t.E2)
S2.RADm2.IB.2_t.E3<-as.numeric(gsub("S","",S2.RADm2.IB.2_t.E3)) ; Store(S2.RADm2.IB.2_t.E3)
S2.RADm2.IB.2_t.E4<-as.numeric(gsub("S","",S2.RADm2.IB.2_t.E4)) ; Store(S2.RADm2.IB.2_t.E4)
S2.RADm2.IB.2_t.E5<-as.numeric(gsub("S","",S2.RADm2.IB.2_t.E5)) ; Store(S2.RADm2.IB.2_t.E5)
S2.RADm2.IB.2_t.E6<-as.numeric(gsub("S","",S2.RADm2.IB.2_t.E6)) ; Store(S2.RADm2.IB.2_t.E6)
S2.RADm2.IB.2_t.E7<-as.numeric(gsub("S","",S2.RADm2.IB.2_t.E7)) ; Store(S2.RADm2.IB.2_t.E7)
S2.RADm2.IB.2_t.E8<-as.numeric(gsub("S","",S2.RADm2.IB.2_t.E8)) ; Store(S2.RADm2.IB.2_t.E8)
S2.RADm2.IB.2_t.E9<-as.numeric(gsub("S","",S2.RADm2.IB.2_t.E9)) ; Store(S2.RADm2.IB.2_t.E9)
S2.RADm2.IB.2_t.E10<-as.numeric(gsub("S","",S2.RADm2.IB.2_t.E10)) ; Store(S2.RADm2.IB.2_t.E10)

S2.RADm2.IB.2_t<-Group_t(list(S2.RADm2.IB.2_t.E1,S2.RADm2.IB.2_t.E2,S2.RADm2.IB.2_t.E3,S2.RADm2.IB.2_t.E4,S2.RADm2.IB.2_t.E5,
                              S2.RADm2.IB.2_t.E6,S2.RADm2.IB.2_t.E7,S2.RADm2.IB.2_t.E8,S2.RADm2.IB.2_t.E9,S2.RADm2.IB.2_t.E10)) ; Store(S2.RADm2.IB.2_t)

#to check if ok
graph3(S2.RADm2.IB.2_t,S2.RADm2.IB.2)

#####################################################################
#####################################################################
# PREDICT S2.SVM with radial kernel on ridit scores
#####################################################################
#####################################################################

library(e1071)

S2.RAD_t.E1<-Pred_RAD(Ridit_mat.E1,metrics.E1,0.25, S2.RAD.E1[[1]][[6]],Ridit_mat_t.E1) ; Store(S2.RAD_t.E1)
S2.RAD_t.E2<-Pred_RAD(Ridit_mat.E2,metrics.E2,0.25, S2.RAD.E2[[1]][[6]],Ridit_mat_t.E2) ; Store(S2.RAD_t.E2)
S2.RAD_t.E3<-Pred_RAD(Ridit_mat.E3,metrics.E3,0.25, S2.RAD.E3[[1]][[6]],Ridit_mat_t.E3) ; Store(S2.RAD_t.E3)
S2.RAD_t.E4<-Pred_RAD(Ridit_mat.E4,metrics.E4,0.4, S2.RAD.E4[[1]][[6]],Ridit_mat_t.E4) ; Store(S2.RAD_t.E4)
S2.RAD_t.E5<-Pred_RAD(Ridit_mat.E5,metrics.E5,0.25, S2.RAD.E5[[1]][[6]],Ridit_mat_t.E5) ; Store(S2.RAD_t.E5)
S2.RAD_t.E6<-Pred_RAD(Ridit_mat.E6,metrics.E6,0.25, S2.RAD.E6[[1]][[6]],Ridit_mat_t.E6) ; Store(S2.RAD_t.E6)
S2.RAD_t.E7<-Pred_RAD(Ridit_mat.E7,metrics.E7,0.25, S2.RAD.E7[[1]][[6]],Ridit_mat_t.E7) ; Store(S2.RAD_t.E7)
S2.RAD_t.E8<-Pred_RAD(Ridit_mat.E8,metrics.E8,0.8, S2.RAD.E8[[1]][[6]],Ridit_mat_t.E8) ; Store(S2.RAD_t.E8)
S2.RAD_t.E9<-Pred_RAD(Ridit_mat.E9,metrics.E9,0.3, S2.RAD.E9[[1]][[6]],Ridit_mat_t.E9) ; Store(S2.RAD_t.E9)
S2.RAD_t.E10<-Pred_RAD(Ridit_mat.E10,metrics.E10,0.25, S2.RAD.E10[[1]][[6]],Ridit_mat_t.E10) ; Store(S2.RAD_t.E10)

S2.RAD_t<-Group_t(list(S2.RAD_t.E1,S2.RAD_t.E2,S2.RAD_t.E3,S2.RAD_t.E4,S2.RAD_t.E5,
                       S2.RAD_t.E6,S2.RAD_t.E7,S2.RAD_t.E8,S2.RAD_t.E9,S2.RAD_t.E10)) ; Store(S2.RAD_t)

#to check if ok
graph3(S2.RAD_t,S2.RAD)

#####################################################################
#####################################################################
# PREDICT Glmnet on scaled matrix
# scaled with delta tfidf and bns
# using isgood and isbad
# regression and multinomial
#####################################################################
#####################################################################

#####################################################################
# PREDICT Glmnet on scaled matrix with delta tfidf
#####################################################################

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

S2.GNETns1_t.E1<-predict(S2.GNETns1.E1[[1]][[6]],scal_mat1_t.E1,type="response", s="lambda.min")[,1]
S2.GNETns1_t.E2<-predict(S2.GNETns1.E2[[1]][[6]],scal_mat1_t.E2,type="response", s="lambda.min")[,1]
S2.GNETns1_t.E3<-predict(S2.GNETns1.E3[[1]][[6]],scal_mat1_t.E3,type="response", s="lambda.min")[,1]
S2.GNETns1_t.E4<-predict(S2.GNETns1.E4[[1]][[6]],scal_mat1_t.E4,type="response", s="lambda.min")[,1]
S2.GNETns1_t.E5<-predict(S2.GNETns1.E5[[1]][[6]],scal_mat1_t.E5,type="response", s="lambda.min")[,1]
S2.GNETns1_t.E6<-predict(S2.GNETns1.E6[[1]][[6]],scal_mat1_t.E6,type="response", s="lambda.min")[,1]
S2.GNETns1_t.E7<-predict(S2.GNETns1.E7[[1]][[6]],scal_mat1_t.E7,type="response", s="lambda.min")[,1]
S2.GNETns1_t.E8<-predict(S2.GNETns1.E8[[1]][[6]],scal_mat1_t.E8,type="response", s="lambda.min")[,1]
S2.GNETns1_t.E9<-predict(S2.GNETns1.E9[[1]][[6]],scal_mat1_t.E9,type="response", s="lambda.min")[,1]
S2.GNETns1_t.E10<-predict(S2.GNETns1.E10[[1]][[6]],scal_mat1_t.E10,type="response", s="lambda.min")[,1]

S2.GNETns1_t<-Group_t(list(S2.GNETns1_t.E1,S2.GNETns1_t.E2,S2.GNETns1_t.E3,S2.GNETns1_t.E4,S2.GNETns1_t.E5,
                           S2.GNETns1_t.E6,S2.GNETns1_t.E7,S2.GNETns1_t.E8,S2.GNETns1_t.E9,S2.GNETns1_t.E10)) ; Store(S2.GNETns1_t)

#to check if ok
graph3(S2.GNETns1_t,S2.GNETns1)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.GNETns1.IB_t.E1<-predict(S2.GNETns1.IB.E1[[1]][[6]],scal.IB_mat1_t.E1,type="response", s="lambda.min")[,1]
S2.GNETns1.IB_t.E2<-predict(S2.GNETns1.IB.E2[[1]][[6]],scal.IB_mat1_t.E2,type="response", s="lambda.min")[,1]
S2.GNETns1.IB_t.E3<-predict(S2.GNETns1.IB.E3[[1]][[6]],scal.IB_mat1_t.E3,type="response", s="lambda.min")[,1]
S2.GNETns1.IB_t.E4<-predict(S2.GNETns1.IB.E4[[1]][[6]],scal.IB_mat1_t.E4,type="response", s="lambda.min")[,1]
S2.GNETns1.IB_t.E5<-predict(S2.GNETns1.IB.E5[[1]][[6]],scal.IB_mat1_t.E5,type="response", s="lambda.min")[,1]
S2.GNETns1.IB_t.E6<-predict(S2.GNETns1.IB.E6[[1]][[6]],scal.IB_mat1_t.E6,type="response", s="lambda.min")[,1]
S2.GNETns1.IB_t.E7<-predict(S2.GNETns1.IB.E7[[1]][[6]],scal.IB_mat1_t.E7,type="response", s="lambda.min")[,1]
S2.GNETns1.IB_t.E8<-predict(S2.GNETns1.IB.E8[[1]][[6]],scal.IB_mat1_t.E8,type="response", s="lambda.min")[,1]
S2.GNETns1.IB_t.E9<-predict(S2.GNETns1.IB.E9[[1]][[6]],scal.IB_mat1_t.E9,type="response", s="lambda.min")[,1]
S2.GNETns1.IB_t.E10<-predict(S2.GNETns1.IB.E10[[1]][[6]],scal.IB_mat1_t.E10,type="response", s="lambda.min")[,1]

S2.GNETns1.IB_t<-Group_t(list(S2.GNETns1.IB_t.E1,S2.GNETns1.IB_t.E2,S2.GNETns1.IB_t.E3,S2.GNETns1.IB_t.E4,S2.GNETns1.IB_t.E5,
                              S2.GNETns1.IB_t.E6,S2.GNETns1.IB_t.E7,S2.GNETns1.IB_t.E8,S2.GNETns1.IB_t.E9,S2.GNETns1.IB_t.E10)) ; Store(S2.GNETns1.IB_t)

#to check if ok
graph3(S2.GNETns1.IB_t,S2.GNETns1.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.GNETns1.2_t.E1<-predict(S2.GNETns1.2.E1[[1]][[6]],scal_mat1.2_t.E1,type="response", s="lambda.min")[,1]
S2.GNETns1.2_t.E2<-predict(S2.GNETns1.2.E2[[1]][[6]],scal_mat1.2_t.E2,type="response", s="lambda.min")[,1]
S2.GNETns1.2_t.E3<-predict(S2.GNETns1.2.E3[[1]][[6]],scal_mat1.2_t.E3,type="response", s="lambda.min")[,1]
S2.GNETns1.2_t.E4<-predict(S2.GNETns1.2.E4[[1]][[6]],scal_mat1.2_t.E4,type="response", s="lambda.min")[,1]
S2.GNETns1.2_t.E5<-predict(S2.GNETns1.2.E5[[1]][[6]],scal_mat1.2_t.E5,type="response", s="lambda.min")[,1]
S2.GNETns1.2_t.E6<-predict(S2.GNETns1.2.E6[[1]][[6]],scal_mat1.2_t.E6,type="response", s="lambda.min")[,1]
S2.GNETns1.2_t.E7<-predict(S2.GNETns1.2.E7[[1]][[6]],scal_mat1.2_t.E7,type="response", s="lambda.min")[,1]
S2.GNETns1.2_t.E8<-predict(S2.GNETns1.2.E8[[1]][[6]],scal_mat1.2_t.E8,type="response", s="lambda.min")[,1]
S2.GNETns1.2_t.E9<-predict(S2.GNETns1.2.E9[[1]][[6]],scal_mat1.2_t.E9,type="response", s="lambda.min")[,1]
S2.GNETns1.2_t.E10<-predict(S2.GNETns1.2.E10[[1]][[6]],scal_mat1.2_t.E10,type="response", s="lambda.min")[,1]

S2.GNETns1.2_t<-Group_t(list(S2.GNETns1.2_t.E1,S2.GNETns1.2_t.E2,S2.GNETns1.2_t.E3,S2.GNETns1.2_t.E4,S2.GNETns1.2_t.E5,
                             S2.GNETns1.2_t.E6,S2.GNETns1.2_t.E7,S2.GNETns1.2_t.E8,S2.GNETns1.2_t.E9,S2.GNETns1.2_t.E10)) ; Store(S2.GNETns1.2_t)

#to check if ok
graph3(S2.GNETns1.2_t,S2.GNETns1.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.GNETns1.IB.2_t.E1<-predict(S2.GNETns1.IB.2.E1[[1]][[6]],scal.IB_mat1.2_t.E1,type="response", s="lambda.min")[,1]
S2.GNETns1.IB.2_t.E2<-predict(S2.GNETns1.IB.2.E2[[1]][[6]],scal.IB_mat1.2_t.E2,type="response", s="lambda.min")[,1]
S2.GNETns1.IB.2_t.E3<-predict(S2.GNETns1.IB.2.E3[[1]][[6]],scal.IB_mat1.2_t.E3,type="response", s="lambda.min")[,1]
S2.GNETns1.IB.2_t.E4<-predict(S2.GNETns1.IB.2.E4[[1]][[6]],scal.IB_mat1.2_t.E4,type="response", s="lambda.min")[,1]
S2.GNETns1.IB.2_t.E5<-predict(S2.GNETns1.IB.2.E5[[1]][[6]],scal.IB_mat1.2_t.E5,type="response", s="lambda.min")[,1]
S2.GNETns1.IB.2_t.E6<-predict(S2.GNETns1.IB.2.E6[[1]][[6]],scal.IB_mat1.2_t.E6,type="response", s="lambda.min")[,1]
S2.GNETns1.IB.2_t.E7<-predict(S2.GNETns1.IB.2.E7[[1]][[6]],scal.IB_mat1.2_t.E7,type="response", s="lambda.min")[,1]
S2.GNETns1.IB.2_t.E8<-predict(S2.GNETns1.IB.2.E8[[1]][[6]],scal.IB_mat1.2_t.E8,type="response", s="lambda.min")[,1]
S2.GNETns1.IB.2_t.E9<-predict(S2.GNETns1.IB.2.E9[[1]][[6]],scal.IB_mat1.2_t.E9,type="response", s="lambda.min")[,1]
S2.GNETns1.IB.2_t.E10<-predict(S2.GNETns1.IB.2.E10[[1]][[6]],scal.IB_mat1.2_t.E10,type="response", s="lambda.min")[,1]

S2.GNETns1.IB.2_t<-Group_t(list(S2.GNETns1.IB.2_t.E1,S2.GNETns1.IB.2_t.E2,S2.GNETns1.IB.2_t.E3,S2.GNETns1.IB.2_t.E4,S2.GNETns1.IB.2_t.E5,
                                S2.GNETns1.IB.2_t.E6,S2.GNETns1.IB.2_t.E7,S2.GNETns1.IB.2_t.E8,S2.GNETns1.IB.2_t.E9,S2.GNETns1.IB.2_t.E10)) ; Store(S2.GNETns1.IB.2_t)

#to check if ok
graph3(S2.GNETns1.IB.2_t,S2.GNETns1.IB.2)

#####################################################################
# PREDICT Glmnet on scaled matrix with bns
#####################################################################

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

S2.GNETns3_t.E1<-predict(S2.GNETns3.E1[[1]][[6]],scal_mat3_t.E1,type="response", s="lambda.min")[,1]
S2.GNETns3_t.E2<-predict(S2.GNETns3.E2[[1]][[6]],scal_mat3_t.E2,type="response", s="lambda.min")[,1]
S2.GNETns3_t.E3<-predict(S2.GNETns3.E3[[1]][[6]],scal_mat3_t.E3,type="response", s="lambda.min")[,1]
S2.GNETns3_t.E4<-predict(S2.GNETns3.E4[[1]][[6]],scal_mat3_t.E4,type="response", s="lambda.min")[,1]
S2.GNETns3_t.E5<-predict(S2.GNETns3.E5[[1]][[6]],scal_mat3_t.E5,type="response", s="lambda.min")[,1]
S2.GNETns3_t.E6<-predict(S2.GNETns3.E6[[1]][[6]],scal_mat3_t.E6,type="response", s="lambda.min")[,1]
S2.GNETns3_t.E7<-predict(S2.GNETns3.E7[[1]][[6]],scal_mat3_t.E7,type="response", s="lambda.min")[,1]
S2.GNETns3_t.E8<-predict(S2.GNETns3.E8[[1]][[6]],scal_mat3_t.E8,type="response", s="lambda.min")[,1]
S2.GNETns3_t.E9<-predict(S2.GNETns3.E9[[1]][[6]],scal_mat3_t.E9,type="response", s="lambda.min")[,1]
S2.GNETns3_t.E10<-predict(S2.GNETns3.E10[[1]][[6]],scal_mat3_t.E10,type="response", s="lambda.min")[,1]

S2.GNETns3_t<-Group_t(list(S2.GNETns3_t.E1,S2.GNETns3_t.E2,S2.GNETns3_t.E3,S2.GNETns3_t.E4,S2.GNETns3_t.E5,
                           S2.GNETns3_t.E6,S2.GNETns3_t.E7,S2.GNETns3_t.E8,S2.GNETns3_t.E9,S2.GNETns3_t.E10)) ; Store(S2.GNETns3_t)

#to check if ok
graph3(S2.GNETns3_t,S2.GNETns3)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.GNETns3.IB_t.E1<-predict(S2.GNETns3.IB.E1[[1]][[6]],scal.IB_mat3_t.E1,type="response", s="lambda.min")[,1]
S2.GNETns3.IB_t.E2<-predict(S2.GNETns3.IB.E2[[1]][[6]],scal.IB_mat3_t.E2,type="response", s="lambda.min")[,1]
S2.GNETns3.IB_t.E3<-predict(S2.GNETns3.IB.E3[[1]][[6]],scal.IB_mat3_t.E3,type="response", s="lambda.min")[,1]
S2.GNETns3.IB_t.E4<-predict(S2.GNETns3.IB.E4[[1]][[6]],scal.IB_mat3_t.E4,type="response", s="lambda.min")[,1]
S2.GNETns3.IB_t.E5<-predict(S2.GNETns3.IB.E5[[1]][[6]],scal.IB_mat3_t.E5,type="response", s="lambda.min")[,1]
S2.GNETns3.IB_t.E6<-predict(S2.GNETns3.IB.E6[[1]][[6]],scal.IB_mat3_t.E6,type="response", s="lambda.min")[,1]
S2.GNETns3.IB_t.E7<-predict(S2.GNETns3.IB.E7[[1]][[6]],scal.IB_mat3_t.E7,type="response", s="lambda.min")[,1]
S2.GNETns3.IB_t.E8<-predict(S2.GNETns3.IB.E8[[1]][[6]],scal.IB_mat3_t.E8,type="response", s="lambda.min")[,1]
S2.GNETns3.IB_t.E9<-predict(S2.GNETns3.IB.E9[[1]][[6]],scal.IB_mat3_t.E9,type="response", s="lambda.min")[,1]
S2.GNETns3.IB_t.E10<-predict(S2.GNETns3.IB.E10[[1]][[6]],scal.IB_mat3_t.E10,type="response", s="lambda.min")[,1]

S2.GNETns3.IB_t<-Group_t(list(S2.GNETns3.IB_t.E1,S2.GNETns3.IB_t.E2,S2.GNETns3.IB_t.E3,S2.GNETns3.IB_t.E4,S2.GNETns3.IB_t.E5,
                              S2.GNETns3.IB_t.E6,S2.GNETns3.IB_t.E7,S2.GNETns3.IB_t.E8,S2.GNETns3.IB_t.E9,S2.GNETns3.IB_t.E10)) ; Store(S2.GNETns3.IB_t)

#to check if ok
graph3(S2.GNETns3.IB_t,S2.GNETns3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.GNETns3.2_t.E1<-predict(S2.GNETns3.2.E1[[1]][[6]],scal_mat3.2_t.E1,type="response", s="lambda.min")[,1]
S2.GNETns3.2_t.E2<-predict(S2.GNETns3.2.E2[[1]][[6]],scal_mat3.2_t.E2,type="response", s="lambda.min")[,1]
S2.GNETns3.2_t.E3<-predict(S2.GNETns3.2.E3[[1]][[6]],scal_mat3.2_t.E3,type="response", s="lambda.min")[,1]
S2.GNETns3.2_t.E4<-predict(S2.GNETns3.2.E4[[1]][[6]],scal_mat3.2_t.E4,type="response", s="lambda.min")[,1]
S2.GNETns3.2_t.E5<-predict(S2.GNETns3.2.E5[[1]][[6]],scal_mat3.2_t.E5,type="response", s="lambda.min")[,1]
S2.GNETns3.2_t.E6<-predict(S2.GNETns3.2.E6[[1]][[6]],scal_mat3.2_t.E6,type="response", s="lambda.min")[,1]
S2.GNETns3.2_t.E7<-predict(S2.GNETns3.2.E7[[1]][[6]],scal_mat3.2_t.E7,type="response", s="lambda.min")[,1]
S2.GNETns3.2_t.E8<-predict(S2.GNETns3.2.E8[[1]][[6]],scal_mat3.2_t.E8,type="response", s="lambda.min")[,1]
S2.GNETns3.2_t.E9<-predict(S2.GNETns3.2.E9[[1]][[6]],scal_mat3.2_t.E9,type="response", s="lambda.min")[,1]
S2.GNETns3.2_t.E10<-predict(S2.GNETns3.2.E10[[1]][[6]],scal_mat3.2_t.E10,type="response", s="lambda.min")[,1]

S2.GNETns3.2_t<-Group_t(list(S2.GNETns3.2_t.E1,S2.GNETns3.2_t.E2,S2.GNETns3.2_t.E3,S2.GNETns3.2_t.E4,S2.GNETns3.2_t.E5,
                             S2.GNETns3.2_t.E6,S2.GNETns3.2_t.E7,S2.GNETns3.2_t.E8,S2.GNETns3.2_t.E9,S2.GNETns3.2_t.E10)) ; Store(S2.GNETns3.2_t)

#to check if ok
graph3(S2.GNETns3.2_t,S2.GNETns3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.GNETns3.IB.2_t.E1<-predict(S2.GNETns3.IB.2.E1[[1]][[6]],scal.IB_mat3.2_t.E1,type="response", s="lambda.min")[,1]
S2.GNETns3.IB.2_t.E2<-predict(S2.GNETns3.IB.2.E2[[1]][[6]],scal.IB_mat3.2_t.E2,type="response", s="lambda.min")[,1]
S2.GNETns3.IB.2_t.E3<-predict(S2.GNETns3.IB.2.E3[[1]][[6]],scal.IB_mat3.2_t.E3,type="response", s="lambda.min")[,1]
S2.GNETns3.IB.2_t.E4<-predict(S2.GNETns3.IB.2.E4[[1]][[6]],scal.IB_mat3.2_t.E4,type="response", s="lambda.min")[,1]
S2.GNETns3.IB.2_t.E5<-predict(S2.GNETns3.IB.2.E5[[1]][[6]],scal.IB_mat3.2_t.E5,type="response", s="lambda.min")[,1]
S2.GNETns3.IB.2_t.E6<-predict(S2.GNETns3.IB.2.E6[[1]][[6]],scal.IB_mat3.2_t.E6,type="response", s="lambda.min")[,1]
S2.GNETns3.IB.2_t.E7<-predict(S2.GNETns3.IB.2.E7[[1]][[6]],scal.IB_mat3.2_t.E7,type="response", s="lambda.min")[,1]
S2.GNETns3.IB.2_t.E8<-predict(S2.GNETns3.IB.2.E8[[1]][[6]],scal.IB_mat3.2_t.E8,type="response", s="lambda.min")[,1]
S2.GNETns3.IB.2_t.E9<-predict(S2.GNETns3.IB.2.E9[[1]][[6]],scal.IB_mat3.2_t.E9,type="response", s="lambda.min")[,1]
S2.GNETns3.IB.2_t.E10<-predict(S2.GNETns3.IB.2.E10[[1]][[6]],scal.IB_mat3.2_t.E10,type="response", s="lambda.min")[,1]

S2.GNETns3.IB.2_t<-Group_t(list(S2.GNETns3.IB.2_t.E1,S2.GNETns3.IB.2_t.E2,S2.GNETns3.IB.2_t.E3,S2.GNETns3.IB.2_t.E4,S2.GNETns3.IB.2_t.E5,
                                S2.GNETns3.IB.2_t.E6,S2.GNETns3.IB.2_t.E7,S2.GNETns3.IB.2_t.E8,S2.GNETns3.IB.2_t.E9,S2.GNETns3.IB.2_t.E10)) ; Store(S2.GNETns3.IB.2_t)

#to check if ok
graph3(S2.GNETns3.IB.2_t,S2.GNETns3.IB.2)

#####################################################################
# PREDICT multinomial glmnet
#####################################################################

library(glmnetcr)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.GNETcr3_t.E1<-as.numeric(as.character(predict(S2.GNETcr3.E1[[1]][[6]],scal_mat3_t.E1,type="class")$class[,95]))
S2.GNETcr3_t.E2<-as.numeric(as.character(predict(S2.GNETcr3.E2[[1]][[6]],scal_mat3_t.E2,type="class")$class[,95]))
S2.GNETcr3_t.E3<-as.numeric(as.character(predict(S2.GNETcr3.E3[[1]][[6]],scal_mat3_t.E3,type="class")$class[,95]))
S2.GNETcr3_t.E4<-as.numeric(as.character(predict(S2.GNETcr3.E4[[1]][[6]],scal_mat3_t.E4,type="class")$class[,95]))
S2.GNETcr3_t.E5<-as.numeric(as.character(predict(S2.GNETcr3.E5[[1]][[6]],scal_mat3_t.E5,type="class")$class[,95]))
S2.GNETcr3_t.E6<-as.numeric(as.character(predict(S2.GNETcr3.E6[[1]][[6]],scal_mat3_t.E6,type="class")$class[,95]))
S2.GNETcr3_t.E7<-as.numeric(as.character(predict(S2.GNETcr3.E7[[1]][[6]],scal_mat3_t.E7,type="class")$class[,95]))
S2.GNETcr3_t.E8<-as.numeric(as.character(predict(S2.GNETcr3.E8[[1]][[6]],scal_mat3_t.E8,type="class")$class[,95]))
S2.GNETcr3_t.E9<-as.numeric(as.character(predict(S2.GNETcr3.E9[[1]][[6]],scal_mat3_t.E9,type="class")$class[,95]))
S2.GNETcr3_t.E10<-as.numeric(as.character(predict(S2.GNETcr3.E10[[1]][[6]],scal_mat3_t.E10,type="class")$class[,95]))

S2.GNETcr3_t<-Group_t(list(S2.GNETcr3_t.E1,S2.GNETcr3_t.E2,S2.GNETcr3_t.E3,S2.GNETcr3_t.E4,S2.GNETcr3_t.E5,
                           S2.GNETcr3_t.E6,S2.GNETcr3_t.E7,S2.GNETcr3_t.E8,S2.GNETcr3_t.E9,S2.GNETcr3_t.E10)) ; Store(S2.GNETcr3_t)

#to check if ok
graph3(S2.GNETcr3_t,S2.GNETcr3)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.GNETcr3.IB_t.E1<-as.numeric(as.character(predict(S2.GNETcr3.IB.E1[[1]][[6]],scal.IB_mat3_t.E1,type="class")$class[,95]))
S2.GNETcr3.IB_t.E2<-as.numeric(as.character(predict(S2.GNETcr3.IB.E2[[1]][[6]],scal.IB_mat3_t.E2,type="class")$class[,95]))
S2.GNETcr3.IB_t.E3<-as.numeric(as.character(predict(S2.GNETcr3.IB.E3[[1]][[6]],scal.IB_mat3_t.E3,type="class")$class[,95]))
S2.GNETcr3.IB_t.E4<-as.numeric(as.character(predict(S2.GNETcr3.IB.E4[[1]][[6]],scal.IB_mat3_t.E4,type="class")$class[,95]))
S2.GNETcr3.IB_t.E5<-as.numeric(as.character(predict(S2.GNETcr3.IB.E5[[1]][[6]],scal.IB_mat3_t.E5,type="class")$class[,95]))
S2.GNETcr3.IB_t.E6<-as.numeric(as.character(predict(S2.GNETcr3.IB.E6[[1]][[6]],scal.IB_mat3_t.E6,type="class")$class[,95]))
S2.GNETcr3.IB_t.E7<-as.numeric(as.character(predict(S2.GNETcr3.IB.E7[[1]][[6]],scal.IB_mat3_t.E7,type="class")$class[,95]))
S2.GNETcr3.IB_t.E8<-as.numeric(as.character(predict(S2.GNETcr3.IB.E8[[1]][[6]],scal.IB_mat3_t.E8,type="class")$class[,95]))
S2.GNETcr3.IB_t.E9<-as.numeric(as.character(predict(S2.GNETcr3.IB.E9[[1]][[6]],scal.IB_mat3_t.E9,type="class")$class[,95]))
S2.GNETcr3.IB_t.E10<-as.numeric(as.character(predict(S2.GNETcr3.IB.E10[[1]][[6]],scal.IB_mat3_t.E10,type="class")$class[,95]))

S2.GNETcr3.IB_t<-Group_t(list(S2.GNETcr3.IB_t.E1,S2.GNETcr3.IB_t.E2,S2.GNETcr3.IB_t.E3,S2.GNETcr3.IB_t.E4,S2.GNETcr3.IB_t.E5,
                              S2.GNETcr3.IB_t.E6,S2.GNETcr3.IB_t.E7,S2.GNETcr3.IB_t.E8,S2.GNETcr3.IB_t.E9,S2.GNETcr3.IB_t.E10)) ; Store(S2.GNETcr3.IB_t)

#to check if ok
graph3(S2.GNETcr3.IB_t,S2.GNETcr3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.GNETcr3.2_t.E1<-as.numeric(as.character(predict(S2.GNETcr3.2.E1[[1]][[6]],scal_mat3.2_t.E1,type="class")$class[,95]))
S2.GNETcr3.2_t.E2<-as.numeric(as.character(predict(S2.GNETcr3.2.E2[[1]][[6]],scal_mat3.2_t.E2,type="class")$class[,95]))
S2.GNETcr3.2_t.E3<-as.numeric(as.character(predict(S2.GNETcr3.2.E3[[1]][[6]],scal_mat3.2_t.E3,type="class")$class[,95]))
S2.GNETcr3.2_t.E4<-as.numeric(as.character(predict(S2.GNETcr3.2.E4[[1]][[6]],scal_mat3.2_t.E4,type="class")$class[,95]))
S2.GNETcr3.2_t.E5<-as.numeric(as.character(predict(S2.GNETcr3.2.E5[[1]][[6]],scal_mat3.2_t.E5,type="class")$class[,95]))
S2.GNETcr3.2_t.E6<-as.numeric(as.character(predict(S2.GNETcr3.2.E6[[1]][[6]],scal_mat3.2_t.E6,type="class")$class[,95]))
S2.GNETcr3.2_t.E7<-as.numeric(as.character(predict(S2.GNETcr3.2.E7[[1]][[6]],scal_mat3.2_t.E7,type="class")$class[,95]))
S2.GNETcr3.2_t.E8<-as.numeric(as.character(predict(S2.GNETcr3.2.E8[[1]][[6]],scal_mat3.2_t.E8,type="class")$class[,95]))
S2.GNETcr3.2_t.E9<-as.numeric(as.character(predict(S2.GNETcr3.2.E9[[1]][[6]],scal_mat3.2_t.E9,type="class")$class[,95]))
S2.GNETcr3.2_t.E10<-as.numeric(as.character(predict(S2.GNETcr3.2.E10[[1]][[6]],scal_mat3.2_t.E10,type="class")$class[,95]))

S2.GNETcr3.2_t<-Group_t(list(S2.GNETcr3.2_t.E1,S2.GNETcr3.2_t.E2,S2.GNETcr3.2_t.E3,S2.GNETcr3.2_t.E4,S2.GNETcr3.2_t.E5,
                             S2.GNETcr3.2_t.E6,S2.GNETcr3.2_t.E7,S2.GNETcr3.2_t.E8,S2.GNETcr3.2_t.E9,S2.GNETcr3.2_t.E10)) ; Store(S2.GNETcr3.2_t)

#to check if ok
graph3(S2.GNETcr3.2_t,S2.GNETcr3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.GNETcr3.IB.2_t.E1<-as.numeric(as.character(predict(S2.GNETcr3.IB.2.E1[[1]][[6]],scal.IB_mat3.2_t.E1,type="class")$class[,95]))
S2.GNETcr3.IB.2_t.E2<-as.numeric(as.character(predict(S2.GNETcr3.IB.2.E2[[1]][[6]],scal.IB_mat3.2_t.E2,type="class")$class[,95]))
S2.GNETcr3.IB.2_t.E3<-as.numeric(as.character(predict(S2.GNETcr3.IB.2.E3[[1]][[6]],scal.IB_mat3.2_t.E3,type="class")$class[,95]))
S2.GNETcr3.IB.2_t.E4<-as.numeric(as.character(predict(S2.GNETcr3.IB.2.E4[[1]][[6]],scal.IB_mat3.2_t.E4,type="class")$class[,95]))
S2.GNETcr3.IB.2_t.E5<-as.numeric(as.character(predict(S2.GNETcr3.IB.2.E5[[1]][[6]],scal.IB_mat3.2_t.E5,type="class")$class[,95]))
S2.GNETcr3.IB.2_t.E6<-as.numeric(as.character(predict(S2.GNETcr3.IB.2.E6[[1]][[6]],scal.IB_mat3.2_t.E6,type="class")$class[,95]))
S2.GNETcr3.IB.2_t.E7<-as.numeric(as.character(predict(S2.GNETcr3.IB.2.E7[[1]][[6]],scal.IB_mat3.2_t.E7,type="class")$class[,95]))
S2.GNETcr3.IB.2_t.E8<-as.numeric(as.character(predict(S2.GNETcr3.IB.2.E8[[1]][[6]],scal.IB_mat3.2_t.E8,type="class")$class[,95]))
S2.GNETcr3.IB.2_t.E9<-as.numeric(as.character(predict(S2.GNETcr3.IB.2.E9[[1]][[6]],scal.IB_mat3.2_t.E9,type="class")$class[,95]))
S2.GNETcr3.IB.2_t.E10<-as.numeric(as.character(predict(S2.GNETcr3.IB.2.E10[[1]][[6]],scal.IB_mat3.2_t.E10,type="class")$class[,95]))

S2.GNETcr3.IB.2_t<-Group_t(list(S2.GNETcr3.IB.2_t.E1,S2.GNETcr3.IB.2_t.E2,S2.GNETcr3.IB.2_t.E3,S2.GNETcr3.IB.2_t.E4,S2.GNETcr3.IB.2_t.E5,
                                S2.GNETcr3.IB.2_t.E6,S2.GNETcr3.IB.2_t.E7,S2.GNETcr3.IB.2_t.E8,S2.GNETcr3.IB.2_t.E9,S2.GNETcr3.IB.2_t.E10)) ; Store(S2.GNETcr3.IB.2_t)

#to check if ok
graph3(S2.GNETcr3.IB.2_t,S2.GNETcr3.IB.2)

#####################################################################
#####################################################################
# Compute simple averages
#####################################################################
#####################################################################

S2.GNET1R.MIX_t<-(S2.GNET1R_t+S2.GNET1R.2_t)/2 ; Store(S2.GNET1R.MIX_t)
S2.SVM3.MIX_t<-(S2.SVM3_t+S2.SVM3.2_t+S2.SVM3.IB_t+S2.SVM3.IB.2_t)/4 ; Store(S2.SVM3.MIX_t)
S2.RAD2.MIX_t<-(S2.RAD2_t+S2.RAD2.2_t+S2.RAD2.IB_t+S2.RAD2.IB.2_t)/4 ; Store(S2.RAD2.MIX_t)
S2.SVMc3.MIX_t<-(S2.SVMc3b_t+S2.SVMc3.2_t-S2.SVMc3.IB_t-S2.SVMc3b.IB.2_t)/4 ; Store(S2.SVMc3.MIX_t)
S2.RADc2.MIX_t<-(S2.RADc2_t+S2.RADc2.2_t-S2.RADc2.IB_t-S2.RADc2.IB.2_t)/4 ; Store(S2.RADc2.MIX_t)
S2.SVMm3.MIX_t<-(S2.SVMm3_t+S2.SVMm3.2_t+S2.SVMm3.IB_t+S2.SVMm3.IB.2_t)/4 ; Store(S2.SVMm3.MIX_t)
S2.RADm2.MIX_t<-(S2.RADm2_t+S2.RADm2.2_t+S2.RADm2.IB_t+S2.RADm2.IB.2_t)/4 ; Store(S2.RADm2.MIX_t)
S2.GNETns1.MIX_t<-(S2.GNETns1_t+S2.GNETns1.2_t+S2.GNETns1.IB_t+S2.GNETns1.IB.2_t)/4 ; Store(S2.GNETns1.MIX_t)
S2.GNETns3.MIX_t<-(S2.GNETns3_t+S2.GNETns3.2_t+S2.GNETns3.IB_t+S2.GNETns3.IB.2_t)/4 ; Store(S2.GNETns3.MIX_t)
S2.GNETcr3.MIX_t<-(S2.GNETcr3_t+S2.GNETcr3.2_t+S2.GNETcr3.IB_t+S2.GNETcr3.IB.2_t)/4 ; Store(S2.GNETcr3.MIX_t)

# to check if ok
graph3(S2.GNET1R.MIX_t,S2.GNET1R.MIX)
graph3(S2.SVM3.MIX_t,S2.SVM3.MIX)
graph3(S2.RAD2.MIX_t,S2.RAD2.MIX)
graph3(S2.SVMc3.MIX_t,S2.SVMc3.MIX)
graph3(S2.RADc2.MIX_t,S2.RADc2.MIX)
graph3(S2.SVMm3.MIX_t,S2.SVMm3.MIX) # set 3 weird
graph3(SVMm3.MIX_t,SVMm3.MIX) # set 3 weird
graph3(S2.RADm2.MIX_t,S2.RADm2.MIX)
graph3(S2.GNETns1.MIX_t,S2.GNETns1.MIX)
graph3(S2.GNETns3.MIX_t,S2.GNETns3.MIX)
graph3(S2.GNETcr3.MIX_t,S2.GNETcr3.MIX)

#####################################################################
#####################################################################
# Predict GBM on reduced matrix with proxies
#####################################################################
#####################################################################

library(gbm)
SEL_GBM1.step_t.E1<-predict(SEL_GBM1.step.E1$fit_full_set,COMBGP1_t.E1,n.trees=SEL_GBM1.step.E1$best_ntrees) ; Store(SEL_GBM1.step_t.E1)
SEL_GBM1.step_t.E2<-predict(SEL_GBM1.step.E2$fit_full_set,COMBGP1_t.E2,n.trees=SEL_GBM1.step.E2$best_ntrees) ; Store(SEL_GBM1.step_t.E2)
SEL_GBM1.step_t.E3<-predict(SEL_GBM1.step.E3$fit_full_set,COMBGP1_t.E3,n.trees=SEL_GBM1.step.E3$best_ntrees) ; Store(SEL_GBM1.step_t.E3)
SEL_GBM1.step_t.E4<-predict(SEL_GBM1.step.E4$fit_full_set,COMBGP1_t.E4,n.trees=SEL_GBM1.step.E4$best_ntrees) ; Store(SEL_GBM1.step_t.E4)
SEL_GBM1.step_t.E5<-predict(SEL_GBM1.step.E5$fit_full_set,COMBGP1_t.E5,n.trees=SEL_GBM1.step.E5$best_ntrees) ; Store(SEL_GBM1.step_t.E5)
SEL_GBM1.step_t.E6<-predict(SEL_GBM1.step.E6$fit_full_set,COMBGP1_t.E6,n.trees=SEL_GBM1.step.E6$best_ntrees) ; Store(SEL_GBM1.step_t.E6)
SEL_GBM1.step_t.E7<-predict(SEL_GBM1.step.E7$fit_full_set,COMBGP1_t.E7,n.trees=SEL_GBM1.step.E7$best_ntrees) ; Store(SEL_GBM1.step_t.E7)
SEL_GBM1.step_t.E8<-predict(SEL_GBM1.step.E8$fit_full_set,COMBGP1_t.E8,n.trees=SEL_GBM1.step.E8$best_ntrees) ; Store(SEL_GBM1.step_t.E8)
SEL_GBM1.step_t.E9<-predict(SEL_GBM1.step.E9$fit_full_set,COMBGP1_t.E9,n.trees=SEL_GBM1.step.E9$best_ntrees) ; Store(SEL_GBM1.step_t.E9)
SEL_GBM1.step_t.E10<-predict(SEL_GBM1.step.E10$fit_full_set,COMBGP1_t.E10,n.trees=SEL_GBM1.step.E10$best_ntrees) ; Store(SEL_GBM1.step_t.E10)

SEL_GBM1.step_t<-Group_t(list(SEL_GBM1.step_t.E1,SEL_GBM1.step_t.E2,SEL_GBM1.step_t.E3,SEL_GBM1.step_t.E4,SEL_GBM1.step_t.E5,
                        SEL_GBM1.step_t.E6,SEL_GBM1.step_t.E7,SEL_GBM1.step_t.E8,SEL_GBM1.step_t.E9,SEL_GBM1.step_t.E10)) ; Store(SEL_GBM1.step_t)

#to check if ok
graph3(SEL_GBM1.step_t,SEL_GBM1.step)


#####################################################################
#####################################################################
# dataset of individual predictions to blend
#####################################################################
#####################################################################

TO_Blend_t<-data.frame(y=NA,
                         SEL_GBM1.step_t,SEL_RF1_t,GNET1R_t,SVM3_t,RAD_t,RAD2_t,SVMc3b_t,RADc2_t,SVMm3_t,RADm2_t,GNETns1_t,GNETns3_t,GNETcr3_t,
                         
                         SVM3.IB_t,RAD2.IB_t,SVMc3.IB_t,RADc2.IB_t,SVMm3.IB_t,RADm2.IB_t,GNETns1.IB_t,GNETns3.IB_t,GNETcr3.IB_t,
                         
                         SEL_RF1.2_t,GNET1R.2_t,SVM3.2_t,RAD2.2_t,SVMc3.2_t,RADc2.2_t,SVMm3.2_t,RADm2.2_t,
                         GNETns1.2_t,GNETns3.2_t,GNETcr3.2_t,
                         
                         SVM3.IB.2_t,RAD2.IB.2_t,SVMc3b.IB.2_t,RADc2.IB.2_t,SVMm3.IB.2_t,RADm2.IB.2_t,
                         GNETns1.IB.2_t,GNETns3.IB.2_t,GNETcr3.IB.2_t,
                         
                         SEL_RF1.MIX_t,GNET1R.MIX_t,SVM3.MIX_t,RAD2.MIX_t,SVMc3.MIX_t,RADc2.MIX_t,
                         SVMm3.MIX_t,RADm2.MIX_t,GNETns1.MIX_t,GNETns3.MIX_t,GNETcr3.MIX_t,
                         
                         S2.GNET1R_t,S2.SVM3_t,S2.RAD_t,S2.RAD2_t,S2.SVMc3b_t,S2.RADc2_t,S2.SVMm3_t,
                         S2.RADm2_t,S2.GNETns1_t,S2.GNETns3_t,S2.GNETcr3_t,
                         
                         S2.SVM3.IB_t,S2.RAD2.IB_t,S2.SVMc3.IB_t,S2.RADc2.IB_t,S2.SVMm3.IB_t,S2.RADm2.IB_t,
                         S2.GNETns1.IB_t,S2.GNETns3.IB_t,S2.GNETcr3.IB_t,
                         
                         S2.GNET1R.2_t,S2.SVM3.2_t,S2.RAD2.2_t,S2.SVMc3.2_t,S2.RADc2.2_t,S2.SVMm3.2_t,S2.RADm2.2_t,
                         S2.GNETns1.2_t,S2.GNETns3.2_t,S2.GNETcr3.2_t,
                         
                         S2.SVM3.IB.2_t,S2.RAD2.IB.2_t,S2.SVMc3b.IB.2_t,S2.RADc2.IB.2_t,S2.SVMm3.IB.2_t,S2.RADm2.IB.2_t,
                         S2.GNETns1.IB.2_t,S2.GNETns3.IB.2_t,S2.GNETcr3.IB.2_t,
                         
                         S2.GNET1R.MIX_t,S2.SVM3.MIX_t,S2.RAD2.MIX_t,S2.SVMc3.MIX_t,S2.RADc2.MIX_t,S2.SVMm3.MIX_t,
                         S2.RADm2.MIX_t,S2.GNETns1.MIX_t,S2.GNETns3.MIX_t,S2.GNETcr3.MIX_t)
names(TO_Blend_t)<-names(TO_Blend)

TO_Blend_t.E1<-TO_Blend_t[test$EssaySet==1,] ; Store(TO_Blend_t.E1)
TO_Blend_t.E2<-TO_Blend_t[test$EssaySet==2,] ; Store(TO_Blend_t.E2)
TO_Blend_t.E3<-TO_Blend_t[test$EssaySet==3,] ; Store(TO_Blend_t.E3)
TO_Blend_t.E4<-TO_Blend_t[test$EssaySet==4,] ; Store(TO_Blend_t.E4)
TO_Blend_t.E5<-TO_Blend_t[test$EssaySet==5,] ; Store(TO_Blend_t.E5)
TO_Blend_t.E6<-TO_Blend_t[test$EssaySet==6,] ; Store(TO_Blend_t.E6)
TO_Blend_t.E7<-TO_Blend_t[test$EssaySet==7,] ; Store(TO_Blend_t.E7)
TO_Blend_t.E8<-TO_Blend_t[test$EssaySet==8,] ; Store(TO_Blend_t.E8)
TO_Blend_t.E9<-TO_Blend_t[test$EssaySet==9,] ; Store(TO_Blend_t.E9)
TO_Blend_t.E10<-TO_Blend_t[test$EssaySet==10,] ; Store(TO_Blend_t.E10)

#####################################################################
# PREDICT blend with GNET
#####################################################################

library(glmnet)

BB_GNET1_t.E1<-predict(BB_GNET1.E1[[1]][[6]],as.matrix(TO_Blend_t.E1[,-1]),type="response", s="lambda.min")[,1]
BB_GNET1_t.E2<-predict(BB_GNET1.E2[[1]][[6]],as.matrix(TO_Blend_t.E2[,-1]),type="response", s="lambda.min")[,1]
BB_GNET1_t.E3<-predict(BB_GNET1.E3[[1]][[6]],as.matrix(TO_Blend_t.E3[,-1]),type="response", s="lambda.min")[,1]
BB_GNET1_t.E4<-predict(BB_GNET1.E4[[1]][[6]],as.matrix(TO_Blend_t.E4[,-1]),type="response", s="lambda.min")[,1]
BB_GNET1_t.E5<-predict(BB_GNET1.E5[[1]][[6]],as.matrix(TO_Blend_t.E5[,-1]),type="response", s="lambda.min")[,1]
BB_GNET1_t.E6<-predict(BB_GNET1.E6[[1]][[6]],as.matrix(TO_Blend_t.E6[,-1]),type="response", s="lambda.min")[,1]
BB_GNET1_t.E7<-predict(BB_GNET1.E7[[1]][[6]],as.matrix(TO_Blend_t.E7[,-1]),type="response", s="lambda.min")[,1]
BB_GNET1_t.E8<-predict(BB_GNET1.E8[[1]][[6]],as.matrix(TO_Blend_t.E8[,-1]),type="response", s="lambda.min")[,1]
BB_GNET1_t.E9<-predict(BB_GNET1.E9[[1]][[6]],as.matrix(TO_Blend_t.E9[,-1]),type="response", s="lambda.min")[,1]
BB_GNET1_t.E10<-predict(BB_GNET1.E10[[1]][[6]],as.matrix(TO_Blend_t.E10[,-1]),type="response", s="lambda.min")[,1]
Store(BB_GNET1_t.E1)
Store(BB_GNET1_t.E2)
Store(BB_GNET1_t.E3)
Store(BB_GNET1_t.E4)
Store(BB_GNET1_t.E5)
Store(BB_GNET1_t.E6)
Store(BB_GNET1_t.E7)
Store(BB_GNET1_t.E8)
Store(BB_GNET1_t.E9)
Store(BB_GNET1_t.E10)
BB_GNET1_t<-Group_t(list(BB_GNET1_t.E1,BB_GNET1_t.E2,BB_GNET1_t.E3,BB_GNET1_t.E4,BB_GNET1_t.E5,
                       BB_GNET1_t.E6,BB_GNET1_t.E7,BB_GNET1_t.E8,BB_GNET1_t.E9,BB_GNET1_t.E10)) ; Store(BB_GNET1_t)

#to check if ok
graph3(BB_GNET1_t,BB_GNET1)

#####################################################################
# PREDICT blend with GAM
#####################################################################

library(mgcv)

BB_GAM1_t.E1<-predict(BB_GAM1.E1[[1]][[6]],TO_Blend_t.E1[,-1],type="response")
BB_GAM1_t.E2<-predict(BB_GAM1.E2[[1]][[6]],TO_Blend_t.E2[,-1],type="response")
BB_GAM1_t.E3<-predict(BB_GAM1.E3[[1]][[6]],TO_Blend_t.E3[,-1],type="response")
BB_GAM1_t.E4<-predict(BB_GAM1.E4[[1]][[6]],TO_Blend_t.E4[,-1],type="response")
BB_GAM1_t.E5<-predict(BB_GAM1.E5[[1]][[6]],TO_Blend_t.E5[,-1],type="response")
BB_GAM1_t.E6<-predict(BB_GAM1.E6[[1]][[6]],TO_Blend_t.E6[,-1],type="response")
BB_GAM1_t.E7<-predict(BB_GAM1.E7[[1]][[6]],TO_Blend_t.E7[,-1],type="response")
BB_GAM1_t.E8<-predict(BB_GAM1.E8[[1]][[6]],TO_Blend_t.E8[,-1],type="response")
BB_GAM1_t.E9<-predict(BB_GAM1.E9[[1]][[6]],TO_Blend_t.E9[,-1],type="response")
BB_GAM1_t.E10<-predict(BB_GAM1.E10[[1]][[6]],TO_Blend_t.E10[,-1],type="response")
Store(BB_GAM1_t.E1)
Store(BB_GAM1_t.E2)
Store(BB_GAM1_t.E3)
Store(BB_GAM1_t.E4)
Store(BB_GAM1_t.E5)
Store(BB_GAM1_t.E6)
Store(BB_GAM1_t.E7)
Store(BB_GAM1_t.E8)
Store(BB_GAM1_t.E9)
Store(BB_GAM1_t.E10)

BB_GAM1_t<-Group_t(list(BB_GAM1_t.E1,BB_GAM1_t.E2,BB_GAM1_t.E3,BB_GAM1_t.E4,BB_GAM1_t.E5,
                          BB_GAM1_t.E6,BB_GAM1_t.E7,BB_GAM1_t.E8,BB_GAM1_t.E9,BB_GAM1_t.E10)) ; Store(BB_GAM1_t)

#to check if ok
graph3(BB_GAM1_t,BB_GAM1)

#####################################################################
#####################################################################
# Blend of Blend with LM
#####################################################################
#####################################################################

TO_Blendx2_t<-data.frame(y=NA,BB_GNET1_t,BB_GAM1_t)
names(TO_Blendx2_t)<-names(TO_Blendx2)

TO_Blendx2_t.E1<-TO_Blendx2_t[test$EssaySet==1,] ; Store(TO_Blendx2_t.E1)
TO_Blendx2_t.E2<-TO_Blendx2_t[test$EssaySet==2,] ; Store(TO_Blendx2_t.E2)
TO_Blendx2_t.E3<-TO_Blendx2_t[test$EssaySet==3,] ; Store(TO_Blendx2_t.E3)
TO_Blendx2_t.E4<-TO_Blendx2_t[test$EssaySet==4,] ; Store(TO_Blendx2_t.E4)
TO_Blendx2_t.E5<-TO_Blendx2_t[test$EssaySet==5,] ; Store(TO_Blendx2_t.E5)
TO_Blendx2_t.E6<-TO_Blendx2_t[test$EssaySet==6,] ; Store(TO_Blendx2_t.E6)
TO_Blendx2_t.E7<-TO_Blendx2_t[test$EssaySet==7,] ; Store(TO_Blendx2_t.E7)
TO_Blendx2_t.E8<-TO_Blendx2_t[test$EssaySet==8,] ; Store(TO_Blendx2_t.E8)
TO_Blendx2_t.E9<-TO_Blendx2_t[test$EssaySet==9,] ; Store(TO_Blendx2_t.E9)
TO_Blendx2_t.E10<-TO_Blendx2_t[test$EssaySet==10,] ; Store(TO_Blendx2_t.E10)

#####################################################################
# PREDICT blend with LM
#####################################################################

BBB_LM1_t.E1<-predict(BBB_LM1.E1[[1]][[6]],TO_Blendx2_t.E1[,-1])
BBB_LM1_t.E2<-predict(BBB_LM1.E2[[1]][[6]],TO_Blendx2_t.E2[,-1])
BBB_LM1_t.E3<-predict(BBB_LM1.E3[[1]][[6]],TO_Blendx2_t.E3[,-1])
BBB_LM1_t.E4<-predict(BBB_LM1.E4[[1]][[6]],TO_Blendx2_t.E4[,-1])
BBB_LM1_t.E5<-predict(BBB_LM1.E5[[1]][[6]],TO_Blendx2_t.E5[,-1])
BBB_LM1_t.E6<-predict(BBB_LM1.E6[[1]][[6]],TO_Blendx2_t.E6[,-1])
BBB_LM1_t.E7<-predict(BBB_LM1.E7[[1]][[6]],TO_Blendx2_t.E7[,-1])
BBB_LM1_t.E8<-predict(BBB_LM1.E8[[1]][[6]],TO_Blendx2_t.E8[,-1])
BBB_LM1_t.E9<-predict(BBB_LM1.E9[[1]][[6]],TO_Blendx2_t.E9[,-1])
BBB_LM1_t.E10<-predict(BBB_LM1.E10[[1]][[6]],TO_Blendx2_t.E10[,-1])
Store(BBB_LM1_t.E1)
Store(BBB_LM1_t.E2)
Store(BBB_LM1_t.E3)
Store(BBB_LM1_t.E4)
Store(BBB_LM1_t.E5)
Store(BBB_LM1_t.E6)
Store(BBB_LM1_t.E7)
Store(BBB_LM1_t.E8)
Store(BBB_LM1_t.E9)
Store(BBB_LM1_t.E10)
BBB_LM1_t<-Group_t(list(BBB_LM1_t.E1,BBB_LM1_t.E2,BBB_LM1_t.E3,BBB_LM1_t.E4,BBB_LM1_t.E5,
                        BBB_LM1_t.E6,BBB_LM1_t.E7,BBB_LM1_t.E8,BBB_LM1_t.E9,BBB_LM1_t.E10)) ; Store(BBB_LM1_t)

#to check if ok
graph3(BBB_LM1_t,BBB_LM1)

#####################################################################
#####################################################################
# adjust BB_GNET1 predictions ADJ (2p)
#####################################################################
#####################################################################

conv_ADJ1d<-function(x,xx) round(xx[1] + xx[2]*x,0)

BB_GNET1_OPT1d_t.E1<-conv_ADJ1d(BB_GNET1_t.E1,BB_GNET1_OPT1d.E1[[1]][[6]]$par) ; Store(BB_GNET1_OPT1d_t.E1)
BB_GNET1_OPT1d_t.E2<-conv_ADJ1d(BB_GNET1_t.E2,BB_GNET1_OPT1d.E2[[1]][[6]]$par) ; Store(BB_GNET1_OPT1d_t.E2)
BB_GNET1_OPT1d_t.E3<-conv_ADJ1d(BB_GNET1_t.E3,BB_GNET1_OPT1d.E3[[1]][[6]]$par) ; Store(BB_GNET1_OPT1d_t.E3)
BB_GNET1_OPT1d_t.E4<-conv_ADJ1d(BB_GNET1_t.E4,BB_GNET1_OPT1d.E4[[1]][[6]]$par) ; Store(BB_GNET1_OPT1d_t.E4)
BB_GNET1_OPT1d_t.E5<-conv_ADJ1d(BB_GNET1_t.E5,BB_GNET1_OPT1d.E5[[1]][[6]]$par) ; Store(BB_GNET1_OPT1d_t.E5)
BB_GNET1_OPT1d_t.E6<-conv_ADJ1d(BB_GNET1_t.E6,BB_GNET1_OPT1d.E6[[1]][[6]]$par) ; Store(BB_GNET1_OPT1d_t.E6)
BB_GNET1_OPT1d_t.E7<-conv_ADJ1d(BB_GNET1_t.E7,BB_GNET1_OPT1d.E7[[1]][[6]]$par) ; Store(BB_GNET1_OPT1d_t.E7)
BB_GNET1_OPT1d_t.E8<-conv_ADJ1d(BB_GNET1_t.E8,BB_GNET1_OPT1d.E8[[1]][[6]]$par) ; Store(BB_GNET1_OPT1d_t.E8)
BB_GNET1_OPT1d_t.E9<-conv_ADJ1d(BB_GNET1_t.E9,BB_GNET1_OPT1d.E9[[1]][[6]]$par) ; Store(BB_GNET1_OPT1d_t.E9)
BB_GNET1_OPT1d_t.E10<-conv_ADJ1d(BB_GNET1_t.E10,BB_GNET1_OPT1d.E10[[1]][[6]]$par) ; Store(BB_GNET1_OPT1d_t.E10)

#####################################################################
#cap predicted values range with possible score range
#####################################################################

BB_GNET1_OPT1d_t.E1<-range_ADJ(BB_GNET1_OPT1d_t.E1,score_range[1,]) ; Store(BB_GNET1_OPT1d_t.E1)
BB_GNET1_OPT1d_t.E2<-range_ADJ(BB_GNET1_OPT1d_t.E2,score_range[2,]) ; Store(BB_GNET1_OPT1d_t.E2)
BB_GNET1_OPT1d_t.E3<-range_ADJ(BB_GNET1_OPT1d_t.E3,score_range[3,]) ; Store(BB_GNET1_OPT1d_t.E3)
BB_GNET1_OPT1d_t.E4<-range_ADJ(BB_GNET1_OPT1d_t.E4,score_range[4,]) ; Store(BB_GNET1_OPT1d_t.E4)
BB_GNET1_OPT1d_t.E5<-range_ADJ(BB_GNET1_OPT1d_t.E5,score_range[5,]) ; Store(BB_GNET1_OPT1d_t.E5)
BB_GNET1_OPT1d_t.E6<-range_ADJ(BB_GNET1_OPT1d_t.E6,score_range[6,]) ; Store(BB_GNET1_OPT1d_t.E6)
BB_GNET1_OPT1d_t.E7<-range_ADJ(BB_GNET1_OPT1d_t.E7,score_range[7,]) ; Store(BB_GNET1_OPT1d_t.E7)
BB_GNET1_OPT1d_t.E8<-range_ADJ(BB_GNET1_OPT1d_t.E8,score_range[8,]) ; Store(BB_GNET1_OPT1d_t.E8)
BB_GNET1_OPT1d_t.E9<-range_ADJ(BB_GNET1_OPT1d_t.E9,score_range[9,]) ; Store(BB_GNET1_OPT1d_t.E9)
BB_GNET1_OPT1d_t.E10<-range_ADJ(BB_GNET1_OPT1d_t.E10,score_range[10,]) ; Store(BB_GNET1_OPT1d_t.E10)

BB_GNET1_OPT1d_t<-Group_t(list(BB_GNET1_OPT1d_t.E1,BB_GNET1_OPT1d_t.E2,BB_GNET1_OPT1d_t.E3,BB_GNET1_OPT1d_t.E4,BB_GNET1_OPT1d_t.E5,
                               BB_GNET1_OPT1d_t.E6,BB_GNET1_OPT1d_t.E7,BB_GNET1_OPT1d_t.E8,BB_GNET1_OPT1d_t.E9,BB_GNET1_OPT1d_t.E10)) ; Store(BB_GNET1_OPT1d_t)

#to check if ok
graph3(BB_GNET1_OPT1d_t,BB_GNET1_OPT1d)

#####################################################################
#####################################################################
# adjust BB_GAM1 predictions ADJ (2p)
#####################################################################
#####################################################################

BB_GAM1_OPT1d_t.E1<-conv_ADJ1d(BB_GAM1_t.E1,BB_GAM1_OPT1d.E1[[1]][[6]]$par) ; Store(BB_GAM1_OPT1d_t.E1)
BB_GAM1_OPT1d_t.E2<-conv_ADJ1d(BB_GAM1_t.E2,BB_GAM1_OPT1d.E2[[1]][[6]]$par) ; Store(BB_GAM1_OPT1d_t.E2)
BB_GAM1_OPT1d_t.E3<-conv_ADJ1d(BB_GAM1_t.E3,BB_GAM1_OPT1d.E3[[1]][[6]]$par) ; Store(BB_GAM1_OPT1d_t.E3)
BB_GAM1_OPT1d_t.E4<-conv_ADJ1d(BB_GAM1_t.E4,BB_GAM1_OPT1d.E4[[1]][[6]]$par) ; Store(BB_GAM1_OPT1d_t.E4)
BB_GAM1_OPT1d_t.E5<-conv_ADJ1d(BB_GAM1_t.E5,BB_GAM1_OPT1d.E5[[1]][[6]]$par) ; Store(BB_GAM1_OPT1d_t.E5)
BB_GAM1_OPT1d_t.E6<-conv_ADJ1d(BB_GAM1_t.E6,BB_GAM1_OPT1d.E6[[1]][[6]]$par) ; Store(BB_GAM1_OPT1d_t.E6)
BB_GAM1_OPT1d_t.E7<-conv_ADJ1d(BB_GAM1_t.E7,BB_GAM1_OPT1d.E7[[1]][[6]]$par) ; Store(BB_GAM1_OPT1d_t.E7)
BB_GAM1_OPT1d_t.E8<-conv_ADJ1d(BB_GAM1_t.E8,BB_GAM1_OPT1d.E8[[1]][[6]]$par) ; Store(BB_GAM1_OPT1d_t.E8)
BB_GAM1_OPT1d_t.E9<-conv_ADJ1d(BB_GAM1_t.E9,BB_GAM1_OPT1d.E9[[1]][[6]]$par) ; Store(BB_GAM1_OPT1d_t.E9)
BB_GAM1_OPT1d_t.E10<-conv_ADJ1d(BB_GAM1_t.E10,BB_GAM1_OPT1d.E10[[1]][[6]]$par) ; Store(BB_GAM1_OPT1d_t.E10)

#####################################################################
#cap predicted values range with possible score range
#####################################################################

BB_GAM1_OPT1d_t.E1<-range_ADJ(BB_GAM1_OPT1d_t.E1,score_range[1,]) ; Store(BB_GAM1_OPT1d_t.E1)
BB_GAM1_OPT1d_t.E2<-range_ADJ(BB_GAM1_OPT1d_t.E2,score_range[2,]) ; Store(BB_GAM1_OPT1d_t.E2)
BB_GAM1_OPT1d_t.E3<-range_ADJ(BB_GAM1_OPT1d_t.E3,score_range[3,]) ; Store(BB_GAM1_OPT1d_t.E3)
BB_GAM1_OPT1d_t.E4<-range_ADJ(BB_GAM1_OPT1d_t.E4,score_range[4,]) ; Store(BB_GAM1_OPT1d_t.E4)
BB_GAM1_OPT1d_t.E5<-range_ADJ(BB_GAM1_OPT1d_t.E5,score_range[5,]) ; Store(BB_GAM1_OPT1d_t.E5)
BB_GAM1_OPT1d_t.E6<-range_ADJ(BB_GAM1_OPT1d_t.E6,score_range[6,]) ; Store(BB_GAM1_OPT1d_t.E6)
BB_GAM1_OPT1d_t.E7<-range_ADJ(BB_GAM1_OPT1d_t.E7,score_range[7,]) ; Store(BB_GAM1_OPT1d_t.E7)
BB_GAM1_OPT1d_t.E8<-range_ADJ(BB_GAM1_OPT1d_t.E8,score_range[8,]) ; Store(BB_GAM1_OPT1d_t.E8)
BB_GAM1_OPT1d_t.E9<-range_ADJ(BB_GAM1_OPT1d_t.E9,score_range[9,]) ; Store(BB_GAM1_OPT1d_t.E9)
BB_GAM1_OPT1d_t.E10<-range_ADJ(BB_GAM1_OPT1d_t.E10,score_range[10,]) ; Store(BB_GAM1_OPT1d_t.E10)

BB_GAM1_OPT1d_t<-Group_t(list(BB_GAM1_OPT1d_t.E1,BB_GAM1_OPT1d_t.E2,BB_GAM1_OPT1d_t.E3,BB_GAM1_OPT1d_t.E4,BB_GAM1_OPT1d_t.E5,
                              BB_GAM1_OPT1d_t.E6,BB_GAM1_OPT1d_t.E7,BB_GAM1_OPT1d_t.E8,BB_GAM1_OPT1d_t.E9,BB_GAM1_OPT1d_t.E10)) ; Store(BB_GAM1_OPT1d_t)

#to check if ok
graph3(BB_GAM1_OPT1d_t,BB_GAM1_OPT1d)

#####################################################################
#####################################################################
# adjust BBB_LM1 predictions ADJ (2p)
#####################################################################
#####################################################################

BBB_LM1_OPT1d_t.E1<-conv_ADJ1d(BBB_LM1_t.E1,BBB_LM1_OPT1d.E1[[1]][[6]]$par) ; Store(BBB_LM1_OPT1d_t.E1)
BBB_LM1_OPT1d_t.E2<-conv_ADJ1d(BBB_LM1_t.E2,BBB_LM1_OPT1d.E2[[1]][[6]]$par) ; Store(BBB_LM1_OPT1d_t.E2)
BBB_LM1_OPT1d_t.E3<-conv_ADJ1d(BBB_LM1_t.E3,BBB_LM1_OPT1d.E3[[1]][[6]]$par) ; Store(BBB_LM1_OPT1d_t.E3)
BBB_LM1_OPT1d_t.E4<-conv_ADJ1d(BBB_LM1_t.E4,BBB_LM1_OPT1d.E4[[1]][[6]]$par) ; Store(BBB_LM1_OPT1d_t.E4)
BBB_LM1_OPT1d_t.E5<-conv_ADJ1d(BBB_LM1_t.E5,BBB_LM1_OPT1d.E5[[1]][[6]]$par) ; Store(BBB_LM1_OPT1d_t.E5)
BBB_LM1_OPT1d_t.E6<-conv_ADJ1d(BBB_LM1_t.E6,BBB_LM1_OPT1d.E6[[1]][[6]]$par) ; Store(BBB_LM1_OPT1d_t.E6)
BBB_LM1_OPT1d_t.E7<-conv_ADJ1d(BBB_LM1_t.E7,BBB_LM1_OPT1d.E7[[1]][[6]]$par) ; Store(BBB_LM1_OPT1d_t.E7)
BBB_LM1_OPT1d_t.E8<-conv_ADJ1d(BBB_LM1_t.E8,BBB_LM1_OPT1d.E8[[1]][[6]]$par) ; Store(BBB_LM1_OPT1d_t.E8)
BBB_LM1_OPT1d_t.E9<-conv_ADJ1d(BBB_LM1_t.E9,BBB_LM1_OPT1d.E9[[1]][[6]]$par) ; Store(BBB_LM1_OPT1d_t.E9)
BBB_LM1_OPT1d_t.E10<-conv_ADJ1d(BBB_LM1_t.E10,BBB_LM1_OPT1d.E10[[1]][[6]]$par) ; Store(BBB_LM1_OPT1d_t.E10)

#####################################################################
#cap predicted values range with possible score range
#####################################################################

BBB_LM1_OPT1d_t.E1<-range_ADJ(BBB_LM1_OPT1d_t.E1,score_range[1,]) ; Store(BBB_LM1_OPT1d_t.E1)
BBB_LM1_OPT1d_t.E2<-range_ADJ(BBB_LM1_OPT1d_t.E2,score_range[2,]) ; Store(BBB_LM1_OPT1d_t.E2)
BBB_LM1_OPT1d_t.E3<-range_ADJ(BBB_LM1_OPT1d_t.E3,score_range[3,]) ; Store(BBB_LM1_OPT1d_t.E3)
BBB_LM1_OPT1d_t.E4<-range_ADJ(BBB_LM1_OPT1d_t.E4,score_range[4,]) ; Store(BBB_LM1_OPT1d_t.E4)
BBB_LM1_OPT1d_t.E5<-range_ADJ(BBB_LM1_OPT1d_t.E5,score_range[5,]) ; Store(BBB_LM1_OPT1d_t.E5)
BBB_LM1_OPT1d_t.E6<-range_ADJ(BBB_LM1_OPT1d_t.E6,score_range[6,]) ; Store(BBB_LM1_OPT1d_t.E6)
BBB_LM1_OPT1d_t.E7<-range_ADJ(BBB_LM1_OPT1d_t.E7,score_range[7,]) ; Store(BBB_LM1_OPT1d_t.E7)
BBB_LM1_OPT1d_t.E8<-range_ADJ(BBB_LM1_OPT1d_t.E8,score_range[8,]) ; Store(BBB_LM1_OPT1d_t.E8)
BBB_LM1_OPT1d_t.E9<-range_ADJ(BBB_LM1_OPT1d_t.E9,score_range[9,]) ; Store(BBB_LM1_OPT1d_t.E9)
BBB_LM1_OPT1d_t.E10<-range_ADJ(BBB_LM1_OPT1d_t.E10,score_range[10,]) ; Store(BBB_LM1_OPT1d_t.E10)

BBB_LM1_OPT1d_t<-Group_t(list(BBB_LM1_OPT1d_t.E1,BBB_LM1_OPT1d_t.E2,BBB_LM1_OPT1d_t.E3,BBB_LM1_OPT1d_t.E4,BBB_LM1_OPT1d_t.E5,
                              BBB_LM1_OPT1d_t.E6,BBB_LM1_OPT1d_t.E7,BBB_LM1_OPT1d_t.E8,BBB_LM1_OPT1d_t.E9,BBB_LM1_OPT1d_t.E10)) ; Store(BBB_LM1_OPT1d_t)

#to check if ok
graph3(BBB_LM1_OPT1d_t,BBB_LM1_OPT1d)

#####################################################################
#####################################################################
# adjust BB_GNET1 predictions ADJ (3p)
#####################################################################
#####################################################################

conv_ADJ2d<-function(x,xx) round(xx[1] + xx[2]*x + xx[3]*x^2,0)
BB_GNET1_OPT2d_t.E1<-conv_ADJ2d(BB_GNET1_t.E1,BB_GNET1_OPT2d.E1[[1]][[6]]$par) ; Store(BB_GNET1_OPT2d_t.E1)
BB_GNET1_OPT2d_t.E2<-conv_ADJ2d(BB_GNET1_t.E2,BB_GNET1_OPT2d.E2[[1]][[6]]$par) ; Store(BB_GNET1_OPT2d_t.E2)
BB_GNET1_OPT2d_t.E3<-conv_ADJ2d(BB_GNET1_t.E3,BB_GNET1_OPT2d.E3[[1]][[6]]$par) ; Store(BB_GNET1_OPT2d_t.E3)
BB_GNET1_OPT2d_t.E4<-conv_ADJ2d(BB_GNET1_t.E4,BB_GNET1_OPT2d.E4[[1]][[6]]$par) ; Store(BB_GNET1_OPT2d_t.E4)
BB_GNET1_OPT2d_t.E5<-conv_ADJ2d(BB_GNET1_t.E5,BB_GNET1_OPT2d.E5[[1]][[6]]$par) ; Store(BB_GNET1_OPT2d_t.E5)
BB_GNET1_OPT2d_t.E6<-conv_ADJ2d(BB_GNET1_t.E6,BB_GNET1_OPT2d.E6[[1]][[6]]$par) ; Store(BB_GNET1_OPT2d_t.E6)
BB_GNET1_OPT2d_t.E7<-conv_ADJ2d(BB_GNET1_t.E7,BB_GNET1_OPT2d.E7[[1]][[6]]$par) ; Store(BB_GNET1_OPT2d_t.E7)
BB_GNET1_OPT2d_t.E8<-conv_ADJ2d(BB_GNET1_t.E8,BB_GNET1_OPT2d.E8[[1]][[6]]$par) ; Store(BB_GNET1_OPT2d_t.E8)
BB_GNET1_OPT2d_t.E9<-conv_ADJ2d(BB_GNET1_t.E9,BB_GNET1_OPT2d.E9[[1]][[6]]$par) ; Store(BB_GNET1_OPT2d_t.E9)
BB_GNET1_OPT2d_t.E10<-conv_ADJ2d(BB_GNET1_t.E10,BB_GNET1_OPT2d.E10[[1]][[6]]$par) ; Store(BB_GNET1_OPT2d_t.E10)

#####################################################################
#cap predicted values range with possible score range
#####################################################################

BB_GNET1_OPT2d_t.E1<-range_ADJ(BB_GNET1_OPT2d_t.E1,score_range[1,]) ; Store(BB_GNET1_OPT2d_t.E1)
BB_GNET1_OPT2d_t.E2<-range_ADJ(BB_GNET1_OPT2d_t.E2,score_range[2,]) ; Store(BB_GNET1_OPT2d_t.E2)
BB_GNET1_OPT2d_t.E3<-range_ADJ(BB_GNET1_OPT2d_t.E3,score_range[3,]) ; Store(BB_GNET1_OPT2d_t.E3)
BB_GNET1_OPT2d_t.E4<-range_ADJ(BB_GNET1_OPT2d_t.E4,score_range[4,]) ; Store(BB_GNET1_OPT2d_t.E4)
BB_GNET1_OPT2d_t.E5<-range_ADJ(BB_GNET1_OPT2d_t.E5,score_range[5,]) ; Store(BB_GNET1_OPT2d_t.E5)
BB_GNET1_OPT2d_t.E6<-range_ADJ(BB_GNET1_OPT2d_t.E6,score_range[6,]) ; Store(BB_GNET1_OPT2d_t.E6)
BB_GNET1_OPT2d_t.E7<-range_ADJ(BB_GNET1_OPT2d_t.E7,score_range[7,]) ; Store(BB_GNET1_OPT2d_t.E7)
BB_GNET1_OPT2d_t.E8<-range_ADJ(BB_GNET1_OPT2d_t.E8,score_range[8,]) ; Store(BB_GNET1_OPT2d_t.E8)
BB_GNET1_OPT2d_t.E9<-range_ADJ(BB_GNET1_OPT2d_t.E9,score_range[9,]) ; Store(BB_GNET1_OPT2d_t.E9)
BB_GNET1_OPT2d_t.E10<-range_ADJ(BB_GNET1_OPT2d_t.E10,score_range[10,]) ; Store(BB_GNET1_OPT2d_t.E10)

BB_GNET1_OPT2d_t<-Group_t(list(BB_GNET1_OPT2d_t.E1,BB_GNET1_OPT2d_t.E2,BB_GNET1_OPT2d_t.E3,BB_GNET1_OPT2d_t.E4,BB_GNET1_OPT2d_t.E5,
                               BB_GNET1_OPT2d_t.E6,BB_GNET1_OPT2d_t.E7,BB_GNET1_OPT2d_t.E8,BB_GNET1_OPT2d_t.E9,BB_GNET1_OPT2d_t.E10)) ; Store(BB_GNET1_OPT2d_t)

#to check if ok
graph3(BB_GNET1_OPT2d_t,BB_GNET1_OPT2d)

#####################################################################
#####################################################################
# adjust BB_GAM1 predictions ADJ (3p)
#####################################################################
#####################################################################

BB_GAM1_OPT2d_t.E1<-conv_ADJ2d(BB_GAM1_t.E1,BB_GAM1_OPT2d.E1[[1]][[6]]$par) ; Store(BB_GAM1_OPT2d_t.E1)
BB_GAM1_OPT2d_t.E2<-conv_ADJ2d(BB_GAM1_t.E2,BB_GAM1_OPT2d.E2[[1]][[6]]$par) ; Store(BB_GAM1_OPT2d_t.E2)
BB_GAM1_OPT2d_t.E3<-conv_ADJ2d(BB_GAM1_t.E3,BB_GAM1_OPT2d.E3[[1]][[6]]$par) ; Store(BB_GAM1_OPT2d_t.E3)
BB_GAM1_OPT2d_t.E4<-conv_ADJ2d(BB_GAM1_t.E4,BB_GAM1_OPT2d.E4[[1]][[6]]$par) ; Store(BB_GAM1_OPT2d_t.E4)
BB_GAM1_OPT2d_t.E5<-conv_ADJ2d(BB_GAM1_t.E5,BB_GAM1_OPT2d.E5[[1]][[6]]$par) ; Store(BB_GAM1_OPT2d_t.E5)
BB_GAM1_OPT2d_t.E6<-conv_ADJ2d(BB_GAM1_t.E6,BB_GAM1_OPT2d.E6[[1]][[6]]$par) ; Store(BB_GAM1_OPT2d_t.E6)
BB_GAM1_OPT2d_t.E7<-conv_ADJ2d(BB_GAM1_t.E7,BB_GAM1_OPT2d.E7[[1]][[6]]$par) ; Store(BB_GAM1_OPT2d_t.E7)
BB_GAM1_OPT2d_t.E8<-conv_ADJ2d(BB_GAM1_t.E8,BB_GAM1_OPT2d.E8[[1]][[6]]$par) ; Store(BB_GAM1_OPT2d_t.E8)
BB_GAM1_OPT2d_t.E9<-conv_ADJ2d(BB_GAM1_t.E9,BB_GAM1_OPT2d.E9[[1]][[6]]$par) ; Store(BB_GAM1_OPT2d_t.E9)
BB_GAM1_OPT2d_t.E10<-conv_ADJ2d(BB_GAM1_t.E10,BB_GAM1_OPT2d.E10[[1]][[6]]$par) ; Store(BB_GAM1_OPT2d_t.E10)

#####################################################################
#cap predicted values range with possible score range
#####################################################################

BB_GAM1_OPT2d_t.E1<-range_ADJ(BB_GAM1_OPT2d_t.E1,score_range[1,]) ; Store(BB_GAM1_OPT2d_t.E1)
BB_GAM1_OPT2d_t.E2<-range_ADJ(BB_GAM1_OPT2d_t.E2,score_range[2,]) ; Store(BB_GAM1_OPT2d_t.E2)
BB_GAM1_OPT2d_t.E3<-range_ADJ(BB_GAM1_OPT2d_t.E3,score_range[3,]) ; Store(BB_GAM1_OPT2d_t.E3)
BB_GAM1_OPT2d_t.E4<-range_ADJ(BB_GAM1_OPT2d_t.E4,score_range[4,]) ; Store(BB_GAM1_OPT2d_t.E4)
BB_GAM1_OPT2d_t.E5<-range_ADJ(BB_GAM1_OPT2d_t.E5,score_range[5,]) ; Store(BB_GAM1_OPT2d_t.E5)
BB_GAM1_OPT2d_t.E6<-range_ADJ(BB_GAM1_OPT2d_t.E6,score_range[6,]) ; Store(BB_GAM1_OPT2d_t.E6)
BB_GAM1_OPT2d_t.E7<-range_ADJ(BB_GAM1_OPT2d_t.E7,score_range[7,]) ; Store(BB_GAM1_OPT2d_t.E7)
BB_GAM1_OPT2d_t.E8<-range_ADJ(BB_GAM1_OPT2d_t.E8,score_range[8,]) ; Store(BB_GAM1_OPT2d_t.E8)
BB_GAM1_OPT2d_t.E9<-range_ADJ(BB_GAM1_OPT2d_t.E9,score_range[9,]) ; Store(BB_GAM1_OPT2d_t.E9)
BB_GAM1_OPT2d_t.E10<-range_ADJ(BB_GAM1_OPT2d_t.E10,score_range[10,]) ; Store(BB_GAM1_OPT2d_t.E10)

BB_GAM1_OPT2d_t<-Group_t(list(BB_GAM1_OPT2d_t.E1,BB_GAM1_OPT2d_t.E2,BB_GAM1_OPT2d_t.E3,BB_GAM1_OPT2d_t.E4,BB_GAM1_OPT2d_t.E5,
                              BB_GAM1_OPT2d_t.E6,BB_GAM1_OPT2d_t.E7,BB_GAM1_OPT2d_t.E8,BB_GAM1_OPT2d_t.E9,BB_GAM1_OPT2d_t.E10)) ; Store(BB_GAM1_OPT2d_t)

#to check if ok
graph3(BB_GAM1_OPT2d_t,BB_GAM1_OPT2d)

#####################################################################
#####################################################################
# adjust BBB_LM1 predictions ADJ (3p)
#####################################################################
#####################################################################

BBB_LM1_OPT2d_t.E1<-conv_ADJ2d(BBB_LM1_t.E1,BBB_LM1_OPT2d.E1[[1]][[6]]$par) ; Store(BBB_LM1_OPT2d_t.E1)
BBB_LM1_OPT2d_t.E2<-conv_ADJ2d(BBB_LM1_t.E2,BBB_LM1_OPT2d.E2[[1]][[6]]$par) ; Store(BBB_LM1_OPT2d_t.E2)
BBB_LM1_OPT2d_t.E3<-conv_ADJ2d(BBB_LM1_t.E3,BBB_LM1_OPT2d.E3[[1]][[6]]$par) ; Store(BBB_LM1_OPT2d_t.E3)
BBB_LM1_OPT2d_t.E4<-conv_ADJ2d(BBB_LM1_t.E4,BBB_LM1_OPT2d.E4[[1]][[6]]$par) ; Store(BBB_LM1_OPT2d_t.E4)
BBB_LM1_OPT2d_t.E5<-conv_ADJ2d(BBB_LM1_t.E5,BBB_LM1_OPT2d.E5[[1]][[6]]$par) ; Store(BBB_LM1_OPT2d_t.E5)
BBB_LM1_OPT2d_t.E6<-conv_ADJ2d(BBB_LM1_t.E6,BBB_LM1_OPT2d.E6[[1]][[6]]$par) ; Store(BBB_LM1_OPT2d_t.E6)
BBB_LM1_OPT2d_t.E7<-conv_ADJ2d(BBB_LM1_t.E7,BBB_LM1_OPT2d.E7[[1]][[6]]$par) ; Store(BBB_LM1_OPT2d_t.E7)
BBB_LM1_OPT2d_t.E8<-conv_ADJ2d(BBB_LM1_t.E8,BBB_LM1_OPT2d.E8[[1]][[6]]$par) ; Store(BBB_LM1_OPT2d_t.E8)
BBB_LM1_OPT2d_t.E9<-conv_ADJ2d(BBB_LM1_t.E9,BBB_LM1_OPT2d.E9[[1]][[6]]$par) ; Store(BBB_LM1_OPT2d_t.E9)
BBB_LM1_OPT2d_t.E10<-conv_ADJ2d(BBB_LM1_t.E10,BBB_LM1_OPT2d.E10[[1]][[6]]$par) ; Store(BBB_LM1_OPT2d_t.E10)

#####################################################################
#cap predicted values range with possible score range
#####################################################################

BBB_LM1_OPT2d_t.E1<-range_ADJ(BBB_LM1_OPT2d_t.E1,score_range[1,]) ; Store(BBB_LM1_OPT2d_t.E1)
BBB_LM1_OPT2d_t.E2<-range_ADJ(BBB_LM1_OPT2d_t.E2,score_range[2,]) ; Store(BBB_LM1_OPT2d_t.E2)
BBB_LM1_OPT2d_t.E3<-range_ADJ(BBB_LM1_OPT2d_t.E3,score_range[3,]) ; Store(BBB_LM1_OPT2d_t.E3)
BBB_LM1_OPT2d_t.E4<-range_ADJ(BBB_LM1_OPT2d_t.E4,score_range[4,]) ; Store(BBB_LM1_OPT2d_t.E4)
BBB_LM1_OPT2d_t.E5<-range_ADJ(BBB_LM1_OPT2d_t.E5,score_range[5,]) ; Store(BBB_LM1_OPT2d_t.E5)
BBB_LM1_OPT2d_t.E6<-range_ADJ(BBB_LM1_OPT2d_t.E6,score_range[6,]) ; Store(BBB_LM1_OPT2d_t.E6)
BBB_LM1_OPT2d_t.E7<-range_ADJ(BBB_LM1_OPT2d_t.E7,score_range[7,]) ; Store(BBB_LM1_OPT2d_t.E7)
BBB_LM1_OPT2d_t.E8<-range_ADJ(BBB_LM1_OPT2d_t.E8,score_range[8,]) ; Store(BBB_LM1_OPT2d_t.E8)
BBB_LM1_OPT2d_t.E9<-range_ADJ(BBB_LM1_OPT2d_t.E9,score_range[9,]) ; Store(BBB_LM1_OPT2d_t.E9)
BBB_LM1_OPT2d_t.E10<-range_ADJ(BBB_LM1_OPT2d_t.E10,score_range[10,]) ; Store(BBB_LM1_OPT2d_t.E10)

BBB_LM1_OPT2d_t<-Group_t(list(BBB_LM1_OPT2d_t.E1,BBB_LM1_OPT2d_t.E2,BBB_LM1_OPT2d_t.E3,BBB_LM1_OPT2d_t.E4,BBB_LM1_OPT2d_t.E5,
                              BBB_LM1_OPT2d_t.E6,BBB_LM1_OPT2d_t.E7,BBB_LM1_OPT2d_t.E8,BBB_LM1_OPT2d_t.E9,BBB_LM1_OPT2d_t.E10)) ; Store(BBB_LM1_OPT2d_t)

#to check if ok
graph3(BBB_LM1_OPT2d_t,BBB_LM1_OPT2d)


#####################################################################
#####################################################################
# BESTOFF
#####################################################################
#####################################################################

BBB_BESTOFF1_23p_t<-rep(0,nrow(test))
tmp<-data.frame(BB_GNET1_OPT1d,BB_GAM1_OPT1d,BBB_LM1_OPT1d,BB_GNET1_OPT2d,BB_GAM1_OPT2d,BBB_LM1_OPT2d)
tmp2<-data.frame(Kappa_score(BB_GNET1_OPT1d),Kappa_score(BB_GAM1_OPT1d),Kappa_score(BBB_LM1_OPT1d),
                 Kappa_score(BB_GNET1_OPT2d),Kappa_score(BB_GAM1_OPT2d),Kappa_score(BBB_LM1_OPT2d))
tmp2$max<-pmax(tmp2[,1],tmp2[,2],tmp2[,3],tmp2[,4],tmp2[,5],tmp2[,6])
tmp3<-data.frame(BB_GNET1_OPT1d_t,BB_GAM1_OPT1d_t,BBB_LM1_OPT1d_t,BB_GNET1_OPT2d_t,BB_GAM1_OPT2d_t,BBB_LM1_OPT2d_t)
for (i in 1:10) BBB_BESTOFF1_23p_t[test$EssaySet==i]<-tmp3[test$EssaySet==i,which(tmp2[i,1:6]==tmp2[i,7])]  
Store(BBB_BESTOFF1_23p_t)

#to check if ok
graph3(BBB_BESTOFF1_23p_t,BBB_BESTOFF1_23p)
graph3(BBB_BESTOFF1_23p_t,training$Score1)

#last check - vizualize if distribution consistent with training set

graph3(BB_GNET1_OPT1d_t,training$Score1)
graph3(BB_GAM1_OPT1d_t,training$Score1)
graph3(BBB_LM1_OPT1d_t,training$Score1)
graph3(BB_GNET1_OPT2d_t,training$Score1)
graph3(BB_GAM1_OPT2d_t,training$Score1)
graph3(BBB_LM1_OPT2d_t,training$Score1)

# for set 2, only BBB_LM1_OPT2d_t is consistent
# decide to select it even the Kappa score is lower

BBB_BESTOFF1_23p_t[test$EssaySet==2]<-BBB_LM1_OPT2d_t[test$EssaySet==2]

#####################################################################
# detect truncated for set 1 and adjust prediction
#####################################################################

last_char<-rep(NA,nrow(test))
for (i in 1:nrow(test)) last_char[i]<-substr(EssayText_t[i],nchar(EssayText_t[i]),nchar(EssayText_t[i]))
trunc<-rep(0,nrow(test))
trunc[!last_char %in% c("!","\"",".","?",")")  & nchar(EssayText_t)<100]<-1
EssayText_t[test$EssaySet==1 & trunc==1]

BBB_BESTOFF1_23p_t_trunc1<-BBB_BESTOFF1_23p_t
BBB_BESTOFF1_23p_t_trunc1[test$EssaySet==1 & trunc==1]<-median(training.E1$Score1)

Store(BBB_BESTOFF1_23p_t_trunc1)

table(BBB_BESTOFF1_23p_t_trunc1==BBB_BESTOFF1_23p_t)

#####################################################################
#SUBMIT
#####################################################################

Gxav_sol<-data.frame(id=test$Id,essay_score=BBB_BESTOFF1_23p_t_trunc1)
write.csv(Gxav_sol,"Gxav_sol.csv",row.names = FALSE)
