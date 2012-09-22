#####################################################################
# Packages required
#####################################################################

# install the following packages if necessary
#install.packages("SOAR")
#install.packages("Matrix")
#install.packages("Hmisc")
#install.packages("MASS")
#install.packages("RTextTools")
#install.packages("tau")
#install.packages("clim.pact")
#install.packages("glmnet")
#install.packages("e1071")
#install.packages("gbm")
#install.packages("randomForest")
#install.packages("mgcv")
#install.packages("glmnetcr")

#####################################################################
#####################################################################
# Load packages
#####################################################################
#####################################################################

library(SOAR) # to handle memory
library(Matrix) # to handle matrix
library(Hmisc) ; library(MASS) # for descriptive analysis
library(RTextTools) # to stem the words
library(tau) # for n-grams
library(tm) # to remove duplicated blanks and punctuation marks
library(clim.pact) # to convert to lower case

#####################################################################
#####################################################################
# Some useful function
#####################################################################
#####################################################################


#####################################################################
# Function to compute and display SQWKappa per essay set and competition metric
#####################################################################

SQWKappa = function (rater.a , rater.b) {
  #pairwise frequencies
  confusion.mat = table(data.frame(rater.a, rater.b))
  confusion.mat = confusion.mat / sum(confusion.mat)
  #get expected pairwise frequencies under independence
  histogram.a = table(rater.a) / length(table(rater.a))
  histogram.b = table(rater.b) / length(table(rater.b))
  expected.mat = histogram.a %*% t(histogram.b)
  expected.mat = expected.mat / sum(expected.mat)
  #get weights
  labels = as.numeric( as.vector (names(table(rater.a)))) 
  labels1 = as.numeric( as.vector (names(table(rater.b))))
  weights = outer(labels, labels1, FUN = function(x,y) (x-y)^2 )
  #calculate kappa
  kappa = 1 - sum(weights*confusion.mat)/sum(weights*expected.mat)
  kappa
}
Store(SQWKappa)

MQWKappa = function (kappas, weights) {
  if (missing(weights)) weights = rep(1, length(kappas)) else weights = weights / mean(weights)
  max999 <- function(x) sign(x)*min(0.999,abs(x))
  min001 <- function(x) sign(x)*max(0.001,abs(x))
  kappas = sapply(kappas, max999)
  kappas = sapply(kappas, min001)    
  r2z = function(x) 0.5*log((1+x)/(1-x))
  z2r = function(x) (exp(2*x)-1) / (exp(2*x)+1)
  kappas = sapply(kappas, r2z)
  kappas = kappas * weights
  kappas = mean(kappas)
  kappas = z2r(kappas)
  kappas
}
Store(MQWKappa)

Kappa_score<-function(Prev) {
  out<-rep(0,11)
  for (i in 1:10) out[i]<-SQWKappa(training$Score1[training$EssaySet==i],round(Prev[training$EssaySet==i],0))
  out[11]<-MQWKappa(out[1:10],rep(1,10))
  out
}
Store(Kappa_score)

#####################################################################
# Function to compute Spearman rho
#####################################################################

Spear_score<-function(Prev) {
  library(Hmisc)
  out<-rep(0,11)
  for (i in 1:10) out[i]<-spearman(Prev[training$EssaySet==i],training$Score1[training$EssaySet==i])
  out[11]<-mean(out[1:10])
  out
}
Store(Spear_score)

#####################################################################
# Function to group predictions per essay set
#####################################################################

Group<-function(E) {
  out<-rep(0,nrow(training))
  for (i in 1:10) out[training$EssaySet==i]<-E[[i]]
  out
}
Store(Group)

#####################################################################
# graph to visualize effect of proxies
#####################################################################

graph1<-function(tmp) {
  par(mfrow=c(4,3))
  for (i in 1:10) {
    if (length(unique(tmp[training$EssaySet==i]))>1) {
      xlim=c(0,quantile(tmp[training$EssaySet==i],0.99))
      truehist(tmp[training$EssaySet==i],axes=F,xlim=xlim,xlab="",ylab="")
      par(new=TRUE)
      ylim=c(quantile(training$Score1[training$EssaySet==i],c(0.01,0.99)))
      plsmo(tmp[training$EssaySet==i],training$Score1[training$EssaySet==i],
            xlim=xlim,ylim=ylim,xlab="",ylab="")
      title(main=paste("EssayTexts ",i," / rho =",round(
        spearman(tmp[training$EssaySet==i],training$Score1[training$EssaySet==i]),2)))
    }}}
Store(graph1)

#####################################################################
# Function to vizualize relation with score
#####################################################################

graph1b<-function(tmp) {
  library(MASS)
  library(Hmisc)
  par(mfrow=c(4,3))
  for (i in 1:10) {
    if (!is.na(tmp[training$EssaySet==i][1])) {
      xlim=range(tmp[training$EssaySet==i])
      truehist(tmp[training$EssaySet==i],axes=F,xlim=xlim,ylab="",xlab="")
      par(new=TRUE)
      ylim=range(training$Score1[training$EssaySet==i])
      plsmo(tmp[training$EssaySet==i],training$Score1[training$EssaySet==i],xlim=xlim,ylim=ylim,ylab="",xlab="")
      title(main=paste("essays ",i," / rho =",round(
        spearman(tmp[training$EssaySet==i],training$Score1[training$EssaySet==i]),2)))
    }}
}
Store(graph1b)

#####################################################################
# Function to plot diagnostic graph
#####################################################################

graph2<-function(x,y) {
  par(mfrow=c(4,3))
  for (i in 1:10) {
    tmp<-data.frame(table(x[training$EssaySet==i],y[training$EssaySet==i]))
    plot(as.numeric(as.character(tmp[,1])),as.numeric(as.character(tmp[,2])),
         pch=19,cex=tmp[,3]^0.5/10,ylab="predicted",xlab="actual",main=paste("set",i))
    lines(0:max(x),0:max(x))
  }
}
Store(graph2)

#####################################################################
#####################################################################
# Read file
#####################################################################
#####################################################################

training<-read.csv('train_rel_2.tsv', header=TRUE, 
	sep = "\t", fileEncoding="windows-1252",quote="")
str(training)

#####################################################################
#####################################################################
# Data manipulation
#####################################################################
#####################################################################

#####################################################################
# convert EssayText_tr to character format
#####################################################################

training[,5] <-as.character(training[,5]) 

#####################################################################
# detect truncated for set 1 and remove them
#####################################################################

last_char<-rep(NA,nrow(training))
for (i in 1:nrow(training)) last_char[i]<-substr(training[i,5],nchar(training[i,5]),nchar(training[i,5]))
trunc<-rep(0,nrow(training))
trunc[!last_char %in% c("!","\"",".","?",")")  & nchar(training[,5])<100]<-1
training[training$EssaySet==1 & trunc==1,c(3,5)]
dim(training[training$EssaySet==1 & trunc==1,c(3,5)])
nrow(training)
training<-training[-which(training$EssaySet==1 & trunc==1),]
nrow(training)

#####################################################################
# remove text from training
#####################################################################

EssayText_tr<-training[,5] ; Store(EssayText_tr)
training<-training[,-5] ; Store(training)

#####################################################################
# average of 2 scores
#####################################################################

training$ScoreA<-training$Score1*0.5+training$Score2*0.5

#####################################################################
#####################################################################
# create dummies which label good/poor essays
#####################################################################
#####################################################################

training$isgood<-rep(0,nrow(training))
training$isgood[training$EssaySet==1 & training$Score1>=3]<-1
training$isgood[training$EssaySet==2 & training$Score1>=3]<-1
training$isgood[training$EssaySet==3 & training$Score1>=2]<-1
training$isgood[training$EssaySet==4 & training$Score1>=2]<-1
training$isgood[training$EssaySet==5 & training$Score1>=1]<-1
training$isgood[training$EssaySet==6 & training$Score1>=1]<-1
training$isgood[training$EssaySet==7 & training$Score1>=2]<-1
training$isgood[training$EssaySet==8 & training$Score1>=2]<-1
training$isgood[training$EssaySet==9 & training$Score1>=2]<-1
training$isgood[training$EssaySet==10 & training$Score1>=2]<-1
table(training$isgood,training$EssaySet)

Store(training)

# note : was not used and replaced by the following

#####################################################################
# create a dummy which labels good essays
#####################################################################

IsGood<-training[,1:2]
IsGood$isgood<-0
IsGood$isgood[training$EssaySet==1 & training$Score1>=3]<-1
IsGood$isgood[training$EssaySet==2 & training$Score1>=3]<-1
IsGood$isgood[training$EssaySet==3 & training$Score1>=2]<-1
IsGood$isgood[training$EssaySet==4 & training$Score1>=2]<-1
IsGood$isgood[training$EssaySet==5 & training$Score1>=1]<-1
IsGood$isgood[training$EssaySet==6 & training$Score1>=1]<-1
IsGood$isgood[training$EssaySet==7 & training$Score1>=2]<-1
IsGood$isgood[training$EssaySet==8 & training$Score1>=2]<-1
IsGood$isgood[training$EssaySet==9 & training$Score1>=2]<-1
IsGood$isgood[training$EssaySet==10 & training$Score1>=2]<-1

IsGood$isgood2<-0
IsGood$isgood2[training$EssaySet==1 & training$Score2>=3]<-1
IsGood$isgood2[training$EssaySet==2 & training$Score2>=3]<-1
IsGood$isgood2[training$EssaySet==3 & training$Score2>=2]<-1
IsGood$isgood2[training$EssaySet==4 & training$Score2>=2]<-1
IsGood$isgood2[training$EssaySet==5 & training$Score2>=1]<-1
IsGood$isgood2[training$EssaySet==6 & training$Score2>=1]<-1
IsGood$isgood2[training$EssaySet==7 & training$Score2>=2]<-1
IsGood$isgood2[training$EssaySet==8 & training$Score2>=2]<-1
IsGood$isgood2[training$EssaySet==9 & training$Score2>=2]<-1
IsGood$isgood2[training$EssaySet==10 & training$Score2>=2]<-1

IsGood$isgoodA<-0
IsGood$isgoodA[training$EssaySet==1 & training$ScoreA>=3]<-1
IsGood$isgoodA[training$EssaySet==2 & training$ScoreA>=3]<-1
IsGood$isgoodA[training$EssaySet==3 & training$ScoreA>=2]<-1
IsGood$isgoodA[training$EssaySet==4 & training$ScoreA>=2]<-1
IsGood$isgoodA[training$EssaySet==5 & training$ScoreA>=1]<-1
IsGood$isgoodA[training$EssaySet==6 & training$ScoreA>=1]<-1
IsGood$isgoodA[training$EssaySet==7 & training$ScoreA>=2]<-1
IsGood$isgoodA[training$EssaySet==8 & training$ScoreA>=2]<-1
IsGood$isgoodA[training$EssaySet==9 & training$ScoreA>=2]<-1
IsGood$isgoodA[training$EssaySet==10 & training$ScoreA>=2]<-1

# Note isgoodA was not used in our final solution

IsGood.E1<-IsGood[IsGood$EssaySet==1,] ; Store(IsGood.E1)
IsGood.E2<-IsGood[IsGood$EssaySet==2,] ; Store(IsGood.E2)
IsGood.E3<-IsGood[IsGood$EssaySet==3,] ; Store(IsGood.E3)
IsGood.E4<-IsGood[IsGood$EssaySet==4,] ; Store(IsGood.E4)
IsGood.E5<-IsGood[IsGood$EssaySet==5,] ; Store(IsGood.E5)
IsGood.E6<-IsGood[IsGood$EssaySet==6,] ; Store(IsGood.E6)
IsGood.E7<-IsGood[IsGood$EssaySet==7,] ; Store(IsGood.E7)
IsGood.E8<-IsGood[IsGood$EssaySet==8,] ; Store(IsGood.E8)
IsGood.E9<-IsGood[IsGood$EssaySet==9,] ; Store(IsGood.E9)
IsGood.E10<-IsGood[IsGood$EssaySet==10,] ; Store(IsGood.E10)

#####################################################################
# create a dummy which labels poor essays
#####################################################################

IsBad<-training[,1:2]
IsBad$isbad<-IsBad$isbad2<-IsBad$isbadA<-0
IsBad$isbad[training$Score1==0]<-1
IsBad$isbad2[training$Score2==0]<-1
IsBad$isbadA[training$ScoreA==0]<-1

IsBad.E1<-IsBad[IsBad$EssaySet==1,] ; Store(IsBad.E1)
IsBad.E2<-IsBad[IsBad$EssaySet==2,] ; Store(IsBad.E2)
IsBad.E3<-IsBad[IsBad$EssaySet==3,] ; Store(IsBad.E3)
IsBad.E4<-IsBad[IsBad$EssaySet==4,] ; Store(IsBad.E4)
IsBad.E5<-IsBad[IsBad$EssaySet==5,] ; Store(IsBad.E5)
IsBad.E6<-IsBad[IsBad$EssaySet==6,] ; Store(IsBad.E6)
IsBad.E7<-IsBad[IsBad$EssaySet==7,] ; Store(IsBad.E7)
IsBad.E8<-IsBad[IsBad$EssaySet==8,] ; Store(IsBad.E8)
IsBad.E9<-IsBad[IsBad$EssaySet==9,] ; Store(IsBad.E9)
IsBad.E10<-IsBad[IsBad$EssaySet==10,] ; Store(IsBad.E10)

#####################################################################
#####################################################################
# Add type and grade
#####################################################################
#####################################################################

training$type<-rep(1,nrow(training)) ; training$type[training$EssaySet %in% c(5:6)]<-2 ; training$type<-factor(training$type)
training$grade<-rep(10,nrow(training)) ; training$grade[training$EssaySet %in% c(10)]<-8 ; training$grade<-factor(training$grade)
training$subject<-rep("science",nrow(training)) ; training$subject[training$EssaySet %in% c(3,4)]<-"ELA"
training$subject[training$EssaySet %in% c(5,6)]<-"biology" ; training$subject[training$EssaySet %in% c(7:9)]<-"english"
training$subject<-factor(training$subject)
  
# Note : the variables are not used in our final solution

#####################################################################
#####################################################################
# Generate proxies
#####################################################################
#####################################################################

#####################################################################
# count number of characters
#####################################################################

training$NCHAR<-sapply(EssayText_tr,nchar)
graph1(training$NCHAR)

#####################################################################
# n-grams
#####################################################################

L1G<-list()
for (i in 1:nrow(training)) L1G[[i]]<-textcnt(EssayText_tr[i], method="string", n=1L)
Store(L1G)

L2G<-list()
for (i in 1:nrow(training)) L2G[[i]]<-textcnt(EssayText_tr[i], method="string", n=2L)
Store(L2G)

L3G<-list()
for (i in 1:nrow(training)) L3G[[i]]<-textcnt(EssayText_tr[i], method="string", n=3L)
Store(L3G)

L4G<-list()
for (i in 1:nrow(training)) L4G[[i]]<-textcnt(EssayText_tr[i], method="string", n=4L)
Store(L4G)

L5G<-list()
for (i in 1:nrow(training)) L5G[[i]]<-textcnt(EssayText_tr[i], method="string", n=5L)
Store(L5G)

#####################################################################
# count number of words
#####################################################################

training$NW<-sapply(L1G,sum)
graph1(training$NW)

#####################################################################
#stats on words length
#####################################################################

tmp<-t(sapply(L1G, function(x) quantile(sapply(rep(names(x),x),nchar),c(0.75,0.9,0.95,0.99))))
training$Q90<-tmp[,2] ;graph1(training$Q90)
training$Q95<-tmp[,3] ;graph1(training$Q95)
training$Q99<-tmp[,4] ;graph1(training$Q99)

#####################################################################
#stem words
#####################################################################

STEM1G = lapply(L1G,function(x) wordStem(names(x))) ; Store(STEM1G)
STEM2G = lapply(L2G,function(x) wordStem(names(x))) ; Store(STEM2G)
STEM3G = lapply(L3G,function(x) wordStem(names(x))) ; Store(STEM3G)
STEM4G = lapply(L4G,function(x) wordStem(names(x))) ; Store(STEM4G)

STEM<-list()
for (i in 1:nrow(training)) STEM[[i]]<-c(STEM1G[[i]],STEM2G[[i]],STEM3G[[i]],STEM4G[[i]])
Store(STEM)
     
#####################################################################
#% of precise verbs
#####################################################################

Prec_verbs<-read.csv("precise_verbs.csv",header=FALSE)
Prec_verbs = sapply(as.character(Prec_verbs[,1]),function(x) wordStem(x))
NPrV<-sapply(STEM1G,function(x) sum((x %in%  Prec_verbs)==TRUE))
NPrV2<-sapply(STEM2G,function(x) sum((x %in%  Prec_verbs)==TRUE))
training$PPrV<-(NPrV+NPrV2)/training$NW
graph1(training$PPrV)

#####################################################################
#% of transition words
#####################################################################

Trans_words<-read.csv("transition_words.csv",header=FALSE)
Trans_words<-as.character(Trans_words[,1])
NTransW<-sapply(L1G,function(x) sum((names(x) %in%  Trans_words)==TRUE))
NTransW2<-sapply(L2G,function(x) sum((names(x) %in%  Trans_words)==TRUE))
NTransW3<-sapply(L3G,function(x) sum((names(x) %in%  Trans_words)==TRUE))
NTransW4<-sapply(L4G,function(x) sum((names(x) %in%  Trans_words)==TRUE))
training$PTransW<-(NTransW+NTransW2+NTransW3+NTransW4)/training$NW
graph1(training$PTransW)

#####################################################################
#% of misspelling
#####################################################################
Norvig<-scan("norvig.txt", what = character())
Norvig<-as.character(Norvig)
Norvig<-names(textcnt(Norvig, method="string", n=1L))

gutenberg<-scan("gutenberg.txt", what = character())
gutenberg<-as.character(gutenberg)
gutenberg<-names(textcnt(gutenberg, method="string", n=1L))

essay_inst<-scan("essay_inst.txt", what = character())
essay_inst<-as.character(essay_inst)
essay_inst<-names(textcnt(essay_inst, method="string", n=1L))

Acad_words<-read.csv("academic words.csv",header=FALSE)
Prec_verbs<-read.csv("precise_verbs.csv",header=FALSE)
Trans_words<-read.csv("transition_words.csv",header=FALSE)
my_list<-read.csv("my_list.csv",header=FALSE)

VOCAB<-c(Norvig,gutenberg,essay_inst,
         as.character(Acad_words[,1]),as.character(Prec_verbs[,1]),as.character(Trans_words[,1]),
         as.character(my_list[,1]))
VOCAB<-unique(VOCAB)
for (i in 1:length(VOCAB)) VOCAB[i]<-gsub('\n'," ",VOCAB[i])
VOCAB <-unlist(strsplit(VOCAB,split=" ",fixed=TRUE))
VOCAB<-unique(VOCAB)
VOCAB<-VOCAB[is.na(as.numeric(VOCAB))]
Store(VOCAB)

NErr<-sapply(L1G,function(x) sum((names(x) %in%  VOCAB)==FALSE))
training$PErr<-NErr/training$NW
graph1(training$PErr)
Store(training)

#####################################################################
#some data work to ensure some consistency accross EssayTexts
#####################################################################

tmp<-EssayText_tr
for (i in 1:nrow(training)) tmp[i]<-gsub("I '","I'",tmp[i])
for (i in 1:nrow(training)) tmp[i]<-gsub(" 's ","'s ",tmp[i])
for (i in 1:nrow(training)) tmp[i]<-gsub("`","'",tmp[i])
for (i in 1:nrow(training)) tmp[i]<-gsub("''","\"",tmp[i])
for (i in 1:nrow(training)) tmp[i]<-gsub("' ","\" ",tmp[i])
for (i in 1:nrow(training)) tmp[i]<-gsub(" '"," \"",tmp[i])
for (i in 1:nrow(training)) tmp[i]<-gsub("\"\"","\"",tmp[i])
EssayText_tr<-tmp
Store(EssayText_tr)

#####################################################################
#presence of QUOTE
#####################################################################

busted<-strsplit(EssayText_tr,split="\"",fixed=TRUE)
training$QuotM<-(sapply(busted,length)-1)
graph1(training$QuotM)

#####################################################################
#presence of  )
#####################################################################

busted<-strsplit(EssayText_tr,split=")",fixed=TRUE)
training$BK<-(sapply(busted,length)-1)
graph1(training$BK)

#####################################################################
#presence of  ,
#####################################################################

busted<-strsplit(EssayText_tr,split=",",fixed=TRUE)
training$CM<-(sapply(busted,length)-1)/training$NW
graph1(training$CM)

#####################################################################
#presence of 's
#####################################################################

busted<-strsplit(EssayText_tr,split="'s",fixed=TRUE)
training$POS<-(sapply(busted,length)-1)
graph1(training$POS)
Store(training)

#####################################################################
#####################################################################
# split training per EssaySet
#####################################################################
#####################################################################

update_split<-function() {
  training.E1<-training[training$EssaySet==1,] ; Store(training.E1)
  training.E2<-training[training$EssaySet==2,] ; Store(training.E2)
  training.E3<-training[training$EssaySet==3,] ; Store(training.E3)
  training.E4<-training[training$EssaySet==4,] ; Store(training.E4)
  training.E5<-training[training$EssaySet==5,] ; Store(training.E5)
  training.E6<-training[training$EssaySet==6,] ; Store(training.E6)
  training.E7<-training[training$EssaySet==7,] ; Store(training.E7)
  training.E8<-training[training$EssaySet==8,] ; Store(training.E8)
  training.E9<-training[training$EssaySet==9,] ; Store(training.E9)
  training.E10<-training[training$EssaySet==10,] ; Store(training.E10)
}
Store(update_split)
update_split()

#####################################################################
#####################################################################
# Produce document-term matrix
#####################################################################
#####################################################################

#####################################################################
# remove unfrequent stemmed words (less than 20 times used)
#####################################################################

tmp<-data.frame(table(unlist(STEM)))
tmp<-as.character(tmp[tmp[,2]>20,1])

STEMsh<-STEM
for (i in 1:nrow(training)) {
  STEMsh[[i]]<-STEM[[i]][STEM[[i]] %in% tmp]
	if (i%%100==0) print(i)}
Store(STEMsh)

#####################################################################
# create features matrix
#####################################################################

Feat_mat<-NULL
for (i in 1:nrow(training)) {
  if (length(STEMsh[[i]])!=0) Feat_mat=  rbind(Feat_mat,data.frame(row_id=i,W=STEMsh[[i]]))
	if (i%%100==0) print(i)}

#####################################################################
# assign number to words (token)
#####################################################################

Feat_mat$token<-as.numeric(as.factor(as.character(Feat_mat$W)))
Store(Feat_mat)

#####################################################################
# incidence matrix
#####################################################################

bin_mat<-as.matrix(sparseMatrix(Feat_mat$row_id,Feat_mat$token,
x=rep(1,nrow(Feat_mat)))) ; Store(bin_mat)

#####################################################################
# split incidence matrix per EssaySet
#####################################################################

Bin_mat.E1<-bin_mat[training$EssaySet==1,] ; Store(Bin_mat.E1)
Bin_mat.E2<-bin_mat[training$EssaySet==2,] ; Store(Bin_mat.E2)
Bin_mat.E3<-bin_mat[training$EssaySet==3,] ; Store(Bin_mat.E3)
Bin_mat.E4<-bin_mat[training$EssaySet==4,] ; Store(Bin_mat.E4)
Bin_mat.E5<-bin_mat[training$EssaySet==5,] ; Store(Bin_mat.E5)
Bin_mat.E6<-bin_mat[training$EssaySet==6,] ; Store(Bin_mat.E6)
Bin_mat.E7<-bin_mat[training$EssaySet==7,] ; Store(Bin_mat.E7)
Bin_mat.E8<-bin_mat[training$EssaySet==8,] ; Store(Bin_mat.E8)
Bin_mat.E9<-bin_mat[training$EssaySet==9,] ; Store(Bin_mat.E9)
Bin_mat.E10<-bin_mat[training$EssaySet==10,] ; Store(Bin_mat.E10)

#####################################################################
# reduce dimension (new matrix start with small letter !)
#####################################################################

bin_mat.E1<-Bin_mat.E1[,-which(colSums(Bin_mat.E1)<5)] ; Store(bin_mat.E1)
bin_mat.E2<-Bin_mat.E2[,-which(colSums(Bin_mat.E2)<5)] ; Store(bin_mat.E2)
bin_mat.E3<-Bin_mat.E3[,-which(colSums(Bin_mat.E3)<5)] ; Store(bin_mat.E3)
bin_mat.E4<-Bin_mat.E4[,-which(colSums(Bin_mat.E4)<5)] ; Store(bin_mat.E4)
bin_mat.E5<-Bin_mat.E5[,-which(colSums(Bin_mat.E5)<5)] ; Store(bin_mat.E5)
bin_mat.E6<-Bin_mat.E6[,-which(colSums(Bin_mat.E6)<5)] ; Store(bin_mat.E6)
bin_mat.E7<-Bin_mat.E7[,-which(colSums(Bin_mat.E7)<5)] ; Store(bin_mat.E7)
bin_mat.E8<-Bin_mat.E8[,-which(colSums(Bin_mat.E8)<5)] ; Store(bin_mat.E8)
bin_mat.E9<-Bin_mat.E9[,-which(colSums(Bin_mat.E9)<5)] ; Store(bin_mat.E9)
bin_mat.E10<-Bin_mat.E10[,-which(colSums(Bin_mat.E10)<5)] ; Store(bin_mat.E10)

#####################################################################
#####################################################################
# Ridit transformation
#####################################################################
#####################################################################

RIDIT<-function(var,var_out) {
  tmp<-data.frame(table(var))
  tmp$var<-as.numeric(as.character(tmp$var))
  tmp$cumsum<-cumsum(tmp$Freq)/sum(tmp$Freq)
  tmp$ridit<-rep(0,nrow(tmp))
  for (i in 1:nrow(tmp)) tmp$ridit[i]<-ifelse(i==1,0,tmp$cumsum[i-1])-(1-tmp$cumsum[i])
  out<-rep(0,length(var_out))
  for (l in 1:nrow(tmp))
    out[var_out>=tmp$var[l]]<-tmp$ridit[l] 
  out
}
Store(RIDIT)

RIDIT_mat<-function(mat,mat_out) {
  out<-mat_out
  for (j in 1:ncol(mat)) out[,j]<-RIDIT(mat[,j],mat_out[,j])  
  out
}
Store(RIDIT_mat)

Ridit_mat.E1<-RIDIT_mat(bin_mat.E1,bin_mat.E1) ; Store(Ridit_mat.E1)
Ridit_mat.E2<-RIDIT_mat(bin_mat.E2,bin_mat.E2) ; Store(Ridit_mat.E2)
Ridit_mat.E3<-RIDIT_mat(bin_mat.E3,bin_mat.E3) ; Store(Ridit_mat.E3)
Ridit_mat.E4<-RIDIT_mat(bin_mat.E4,bin_mat.E4) ; Store(Ridit_mat.E4)
Ridit_mat.E5<-RIDIT_mat(bin_mat.E5,bin_mat.E5) ; Store(Ridit_mat.E5)
Ridit_mat.E6<-RIDIT_mat(bin_mat.E6,bin_mat.E6) ; Store(Ridit_mat.E6)
Ridit_mat.E7<-RIDIT_mat(bin_mat.E7,bin_mat.E7) ; Store(Ridit_mat.E7)
Ridit_mat.E8<-RIDIT_mat(bin_mat.E8,bin_mat.E8) ; Store(Ridit_mat.E8)
Ridit_mat.E9<-RIDIT_mat(bin_mat.E9,bin_mat.E9) ; Store(Ridit_mat.E9)
Ridit_mat.E10<-RIDIT_mat(bin_mat.E10,bin_mat.E10) ; Store(Ridit_mat.E10)

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

EssayText_tr2<-EssayText_tr
EssayText_tr2<-sapply(EssayText_tr, function(x) gsub("n't"," not",x))
EssayText_tr2<-sapply(EssayText_tr, function(x) gsub("cannot","can not",x))
EssayText_tr2<-sapply(EssayText_tr2, stripWhitespace)
EssayText_tr2<-sapply(EssayText_tr2, tolower)
EssayText_tr2<-sapply(EssayText_tr2, removePunctuation)

#####################################################################
#function to stem text
#####################################################################

conc<-function(x) {
  x<-x[!x %in% c("the","a","an","this","that","these","those","to","is","are","be","or","and")]
  out<-x[1]
  for (i in 2:length(x)) out<-paste(out,x[i])
  out
}
Store(conc)

stem_text<-function(x) {
  tmp<-strsplit(x,split=" ",fixed=TRUE)
  out<-list()
  for (i in 1:length(x)) {
    tmp2<-sapply(tmp[[i]],wordStem)
    out[[i]]<-conc(tmp2)
    out[[i]]<-names(textcnt(out[[i]], method="string", n=1L))
    if (i%%500==0) print(paste(i))  
  }
  out
}
Store(stem_text)

STEM.1<-stem_text(EssayText_tr2) ; Store(STEM.1)

#####################################################################
# Produce document-term matrix Bin_mat.1.Ex
#####################################################################

#####################################################################
# remove unfrequent stemmed words (less than 5 times used)
#####################################################################

tmp<-data.frame(table(unlist(STEM.1)))
tmp<-as.character(tmp[tmp[,2]>5,1])

STEMsh.1<-STEM.1
for (i in 1:nrow(training)) {
  STEMsh.1[[i]]<-STEM.1[[i]][STEM.1[[i]] %in% tmp]
  if (i%%100==0) print(i)}
Store(STEMsh.1)

#####################################################################
# create features matrix
#####################################################################

Feat_mat.1<-NULL
for (i in 1:nrow(training)) {
  if (length(STEMsh.1[[i]])!=0) Feat_mat.1=  rbind(Feat_mat.1,data.frame(row_id=i,W=STEMsh.1[[i]]))
  if (i%%100==0) print(i)}

#####################################################################
# assign number to words (token)
#####################################################################

Feat_mat.1$token<-as.numeric(as.factor(as.character(Feat_mat.1$W)))
Store(Feat_mat.1)

#####################################################################
# incidence matrix
#####################################################################

bin_mat.1<-as.matrix(sparseMatrix(Feat_mat.1$row_id,Feat_mat.1$token,
                                  x=rep(1,nrow(Feat_mat.1)))) ; Store(bin_mat.1)

#####################################################################
# split incidence matrix per EssaySet
#####################################################################

Bin_mat.1.E1<-bin_mat.1[training$EssaySet==1,] ; Store(Bin_mat.1.E1)
Bin_mat.1.E2<-bin_mat.1[training$EssaySet==2,] ; Store(Bin_mat.1.E2)
Bin_mat.1.E3<-bin_mat.1[training$EssaySet==3,] ; Store(Bin_mat.1.E3)
Bin_mat.1.E4<-bin_mat.1[training$EssaySet==4,] ; Store(Bin_mat.1.E4)
Bin_mat.1.E5<-bin_mat.1[training$EssaySet==5,] ; Store(Bin_mat.1.E5)
Bin_mat.1.E6<-bin_mat.1[training$EssaySet==6,] ; Store(Bin_mat.1.E6)
Bin_mat.1.E7<-bin_mat.1[training$EssaySet==7,] ; Store(Bin_mat.1.E7)
Bin_mat.1.E8<-bin_mat.1[training$EssaySet==8,] ; Store(Bin_mat.1.E8)
Bin_mat.1.E9<-bin_mat.1[training$EssaySet==9,] ; Store(Bin_mat.1.E9)
Bin_mat.1.E10<-bin_mat.1[training$EssaySet==10,] ; Store(Bin_mat.1.E10)

#####################################################################
#####################################################################
# Ridit transformation
#####################################################################
#####################################################################

Ridit_mat.1.E1<-RIDIT_mat(Bin_mat.1.E1,Bin_mat.1.E1) ; Store(Ridit_mat.1.E1)
Ridit_mat.1.E2<-RIDIT_mat(Bin_mat.1.E2,Bin_mat.1.E2) ; Store(Ridit_mat.1.E2)
Ridit_mat.1.E3<-RIDIT_mat(Bin_mat.1.E3,Bin_mat.1.E3) ; Store(Ridit_mat.1.E3)
Ridit_mat.1.E4<-RIDIT_mat(Bin_mat.1.E4,Bin_mat.1.E4) ; Store(Ridit_mat.1.E4)
Ridit_mat.1.E5<-RIDIT_mat(Bin_mat.1.E5,Bin_mat.1.E5) ; Store(Ridit_mat.1.E5)
Ridit_mat.1.E6<-RIDIT_mat(Bin_mat.1.E6,Bin_mat.1.E6) ; Store(Ridit_mat.1.E6)
Ridit_mat.1.E7<-RIDIT_mat(Bin_mat.1.E7,Bin_mat.1.E7) ; Store(Ridit_mat.1.E7)
Ridit_mat.1.E8<-RIDIT_mat(Bin_mat.1.E8,Bin_mat.1.E8) ; Store(Ridit_mat.1.E8)
Ridit_mat.1.E9<-RIDIT_mat(Bin_mat.1.E9,Bin_mat.1.E9) ; Store(Ridit_mat.1.E9)
Ridit_mat.1.E10<-RIDIT_mat(Bin_mat.1.E10,Bin_mat.1.E10) ; Store(Ridit_mat.1.E10)

#####################################################################
#####################################################################
# Split training data into 5 folds
#####################################################################
#####################################################################

split_k<-function(seed,data) {
  set.seed(seed)
  n_5<-round(nrow(data)/5)
  r<-1:nrow(data)
  K.1 <-sample(r,n_5,replace=FALSE)
  K.2 <-sample(r[-c(K.1)],n_5,replace=FALSE)
  K.3 <-sample(r[-c(K.1,K.2)],n_5,replace=FALSE)
  K.4 <-sample(r[-c(K.1,K.2,K.3)],n_5,replace=FALSE)
  K.5 <-r[-c(K.1,K.2,K.3,K.4)]
  list(K.1,K.2,K.3,K.4,K.5)
}
Store(split_k)

K.E1<-split_k(2506,training.E1) ; Store(K.E1)
K.E2<-split_k(606,training.E2) ; Store(K.E2)
K.E3<-split_k(287,training.E3) ; Store(K.E3)
K.E4<-split_k(1540,training.E4) ; Store(K.E4)
K.E5<-split_k(973,training.E5) ; Store(K.E5)
K.E6<-split_k(306,training.E6) ; Store(K.E6)
K.E7<-split_k(216,training.E7) ; Store(K.E7)
K.E8<-split_k(2358,training.E8) ; Store(K.E8)
K.E9<-split_k(5761,training.E9) ; Store(K.E9)
K.E10<-split_k(2022,training.E10) ; Store(K.E10)

#####################################################################
#####################################################################
# train GNET (Regularized GLM) to reduce dimension
#####################################################################
#####################################################################

# function to train GNET
#####################################################################

cv5_GNET<-function(data,K,train_scores,alpha,standardize,seed) {
  set.seed(seed)
  out<-list()
  out[[1]]<-list()
  for (i in 1:6) {
    out[[1]][[i]]<- if (i<=5) 
      cv.glmnet(as.matrix(data[-K[[i]],]),train_scores[-K[[i]]],family="gaussian", alpha=alpha,standardize=standardize) else
        cv.glmnet(as.matrix(data),train_scores,family="gaussian", alpha=alpha,standardize=standardize)
    print(paste("set",i,"done"))}      
  out[[2]]<-rep(0,length(train_scores))
  for (i in 1:5)     out[[2]][K[[i]]]<-predict(out[[1]][[i]],as.matrix(data[K[[i]],]),type="response", s="lambda.min")[,1] 
  out
}
Store(cv5_GNET)

# train GNET
#####################################################################

library(glmnet)
GNET1R.1.E1<-cv5_GNET(Ridit_mat.1.E1,K.E1,training.E1$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.1.E1)
GNET1R.1.E2<-cv5_GNET(Ridit_mat.1.E2,K.E2,training.E2$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.1.E2)
GNET1R.1.E3<-cv5_GNET(Ridit_mat.1.E3,K.E3,training.E3$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.1.E3)
GNET1R.1.E4<-cv5_GNET(Ridit_mat.1.E4,K.E4,training.E4$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.1.E4)
GNET1R.1.E5<-cv5_GNET(Ridit_mat.1.E5,K.E5,training.E5$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.1.E5)
GNET1R.1.E6<-cv5_GNET(Ridit_mat.1.E6,K.E6,training.E6$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.1.E6)
GNET1R.1.E7<-cv5_GNET(Ridit_mat.1.E7,K.E7,training.E7$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.1.E7)
GNET1R.1.E8<-cv5_GNET(Ridit_mat.1.E8,K.E8,training.E8$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.1.E8)
GNET1R.1.E9<-cv5_GNET(Ridit_mat.1.E9,K.E9,training.E9$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.1.E9)
GNET1R.1.E10<-cv5_GNET(Ridit_mat.1.E10,K.E10,training.E10$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.1.E10)

GNET1R.1<-Group(list(GNET1R.1.E1[[2]],GNET1R.1.E2[[2]],GNET1R.1.E3[[2]],GNET1R.1.E4[[2]],GNET1R.1.E5[[2]],
                     GNET1R.1.E6[[2]],GNET1R.1.E7[[2]],GNET1R.1.E8[[2]],GNET1R.1.E9[[2]],GNET1R.1.E10[[2]])) ; Store(GNET1R.1)
Spear_score(GNET1R.1)
graph1b(GNET1R.1)
Kappa_score(GNET1R.1)

#####################################################################
#####################################################################
# extract useful words for GNET1R.1 fit
#####################################################################
#####################################################################

useful_words<-function(fit) {
  out<-list()
  for (i in 1:6) {
    VARSelect<-names(which(fit[[i]]$glmnet.fit$beta[,which(fit[[i]]$lambda==fit[[i]]$lambda.min)]!=0))
    VARSelect<-gsub("V","",VARSelect)
    tmp<-unique(Feat_mat.1[,c(2,3)])
    out[[i]]<-as.character(tmp[tmp[,2] %in% VARSelect,1])    
  }
  out
}
Store(useful_words)

useful_words.E1<-useful_words(GNET1R.1.E1[[1]]) ; Store(useful_words.E1)
useful_words.E2<-useful_words(GNET1R.1.E2[[1]]) ; Store(useful_words.E2)
useful_words.E3<-useful_words(GNET1R.1.E3[[1]]) ; Store(useful_words.E3)
useful_words.E4<-useful_words(GNET1R.1.E4[[1]]) ; Store(useful_words.E4)
useful_words.E5<-useful_words(GNET1R.1.E5[[1]]) ; Store(useful_words.E5)
useful_words.E6<-useful_words(GNET1R.1.E6[[1]]) ; Store(useful_words.E6)
useful_words.E7<-useful_words(GNET1R.1.E7[[1]]) ; Store(useful_words.E7)
useful_words.E8<-useful_words(GNET1R.1.E8[[1]]) ; Store(useful_words.E8)
useful_words.E9<-useful_words(GNET1R.1.E9[[1]]) ; Store(useful_words.E9)
useful_words.E10<-useful_words(GNET1R.1.E10[[1]]) ; Store(useful_words.E10)

#####################################################################
# Stem text
#####################################################################

# function to stem text
#####################################################################

conc_remove<-function(x,word_list) {
  x<-x[x %in% word_list]
  out<-x[1]
  for (i in 2:length(x)) out<-paste(out,x[i])
  out
}
Store(conc_remove)

stem_remove_text<-function(x,word_list) {
  tmp<-strsplit(x,split=" ",fixed=TRUE)
  out<-rep("tofill",length(x))
  for (i in 1:length(x)) {
    tmp2<-sapply(tmp[[i]],wordStem)
    out[i]<-conc_remove(tmp2,word_list)
  }
  out
}
Store(stem_remove_text)

stem_n_grams<-function(set,word_list) {
  out<-list()
  for (j in 1:6) {
    tmp<-stem_remove_text(EssayText_tr2[training$EssaySet==set],word_list[[j]])
    out[[j]]<-list()
    for (i in 1:length(tmp)) {
      out[[j]][[i]]<-names(c(textcnt(tmp[i], method="string", n=1L),textcnt(tmp[i], method="string", n=2L),
                             textcnt(tmp[i], method="string", n=3L),textcnt(tmp[i], method="string", n=4L),
                             textcnt(tmp[i], method="string", n=5L)))
    }    
    print(paste("set",j,"done"))  
  }
  out
} 

STEM.E1<-stem_n_grams(1,useful_words.E1) ; Store(STEM.E1)
STEM.E2<-stem_n_grams(2,useful_words.E2) ; Store(STEM.E2)
STEM.E3<-stem_n_grams(3,useful_words.E3) ; Store(STEM.E3)
STEM.E4<-stem_n_grams(4,useful_words.E4) ; Store(STEM.E4)
STEM.E5<-stem_n_grams(5,useful_words.E5) ; Store(STEM.E5)
STEM.E6<-stem_n_grams(6,useful_words.E6) ; Store(STEM.E6)
STEM.E7<-stem_n_grams(7,useful_words.E7) ; Store(STEM.E7)
STEM.E8<-stem_n_grams(8,useful_words.E8) ; Store(STEM.E8)
STEM.E9<-stem_n_grams(9,useful_words.E9) ; Store(STEM.E9)
STEM.E10<-stem_n_grams(10,useful_words.E10) ; Store(STEM.E10)

#####################################################################
#####################################################################
# Produce document-term matrix bin_mat.2.Ex
#####################################################################
#####################################################################

#####################################################################
# remove unfrequent n-grams (less than 5 times used)
#####################################################################

shorten<-function(x) {
  out<-list()
  for (j in 1:6) {
    out[[j]]<-list()
    tmp<-data.frame(table(unlist(x[[j]])))
    tmp<-as.character(tmp[tmp[,2]>5,1])
    for (i in 1:length(x[[j]])) {
      out[[j]][[i]]<-x[[j]][[i]][x[[j]][[i]] %in% tmp]
    }
    print(paste("set",j,"done"))
  }  
  out
}
Store(shorten)

STEMsh.E1<-shorten(STEM.E1) ; Store(STEMsh.E1)
STEMsh.E2<-shorten(STEM.E2) ; Store(STEMsh.E2)
STEMsh.E3<-shorten(STEM.E3) ; Store(STEMsh.E3)
STEMsh.E4<-shorten(STEM.E4) ; Store(STEMsh.E4)
STEMsh.E5<-shorten(STEM.E5) ; Store(STEMsh.E5)
STEMsh.E6<-shorten(STEM.E6) ; Store(STEMsh.E6)
STEMsh.E7<-shorten(STEM.E7) ; Store(STEMsh.E7)
STEMsh.E8<-shorten(STEM.E8) ; Store(STEMsh.E8)
STEMsh.E9<-shorten(STEM.E9) ; Store(STEMsh.E9)
STEMsh.E10<-shorten(STEM.E10) ; Store(STEMsh.E10)

#####################################################################
# create features matrix
#####################################################################

Create_Feat_mat<-function(x) {
  out<-list()
  for (j in 1:6) {
    out[[j]]<-data.frame(row_id=1,W=x[[j]][[1]])
    for (i in 2:length(x[[j]])) {
      if (length(x[[j]][[i]])!=0) out[[j]]=  rbind(out[[j]],data.frame(row_id=i,W=x[[j]][[i]]))  
    }
    print(paste("set",j,"done"))
  }
  out
}

Feat_mat.E1<-Create_Feat_mat(STEMsh.E1) ; Store(Feat_mat.E1)
Feat_mat.E2<-Create_Feat_mat(STEMsh.E2) ; Store(Feat_mat.E2)
Feat_mat.E3<-Create_Feat_mat(STEMsh.E3) ; Store(Feat_mat.E3)
Feat_mat.E4<-Create_Feat_mat(STEMsh.E4) ; Store(Feat_mat.E4)
Feat_mat.E5<-Create_Feat_mat(STEMsh.E5) ; Store(Feat_mat.E5)
Feat_mat.E6<-Create_Feat_mat(STEMsh.E6) ; Store(Feat_mat.E6)
Feat_mat.E7<-Create_Feat_mat(STEMsh.E7) ; Store(Feat_mat.E7)
Feat_mat.E8<-Create_Feat_mat(STEMsh.E8) ; Store(Feat_mat.E8)
Feat_mat.E9<-Create_Feat_mat(STEMsh.E9) ; Store(Feat_mat.E9)
Feat_mat.E10<-Create_Feat_mat(STEMsh.E10) ; Store(Feat_mat.E10)

#####################################################################
# assign number to words (token)
#####################################################################

Ass_token<-function(x) {
  out<-x
  for (j in 1:6) out[[j]]$token<-as.numeric(as.factor(as.character(x[[j]]$W)))  
  out
}

Feat_mat.E1<-Ass_token(Feat_mat.E1) ; Store(Feat_mat.E1)
Feat_mat.E2<-Ass_token(Feat_mat.E2) ; Store(Feat_mat.E2)
Feat_mat.E3<-Ass_token(Feat_mat.E3) ; Store(Feat_mat.E3)
Feat_mat.E4<-Ass_token(Feat_mat.E4) ; Store(Feat_mat.E4)
Feat_mat.E5<-Ass_token(Feat_mat.E5) ; Store(Feat_mat.E5)
Feat_mat.E6<-Ass_token(Feat_mat.E6) ; Store(Feat_mat.E6)
Feat_mat.E7<-Ass_token(Feat_mat.E7) ; Store(Feat_mat.E7)
Feat_mat.E8<-Ass_token(Feat_mat.E8) ; Store(Feat_mat.E8)
Feat_mat.E9<-Ass_token(Feat_mat.E9) ; Store(Feat_mat.E9)
Feat_mat.E10<-Ass_token(Feat_mat.E10) ; Store(Feat_mat.E10)

#####################################################################
# incidence matrix
#####################################################################

Inc_mat<-function(x) {
  out<-list()
  for (j in 1:6) out[[j]]<-as.matrix(sparseMatrix(x[[j]]$row_id,x[[j]]$token,x=rep(1,nrow(x[[j]]))))  
  out
}

bin_mat.2.E1<-Inc_mat(Feat_mat.E1) ; Store(bin_mat.2.E1)
bin_mat.2.E2<-Inc_mat(Feat_mat.E2) ; Store(bin_mat.2.E2)
bin_mat.2.E3<-Inc_mat(Feat_mat.E3) ; Store(bin_mat.2.E3)
bin_mat.2.E4<-Inc_mat(Feat_mat.E4) ; Store(bin_mat.2.E4)
bin_mat.2.E5<-Inc_mat(Feat_mat.E5) ; Store(bin_mat.2.E5)
bin_mat.2.E6<-Inc_mat(Feat_mat.E6) ; Store(bin_mat.2.E6)
bin_mat.2.E7<-Inc_mat(Feat_mat.E7) ; Store(bin_mat.2.E7)
bin_mat.2.E8<-Inc_mat(Feat_mat.E8) ; Store(bin_mat.2.E8)
bin_mat.2.E9<-Inc_mat(Feat_mat.E9) ; Store(bin_mat.2.E9)
bin_mat.2.E10<-Inc_mat(Feat_mat.E10) ; Store(bin_mat.2.E10)

#####################################################################
#####################################################################
# Ridit transformation
#####################################################################
#####################################################################

RIDIT_mat5<-function(b_in,b_out) {
  out<-list()
  for (j in 1:6) out[[j]]<-RIDIT_mat(b_in[[j]],b_out[[j]])
  out
}

Ridit_mat.2.E1<-RIDIT_mat5(bin_mat.2.E1,bin_mat.2.E1) ; Store(Ridit_mat.2.E1)
Ridit_mat.2.E2<-RIDIT_mat5(bin_mat.2.E2,bin_mat.2.E2) ; Store(Ridit_mat.2.E2)
Ridit_mat.2.E3<-RIDIT_mat5(bin_mat.2.E3,bin_mat.2.E3) ; Store(Ridit_mat.2.E3)
Ridit_mat.2.E4<-RIDIT_mat5(bin_mat.2.E4,bin_mat.2.E4) ; Store(Ridit_mat.2.E4)
Ridit_mat.2.E5<-RIDIT_mat5(bin_mat.2.E5,bin_mat.2.E5) ; Store(Ridit_mat.2.E5)
Ridit_mat.2.E6<-RIDIT_mat5(bin_mat.2.E6,bin_mat.2.E6) ; Store(Ridit_mat.2.E6)
Ridit_mat.2.E7<-RIDIT_mat5(bin_mat.2.E7,bin_mat.2.E7) ; Store(Ridit_mat.2.E7)
Ridit_mat.2.E8<-RIDIT_mat5(bin_mat.2.E8,bin_mat.2.E8) ; Store(Ridit_mat.2.E8)
Ridit_mat.2.E9<-RIDIT_mat5(bin_mat.2.E9,bin_mat.2.E9) ; Store(Ridit_mat.2.E9)
Ridit_mat.2.E10<-RIDIT_mat5(bin_mat.2.E10,bin_mat.2.E10) ; Store(Ridit_mat.2.E10)

#####################################################################
#####################################################################
# Compute Scaling metrics (Delta tfidf and Binormal separation)
#####################################################################
#####################################################################

#####################################################################
# Function to compute metrics
#####################################################################

METRICS<-function(bin_mat,train_labels) {
  numTrainExples = nrow(bin_mat)
  numTags = ncol(bin_mat)
  # number of documents in the positively labeled training set
  P = sum(train_labels==1)
  # number of documents in the positively labeled training set with term t
  Pt = colSums(bin_mat[train_labels==1,])
  # number of documents in the negatively labeled training set
  N = sum(train_labels==0)
  # number of documents in the negatively labeled training set with term t
  Nt = colSums(bin_mat[train_labels==0,])
  
  ########### metrics computation
  # idf
  idf = log((N+P)/(Pt+Nt+1))
  # delta_tfidf
  delta_tfidf<-log2(N/P*(Pt+1)/(Nt+1)) ; delta_tfidf[Nt+Pt<=10]<-0
  # log odds ratio
  log_odds <- NA
  #log_odds <- log(((Pt+1)*(N+1-Nt))/((Nt+1)*(P+1-Pt)))
  # bi-normal separation
  bns = abs(qnorm(pmin(0.999,(Pt+1)/P))-qnorm(pmin(0.999,(Nt+1)/N)))
  # entropy function
  nln<-function(x) x*log2(x)
  H<-function(x,y) -nln(x/(x+y)) - nln(y/(x+y))
  # information gain
  info_gain = H(P,N)-((Pt+Nt)/(P-Pt+N-Nt)*H(Pt+1,Nt+1) + (1-(Pt+Nt)/(P-Pt+N-Nt))*H(P-Pt,N-Nt)) 
  list(delta_tfidf=delta_tfidf,log_odds=log_odds,bns=bns,info_gain=info_gain)
}
Store(METRICS)

cv5_METRICS<-function(bin_mat,train_labels,K) {
  out<-list()
  for (i in 1:6) {
    out[[i]]<- if (i<=5) 
      METRICS(bin_mat[-K[[i]],],train_labels[-K[[i]]]) else
        METRICS(bin_mat,train_labels)
    print(paste("set",i,"done"))}
  out
}
Store(cv5_METRICS)

# 1rst set of bin_mat
# compute metrics using the dummy "isgood"
#####################################################################

metrics.E1<-cv5_METRICS(bin_mat.E1,IsGood.E1$isgood,K.E1) ; Store(metrics.E1)
metrics.E2<-cv5_METRICS(bin_mat.E2,IsGood.E2$isgood,K.E2) ; Store(metrics.E2)
metrics.E3<-cv5_METRICS(bin_mat.E3,IsGood.E3$isgood,K.E3) ; Store(metrics.E3)
metrics.E4<-cv5_METRICS(bin_mat.E4,IsGood.E4$isgood,K.E4) ; Store(metrics.E4)
metrics.E5<-cv5_METRICS(bin_mat.E5,IsGood.E5$isgood,K.E5) ; Store(metrics.E5)
metrics.E6<-cv5_METRICS(bin_mat.E6,IsGood.E6$isgood,K.E6) ; Store(metrics.E6)
metrics.E7<-cv5_METRICS(bin_mat.E7,IsGood.E7$isgood,K.E7) ; Store(metrics.E7)
metrics.E8<-cv5_METRICS(bin_mat.E8,IsGood.E8$isgood,K.E8) ; Store(metrics.E8)
metrics.E9<-cv5_METRICS(bin_mat.E9,IsGood.E9$isgood,K.E9) ; Store(metrics.E9)
metrics.E10<-cv5_METRICS(bin_mat.E10,IsGood.E10$isgood,K.E10) ; Store(metrics.E10)

# 1rst set of bin_mat
# compute metrics.IB using the dummy "isbad"
#####################################################################

metrics.IB.E1<-cv5_METRICS(bin_mat.E1,IsBad.E1$isbad,K.E1) ; Store(metrics.IB.E1)
metrics.IB.E2<-cv5_METRICS(bin_mat.E2,IsBad.E2$isbad,K.E2) ; Store(metrics.IB.E2)
metrics.IB.E3<-cv5_METRICS(bin_mat.E3,IsBad.E3$isbad,K.E3) ; Store(metrics.IB.E3)
metrics.IB.E4<-cv5_METRICS(bin_mat.E4,IsBad.E4$isbad,K.E4) ; Store(metrics.IB.E4)
metrics.IB.E5<-cv5_METRICS(bin_mat.E5,IsBad.E5$isbad,K.E5) ; Store(metrics.IB.E5)
metrics.IB.E6<-cv5_METRICS(bin_mat.E6,IsBad.E6$isbad,K.E6) ; Store(metrics.IB.E6)
metrics.IB.E7<-cv5_METRICS(bin_mat.E7,IsBad.E7$isbad,K.E7) ; Store(metrics.IB.E7)
metrics.IB.E8<-cv5_METRICS(bin_mat.E8,IsBad.E8$isbad,K.E8) ; Store(metrics.IB.E8)
metrics.IB.E9<-cv5_METRICS(bin_mat.E9,IsBad.E9$isbad,K.E9) ; Store(metrics.IB.E9)
metrics.IB.E10<-cv5_METRICS(bin_mat.E10,IsBad.E10$isbad,K.E10) ; Store(metrics.IB.E10)

# 2nd set of bin_mat
# compute metrics.2 using the dummy "isgood"
#####################################################################

cv5_METRICS.2<-function(bin_mat.2,train_labels,K) {
  out<-list()
  for (i in 1:6) {
    out[[i]]<- if (i<=5) 
      METRICS(bin_mat.2[[i]][-K[[i]],],train_labels[-K[[i]]]) else
        METRICS(bin_mat.2[[i]],train_labels)
    print(paste("set",i,"done"))}
  out
}
Store(cv5_METRICS.2)

metrics.2.E1<-cv5_METRICS.2(bin_mat.2.E1,IsGood.E1$isgood,K.E1) ; Store(metrics.2.E1)
metrics.2.E2<-cv5_METRICS.2(bin_mat.2.E2,IsGood.E2$isgood,K.E2) ; Store(metrics.2.E2)
metrics.2.E3<-cv5_METRICS.2(bin_mat.2.E3,IsGood.E3$isgood,K.E3) ; Store(metrics.2.E3)
metrics.2.E4<-cv5_METRICS.2(bin_mat.2.E4,IsGood.E4$isgood,K.E4) ; Store(metrics.2.E4)
metrics.2.E5<-cv5_METRICS.2(bin_mat.2.E5,IsGood.E5$isgood,K.E5) ; Store(metrics.2.E5)
metrics.2.E6<-cv5_METRICS.2(bin_mat.2.E6,IsGood.E6$isgood,K.E6) ; Store(metrics.2.E6)
metrics.2.E7<-cv5_METRICS.2(bin_mat.2.E7,IsGood.E7$isgood,K.E7) ; Store(metrics.2.E7)
metrics.2.E8<-cv5_METRICS.2(bin_mat.2.E8,IsGood.E8$isgood,K.E8) ; Store(metrics.2.E8)
metrics.2.E9<-cv5_METRICS.2(bin_mat.2.E9,IsGood.E9$isgood,K.E9) ; Store(metrics.2.E9)
metrics.2.E10<-cv5_METRICS.2(bin_mat.2.E10,IsGood.E10$isgood,K.E10) ; Store(metrics.2.E10)

# 2nd set of bin_mat
# compute metrics.IB.2 using the dummy "isbad"
#####################################################################

metrics.IB.2.E1<-cv5_METRICS.2(bin_mat.2.E1,IsBad.E1$isbad,K.E1) ; Store(metrics.IB.2.E1)
metrics.IB.2.E2<-cv5_METRICS.2(bin_mat.2.E2,IsBad.E2$isbad,K.E2) ; Store(metrics.IB.2.E2)
metrics.IB.2.E3<-cv5_METRICS.2(bin_mat.2.E3,IsBad.E3$isbad,K.E3) ; Store(metrics.IB.2.E3)
metrics.IB.2.E4<-cv5_METRICS.2(bin_mat.2.E4,IsBad.E4$isbad,K.E4) ; Store(metrics.IB.2.E4)
metrics.IB.2.E5<-cv5_METRICS.2(bin_mat.2.E5,IsBad.E5$isbad,K.E5) ; Store(metrics.IB.2.E5)
metrics.IB.2.E6<-cv5_METRICS.2(bin_mat.2.E6,IsBad.E6$isbad,K.E6) ; Store(metrics.IB.2.E6)
metrics.IB.2.E7<-cv5_METRICS.2(bin_mat.2.E7,IsBad.E7$isbad,K.E7) ; Store(metrics.IB.2.E7)
metrics.IB.2.E8<-cv5_METRICS.2(bin_mat.2.E8,IsBad.E8$isbad,K.E8) ; Store(metrics.IB.2.E8)
metrics.IB.2.E9<-cv5_METRICS.2(bin_mat.2.E9,IsBad.E9$isbad,K.E9) ; Store(metrics.IB.2.E9)
metrics.IB.2.E10<-cv5_METRICS.2(bin_mat.2.E10,IsBad.E10$isbad,K.E10) ; Store(metrics.IB.2.E10)

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

scal_mat<-function(mat,metrics,type) {
  out<-list()
  for (i in 1:6) {
    out[[i]]<- sweep(mat,MARGIN=2,metrics[[i]][[type]],'*')
    print(paste("set",i,"done"))}      
  out
}
Store(scal_mat)

scal_mat1.E1<-scal_mat(bin_mat.E1,metrics.E1,1) ; Store(scal_mat1.E1)
scal_mat1.E2<-scal_mat(bin_mat.E2,metrics.E2,1) ; Store(scal_mat1.E2)
scal_mat1.E3<-scal_mat(bin_mat.E3,metrics.E3,1) ; Store(scal_mat1.E3)
scal_mat1.E4<-scal_mat(bin_mat.E4,metrics.E4,1) ; Store(scal_mat1.E4)
scal_mat1.E5<-scal_mat(bin_mat.E5,metrics.E5,1) ; Store(scal_mat1.E5)
scal_mat1.E6<-scal_mat(bin_mat.E6,metrics.E6,1) ; Store(scal_mat1.E6)
scal_mat1.E7<-scal_mat(bin_mat.E7,metrics.E7,1) ; Store(scal_mat1.E7)
scal_mat1.E8<-scal_mat(bin_mat.E8,metrics.E8,1) ; Store(scal_mat1.E8)
scal_mat1.E9<-scal_mat(bin_mat.E9,metrics.E9,1) ; Store(scal_mat1.E9)
scal_mat1.E10<-scal_mat(bin_mat.E10,metrics.E10,1) ; Store(scal_mat1.E10)

# 1rst set of bin_mat
# based on dummy "is.bad"
#####################################################################

scal.IB_mat1.E1<-scal_mat(bin_mat.E1,metrics.IB.E1,1) ; Store(scal.IB_mat1.E1)
scal.IB_mat1.E2<-scal_mat(bin_mat.E2,metrics.IB.E2,1) ; Store(scal.IB_mat1.E2)
scal.IB_mat1.E3<-scal_mat(bin_mat.E3,metrics.IB.E3,1) ; Store(scal.IB_mat1.E3)
scal.IB_mat1.E4<-scal_mat(bin_mat.E4,metrics.IB.E4,1) ; Store(scal.IB_mat1.E4)
scal.IB_mat1.E5<-scal_mat(bin_mat.E5,metrics.IB.E5,1) ; Store(scal.IB_mat1.E5)
scal.IB_mat1.E6<-scal_mat(bin_mat.E6,metrics.IB.E6,1) ; Store(scal.IB_mat1.E6)
scal.IB_mat1.E7<-scal_mat(bin_mat.E7,metrics.IB.E7,1) ; Store(scal.IB_mat1.E7)
scal.IB_mat1.E8<-scal_mat(bin_mat.E8,metrics.IB.E8,1) ; Store(scal.IB_mat1.E8)
scal.IB_mat1.E9<-scal_mat(bin_mat.E9,metrics.IB.E9,1) ; Store(scal.IB_mat1.E9)
scal.IB_mat1.E10<-scal_mat(bin_mat.E10,metrics.IB.E10,1) ; Store(scal.IB_mat1.E10)

# 2nd set of bin_mat
# based on dummy "isgood"
#####################################################################

scal_mat.2<-function(mat,metrics,type) {
  out<-list()
  for (i in 1:6) {
    out[[i]]<- sweep(mat[[i]],MARGIN=2,metrics[[i]][[type]],'*')
    print(paste("set",i,"done"))}      
  out
}
Store(scal_mat.2)

scal_mat1.2.E1<-scal_mat.2(bin_mat.2.E1,metrics.2.E1,1) ; Store(scal_mat1.2.E1)
scal_mat1.2.E2<-scal_mat.2(bin_mat.2.E2,metrics.2.E2,1) ; Store(scal_mat1.2.E2)
scal_mat1.2.E3<-scal_mat.2(bin_mat.2.E3,metrics.2.E3,1) ; Store(scal_mat1.2.E3)
scal_mat1.2.E4<-scal_mat.2(bin_mat.2.E4,metrics.2.E4,1) ; Store(scal_mat1.2.E4)
scal_mat1.2.E5<-scal_mat.2(bin_mat.2.E5,metrics.2.E5,1) ; Store(scal_mat1.2.E5)
scal_mat1.2.E6<-scal_mat.2(bin_mat.2.E6,metrics.2.E6,1) ; Store(scal_mat1.2.E6)
scal_mat1.2.E7<-scal_mat.2(bin_mat.2.E7,metrics.2.E7,1) ; Store(scal_mat1.2.E7)
scal_mat1.2.E8<-scal_mat.2(bin_mat.2.E8,metrics.2.E8,1) ; Store(scal_mat1.2.E8)
scal_mat1.2.E9<-scal_mat.2(bin_mat.2.E9,metrics.2.E9,1) ; Store(scal_mat1.2.E9)
scal_mat1.2.E10<-scal_mat.2(bin_mat.2.E10,metrics.2.E10,1) ; Store(scal_mat1.2.E10)

# 2nd set of bin_mat
# based on dummy "isbad"
#####################################################################

scal.IB_mat1.2.E1<-scal_mat.2(bin_mat.2.E1,metrics.IB.2.E1,1) ; Store(scal.IB_mat1.2.E1)
scal.IB_mat1.2.E2<-scal_mat.2(bin_mat.2.E2,metrics.IB.2.E2,1) ; Store(scal.IB_mat1.2.E2)
scal.IB_mat1.2.E3<-scal_mat.2(bin_mat.2.E3,metrics.IB.2.E3,1) ; Store(scal.IB_mat1.2.E3)
scal.IB_mat1.2.E4<-scal_mat.2(bin_mat.2.E4,metrics.IB.2.E4,1) ; Store(scal.IB_mat1.2.E4)
scal.IB_mat1.2.E5<-scal_mat.2(bin_mat.2.E5,metrics.IB.2.E5,1) ; Store(scal.IB_mat1.2.E5)
scal.IB_mat1.2.E6<-scal_mat.2(bin_mat.2.E6,metrics.IB.2.E6,1) ; Store(scal.IB_mat1.2.E6)
scal.IB_mat1.2.E7<-scal_mat.2(bin_mat.2.E7,metrics.IB.2.E7,1) ; Store(scal.IB_mat1.2.E7)
scal.IB_mat1.2.E8<-scal_mat.2(bin_mat.2.E8,metrics.IB.2.E8,1) ; Store(scal.IB_mat1.2.E8)
scal.IB_mat1.2.E9<-scal_mat.2(bin_mat.2.E9,metrics.IB.2.E9,1) ; Store(scal.IB_mat1.2.E9)
scal.IB_mat1.2.E10<-scal_mat.2(bin_mat.2.E10,metrics.IB.2.E10,1) ; Store(scal.IB_mat1.2.E10)

#####################################################################
# scale incidence matrix with bns
#####################################################################

# 1rst set of bin_mat
# based on dummy "isgood"
#####################################################################

scal_mat3.E1<-scal_mat(bin_mat.E1,metrics.E1,3) ; Store(scal_mat3.E1)
scal_mat3.E2<-scal_mat(bin_mat.E2,metrics.E2,3) ; Store(scal_mat3.E2)
scal_mat3.E3<-scal_mat(bin_mat.E3,metrics.E3,3) ; Store(scal_mat3.E3)
scal_mat3.E4<-scal_mat(bin_mat.E4,metrics.E4,3) ; Store(scal_mat3.E4)
scal_mat3.E5<-scal_mat(bin_mat.E5,metrics.E5,3) ; Store(scal_mat3.E5)
scal_mat3.E6<-scal_mat(bin_mat.E6,metrics.E6,3) ; Store(scal_mat3.E6)
scal_mat3.E7<-scal_mat(bin_mat.E7,metrics.E7,3) ; Store(scal_mat3.E7)
scal_mat3.E8<-scal_mat(bin_mat.E8,metrics.E8,3) ; Store(scal_mat3.E8)
scal_mat3.E9<-scal_mat(bin_mat.E9,metrics.E9,3) ; Store(scal_mat3.E9)
scal_mat3.E10<-scal_mat(bin_mat.E10,metrics.E10,3) ; Store(scal_mat3.E10)

# 1rst set of bin_mat
# based on dummy "isbad"
#####################################################################

scal.IB_mat3.E1<-scal_mat(bin_mat.E1,metrics.IB.E1,3) ; Store(scal.IB_mat3.E1)
scal.IB_mat3.E2<-scal_mat(bin_mat.E2,metrics.IB.E2,3) ; Store(scal.IB_mat3.E2)
scal.IB_mat3.E3<-scal_mat(bin_mat.E3,metrics.IB.E3,3) ; Store(scal.IB_mat3.E3)
scal.IB_mat3.E4<-scal_mat(bin_mat.E4,metrics.IB.E4,3) ; Store(scal.IB_mat3.E4)
scal.IB_mat3.E5<-scal_mat(bin_mat.E5,metrics.IB.E5,3) ; Store(scal.IB_mat3.E5)
scal.IB_mat3.E6<-scal_mat(bin_mat.E6,metrics.IB.E6,3) ; Store(scal.IB_mat3.E6)
scal.IB_mat3.E7<-scal_mat(bin_mat.E7,metrics.IB.E7,3) ; Store(scal.IB_mat3.E7)
scal.IB_mat3.E8<-scal_mat(bin_mat.E8,metrics.IB.E8,3) ; Store(scal.IB_mat3.E8)
scal.IB_mat3.E9<-scal_mat(bin_mat.E9,metrics.IB.E9,3) ; Store(scal.IB_mat3.E9)
scal.IB_mat3.E10<-scal_mat(bin_mat.E10,metrics.IB.E10,3) ; Store(scal.IB_mat3.E10)

# 2nd set of bin_mat
# based on dummy "isgood"
#####################################################################

scal_mat3.2.E1<-scal_mat.2(bin_mat.2.E1,metrics.2.E1,3) ; Store(scal_mat3.2.E1)
scal_mat3.2.E2<-scal_mat.2(bin_mat.2.E2,metrics.2.E2,3) ; Store(scal_mat3.2.E2)
scal_mat3.2.E3<-scal_mat.2(bin_mat.2.E3,metrics.2.E3,3) ; Store(scal_mat3.2.E3)
scal_mat3.2.E4<-scal_mat.2(bin_mat.2.E4,metrics.2.E4,3) ; Store(scal_mat3.2.E4)
scal_mat3.2.E5<-scal_mat.2(bin_mat.2.E5,metrics.2.E5,3) ; Store(scal_mat3.2.E5)
scal_mat3.2.E6<-scal_mat.2(bin_mat.2.E6,metrics.2.E6,3) ; Store(scal_mat3.2.E6)
scal_mat3.2.E7<-scal_mat.2(bin_mat.2.E7,metrics.2.E7,3) ; Store(scal_mat3.2.E7)
scal_mat3.2.E8<-scal_mat.2(bin_mat.2.E8,metrics.2.E8,3) ; Store(scal_mat3.2.E8)
scal_mat3.2.E9<-scal_mat.2(bin_mat.2.E9,metrics.2.E9,3) ; Store(scal_mat3.2.E9)
scal_mat3.2.E10<-scal_mat.2(bin_mat.2.E10,metrics.2.E10,3) ; Store(scal_mat3.2.E10)

# 2nd set of bin_mat
# based on dummy "isbad"
#####################################################################

scal.IB_mat3.2.E1<-scal_mat.2(bin_mat.2.E1,metrics.IB.2.E1,3) ; Store(scal.IB_mat3.2.E1)
scal.IB_mat3.2.E2<-scal_mat.2(bin_mat.2.E2,metrics.IB.2.E2,3) ; Store(scal.IB_mat3.2.E2)
scal.IB_mat3.2.E3<-scal_mat.2(bin_mat.2.E3,metrics.IB.2.E3,3) ; Store(scal.IB_mat3.2.E3)
scal.IB_mat3.2.E4<-scal_mat.2(bin_mat.2.E4,metrics.IB.2.E4,3) ; Store(scal.IB_mat3.2.E4)
scal.IB_mat3.2.E5<-scal_mat.2(bin_mat.2.E5,metrics.IB.2.E5,3) ; Store(scal.IB_mat3.2.E5)
scal.IB_mat3.2.E6<-scal_mat.2(bin_mat.2.E6,metrics.IB.2.E6,3) ; Store(scal.IB_mat3.2.E6)
scal.IB_mat3.2.E7<-scal_mat.2(bin_mat.2.E7,metrics.IB.2.E7,3) ; Store(scal.IB_mat3.2.E7)
scal.IB_mat3.2.E8<-scal_mat.2(bin_mat.2.E8,metrics.IB.2.E8,3) ; Store(scal.IB_mat3.2.E8)
scal.IB_mat3.2.E9<-scal_mat.2(bin_mat.2.E9,metrics.IB.2.E9,3) ; Store(scal.IB_mat3.2.E9)
scal.IB_mat3.2.E10<-scal_mat.2(bin_mat.2.E10,metrics.IB.2.E10,3) ; Store(scal.IB_mat3.2.E10)

#####################################################################
#####################################################################
# train GNET (standardize=FALSE) on 
# 2 sets of bin_mat after Ridit transformation
#####################################################################
#####################################################################

# 1rst set of bin_mat (transformed with ridit coding) 
#####################################################################

library(glmnet)
GNET1R.E1<-cv5_GNET(Ridit_mat.E1,K.E1,training.E1$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.E1)
GNET1R.E2<-cv5_GNET(Ridit_mat.E2,K.E2,training.E2$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.E2)
GNET1R.E3<-cv5_GNET(Ridit_mat.E3,K.E3,training.E3$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.E3)
GNET1R.E4<-cv5_GNET(Ridit_mat.E4,K.E4,training.E4$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.E4)
GNET1R.E5<-cv5_GNET(Ridit_mat.E5,K.E5,training.E5$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.E5)
GNET1R.E6<-cv5_GNET(Ridit_mat.E6,K.E6,training.E6$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.E6)
GNET1R.E7<-cv5_GNET(Ridit_mat.E7,K.E7,training.E7$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.E7)
GNET1R.E8<-cv5_GNET(Ridit_mat.E8,K.E8,training.E8$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.E8)
GNET1R.E9<-cv5_GNET(Ridit_mat.E9,K.E9,training.E9$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.E9)
GNET1R.E10<-cv5_GNET(Ridit_mat.E10,K.E10,training.E10$Score1,alpha=0.5,FALSE,1698) ; Store(GNET1R.E10)

GNET1R<-Group(list(GNET1R.E1[[2]],GNET1R.E2[[2]],GNET1R.E3[[2]],GNET1R.E4[[2]],GNET1R.E5[[2]],
                   GNET1R.E6[[2]],GNET1R.E7[[2]],GNET1R.E8[[2]],GNET1R.E9[[2]],GNET1R.E10[[2]])) ; Store(GNET1R)
Spear_score(GNET1R)
graph1b(GNET1R)
Kappa_score(GNET1R)

# 2nd set of bin_mat (transformed with ridit coding) 
#####################################################################

# function to train glmnet on a list of data matrix
#####################################################################

cv5_GNET2<-function(data,K,train_scores,alpha,seed) {
  set.seed(seed)
  out<-list()
  out[[1]]<-list()
  for (i in 1:6) {
    out[[1]][[i]]<- if (i<=5) 
      cv.glmnet(as.matrix(data[[i]][-K[[i]],]),y=train_scores[-K[[i]]],family="gaussian",standardize=FALSE, alpha=alpha) else
        cv.glmnet(as.matrix(data[[i]]),y=train_scores,family="gaussian",standardize=FALSE, alpha=alpha)
    print(paste("set",i,"done"))}    	
  out[[2]]<-rep(0,length(train_scores))
  for (i in 1:5) out[[2]][K[[i]]]<-predict(out[[1]][[i]],as.matrix(data[[i]][K[[i]],]),type="response", s="lambda.min")[,1]
  out
}
Store(cv5_GNET2)

library(glmnet)
GNET1R.2.E1<-cv5_GNET2(Ridit_mat.2.E1,K.E1,training.E1$Score1,alpha=0.5,1698) ; Store(GNET1R.2.E1)
GNET1R.2.E2<-cv5_GNET2(Ridit_mat.2.E2,K.E2,training.E2$Score1,alpha=0.5,1698) ; Store(GNET1R.2.E2)
GNET1R.2.E3<-cv5_GNET2(Ridit_mat.2.E3,K.E3,training.E3$Score1,alpha=0.5,1698) ; Store(GNET1R.2.E3)
GNET1R.2.E4<-cv5_GNET2(Ridit_mat.2.E4,K.E4,training.E4$Score1,alpha=0.5,1698) ; Store(GNET1R.2.E4)
GNET1R.2.E5<-cv5_GNET2(Ridit_mat.2.E5,K.E5,training.E5$Score1,alpha=0.5,1698) ; Store(GNET1R.2.E5)
GNET1R.2.E6<-cv5_GNET2(Ridit_mat.2.E6,K.E6,training.E6$Score1,alpha=0.5,1698) ; Store(GNET1R.2.E6)
GNET1R.2.E7<-cv5_GNET2(Ridit_mat.2.E7,K.E7,training.E7$Score1,alpha=0.5,1698) ; Store(GNET1R.2.E7)
GNET1R.2.E8<-cv5_GNET2(Ridit_mat.2.E8,K.E8,training.E8$Score1,alpha=0.5,1698) ; Store(GNET1R.2.E8)
GNET1R.2.E9<-cv5_GNET2(Ridit_mat.2.E9,K.E9,training.E9$Score1,alpha=0.5,1698) ; Store(GNET1R.2.E9)
GNET1R.2.E10<-cv5_GNET2(Ridit_mat.2.E10,K.E10,training.E10$Score1,alpha=0.5,1698) ; Store(GNET1R.2.E10)

GNET1R.2<-Group(list(GNET1R.2.E1[[2]],GNET1R.2.E2[[2]],GNET1R.2.E3[[2]],GNET1R.2.E4[[2]],GNET1R.2.E5[[2]],
                     GNET1R.2.E6[[2]],GNET1R.2.E7[[2]],GNET1R.2.E8[[2]],GNET1R.2.E9[[2]],GNET1R.2.E10[[2]])) ; Store(GNET1R.2)
Spear_score(GNET1R.2)
graph1b(GNET1R.2)
Kappa_score(GNET1R.2)

#####################################################################
#####################################################################
# TRAIN SVM
# on 2 sets of bin mat
# scaled with bns using "isgood" or "isbad"
# with linear and radial kernels
# regression, multinomial and binary classification
#####################################################################
#####################################################################

#####################################################################
# TRAIN SVM with linear kernel on scaled matrix
#####################################################################

# function to train SVM with linear kernel
#####################################################################

cv5_SVM<-function(data,K,train_scores,cost,seed) {
  set.seed(seed)
  out<-list()
  out[[1]]<-list()
  for (i in 1:6) {
    out[[1]][[i]]<-if (i<=5) 
      svm(x=data[[i]][-K[[i]],],y=train_scores[-K[[i]]],cost=cost,kernel="linear",scale=FALSE) else
        svm(x=data[[i]],y=train_scores,cost=cost,kernel="linear",scale=FALSE)
    print(paste("set",i,"done"))}    	
  out[[2]]<-rep(0,length(train_scores))
  for (i in 1:5) out[[2]][K[[i]]]<-predict(out[[1]][[i]],data[[i]][K[[i]],]) 
  out
}
Store(cv5_SVM)

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

SVM3.E1<-cv5_SVM(scal_mat3.E1,K.E1,training.E1$Score1,0.1,267) ; Store(SVM3.E1)
SVM3.E2<-cv5_SVM(scal_mat3.E2,K.E2,training.E2$Score1,0.1,207) ; Store(SVM3.E2)
SVM3.E3<-cv5_SVM(scal_mat3.E3,K.E3,training.E3$Score1,0.1,167) ; Store(SVM3.E3)
SVM3.E4<-cv5_SVM(scal_mat3.E4,K.E4,training.E4$Score1,0.1,287) ; Store(SVM3.E4)
SVM3.E5<-cv5_SVM(scal_mat3.E5,K.E5,training.E5$Score1,0.1,232) ; Store(SVM3.E5)
SVM3.E6<-cv5_SVM(scal_mat3.E6,K.E6,training.E6$Score1,0.1,223) ; Store(SVM3.E6)
SVM3.E7<-cv5_SVM(scal_mat3.E7,K.E7,training.E7$Score1,0.1,478) ; Store(SVM3.E7)
SVM3.E8<-cv5_SVM(scal_mat3.E8,K.E8,training.E8$Score1,0.1,238) ; Store(SVM3.E8)
SVM3.E9<-cv5_SVM(scal_mat3.E9,K.E9,training.E9$Score1,0.1,108) ; Store(SVM3.E9)
SVM3.E10<-cv5_SVM(scal_mat3.E10,K.E10,training.E10$Score1,0.1,232) ; Store(SVM3.E10)

SVM3<-Group(list(SVM3.E1[[2]],SVM3.E2[[2]],SVM3.E3[[2]],SVM3.E4[[2]],SVM3.E5[[2]],
                 SVM3.E6[[2]],SVM3.E7[[2]],SVM3.E8[[2]],SVM3.E9[[2]],SVM3.E10[[2]])) ; Store(SVM3)

Spear_score(SVM3)
graph1b(SVM3)
Kappa_score(SVM3)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

SVM3.IB.E1<-cv5_SVM(scal.IB_mat3.E1,K.E1,training.E1$Score1,0.1,267) ; Store(SVM3.IB.E1)
SVM3.IB.E2<-cv5_SVM(scal.IB_mat3.E2,K.E2,training.E2$Score1,0.1,207) ; Store(SVM3.IB.E2)
SVM3.IB.E3<-cv5_SVM(scal.IB_mat3.E3,K.E3,training.E3$Score1,0.1,167) ; Store(SVM3.IB.E3)
SVM3.IB.E4<-cv5_SVM(scal.IB_mat3.E4,K.E4,training.E4$Score1,0.1,287) ; Store(SVM3.IB.E4)
SVM3.IB.E5<-cv5_SVM(scal.IB_mat3.E5,K.E5,training.E5$Score1,0.1,832) ; Store(SVM3.IB.E5)
SVM3.IB.E6<-cv5_SVM(scal.IB_mat3.E6,K.E6,training.E6$Score1,0.1,253) ; Store(SVM3.IB.E6)
SVM3.IB.E7<-cv5_SVM(scal.IB_mat3.E7,K.E7,training.E7$Score1,0.1,478) ; Store(SVM3.IB.E7)
SVM3.IB.E8<-cv5_SVM(scal.IB_mat3.E8,K.E8,training.E8$Score1,0.1,238) ; Store(SVM3.IB.E8)
SVM3.IB.E9<-cv5_SVM(scal.IB_mat3.E9,K.E9,training.E9$Score1,0.1,108) ; Store(SVM3.IB.E9)
SVM3.IB.E10<-cv5_SVM(scal.IB_mat3.E10,K.E10,training.E10$Score1,0.1,232) ; Store(SVM3.IB.E10)

SVM3.IB<-Group(list(SVM3.IB.E1[[2]],SVM3.IB.E2[[2]],SVM3.IB.E3[[2]],SVM3.IB.E4[[2]],SVM3.IB.E5[[2]],
                    SVM3.IB.E6[[2]],SVM3.IB.E7[[2]],SVM3.IB.E8[[2]],SVM3.IB.E9[[2]],SVM3.IB.E10[[2]])) ; Store(SVM3.IB)

Spear_score(SVM3.IB)
graph1b(SVM3.IB)
Kappa_score(SVM3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

SVM3.2.E1<-cv5_SVM(scal_mat3.2.E1,K.E1,training.E1$Score1,0.1,267) ; Store(SVM3.2.E1)
SVM3.2.E2<-cv5_SVM(scal_mat3.2.E2,K.E2,training.E2$Score1,0.1,207) ; Store(SVM3.2.E2)
SVM3.2.E3<-cv5_SVM(scal_mat3.2.E3,K.E3,training.E3$Score1,0.1,167) ; Store(SVM3.2.E3)
SVM3.2.E4<-cv5_SVM(scal_mat3.2.E4,K.E4,training.E4$Score1,0.1,287) ; Store(SVM3.2.E4)
SVM3.2.E5<-cv5_SVM(scal_mat3.2.E5,K.E5,training.E5$Score1,0.1,232) ; Store(SVM3.2.E5)
SVM3.2.E6<-cv5_SVM(scal_mat3.2.E6,K.E6,training.E6$Score1,0.1,223) ; Store(SVM3.2.E6)
SVM3.2.E7<-cv5_SVM(scal_mat3.2.E7,K.E7,training.E7$Score1,0.1,478) ; Store(SVM3.2.E7)
SVM3.2.E8<-cv5_SVM(scal_mat3.2.E8,K.E8,training.E8$Score1,0.1,238) ; Store(SVM3.2.E8)
SVM3.2.E9<-cv5_SVM(scal_mat3.2.E9,K.E9,training.E9$Score1,0.1,108) ; Store(SVM3.2.E9)
SVM3.2.E10<-cv5_SVM(scal_mat3.2.E10,K.E10,training.E10$Score1,0.1,232) ; Store(SVM3.2.E10)

SVM3.2<-Group(list(SVM3.2.E1[[2]],SVM3.2.E2[[2]],SVM3.2.E3[[2]],SVM3.2.E4[[2]],SVM3.2.E5[[2]],
                   SVM3.2.E6[[2]],SVM3.2.E7[[2]],SVM3.2.E8[[2]],SVM3.2.E9[[2]],SVM3.2.E10[[2]])) ; Store(SVM3.2)

Spear_score(SVM3.2)
graph1b(SVM3.2)
Kappa_score(SVM3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

SVM3.IB.2.E1<-cv5_SVM(scal.IB_mat3.2.E1,K.E1,training.E1$Score1,0.1,267) ; Store(SVM3.IB.2.E1)
SVM3.IB.2.E2<-cv5_SVM(scal.IB_mat3.2.E2,K.E2,training.E2$Score1,0.1,207) ; Store(SVM3.IB.2.E2)
SVM3.IB.2.E3<-cv5_SVM(scal.IB_mat3.2.E3,K.E3,training.E3$Score1,0.1,167) ; Store(SVM3.IB.2.E3)
SVM3.IB.2.E4<-cv5_SVM(scal.IB_mat3.2.E4,K.E4,training.E4$Score1,0.1,287) ; Store(SVM3.IB.2.E4)
SVM3.IB.2.E5<-cv5_SVM(scal.IB_mat3.2.E5,K.E5,training.E5$Score1,0.1,232) ; Store(SVM3.IB.2.E5)
SVM3.IB.2.E6<-cv5_SVM(scal.IB_mat3.2.E6,K.E6,training.E6$Score1,0.1,223) ; Store(SVM3.IB.2.E6)
SVM3.IB.2.E7<-cv5_SVM(scal.IB_mat3.2.E7,K.E7,training.E7$Score1,0.1,478) ; Store(SVM3.IB.2.E7)
SVM3.IB.2.E8<-cv5_SVM(scal.IB_mat3.2.E8,K.E8,training.E8$Score1,0.1,238) ; Store(SVM3.IB.2.E8)
SVM3.IB.2.E9<-cv5_SVM(scal.IB_mat3.2.E9,K.E9,training.E9$Score1,0.1,108) ; Store(SVM3.IB.2.E9)
SVM3.IB.2.E10<-cv5_SVM(scal.IB_mat3.2.E10,K.E10,training.E10$Score1,0.1,232) ; Store(SVM3.IB.2.E10)

SVM3.IB.2<-Group(list(SVM3.IB.2.E1[[2]],SVM3.IB.2.E2[[2]],SVM3.IB.2.E3[[2]],SVM3.IB.2.E4[[2]],SVM3.IB.2.E5[[2]],
                      SVM3.IB.2.E6[[2]],SVM3.IB.2.E7[[2]],SVM3.IB.2.E8[[2]],SVM3.IB.2.E9[[2]],SVM3.IB.2.E10[[2]])) ; Store(SVM3.IB.2)

Spear_score(SVM3.IB.2)
graph1b(SVM3.IB.2)
Kappa_score(SVM3.IB.2)

#####################################################################
# TRAIN SVM with radial kernel on scaled matrix
#####################################################################

Cv5_RAD2<-function(data,K,y,cost,gamma,kernel,seed) {
  out<-list()
  out[[1]]<-list()
  out[[2]]<-rep(0,nrow(data[[1]]))
  data<-as.matrix(data)
  for (i in 1:5) {
    set.seed(seed+i*100)
    out[[1]][[i]]<-svm(x=data[[i]][-K[[i]],],y=y[-K[[i]]],cost=cost,gamma=gamma,kernel=kernel,scale=FALSE)
    out[[2]][K[[i]]]<-predict(out[[1]][[i]],data[[i]][K[[i]],])
    print(paste("set",i,"done"))}      
  out[[1]][[6]]<-svm(x=data[[6]],y=y,cost=cost,gamma=gamma,kernel=kernel,scale=FALSE)
  out
}
Store(Cv5_RAD2)

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

RAD2.E1<-Cv5_RAD2(scal_mat3.E1,K.E1,training.E1$Score1,3,0.03,"radial",892) ; Store(RAD2.E1)
RAD2.E2<-Cv5_RAD2(scal_mat3.E2,K.E2,training.E2$Score1,3,0.03,"radial",892) ; Store(RAD2.E2)
RAD2.E3<-Cv5_RAD2(scal_mat3.E3,K.E3,training.E3$Score1,3,0.03,"radial",892) ; Store(RAD2.E3)
RAD2.E4<-Cv5_RAD2(scal_mat3.E4,K.E4,training.E4$Score1,3,0.03,"radial",892) ; Store(RAD2.E4)
RAD2.E5<-Cv5_RAD2(scal_mat3.E5,K.E5,training.E5$Score1,3,0.03,"radial",892) ; Store(RAD2.E5)
RAD2.E6<-Cv5_RAD2(scal_mat3.E6,K.E6,training.E6$Score1,3,0.03,"radial",892) ; Store(RAD2.E6)
RAD2.E7<-Cv5_RAD2(scal_mat3.E7,K.E7,training.E7$Score1,3,0.03,"radial",892) ; Store(RAD2.E7)
RAD2.E8<-Cv5_RAD2(scal_mat3.E8,K.E8,training.E8$Score1,3,0.03,"radial",892) ; Store(RAD2.E8)
RAD2.E9<-Cv5_RAD2(scal_mat3.E9,K.E9,training.E9$Score1,3,0.03,"radial",892) ; Store(RAD2.E9)
RAD2.E10<-Cv5_RAD2(scal_mat3.E10,K.E10,training.E10$Score1,3,0.03,"radial",892) ; Store(RAD2.E10)

RAD2<-Group(list(RAD2.E1[[2]],RAD2.E2[[2]],RAD2.E3[[2]],RAD2.E4[[2]],RAD2.E5[[2]],
                 RAD2.E6[[2]],RAD2.E7[[2]],RAD2.E8[[2]],RAD2.E9[[2]],RAD2.E10[[2]])) ; Store(RAD2)
Spear_score(RAD2)
graph1b(RAD2)
Kappa_score(RAD2)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

RAD2.IB.E1<-Cv5_RAD2(scal.IB_mat3.E1,K.E1,training.E1$Score1,3,0.03,"radial",892) ; Store(RAD2.IB.E1)
RAD2.IB.E2<-Cv5_RAD2(scal.IB_mat3.E2,K.E2,training.E2$Score1,3,0.03,"radial",892) ; Store(RAD2.IB.E2)
RAD2.IB.E3<-Cv5_RAD2(scal.IB_mat3.E3,K.E3,training.E3$Score1,3,0.03,"radial",892) ; Store(RAD2.IB.E3)
RAD2.IB.E4<-Cv5_RAD2(scal.IB_mat3.E4,K.E4,training.E4$Score1,3,0.03,"radial",892) ; Store(RAD2.IB.E4)
RAD2.IB.E5<-Cv5_RAD2(scal.IB_mat3.E5,K.E5,training.E5$Score1,3,0.03,"radial",892) ; Store(RAD2.IB.E5)
RAD2.IB.E6<-Cv5_RAD2(scal.IB_mat3.E6,K.E6,training.E6$Score1,3,0.03,"radial",892) ; Store(RAD2.IB.E6)
RAD2.IB.E7<-Cv5_RAD2(scal.IB_mat3.E7,K.E7,training.E7$Score1,3,0.03,"radial",892) ; Store(RAD2.IB.E7)
RAD2.IB.E8<-Cv5_RAD2(scal.IB_mat3.E8,K.E8,training.E8$Score1,3,0.03,"radial",892) ; Store(RAD2.IB.E8)
RAD2.IB.E9<-Cv5_RAD2(scal.IB_mat3.E9,K.E9,training.E9$Score1,3,0.03,"radial",892) ; Store(RAD2.IB.E9)
RAD2.IB.E10<-Cv5_RAD2(scal.IB_mat3.E10,K.E10,training.E10$Score1,3,0.03,"radial",892) ; Store(RAD2.IB.E10)

RAD2.IB<-Group(list(RAD2.IB.E1[[2]],RAD2.IB.E2[[2]],RAD2.IB.E3[[2]],RAD2.IB.E4[[2]],RAD2.IB.E5[[2]],
                    RAD2.IB.E6[[2]],RAD2.IB.E7[[2]],RAD2.IB.E8[[2]],RAD2.IB.E9[[2]],RAD2.IB.E10[[2]])) ; Store(RAD2.IB)
Spear_score(RAD2.IB)
graph1b(RAD2.IB)
Kappa_score(RAD2.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

RAD2.2.E1<-Cv5_RAD2(scal_mat3.2.E1,K.E1,training.E1$Score1,1,0.01,"radial",892) ; Store(RAD2.2.E1)
RAD2.2.E2<-Cv5_RAD2(scal_mat3.2.E2,K.E2,training.E2$Score1,1,0.01,"radial",892) ; Store(RAD2.2.E2)
RAD2.2.E3<-Cv5_RAD2(scal_mat3.2.E3,K.E3,training.E3$Score1,1,0.01,"radial",892) ; Store(RAD2.2.E3)
RAD2.2.E4<-Cv5_RAD2(scal_mat3.2.E4,K.E4,training.E4$Score1,1,0.01,"radial",892) ; Store(RAD2.2.E4)
RAD2.2.E5<-Cv5_RAD2(scal_mat3.2.E5,K.E5,training.E5$Score1,1,0.01,"radial",892) ; Store(RAD2.2.E5)
RAD2.2.E6<-Cv5_RAD2(scal_mat3.2.E6,K.E6,training.E6$Score1,1,0.01,"radial",892) ; Store(RAD2.2.E6)
RAD2.2.E7<-Cv5_RAD2(scal_mat3.2.E7,K.E7,training.E7$Score1,1,0.01,"radial",892) ; Store(RAD2.2.E7)
RAD2.2.E8<-Cv5_RAD2(scal_mat3.2.E8,K.E8,training.E8$Score1,1,0.01,"radial",892) ; Store(RAD2.2.E8)
RAD2.2.E9<-Cv5_RAD2(scal_mat3.2.E9,K.E9,training.E9$Score1,1,0.01,"radial",892) ; Store(RAD2.2.E9)
RAD2.2.E10<-Cv5_RAD2(scal_mat3.2.E10,K.E10,training.E10$Score1,1,0.01,"radial",892) ; Store(RAD2.2.E10)

RAD2.2<-Group(list(RAD2.2.E1[[2]],RAD2.2.E2[[2]],RAD2.2.E3[[2]],RAD2.2.E4[[2]],RAD2.2.E5[[2]],
                   RAD2.2.E6[[2]],RAD2.2.E7[[2]],RAD2.2.E8[[2]],RAD2.2.E9[[2]],RAD2.2.E10[[2]])) ; Store(RAD2.2)
Spear_score(RAD2.2)
graph1b(RAD2.2)
Kappa_score(RAD2.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

RAD2.IB.2.E1<-Cv5_RAD2(scal.IB_mat3.2.E1,K.E1,training.E1$Score1,3,0.01,"radial",892) ; Store(RAD2.IB.2.E1)
RAD2.IB.2.E2<-Cv5_RAD2(scal.IB_mat3.2.E2,K.E2,training.E2$Score1,3,0.01,"radial",892) ; Store(RAD2.IB.2.E2)
RAD2.IB.2.E3<-Cv5_RAD2(scal.IB_mat3.2.E3,K.E3,training.E3$Score1,3,0.01,"radial",892) ; Store(RAD2.IB.2.E3)
RAD2.IB.2.E4<-Cv5_RAD2(scal.IB_mat3.2.E4,K.E4,training.E4$Score1,3,0.01,"radial",892) ; Store(RAD2.IB.2.E4)
RAD2.IB.2.E5<-Cv5_RAD2(scal.IB_mat3.2.E5,K.E5,training.E5$Score1,3,0.01,"radial",892) ; Store(RAD2.IB.2.E5)
RAD2.IB.2.E6<-Cv5_RAD2(scal.IB_mat3.2.E6,K.E6,training.E6$Score1,3,0.01,"radial",892) ; Store(RAD2.IB.2.E6)
RAD2.IB.2.E7<-Cv5_RAD2(scal.IB_mat3.2.E7,K.E7,training.E7$Score1,3,0.01,"radial",892) ; Store(RAD2.IB.2.E7)
RAD2.IB.2.E8<-Cv5_RAD2(scal.IB_mat3.2.E8,K.E8,training.E8$Score1,3,0.01,"radial",892) ; Store(RAD2.IB.2.E8)
RAD2.IB.2.E9<-Cv5_RAD2(scal.IB_mat3.2.E9,K.E9,training.E9$Score1,3,0.01,"radial",892) ; Store(RAD2.IB.2.E9)
RAD2.IB.2.E10<-Cv5_RAD2(scal.IB_mat3.2.E10,K.E10,training.E10$Score1,3,0.01,"radial",892) ; Store(RAD2.IB.2.E10)

RAD2.IB.2<-Group(list(RAD2.IB.2.E1[[2]],RAD2.IB.2.E2[[2]],RAD2.IB.2.E3[[2]],RAD2.IB.2.E4[[2]],RAD2.IB.2.E5[[2]],
                      RAD2.IB.2.E6[[2]],RAD2.IB.2.E7[[2]],RAD2.IB.2.E8[[2]],RAD2.IB.2.E9[[2]],RAD2.IB.2.E10[[2]])) ; Store(RAD2.IB.2)
Spear_score(RAD2.IB.2)
graph1b(RAD2.IB.2)
Kappa_score(RAD2.IB.2)

#####################################################################
# TRAIN SVM with linear kernel to model binary response good/poor essays
#####################################################################

# function to train SVMc
#####################################################################

cv5_SVMc<-function(data,K,train_labels,cost,seed) {
  set.seed(seed)
  out<-list()
  out[[1]]<-list()
  for (i in 1:6) {
    out[[1]][[i]]<-if (i<=5) 
      svm(x=data[[i]][-K[[i]],],y=factor(train_labels[-K[[i]]]),cost=cost,kernel="linear",scale=FALSE,probability =TRUE) else
        svm(x=data[[i]],y=factor(train_labels),cost=cost,kernel="linear",scale=FALSE,probability =TRUE)
    print(paste("set",i,"done"))}
  out[[2]]<-rep(0,length(train_labels))
  for (i in 1:5) {
    out[[2]][K[[i]]]<-attr(predict(out[[1]][[i]],data[[i]][K[[i]],],probability =TRUE),"probabilities")[,"1"]} 
  out
}
Store(cv5_SVMc)

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

SVMc3b.E1<-cv5_SVMc(scal_mat3.E1,K.E1,IsGood.E1$isgood,0.03,267) ; Store(SVMc3b.E1)
SVMc3b.E2<-cv5_SVMc(scal_mat3.E2,K.E2,IsGood.E2$isgood,0.03,207) ; Store(SVMc3b.E2)
SVMc3b.E3<-cv5_SVMc(scal_mat3.E3,K.E3,IsGood.E3$isgood,0.03,167) ; Store(SVMc3b.E3)
SVMc3b.E4<-cv5_SVMc(scal_mat3.E4,K.E4,IsGood.E4$isgood,0.03,287) ; Store(SVMc3b.E4)
SVMc3b.E5<-cv5_SVMc(scal_mat3.E5,K.E5,IsGood.E5$isgood,0.03,232) ; Store(SVMc3b.E5)
SVMc3b.E6<-cv5_SVMc(scal_mat3.E6,K.E6,IsGood.E6$isgood,0.03,223) ; Store(SVMc3b.E6)
SVMc3b.E7<-cv5_SVMc(scal_mat3.E7,K.E7,IsGood.E7$isgood,0.03,478) ; Store(SVMc3b.E7)
SVMc3b.E8<-cv5_SVMc(scal_mat3.E8,K.E8,IsGood.E8$isgood,0.03,238) ; Store(SVMc3b.E8)
SVMc3b.E9<-cv5_SVMc(scal_mat3.E9,K.E9,IsGood.E9$isgood,0.03,208) ; Store(SVMc3b.E9)
SVMc3b.E10<-cv5_SVMc(scal_mat3.E10,K.E10,IsGood.E10$isgood,0.03,138) ; Store(SVMc3b.E10)

SVMc3b<-Group(list(SVMc3b.E1[[2]],SVMc3b.E2[[2]],SVMc3b.E3[[2]],SVMc3b.E4[[2]],SVMc3b.E5[[2]],
                   SVMc3b.E6[[2]],SVMc3b.E7[[2]],SVMc3b.E8[[2]],SVMc3b.E9[[2]],SVMc3b.E10[[2]])) ; Store(SVMc3b)

Spear_score(SVMc3b)
graph1b(SVMc3b)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

library(e1071)
SVMc3.IB.E1<-cv5_SVMc(scal.IB_mat3.E1,K.E1,IsBad.E1$isbad,0.1,267) ; Store(SVMc3.IB.E1)
SVMc3.IB.E2<-cv5_SVMc(scal.IB_mat3.E2,K.E2,IsBad.E2$isbad,0.1,207) ; Store(SVMc3.IB.E2)
SVMc3.IB.E3<-cv5_SVMc(scal.IB_mat3.E3,K.E3,IsBad.E3$isbad,0.1,167) ; Store(SVMc3.IB.E3)
SVMc3.IB.E4<-cv5_SVMc(scal.IB_mat3.E4,K.E4,IsBad.E4$isbad,0.1,287) ; Store(SVMc3.IB.E4)
SVMc3.IB.E5<-cv5_SVMc(scal.IB_mat3.E5,K.E5,IsBad.E5$isbad,0.1,232) ; Store(SVMc3.IB.E5)
SVMc3.IB.E6<-cv5_SVMc(scal.IB_mat3.E6,K.E6,IsBad.E6$isbad,0.1,223) ; Store(SVMc3.IB.E6)
SVMc3.IB.E7<-cv5_SVMc(scal.IB_mat3.E7,K.E7,IsBad.E7$isbad,0.1,478) ; Store(SVMc3.IB.E7)
SVMc3.IB.E8<-cv5_SVMc(scal.IB_mat3.E8,K.E8,IsBad.E8$isbad,0.1,238) ; Store(SVMc3.IB.E8)
SVMc3.IB.E9<-cv5_SVMc(scal.IB_mat3.E9,K.E9,IsBad.E9$isbad,0.1,208) ; Store(SVMc3.IB.E9)
SVMc3.IB.E10<-cv5_SVMc(scal.IB_mat3.E10,K.E10,IsBad.E10$isbad,0.1,138) ; Store(SVMc3.IB.E10)

SVMc3.IB<-Group(list(SVMc3.IB.E1[[2]],SVMc3.IB.E2[[2]],SVMc3.IB.E3[[2]],SVMc3.IB.E4[[2]],SVMc3.IB.E5[[2]],
                     SVMc3.IB.E6[[2]],SVMc3.IB.E7[[2]],SVMc3.IB.E8[[2]],SVMc3.IB.E9[[2]],SVMc3.IB.E10[[2]])) ; Store(SVMc3.IB)

Spear_score(SVMc3.IB)
graph1b(SVMc3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

SVMc3.2.E1<-cv5_SVMc(scal_mat3.2.E1,K.E1,IsGood.E1$isgood,0.1,267) ; Store(SVMc3.2.E1)
SVMc3.2.E2<-cv5_SVMc(scal_mat3.2.E2,K.E2,IsGood.E2$isgood,0.1,207) ; Store(SVMc3.2.E2)
SVMc3.2.E3<-cv5_SVMc(scal_mat3.2.E3,K.E3,IsGood.E3$isgood,0.1,167) ; Store(SVMc3.2.E3)
SVMc3.2.E4<-cv5_SVMc(scal_mat3.2.E4,K.E4,IsGood.E4$isgood,0.1,287) ; Store(SVMc3.2.E4)
SVMc3.2.E5<-cv5_SVMc(scal_mat3.2.E5,K.E5,IsGood.E5$isgood,0.1,232) ; Store(SVMc3.2.E5)
SVMc3.2.E6<-cv5_SVMc(scal_mat3.2.E6,K.E6,IsGood.E6$isgood,0.1,223) ; Store(SVMc3.2.E6)
SVMc3.2.E7<-cv5_SVMc(scal_mat3.2.E7,K.E7,IsGood.E7$isgood,0.1,478) ; Store(SVMc3.2.E7)
SVMc3.2.E8<-cv5_SVMc(scal_mat3.2.E8,K.E8,IsGood.E8$isgood,0.1,238) ; Store(SVMc3.2.E8)
SVMc3.2.E9<-cv5_SVMc(scal_mat3.2.E9,K.E9,IsGood.E9$isgood,0.1,208) ; Store(SVMc3.2.E9)
SVMc3.2.E10<-cv5_SVMc(scal_mat3.2.E10,K.E10,IsGood.E10$isgood,0.1,138) ; Store(SVMc3.2.E10)

SVMc3.2<-Group(list(SVMc3.2.E1[[2]],SVMc3.2.E2[[2]],SVMc3.2.E3[[2]],SVMc3.2.E4[[2]],SVMc3.2.E5[[2]],
                    SVMc3.2.E6[[2]],SVMc3.2.E7[[2]],SVMc3.2.E8[[2]],SVMc3.2.E9[[2]],SVMc3.2.E10[[2]])) ; Store(SVMc3.2)

Spear_score(SVMc3.2)
graph1b(SVMc3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

SVMc3b.IB.2.E1<-cv5_SVMc(scal.IB_mat3.2.E1,K.E1,IsBad.E1$isbad,0.03,267) ; Store(SVMc3b.IB.2.E1)
SVMc3b.IB.2.E2<-cv5_SVMc(scal.IB_mat3.2.E2,K.E2,IsBad.E2$isbad,0.03,207) ; Store(SVMc3b.IB.2.E2)
SVMc3b.IB.2.E3<-cv5_SVMc(scal.IB_mat3.2.E3,K.E3,IsBad.E3$isbad,0.03,167) ; Store(SVMc3b.IB.2.E3)
SVMc3b.IB.2.E4<-cv5_SVMc(scal.IB_mat3.2.E4,K.E4,IsBad.E4$isbad,0.03,287) ; Store(SVMc3b.IB.2.E4)
SVMc3b.IB.2.E5<-cv5_SVMc(scal.IB_mat3.2.E5,K.E5,IsBad.E5$isbad,0.03,232) ; Store(SVMc3b.IB.2.E5)
SVMc3b.IB.2.E6<-cv5_SVMc(scal.IB_mat3.2.E6,K.E6,IsBad.E6$isbad,0.03,223) ; Store(SVMc3b.IB.2.E6)
SVMc3b.IB.2.E7<-cv5_SVMc(scal.IB_mat3.2.E7,K.E7,IsBad.E7$isbad,0.03,478) ; Store(SVMc3b.IB.2.E7)
SVMc3b.IB.2.E8<-cv5_SVMc(scal.IB_mat3.2.E8,K.E8,IsBad.E8$isbad,0.03,238) ; Store(SVMc3b.IB.2.E8)
SVMc3b.IB.2.E9<-cv5_SVMc(scal.IB_mat3.2.E9,K.E9,IsBad.E9$isbad,0.03,208) ; Store(SVMc3b.IB.2.E9)
SVMc3b.IB.2.E10<-cv5_SVMc(scal.IB_mat3.2.E10,K.E10,IsBad.E10$isbad,0.03,138) ; Store(SVMc3b.IB.2.E10)

SVMc3b.IB.2<-Group(list(SVMc3b.IB.2.E1[[2]],SVMc3b.IB.2.E2[[2]],SVMc3b.IB.2.E3[[2]],SVMc3b.IB.2.E4[[2]],SVMc3b.IB.2.E5[[2]],
                        SVMc3b.IB.2.E6[[2]],SVMc3b.IB.2.E7[[2]],SVMc3b.IB.2.E8[[2]],SVMc3b.IB.2.E9[[2]],SVMc3b.IB.2.E10[[2]])) ; Store(SVMc3b.IB.2)

Spear_score(SVMc3b.IB.2)
graph1b(SVMc3b.IB.2)

#####################################################################
# TRAIN SVM to model binary response good/poor essays
# with radial kernel on scaled matrix
#####################################################################

Cv5_RADc2<-function(data,K,y,cost,gamma,kernel,seed) {
  out<-list()
  out[[1]]<-list()
  out[[2]]<-rep(0,nrow(data[[1]]))
  data<-as.matrix(data)
  for (i in 1:5) {
    set.seed(seed+i*100)
    out[[1]][[i]]<-svm(x=data[[i]][-K[[i]],],y=factor(y[-K[[i]]]),cost=cost,gamma=gamma,kernel=kernel,scale=FALSE,probability =TRUE)
    out[[2]][K[[i]]]<-attr(predict(out[[1]][[i]],data[[i]][K[[i]],],probability =TRUE),"probabilities")[,"1"]
    print(paste("set",i,"done"))}      
  out[[1]][[6]]<-svm(x=data[[6]],y=factor(y),cost=cost,gamma=gamma,kernel=kernel,scale=FALSE,probability =TRUE)
  out
}
Store(Cv5_RADc2)

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

RADc2.E1<-Cv5_RADc2(scal_mat3.E1,K.E1,IsGood.E1$isgood,3,0.03,"radial",546) ; Store(RADc2.E1)
RADc2.E2<-Cv5_RADc2(scal_mat3.E2,K.E2,IsGood.E2$isgood,3,0.03,"radial",546) ; Store(RADc2.E2)
RADc2.E3<-Cv5_RADc2(scal_mat3.E3,K.E3,IsGood.E3$isgood,3,0.03,"radial",546) ; Store(RADc2.E3)
RADc2.E4<-Cv5_RADc2(scal_mat3.E4,K.E4,IsGood.E4$isgood,3,0.03,"radial",546) ; Store(RADc2.E4)
RADc2.E5<-Cv5_RADc2(scal_mat3.E5,K.E5,IsGood.E5$isgood,3,0.03,"radial",546) ; Store(RADc2.E5)
RADc2.E6<-Cv5_RADc2(scal_mat3.E6,K.E6,IsGood.E6$isgood,3,0.03,"radial",546) ; Store(RADc2.E6)
RADc2.E7<-Cv5_RADc2(scal_mat3.E7,K.E7,IsGood.E7$isgood,3,0.03,"radial",546) ; Store(RADc2.E7)
RADc2.E8<-Cv5_RADc2(scal_mat3.E8,K.E8,IsGood.E8$isgood,3,0.03,"radial",546) ; Store(RADc2.E8)
RADc2.E9<-Cv5_RADc2(scal_mat3.E9,K.E9,IsGood.E9$isgood,3,0.03,"radial",546) ; Store(RADc2.E9)
RADc2.E10<-Cv5_RADc2(scal_mat3.E10,K.E10,IsGood.E10$isgood,3,0.03,"radial",546) ; Store(RADc2.E10)

RADc2<-Group(list(RADc2.E1[[2]],RADc2.E2[[2]],RADc2.E3[[2]],RADc2.E4[[2]],RADc2.E5[[2]],
                  RADc2.E6[[2]],RADc2.E7[[2]],RADc2.E8[[2]],RADc2.E9[[2]],RADc2.E10[[2]])) ; Store(RADc2)
Spear_score(RADc2)
graph1b(RADc2)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

RADc2.IB.E1<-Cv5_RADc2(scal.IB_mat3.E1,K.E1,IsBad.E1$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.E1)
RADc2.IB.E2<-Cv5_RADc2(scal.IB_mat3.E2,K.E2,IsBad.E2$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.E2)
RADc2.IB.E3<-Cv5_RADc2(scal.IB_mat3.E3,K.E3,IsBad.E3$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.E3)
RADc2.IB.E4<-Cv5_RADc2(scal.IB_mat3.E4,K.E4,IsBad.E4$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.E4)
RADc2.IB.E5<-Cv5_RADc2(scal.IB_mat3.E5,K.E5,IsBad.E5$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.E5)
RADc2.IB.E6<-Cv5_RADc2(scal.IB_mat3.E6,K.E6,IsBad.E6$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.E6)
RADc2.IB.E7<-Cv5_RADc2(scal.IB_mat3.E7,K.E7,IsBad.E7$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.E7)
RADc2.IB.E8<-Cv5_RADc2(scal.IB_mat3.E8,K.E8,IsBad.E8$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.E8)
RADc2.IB.E9<-Cv5_RADc2(scal.IB_mat3.E9,K.E9,IsBad.E9$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.E9)
RADc2.IB.E10<-Cv5_RADc2(scal.IB_mat3.E10,K.E10,IsBad.E10$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.E10)

RADc2.IB<-Group(list(RADc2.IB.E1[[2]],RADc2.IB.E2[[2]],RADc2.IB.E3[[2]],RADc2.IB.E4[[2]],RADc2.IB.E5[[2]],
                     RADc2.IB.E6[[2]],RADc2.IB.E7[[2]],RADc2.IB.E8[[2]],RADc2.IB.E9[[2]],RADc2.IB.E10[[2]])) ; Store(RADc2.IB)
Spear_score(RADc2.IB)
graph1b(RADc2.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

RADc2.2.E1<-Cv5_RADc2(scal_mat3.2.E1,K.E1,IsGood.E1$isgood,1,0.01,"radial",546) ; Store(RADc2.2.E1)
RADc2.2.E2<-Cv5_RADc2(scal_mat3.2.E2,K.E2,IsGood.E2$isgood,1,0.01,"radial",546) ; Store(RADc2.2.E2)
RADc2.2.E3<-Cv5_RADc2(scal_mat3.2.E3,K.E3,IsGood.E3$isgood,1,0.01,"radial",546) ; Store(RADc2.2.E3)
RADc2.2.E4<-Cv5_RADc2(scal_mat3.2.E4,K.E4,IsGood.E4$isgood,1,0.01,"radial",546) ; Store(RADc2.2.E4)
RADc2.2.E5<-Cv5_RADc2(scal_mat3.2.E5,K.E5,IsGood.E5$isgood,1,0.01,"radial",546) ; Store(RADc2.2.E5)
RADc2.2.E6<-Cv5_RADc2(scal_mat3.2.E6,K.E6,IsGood.E6$isgood,1,0.01,"radial",546) ; Store(RADc2.2.E6)
RADc2.2.E7<-Cv5_RADc2(scal_mat3.2.E7,K.E7,IsGood.E7$isgood,1,0.01,"radial",546) ; Store(RADc2.2.E7)
RADc2.2.E8<-Cv5_RADc2(scal_mat3.2.E8,K.E8,IsGood.E8$isgood,1,0.01,"radial",546) ; Store(RADc2.2.E8)
RADc2.2.E9<-Cv5_RADc2(scal_mat3.2.E9,K.E9,IsGood.E9$isgood,1,0.01,"radial",546) ; Store(RADc2.2.E9)
RADc2.2.E10<-Cv5_RADc2(scal_mat3.2.E10,K.E10,IsGood.E10$isgood,1,0.01,"radial",546) ; Store(RADc2.2.E10)

RADc2.2<-Group(list(RADc2.2.E1[[2]],RADc2.2.E2[[2]],RADc2.2.E3[[2]],RADc2.2.E4[[2]],RADc2.2.E5[[2]],
                    RADc2.2.E6[[2]],RADc2.2.E7[[2]],RADc2.2.E8[[2]],RADc2.2.E9[[2]],RADc2.2.E10[[2]])) ; Store(RADc2.2)
Spear_score(RADc2.2)
graph1b(RADc2.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

RADc2.IB.2.E1<-Cv5_RADc2(scal.IB_mat3.2.E1,K.E1,IsBad.E1$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.2.E1)
RADc2.IB.2.E2<-Cv5_RADc2(scal.IB_mat3.2.E2,K.E2,IsBad.E2$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.2.E2)
RADc2.IB.2.E3<-Cv5_RADc2(scal.IB_mat3.2.E3,K.E3,IsBad.E3$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.2.E3)
RADc2.IB.2.E4<-Cv5_RADc2(scal.IB_mat3.2.E4,K.E4,IsBad.E4$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.2.E4)
RADc2.IB.2.E5<-Cv5_RADc2(scal.IB_mat3.2.E5,K.E5,IsBad.E5$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.2.E5)
RADc2.IB.2.E6<-Cv5_RADc2(scal.IB_mat3.2.E6,K.E6,IsBad.E6$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.2.E6)
RADc2.IB.2.E7<-Cv5_RADc2(scal.IB_mat3.2.E7,K.E7,IsBad.E7$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.2.E7)
RADc2.IB.2.E8<-Cv5_RADc2(scal.IB_mat3.2.E8,K.E8,IsBad.E8$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.2.E8)
RADc2.IB.2.E9<-Cv5_RADc2(scal.IB_mat3.2.E9,K.E9,IsBad.E9$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.2.E9)
RADc2.IB.2.E10<-Cv5_RADc2(scal.IB_mat3.2.E10,K.E10,IsBad.E10$isbad,3,0.03,"radial",546) ; Store(RADc2.IB.2.E10)

RADc2.IB.2<-Group(list(RADc2.IB.2.E1[[2]],RADc2.IB.2.E2[[2]],RADc2.IB.2.E3[[2]],RADc2.IB.2.E4[[2]],RADc2.IB.2.E5[[2]],
                       RADc2.IB.2.E6[[2]],RADc2.IB.2.E7[[2]],RADc2.IB.2.E8[[2]],RADc2.IB.2.E9[[2]],RADc2.IB.2.E10[[2]])) ; Store(RADc2.IB.2)
Spear_score(RADc2.IB.2)
graph1b(RADc2.IB.2)

#####################################################################
# TRAIN Multinomial SVM
#####################################################################

# function to train SVMm
#####################################################################

cv5_SVMm<-function(data,K,train_scores,cost,seed) {
  set.seed(seed)
  out<-list()
  y=factor(paste("S",train_scores,sep="")) 
  out[[1]]<-list()
  for (i in 1:6) {
    out[[1]][[i]]<- if (i<=5) 
      svm(x=data[[i]][-K[[i]],],y=y[-K[[i]]],cost=cost,kernel="linear",scale=FALSE)  else 	
        svm(x=data[[i]],y=y,cost=cost,kernel="linear",scale=FALSE)
    print(paste("set",i,"done"))}    	
  out[[2]]<-rep(0,length(train_scores))
  for (i in 1:5) out[[2]][K[[i]]]<-as.character(predict(out[[1]][[i]],data[[i]][K[[i]],]))
  out
}
Store(cv5_SVMm)

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

SVMm3.E1<-cv5_SVMm(scal_mat3.E1,K.E1,training.E1$Score1,0.1,207) ; Store(SVMm3.E1)
SVMm3.E2<-cv5_SVMm(scal_mat3.E2,K.E2,training.E2$Score1,0.1,200) ; Store(SVMm3.E2)
SVMm3.E3<-cv5_SVMm(scal_mat3.E3,K.E3,training.E3$Score1,0.1,168) ; Store(SVMm3.E3)
SVMm3.E4<-cv5_SVMm(scal_mat3.E4,K.E4,training.E4$Score1,0.1,288) ; Store(SVMm3.E4)
SVMm3.E5<-cv5_SVMm(scal_mat3.E5,K.E5,training.E5$Score1,0.1,235) ; Store(SVMm3.E5)
SVMm3.E6<-cv5_SVMm(scal_mat3.E6,K.E6,training.E6$Score1,0.1,258) ; Store(SVMm3.E6)
SVMm3.E7<-cv5_SVMm(scal_mat3.E7,K.E7,training.E7$Score1,0.1,431) ; Store(SVMm3.E7)
SVMm3.E8<-cv5_SVMm(scal_mat3.E8,K.E8,training.E8$Score1,0.1,212) ; Store(SVMm3.E8)
SVMm3.E9<-cv5_SVMm(scal_mat3.E9,K.E9,training.E9$Score1,0.1,812) ; Store(SVMm3.E9)
SVMm3.E10<-cv5_SVMm(scal_mat3.E10,K.E10,training.E10$Score1,0.1,210) ; Store(SVMm3.E10)

SVMm3.E1[[2]]<-as.numeric(gsub("S","",SVMm3.E1[[2]])) ; Store(SVMm3.E1)
SVMm3.E2[[2]]<-as.numeric(gsub("S","",SVMm3.E2[[2]])) ; Store(SVMm3.E2)
SVMm3.E3[[2]]<-as.numeric(gsub("S","",SVMm3.E3[[2]])) ; Store(SVMm3.E3)
SVMm3.E4[[2]]<-as.numeric(gsub("S","",SVMm3.E4[[2]])) ; Store(SVMm3.E4)
SVMm3.E5[[2]]<-as.numeric(gsub("S","",SVMm3.E5[[2]])) ; Store(SVMm3.E5)
SVMm3.E6[[2]]<-as.numeric(gsub("S","",SVMm3.E6[[2]])) ; Store(SVMm3.E6)
SVMm3.E7[[2]]<-as.numeric(gsub("S","",SVMm3.E7[[2]])) ; Store(SVMm3.E7)
SVMm3.E8[[2]]<-as.numeric(gsub("S","",SVMm3.E8[[2]])) ; Store(SVMm3.E8)
SVMm3.E9[[2]]<-as.numeric(gsub("S","",SVMm3.E9[[2]])) ; Store(SVMm3.E9)
SVMm3.E10[[2]]<-as.numeric(gsub("S","",SVMm3.E10[[2]])) ; Store(SVMm3.E10)

SVMm3<-Group(list(SVMm3.E1[[2]],SVMm3.E2[[2]],SVMm3.E3[[2]],SVMm3.E4[[2]],SVMm3.E5[[2]],
                  SVMm3.E6[[2]],SVMm3.E7[[2]],SVMm3.E8[[2]],SVMm3.E9[[2]],SVMm3.E10[[2]])) ; Store(SVMm3)

Spear_score(SVMm3)
graph1b(SVMm3)
Kappa_score(SVMm3)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

SVMm3.IB.E1<-cv5_SVMm(scal.IB_mat3.E1,K.E1,training.E1$Score1,0.1,207) ; Store(SVMm3.IB.E1)
SVMm3.IB.E2<-cv5_SVMm(scal.IB_mat3.E2,K.E2,training.E2$Score1,0.1,200) ; Store(SVMm3.IB.E2)
SVMm3.IB.E3<-cv5_SVMm(scal.IB_mat3.E3,K.E3,training.E3$Score1,0.1,168) ; Store(SVMm3.IB.E3)
SVMm3.IB.E4<-cv5_SVMm(scal.IB_mat3.E4,K.E4,training.E4$Score1,0.1,288) ; Store(SVMm3.IB.E4)
SVMm3.IB.E5<-cv5_SVMm(scal.IB_mat3.E5,K.E5,training.E5$Score1,0.1,235) ; Store(SVMm3.IB.E5)
SVMm3.IB.E6<-cv5_SVMm(scal.IB_mat3.E6,K.E6,training.E6$Score1,0.1,258) ; Store(SVMm3.IB.E6)
SVMm3.IB.E7<-cv5_SVMm(scal.IB_mat3.E7,K.E7,training.E7$Score1,0.1,431) ; Store(SVMm3.IB.E7)
SVMm3.IB.E8<-cv5_SVMm(scal.IB_mat3.E8,K.E8,training.E8$Score1,0.1,212) ; Store(SVMm3.IB.E8)
SVMm3.IB.E9<-cv5_SVMm(scal.IB_mat3.E9,K.E9,training.E9$Score1,0.1,812) ; Store(SVMm3.IB.E9)
SVMm3.IB.E10<-cv5_SVMm(scal.IB_mat3.E10,K.E10,training.E10$Score1,0.1,210) ; Store(SVMm3.IB.E10)

SVMm3.IB.E1[[2]]<-as.numeric(gsub("S","",SVMm3.IB.E1[[2]])) ; Store(SVMm3.IB.E1)
SVMm3.IB.E2[[2]]<-as.numeric(gsub("S","",SVMm3.IB.E2[[2]])) ; Store(SVMm3.IB.E2)
SVMm3.IB.E3[[2]]<-as.numeric(gsub("S","",SVMm3.IB.E3[[2]])) ; Store(SVMm3.IB.E3)
SVMm3.IB.E4[[2]]<-as.numeric(gsub("S","",SVMm3.IB.E4[[2]])) ; Store(SVMm3.IB.E4)
SVMm3.IB.E5[[2]]<-as.numeric(gsub("S","",SVMm3.IB.E5[[2]])) ; Store(SVMm3.IB.E5)
SVMm3.IB.E6[[2]]<-as.numeric(gsub("S","",SVMm3.IB.E6[[2]])) ; Store(SVMm3.IB.E6)
SVMm3.IB.E7[[2]]<-as.numeric(gsub("S","",SVMm3.IB.E7[[2]])) ; Store(SVMm3.IB.E7)
SVMm3.IB.E8[[2]]<-as.numeric(gsub("S","",SVMm3.IB.E8[[2]])) ; Store(SVMm3.IB.E8)
SVMm3.IB.E9[[2]]<-as.numeric(gsub("S","",SVMm3.IB.E9[[2]])) ; Store(SVMm3.IB.E9)
SVMm3.IB.E10[[2]]<-as.numeric(gsub("S","",SVMm3.IB.E10[[2]])) ; Store(SVMm3.IB.E10)

SVMm3.IB<-Group(list(SVMm3.IB.E1[[2]],SVMm3.IB.E2[[2]],SVMm3.IB.E3[[2]],SVMm3.IB.E4[[2]],SVMm3.IB.E5[[2]],
                     SVMm3.IB.E6[[2]],SVMm3.IB.E7[[2]],SVMm3.IB.E8[[2]],SVMm3.IB.E9[[2]],SVMm3.IB.E10[[2]])) ; Store(SVMm3.IB)

Spear_score(SVMm3.IB)
graph1b(SVMm3.IB)
Kappa_score(SVMm3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

SVMm3.2.E1<-cv5_SVMm(scal_mat3.2.E1,K.E1,training.E1$Score1,0.1,207) ; Store(SVMm3.2.E1)
SVMm3.2.E2<-cv5_SVMm(scal_mat3.2.E2,K.E2,training.E2$Score1,0.1,200) ; Store(SVMm3.2.E2)
SVMm3.2.E3<-cv5_SVMm(scal_mat3.2.E3,K.E3,training.E3$Score1,0.1,168) ; Store(SVMm3.2.E3)
SVMm3.2.E4<-cv5_SVMm(scal_mat3.2.E4,K.E4,training.E4$Score1,0.1,288) ; Store(SVMm3.2.E4)
SVMm3.2.E5<-cv5_SVMm(scal_mat3.2.E5,K.E5,training.E5$Score1,0.1,235) ; Store(SVMm3.2.E5)
SVMm3.2.E6<-cv5_SVMm(scal_mat3.2.E6,K.E6,training.E6$Score1,0.1,258) ; Store(SVMm3.2.E6)
SVMm3.2.E7<-cv5_SVMm(scal_mat3.2.E7,K.E7,training.E7$Score1,0.1,431) ; Store(SVMm3.2.E7)
SVMm3.2.E8<-cv5_SVMm(scal_mat3.2.E8,K.E8,training.E8$Score1,0.1,212) ; Store(SVMm3.2.E8)
SVMm3.2.E9<-cv5_SVMm(scal_mat3.2.E9,K.E9,training.E9$Score1,0.1,812) ; Store(SVMm3.2.E9)
SVMm3.2.E10<-cv5_SVMm(scal_mat3.2.E10,K.E10,training.E10$Score1,0.1,210) ; Store(SVMm3.2.E10)

SVMm3.2.E1[[2]]<-as.numeric(gsub("S","",SVMm3.2.E1[[2]])) ; Store(SVMm3.2.E1)
SVMm3.2.E2[[2]]<-as.numeric(gsub("S","",SVMm3.2.E2[[2]])) ; Store(SVMm3.2.E2)
SVMm3.2.E3[[2]]<-as.numeric(gsub("S","",SVMm3.2.E3[[2]])) ; Store(SVMm3.2.E3)
SVMm3.2.E4[[2]]<-as.numeric(gsub("S","",SVMm3.2.E4[[2]])) ; Store(SVMm3.2.E4)
SVMm3.2.E5[[2]]<-as.numeric(gsub("S","",SVMm3.2.E5[[2]])) ; Store(SVMm3.2.E5)
SVMm3.2.E6[[2]]<-as.numeric(gsub("S","",SVMm3.2.E6[[2]])) ; Store(SVMm3.2.E6)
SVMm3.2.E7[[2]]<-as.numeric(gsub("S","",SVMm3.2.E7[[2]])) ; Store(SVMm3.2.E7)
SVMm3.2.E8[[2]]<-as.numeric(gsub("S","",SVMm3.2.E8[[2]])) ; Store(SVMm3.2.E8)
SVMm3.2.E9[[2]]<-as.numeric(gsub("S","",SVMm3.2.E9[[2]])) ; Store(SVMm3.2.E9)
SVMm3.2.E10[[2]]<-as.numeric(gsub("S","",SVMm3.2.E10[[2]])) ; Store(SVMm3.2.E10)

SVMm3.2<-Group(list(SVMm3.2.E1[[2]],SVMm3.2.E2[[2]],SVMm3.2.E3[[2]],SVMm3.2.E4[[2]],SVMm3.2.E5[[2]],
                    SVMm3.2.E6[[2]],SVMm3.2.E7[[2]],SVMm3.2.E8[[2]],SVMm3.2.E9[[2]],SVMm3.2.E10[[2]])) ; Store(SVMm3.2)

Spear_score(SVMm3.2)
graph1b(SVMm3.2)
Kappa_score(SVMm3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

SVMm3.IB.2.E1<-cv5_SVMm(scal.IB_mat3.2.E1,K.E1,training.E1$Score1,0.1,207) ; Store(SVMm3.IB.2.E1)
SVMm3.IB.2.E2<-cv5_SVMm(scal.IB_mat3.2.E2,K.E2,training.E2$Score1,0.1,200) ; Store(SVMm3.IB.2.E2)
SVMm3.IB.2.E3<-cv5_SVMm(scal.IB_mat3.2.E3,K.E3,training.E3$Score1,0.1,168) ; Store(SVMm3.IB.2.E3)
SVMm3.IB.2.E4<-cv5_SVMm(scal.IB_mat3.2.E4,K.E4,training.E4$Score1,0.1,288) ; Store(SVMm3.IB.2.E4)
SVMm3.IB.2.E5<-cv5_SVMm(scal.IB_mat3.2.E5,K.E5,training.E5$Score1,0.1,235) ; Store(SVMm3.IB.2.E5)
SVMm3.IB.2.E6<-cv5_SVMm(scal.IB_mat3.2.E6,K.E6,training.E6$Score1,0.1,258) ; Store(SVMm3.IB.2.E6)
SVMm3.IB.2.E7<-cv5_SVMm(scal.IB_mat3.2.E7,K.E7,training.E7$Score1,0.1,431) ; Store(SVMm3.IB.2.E7)
SVMm3.IB.2.E8<-cv5_SVMm(scal.IB_mat3.2.E8,K.E8,training.E8$Score1,0.1,212) ; Store(SVMm3.IB.2.E8)
SVMm3.IB.2.E9<-cv5_SVMm(scal.IB_mat3.2.E9,K.E9,training.E9$Score1,0.1,812) ; Store(SVMm3.IB.2.E9)
SVMm3.IB.2.E10<-cv5_SVMm(scal.IB_mat3.2.E10,K.E10,training.E10$Score1,0.1,210) ; Store(SVMm3.IB.2.E10)

SVMm3.IB.2.E1[[2]]<-as.numeric(gsub("S","",SVMm3.IB.2.E1[[2]])) ; Store(SVMm3.IB.2.E1)
SVMm3.IB.2.E2[[2]]<-as.numeric(gsub("S","",SVMm3.IB.2.E2[[2]])) ; Store(SVMm3.IB.2.E2)
SVMm3.IB.2.E3[[2]]<-as.numeric(gsub("S","",SVMm3.IB.2.E3[[2]])) ; Store(SVMm3.IB.2.E3)
SVMm3.IB.2.E4[[2]]<-as.numeric(gsub("S","",SVMm3.IB.2.E4[[2]])) ; Store(SVMm3.IB.2.E4)
SVMm3.IB.2.E5[[2]]<-as.numeric(gsub("S","",SVMm3.IB.2.E5[[2]])) ; Store(SVMm3.IB.2.E5)
SVMm3.IB.2.E6[[2]]<-as.numeric(gsub("S","",SVMm3.IB.2.E6[[2]])) ; Store(SVMm3.IB.2.E6)
SVMm3.IB.2.E7[[2]]<-as.numeric(gsub("S","",SVMm3.IB.2.E7[[2]])) ; Store(SVMm3.IB.2.E7)
SVMm3.IB.2.E8[[2]]<-as.numeric(gsub("S","",SVMm3.IB.2.E8[[2]])) ; Store(SVMm3.IB.2.E8)
SVMm3.IB.2.E9[[2]]<-as.numeric(gsub("S","",SVMm3.IB.2.E9[[2]])) ; Store(SVMm3.IB.2.E9)
SVMm3.IB.2.E10[[2]]<-as.numeric(gsub("S","",SVMm3.IB.2.E10[[2]])) ; Store(SVMm3.IB.2.E10)

SVMm3.IB.2<-Group(list(SVMm3.IB.2.E1[[2]],SVMm3.IB.2.E2[[2]],SVMm3.IB.2.E3[[2]],SVMm3.IB.2.E4[[2]],SVMm3.IB.2.E5[[2]],
                       SVMm3.IB.2.E6[[2]],SVMm3.IB.2.E7[[2]],SVMm3.IB.2.E8[[2]],SVMm3.IB.2.E9[[2]],SVMm3.IB.2.E10[[2]])) ; Store(SVMm3.IB.2)

Spear_score(SVMm3.IB.2)
graph1b(SVMm3.IB.2)
Kappa_score(SVMm3.IB.2)

#####################################################################
# TRAIN SVM multinomial
# with radial kernel on scaled matrix
#####################################################################

Cv5_RADm2<-function(data,K,y,cost,gamma,kernel,seed) {
  out<-list()
  out[[1]]<-list()
  out[[2]]<-rep(0,nrow(data[[1]]))
  y=factor(paste("S",y,sep="")) 
  data<-as.matrix(data)
  for (i in 1:5) {
    set.seed(seed+i*100)
    out[[1]][[i]]<-svm(x=data[[i]][-K[[i]],],y=factor(y[-K[[i]]]),cost=cost,gamma=gamma,kernel=kernel,scale=FALSE)
    out[[2]][K[[i]]]<-as.character(predict(out[[1]][[i]],data[[i]][K[[i]],]))
    print(paste("set",i,"done"))}      
  out[[1]][[6]]<-svm(x=data[[6]],y=factor(y),cost=cost,gamma=gamma,kernel=kernel,scale=FALSE)
  out
}
Store(Cv5_RADm2)

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

RADm2.E1<-Cv5_RADm2(scal_mat3.E1,K.E1,training.E1$Score1,3,0.03,"radial",469) ; Store(RADm2.E1)
RADm2.E2<-Cv5_RADm2(scal_mat3.E2,K.E2,training.E2$Score1,3,0.03,"radial",469) ; Store(RADm2.E2)
RADm2.E3<-Cv5_RADm2(scal_mat3.E3,K.E3,training.E3$Score1,3,0.03,"radial",469) ; Store(RADm2.E3)
RADm2.E4<-Cv5_RADm2(scal_mat3.E4,K.E4,training.E4$Score1,3,0.03,"radial",469) ; Store(RADm2.E4)
RADm2.E5<-Cv5_RADm2(scal_mat3.E5,K.E5,training.E5$Score1,3,0.03,"radial",469) ; Store(RADm2.E5)
RADm2.E6<-Cv5_RADm2(scal_mat3.E6,K.E6,training.E6$Score1,3,0.03,"radial",469) ; Store(RADm2.E6)
RADm2.E7<-Cv5_RADm2(scal_mat3.E7,K.E7,training.E7$Score1,3,0.03,"radial",469) ; Store(RADm2.E7)
RADm2.E8<-Cv5_RADm2(scal_mat3.E8,K.E8,training.E8$Score1,3,0.03,"radial",469) ; Store(RADm2.E8)
RADm2.E9<-Cv5_RADm2(scal_mat3.E9,K.E9,training.E9$Score1,3,0.03,"radial",469) ; Store(RADm2.E9)
RADm2.E10<-Cv5_RADm2(scal_mat3.E10,K.E10,training.E10$Score1,3,0.03,"radial",469) ; Store(RADm2.E10)

RADm2.E1[[2]]<-as.numeric(gsub("S","",RADm2.E1[[2]])) ; Store(RADm2.E1)
RADm2.E2[[2]]<-as.numeric(gsub("S","",RADm2.E2[[2]])) ; Store(RADm2.E2)
RADm2.E3[[2]]<-as.numeric(gsub("S","",RADm2.E3[[2]])) ; Store(RADm2.E3)
RADm2.E4[[2]]<-as.numeric(gsub("S","",RADm2.E4[[2]])) ; Store(RADm2.E4)
RADm2.E5[[2]]<-as.numeric(gsub("S","",RADm2.E5[[2]])) ; Store(RADm2.E5)
RADm2.E6[[2]]<-as.numeric(gsub("S","",RADm2.E6[[2]])) ; Store(RADm2.E6)
RADm2.E7[[2]]<-as.numeric(gsub("S","",RADm2.E7[[2]])) ; Store(RADm2.E7)
RADm2.E8[[2]]<-as.numeric(gsub("S","",RADm2.E8[[2]])) ; Store(RADm2.E8)
RADm2.E9[[2]]<-as.numeric(gsub("S","",RADm2.E9[[2]])) ; Store(RADm2.E9)
RADm2.E10[[2]]<-as.numeric(gsub("S","",RADm2.E10[[2]])) ; Store(RADm2.E10)

RADm2<-Group(list(RADm2.E1[[2]],RADm2.E2[[2]],RADm2.E3[[2]],RADm2.E4[[2]],RADm2.E5[[2]],
                  RADm2.E6[[2]],RADm2.E7[[2]],RADm2.E8[[2]],RADm2.E9[[2]],RADm2.E10[[2]])) ; Store(RADm2)
Spear_score(RADm2)
graph1b(RADm2)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

RADm2.IB.E1<-Cv5_RADm2(scal.IB_mat3.E1,K.E1,training.E1$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.E1)
RADm2.IB.E2<-Cv5_RADm2(scal.IB_mat3.E2,K.E2,training.E2$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.E2)
RADm2.IB.E3<-Cv5_RADm2(scal.IB_mat3.E3,K.E3,training.E3$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.E3)
RADm2.IB.E4<-Cv5_RADm2(scal.IB_mat3.E4,K.E4,training.E4$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.E4)
RADm2.IB.E5<-Cv5_RADm2(scal.IB_mat3.E5,K.E5,training.E5$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.E5)
RADm2.IB.E6<-Cv5_RADm2(scal.IB_mat3.E6,K.E6,training.E6$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.E6)
RADm2.IB.E7<-Cv5_RADm2(scal.IB_mat3.E7,K.E7,training.E7$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.E7)
RADm2.IB.E8<-Cv5_RADm2(scal.IB_mat3.E8,K.E8,training.E8$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.E8)
RADm2.IB.E9<-Cv5_RADm2(scal.IB_mat3.E9,K.E9,training.E9$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.E9)
RADm2.IB.E10<-Cv5_RADm2(scal.IB_mat3.E10,K.E10,training.E10$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.E10)

RADm2.IB.E1[[2]]<-as.numeric(gsub("S","",RADm2.IB.E1[[2]])) ; Store(RADm2.IB.E1)
RADm2.IB.E2[[2]]<-as.numeric(gsub("S","",RADm2.IB.E2[[2]])) ; Store(RADm2.IB.E2)
RADm2.IB.E3[[2]]<-as.numeric(gsub("S","",RADm2.IB.E3[[2]])) ; Store(RADm2.IB.E3)
RADm2.IB.E4[[2]]<-as.numeric(gsub("S","",RADm2.IB.E4[[2]])) ; Store(RADm2.IB.E4)
RADm2.IB.E5[[2]]<-as.numeric(gsub("S","",RADm2.IB.E5[[2]])) ; Store(RADm2.IB.E5)
RADm2.IB.E6[[2]]<-as.numeric(gsub("S","",RADm2.IB.E6[[2]])) ; Store(RADm2.IB.E6)
RADm2.IB.E7[[2]]<-as.numeric(gsub("S","",RADm2.IB.E7[[2]])) ; Store(RADm2.IB.E7)
RADm2.IB.E8[[2]]<-as.numeric(gsub("S","",RADm2.IB.E8[[2]])) ; Store(RADm2.IB.E8)
RADm2.IB.E9[[2]]<-as.numeric(gsub("S","",RADm2.IB.E9[[2]])) ; Store(RADm2.IB.E9)
RADm2.IB.E10[[2]]<-as.numeric(gsub("S","",RADm2.IB.E10[[2]])) ; Store(RADm2.IB.E10)

RADm2.IB<-Group(list(RADm2.IB.E1[[2]],RADm2.IB.E2[[2]],RADm2.IB.E3[[2]],RADm2.IB.E4[[2]],RADm2.IB.E5[[2]],
                     RADm2.IB.E6[[2]],RADm2.IB.E7[[2]],RADm2.IB.E8[[2]],RADm2.IB.E9[[2]],RADm2.IB.E10[[2]])) ; Store(RADm2.IB)
Spear_score(RADm2.IB)
graph1b(RADm2.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

RADm2.2.E1<-Cv5_RADm2(scal_mat3.2.E1,K.E1,training.E1$Score1,3,0.01,"radial",469) ; Store(RADm2.2.E1)
RADm2.2.E2<-Cv5_RADm2(scal_mat3.2.E2,K.E2,training.E2$Score1,3,0.01,"radial",469) ; Store(RADm2.2.E2)
RADm2.2.E3<-Cv5_RADm2(scal_mat3.2.E3,K.E3,training.E3$Score1,3,0.01,"radial",469) ; Store(RADm2.2.E3)
RADm2.2.E4<-Cv5_RADm2(scal_mat3.2.E4,K.E4,training.E4$Score1,3,0.01,"radial",469) ; Store(RADm2.2.E4)
RADm2.2.E5<-Cv5_RADm2(scal_mat3.2.E5,K.E5,training.E5$Score1,3,0.01,"radial",469) ; Store(RADm2.2.E5)
RADm2.2.E6<-Cv5_RADm2(scal_mat3.2.E6,K.E6,training.E6$Score1,3,0.01,"radial",469) ; Store(RADm2.2.E6)
RADm2.2.E7<-Cv5_RADm2(scal_mat3.2.E7,K.E7,training.E7$Score1,3,0.01,"radial",469) ; Store(RADm2.2.E7)
RADm2.2.E8<-Cv5_RADm2(scal_mat3.2.E8,K.E8,training.E8$Score1,3,0.01,"radial",469) ; Store(RADm2.2.E8)
RADm2.2.E9<-Cv5_RADm2(scal_mat3.2.E9,K.E9,training.E9$Score1,3,0.01,"radial",469) ; Store(RADm2.2.E9)
RADm2.2.E10<-Cv5_RADm2(scal_mat3.2.E10,K.E10,training.E10$Score1,3,0.01,"radial",469) ; Store(RADm2.2.E10)

RADm2.2.E1[[2]]<-as.numeric(gsub("S","",RADm2.2.E1[[2]])) ; Store(RADm2.2.E1)
RADm2.2.E2[[2]]<-as.numeric(gsub("S","",RADm2.2.E2[[2]])) ; Store(RADm2.2.E2)
RADm2.2.E3[[2]]<-as.numeric(gsub("S","",RADm2.2.E3[[2]])) ; Store(RADm2.2.E3)
RADm2.2.E4[[2]]<-as.numeric(gsub("S","",RADm2.2.E4[[2]])) ; Store(RADm2.2.E4)
RADm2.2.E5[[2]]<-as.numeric(gsub("S","",RADm2.2.E5[[2]])) ; Store(RADm2.2.E5)
RADm2.2.E6[[2]]<-as.numeric(gsub("S","",RADm2.2.E6[[2]])) ; Store(RADm2.2.E6)
RADm2.2.E7[[2]]<-as.numeric(gsub("S","",RADm2.2.E7[[2]])) ; Store(RADm2.2.E7)
RADm2.2.E8[[2]]<-as.numeric(gsub("S","",RADm2.2.E8[[2]])) ; Store(RADm2.2.E8)
RADm2.2.E9[[2]]<-as.numeric(gsub("S","",RADm2.2.E9[[2]])) ; Store(RADm2.2.E9)
RADm2.2.E10[[2]]<-as.numeric(gsub("S","",RADm2.2.E10[[2]])) ; Store(RADm2.2.E10)

RADm2.2<-Group(list(RADm2.2.E1[[2]],RADm2.2.E2[[2]],RADm2.2.E3[[2]],RADm2.2.E4[[2]],RADm2.2.E5[[2]],
                    RADm2.2.E6[[2]],RADm2.2.E7[[2]],RADm2.2.E8[[2]],RADm2.2.E9[[2]],RADm2.2.E10[[2]])) ; Store(RADm2.2)
Spear_score(RADm2.2)
graph1b(RADm2.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

RADm2.IB.2.E1<-Cv5_RADm2(scal.IB_mat3.2.E1,K.E1,training.E1$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.2.E1)
RADm2.IB.2.E2<-Cv5_RADm2(scal.IB_mat3.2.E2,K.E2,training.E2$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.2.E2)
RADm2.IB.2.E3<-Cv5_RADm2(scal.IB_mat3.2.E3,K.E3,training.E3$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.2.E3)
RADm2.IB.2.E4<-Cv5_RADm2(scal.IB_mat3.2.E4,K.E4,training.E4$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.2.E4)
RADm2.IB.2.E5<-Cv5_RADm2(scal.IB_mat3.2.E5,K.E5,training.E5$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.2.E5)
RADm2.IB.2.E6<-Cv5_RADm2(scal.IB_mat3.2.E6,K.E6,training.E6$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.2.E6)
RADm2.IB.2.E7<-Cv5_RADm2(scal.IB_mat3.2.E7,K.E7,training.E7$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.2.E7)
RADm2.IB.2.E8<-Cv5_RADm2(scal.IB_mat3.2.E8,K.E8,training.E8$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.2.E8)
RADm2.IB.2.E9<-Cv5_RADm2(scal.IB_mat3.2.E9,K.E9,training.E9$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.2.E9)
RADm2.IB.2.E10<-Cv5_RADm2(scal.IB_mat3.2.E10,K.E10,training.E10$Score1,3,0.03,"radial",469) ; Store(RADm2.IB.2.E10)

RADm2.IB.2.E1[[2]]<-as.numeric(gsub("S","",RADm2.IB.2.E1[[2]])) ; Store(RADm2.IB.2.E1)
RADm2.IB.2.E2[[2]]<-as.numeric(gsub("S","",RADm2.IB.2.E2[[2]])) ; Store(RADm2.IB.2.E2)
RADm2.IB.2.E3[[2]]<-as.numeric(gsub("S","",RADm2.IB.2.E3[[2]])) ; Store(RADm2.IB.2.E3)
RADm2.IB.2.E4[[2]]<-as.numeric(gsub("S","",RADm2.IB.2.E4[[2]])) ; Store(RADm2.IB.2.E4)
RADm2.IB.2.E5[[2]]<-as.numeric(gsub("S","",RADm2.IB.2.E5[[2]])) ; Store(RADm2.IB.2.E5)
RADm2.IB.2.E6[[2]]<-as.numeric(gsub("S","",RADm2.IB.2.E6[[2]])) ; Store(RADm2.IB.2.E6)
RADm2.IB.2.E7[[2]]<-as.numeric(gsub("S","",RADm2.IB.2.E7[[2]])) ; Store(RADm2.IB.2.E7)
RADm2.IB.2.E8[[2]]<-as.numeric(gsub("S","",RADm2.IB.2.E8[[2]])) ; Store(RADm2.IB.2.E8)
RADm2.IB.2.E9[[2]]<-as.numeric(gsub("S","",RADm2.IB.2.E9[[2]])) ; Store(RADm2.IB.2.E9)
RADm2.IB.2.E10[[2]]<-as.numeric(gsub("S","",RADm2.IB.2.E10[[2]])) ; Store(RADm2.IB.2.E10)

RADm2.IB.2<-Group(list(RADm2.IB.2.E1[[2]],RADm2.IB.2.E2[[2]],RADm2.IB.2.E3[[2]],RADm2.IB.2.E4[[2]],RADm2.IB.2.E5[[2]],
                       RADm2.IB.2.E6[[2]],RADm2.IB.2.E7[[2]],RADm2.IB.2.E8[[2]],RADm2.IB.2.E9[[2]],RADm2.IB.2.E10[[2]])) ; Store(RADm2.IB.2)
Spear_score(RADm2.IB.2)
graph1b(RADm2.IB.2)

#####################################################################
#####################################################################
# TRAIN SVM with radial kernel on ridit scores after PCA reduction
#####################################################################
#####################################################################

Cv5_RAD<-function(data,metrics,M,K,y,cost,gamma,kernel,seed) {
  out<-list()
  out[[1]]<-list()
  out[[2]]<-rep(0,nrow(data))
  data<-as.matrix(data)
  for (i in 1:5) {
    set.seed(seed+i*100)
    WHICH<-which(metrics[[i]][[3]]>M)
    pc.cr<-princomp(data[-K[[i]],WHICH],centre=FALSE,scale=FALSE)
    tmp3<-cumsum(pc.cr$sdev^2)/sum(pc.cr$sdev^2)
    DPCA<-as.matrix(predict(pc.cr,newdata=data[-K[[i]],WHICH])[,1:min(which(tmp3>0.98))])
    DPCAnew<-as.matrix(predict(pc.cr,newdata=data[K[[i]],WHICH])[,1:min(which(tmp3>0.98))])
    out[[1]][[i]]<-svm(x=DPCA,y=y[-K[[i]]],cost=cost,gamma=gamma,kernel=kernel,scale=FALSE)
    out[[2]][K[[i]]]<-predict(out[[1]][[i]],DPCAnew)
    print(paste("set",i,"done"))}      
  WHICH<-which(metrics[[6]][[3]]>M)
  pc.cr<-princomp(data[,WHICH],centre=FALSE,scale=FALSE)
  tmp3<-cumsum(pc.cr$sdev^2)/sum(pc.cr$sdev^2)
  DPCA<-as.matrix(predict(pc.cr,newdata=data[,WHICH])[,1:min(which(tmp3>0.98))])
  out[[1]][[6]]<-svm(x=DPCA,y=y,cost=cost,gamma=gamma,kernel=kernel,scale=FALSE)
  out
}
Store(Cv5_RAD)

# 1rst set of bin_mat (transformed with ridit coding) 
#####################################################################

library(e1071)
RAD.E1<-Cv5_RAD(Ridit_mat.E1,metrics.E1,0.25,K.E1,training.E1$Score1,2,0.01,"radial",892) ; Store(RAD.E1)
RAD.E2<-Cv5_RAD(Ridit_mat.E2,metrics.E2,0.25,K.E2,training.E2$Score1,2,0.01,"radial",892) ; Store(RAD.E2)
RAD.E3<-Cv5_RAD(Ridit_mat.E3,metrics.E3,0.25,K.E3,training.E3$Score1,2,0.01,"radial",892) ; Store(RAD.E3)
RAD.E4<-Cv5_RAD(Ridit_mat.E4,metrics.E4,0.4,K.E4,training.E4$Score1,2,0.01,"radial",892) ; Store(RAD.E4)
RAD.E5<-Cv5_RAD(Ridit_mat.E5,metrics.E5,0.25,K.E5,training.E5$Score1,2,0.01,"radial",892) ; Store(RAD.E5)
RAD.E6<-Cv5_RAD(Ridit_mat.E6,metrics.E6,0.25,K.E6,training.E6$Score1,2,0.01,"radial",892) ; Store(RAD.E6)
RAD.E7<-Cv5_RAD(Ridit_mat.E7,metrics.E7,0.25,K.E7,training.E7$Score1,2,0.01,"radial",892) ; Store(RAD.E7)
RAD.E8<-Cv5_RAD(Ridit_mat.E8,metrics.E8,0.8,K.E8,training.E8$Score1,2,0.01,"radial",892) ; Store(RAD.E8)
RAD.E9<-Cv5_RAD(Ridit_mat.E9,metrics.E9,0.3,K.E9,training.E9$Score1,2,0.01,"radial",892) ; Store(RAD.E9)
RAD.E10<-Cv5_RAD(Ridit_mat.E10,metrics.E10,0.25,K.E10,training.E10$Score1,2,0.01,"radial",892) ; Store(RAD.E10)

RAD<-Group(list(RAD.E1[[2]],RAD.E2[[2]],RAD.E3[[2]],RAD.E4[[2]],RAD.E5[[2]],
                RAD.E6[[2]],RAD.E7[[2]],RAD.E8[[2]],RAD.E9[[2]],RAD.E10[[2]])) ; Store(RAD)

Spear_score(RAD)
graph1b(RAD)
Kappa_score(RAD)

#####################################################################
#####################################################################
# TRAIN Glmnet on scaled matrix
# scaled with delta tfidf and bns
# using isgood and isbad
# regression and multinomial
#####################################################################
#####################################################################

#####################################################################
# TRAIN Glmnet on scaled matrix with delta tfidf
#####################################################################

library(glmnet)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

GNETns1.E1<-cv5_GNET2(scal_mat1.E1,K.E1,training.E1$Score1,alpha=0.5,1698) ; Store(GNETns1.E1)
GNETns1.E2<-cv5_GNET2(scal_mat1.E2,K.E2,training.E2$Score1,alpha=0.5,1698) ; Store(GNETns1.E2)
GNETns1.E3<-cv5_GNET2(scal_mat1.E3,K.E3,training.E3$Score1,alpha=0.5,1698) ; Store(GNETns1.E3)
GNETns1.E4<-cv5_GNET2(scal_mat1.E4,K.E4,training.E4$Score1,alpha=0.5,1698) ; Store(GNETns1.E4)
GNETns1.E5<-cv5_GNET2(scal_mat1.E5,K.E5,training.E5$Score1,alpha=0.5,1698) ; Store(GNETns1.E5)
GNETns1.E6<-cv5_GNET2(scal_mat1.E6,K.E6,training.E6$Score1,alpha=0.5,1698) ; Store(GNETns1.E6)
GNETns1.E7<-cv5_GNET2(scal_mat1.E7,K.E7,training.E7$Score1,alpha=0.5,1698) ; Store(GNETns1.E7)
GNETns1.E8<-cv5_GNET2(scal_mat1.E8,K.E8,training.E8$Score1,alpha=0.5,1698) ; Store(GNETns1.E8)
GNETns1.E9<-cv5_GNET2(scal_mat1.E9,K.E9,training.E9$Score1,alpha=0.5,1698) ; Store(GNETns1.E9)
GNETns1.E10<-cv5_GNET2(scal_mat1.E10,K.E10,training.E10$Score1,alpha=0.5,1698) ; Store(GNETns1.E10)

GNETns1<-Group(list(GNETns1.E1[[2]],GNETns1.E2[[2]],GNETns1.E3[[2]],GNETns1.E4[[2]],GNETns1.E5[[2]],
                    GNETns1.E6[[2]],GNETns1.E7[[2]],GNETns1.E8[[2]],GNETns1.E9[[2]],GNETns1.E10[[2]])) ; Store(GNETns1)

Spear_score(GNETns1)
graph1b(GNETns1)
Kappa_score(GNETns1)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

GNETns1.IB.E1<-cv5_GNET2(scal.IB_mat1.E1,K.E1,training.E1$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.E1)
GNETns1.IB.E2<-cv5_GNET2(scal.IB_mat1.E2,K.E2,training.E2$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.E2)
GNETns1.IB.E3<-cv5_GNET2(scal.IB_mat1.E3,K.E3,training.E3$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.E3)
GNETns1.IB.E4<-cv5_GNET2(scal.IB_mat1.E4,K.E4,training.E4$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.E4)
GNETns1.IB.E5<-cv5_GNET2(scal.IB_mat1.E5,K.E5,training.E5$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.E5)
GNETns1.IB.E6<-cv5_GNET2(scal.IB_mat1.E6,K.E6,training.E6$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.E6)
GNETns1.IB.E7<-cv5_GNET2(scal.IB_mat1.E7,K.E7,training.E7$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.E7)
GNETns1.IB.E8<-cv5_GNET2(scal.IB_mat1.E8,K.E8,training.E8$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.E8)
GNETns1.IB.E9<-cv5_GNET2(scal.IB_mat1.E9,K.E9,training.E9$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.E9)
GNETns1.IB.E10<-cv5_GNET2(scal.IB_mat1.E10,K.E10,training.E10$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.E10)

GNETns1.IB<-Group(list(GNETns1.IB.E1[[2]],GNETns1.IB.E2[[2]],GNETns1.IB.E3[[2]],GNETns1.IB.E4[[2]],GNETns1.IB.E5[[2]],
                       GNETns1.IB.E6[[2]],GNETns1.IB.E7[[2]],GNETns1.IB.E8[[2]],GNETns1.IB.E9[[2]],GNETns1.IB.E10[[2]])) ; Store(GNETns1.IB)

Spear_score(GNETns1.IB)
graph1b(GNETns1.IB)
Kappa_score(GNETns1.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

GNETns1.2.E1<-cv5_GNET2(scal_mat1.2.E1,K.E1,training.E1$Score1,alpha=0.5,1698) ; Store(GNETns1.2.E1)
GNETns1.2.E2<-cv5_GNET2(scal_mat1.2.E2,K.E2,training.E2$Score1,alpha=0.5,1698) ; Store(GNETns1.2.E2)
GNETns1.2.E3<-cv5_GNET2(scal_mat1.2.E3,K.E3,training.E3$Score1,alpha=0.5,1698) ; Store(GNETns1.2.E3)
GNETns1.2.E4<-cv5_GNET2(scal_mat1.2.E4,K.E4,training.E4$Score1,alpha=0.5,1698) ; Store(GNETns1.2.E4)
GNETns1.2.E5<-cv5_GNET2(scal_mat1.2.E5,K.E5,training.E5$Score1,alpha=0.5,1698) ; Store(GNETns1.2.E5)
GNETns1.2.E6<-cv5_GNET2(scal_mat1.2.E6,K.E6,training.E6$Score1,alpha=0.5,1698) ; Store(GNETns1.2.E6)
GNETns1.2.E7<-cv5_GNET2(scal_mat1.2.E7,K.E7,training.E7$Score1,alpha=0.5,1698) ; Store(GNETns1.2.E7)
GNETns1.2.E8<-cv5_GNET2(scal_mat1.2.E8,K.E8,training.E8$Score1,alpha=0.5,1698) ; Store(GNETns1.2.E8)
GNETns1.2.E9<-cv5_GNET2(scal_mat1.2.E9,K.E9,training.E9$Score1,alpha=0.5,1698) ; Store(GNETns1.2.E9)
GNETns1.2.E10<-cv5_GNET2(scal_mat1.2.E10,K.E10,training.E10$Score1,alpha=0.5,1698) ; Store(GNETns1.2.E10)

GNETns1.2<-Group(list(GNETns1.2.E1[[2]],GNETns1.2.E2[[2]],GNETns1.2.E3[[2]],GNETns1.2.E4[[2]],GNETns1.2.E5[[2]],
                      GNETns1.2.E6[[2]],GNETns1.2.E7[[2]],GNETns1.2.E8[[2]],GNETns1.2.E9[[2]],GNETns1.2.E10[[2]])) ; Store(GNETns1.2)

Spear_score(GNETns1.2)
graph1b(GNETns1.2)
Kappa_score(GNETns1.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

GNETns1.IB.2.E1<-cv5_GNET2(scal.IB_mat1.2.E1,K.E1,training.E1$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.2.E1)
GNETns1.IB.2.E2<-cv5_GNET2(scal.IB_mat1.2.E2,K.E2,training.E2$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.2.E2)
GNETns1.IB.2.E3<-cv5_GNET2(scal.IB_mat1.2.E3,K.E3,training.E3$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.2.E3)
GNETns1.IB.2.E4<-cv5_GNET2(scal.IB_mat1.2.E4,K.E4,training.E4$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.2.E4)
GNETns1.IB.2.E5<-cv5_GNET2(scal.IB_mat1.2.E5,K.E5,training.E5$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.2.E5)
GNETns1.IB.2.E6<-cv5_GNET2(scal.IB_mat1.2.E6,K.E6,training.E6$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.2.E6)
GNETns1.IB.2.E7<-cv5_GNET2(scal.IB_mat1.2.E7,K.E7,training.E7$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.2.E7)
GNETns1.IB.2.E8<-cv5_GNET2(scal.IB_mat1.2.E8,K.E8,training.E8$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.2.E8)
GNETns1.IB.2.E9<-cv5_GNET2(scal.IB_mat1.2.E9,K.E9,training.E9$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.2.E9)
GNETns1.IB.2.E10<-cv5_GNET2(scal.IB_mat1.2.E10,K.E10,training.E10$Score1,alpha=0.5,1698) ; Store(GNETns1.IB.2.E10)

GNETns1.IB.2<-Group(list(GNETns1.IB.2.E1[[2]],GNETns1.IB.2.E2[[2]],GNETns1.IB.2.E3[[2]],GNETns1.IB.2.E4[[2]],GNETns1.IB.2.E5[[2]],
                         GNETns1.IB.2.E6[[2]],GNETns1.IB.2.E7[[2]],GNETns1.IB.2.E8[[2]],GNETns1.IB.2.E9[[2]],GNETns1.IB.2.E10[[2]])) ; Store(GNETns1.IB.2)

Spear_score(GNETns1.IB.2)
graph1b(GNETns1.IB.2)
Kappa_score(GNETns1.IB.2)

#####################################################################
# TRAIN Glmnet on scaled matrix with bns
#####################################################################

library(glmnet)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

GNETns3.E1<-cv5_GNET2(scal_mat3.E1,K.E1,training.E1$Score1,alpha=0.5,1698) ; Store(GNETns3.E1)
GNETns3.E2<-cv5_GNET2(scal_mat3.E2,K.E2,training.E2$Score1,alpha=0.5,1698) ; Store(GNETns3.E2)
GNETns3.E3<-cv5_GNET2(scal_mat3.E3,K.E3,training.E3$Score1,alpha=0.5,1698) ; Store(GNETns3.E3)
GNETns3.E4<-cv5_GNET2(scal_mat3.E4,K.E4,training.E4$Score1,alpha=0.5,1698) ; Store(GNETns3.E4)
GNETns3.E5<-cv5_GNET2(scal_mat3.E5,K.E5,training.E5$Score1,alpha=0.5,1698) ; Store(GNETns3.E5)
GNETns3.E6<-cv5_GNET2(scal_mat3.E6,K.E6,training.E6$Score1,alpha=0.5,1698) ; Store(GNETns3.E6)
GNETns3.E7<-cv5_GNET2(scal_mat3.E7,K.E7,training.E7$Score1,alpha=0.5,1698) ; Store(GNETns3.E7)
GNETns3.E8<-cv5_GNET2(scal_mat3.E8,K.E8,training.E8$Score1,alpha=0.5,1698) ; Store(GNETns3.E8)
GNETns3.E9<-cv5_GNET2(scal_mat3.E9,K.E9,training.E9$Score1,alpha=0.5,1698) ; Store(GNETns3.E9)
GNETns3.E10<-cv5_GNET2(scal_mat3.E10,K.E10,training.E10$Score1,alpha=0.5,1698) ; Store(GNETns3.E10)

GNETns3<-Group(list(GNETns3.E1[[2]],GNETns3.E2[[2]],GNETns3.E3[[2]],GNETns3.E4[[2]],GNETns3.E5[[2]],
                    GNETns3.E6[[2]],GNETns3.E7[[2]],GNETns3.E8[[2]],GNETns3.E9[[2]],GNETns3.E10[[2]])) ; Store(GNETns3)

Spear_score(GNETns3)
graph1b(GNETns3)
Kappa_score(GNETns3)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

GNETns3.IB.E1<-cv5_GNET2(scal.IB_mat3.E1,K.E1,training.E1$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.E1)
GNETns3.IB.E2<-cv5_GNET2(scal.IB_mat3.E2,K.E2,training.E2$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.E2)
GNETns3.IB.E3<-cv5_GNET2(scal.IB_mat3.E3,K.E3,training.E3$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.E3)
GNETns3.IB.E4<-cv5_GNET2(scal.IB_mat3.E4,K.E4,training.E4$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.E4)
GNETns3.IB.E5<-cv5_GNET2(scal.IB_mat3.E5,K.E5,training.E5$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.E5)
GNETns3.IB.E6<-cv5_GNET2(scal.IB_mat3.E6,K.E6,training.E6$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.E6)
GNETns3.IB.E7<-cv5_GNET2(scal.IB_mat3.E7,K.E7,training.E7$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.E7)
GNETns3.IB.E8<-cv5_GNET2(scal.IB_mat3.E8,K.E8,training.E8$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.E8)
GNETns3.IB.E9<-cv5_GNET2(scal.IB_mat3.E9,K.E9,training.E9$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.E9)
GNETns3.IB.E10<-cv5_GNET2(scal.IB_mat3.E10,K.E10,training.E10$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.E10)

GNETns3.IB<-Group(list(GNETns3.IB.E1[[2]],GNETns3.IB.E2[[2]],GNETns3.IB.E3[[2]],GNETns3.IB.E4[[2]],GNETns3.IB.E5[[2]],
                       GNETns3.IB.E6[[2]],GNETns3.IB.E7[[2]],GNETns3.IB.E8[[2]],GNETns3.IB.E9[[2]],GNETns3.IB.E10[[2]])) ; Store(GNETns3.IB)

Spear_score(GNETns3.IB)
graph1b(GNETns3.IB)
Kappa_score(GNETns3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

GNETns3.2.E1<-cv5_GNET2(scal_mat3.2.E1,K.E1,training.E1$Score1,alpha=0.5,1698) ; Store(GNETns3.2.E1)
GNETns3.2.E2<-cv5_GNET2(scal_mat3.2.E2,K.E2,training.E2$Score1,alpha=0.5,1698) ; Store(GNETns3.2.E2)
GNETns3.2.E3<-cv5_GNET2(scal_mat3.2.E3,K.E3,training.E3$Score1,alpha=0.5,1698) ; Store(GNETns3.2.E3)
GNETns3.2.E4<-cv5_GNET2(scal_mat3.2.E4,K.E4,training.E4$Score1,alpha=0.5,1698) ; Store(GNETns3.2.E4)
GNETns3.2.E5<-cv5_GNET2(scal_mat3.2.E5,K.E5,training.E5$Score1,alpha=0.5,1698) ; Store(GNETns3.2.E5)
GNETns3.2.E6<-cv5_GNET2(scal_mat3.2.E6,K.E6,training.E6$Score1,alpha=0.5,1698) ; Store(GNETns3.2.E6)
GNETns3.2.E7<-cv5_GNET2(scal_mat3.2.E7,K.E7,training.E7$Score1,alpha=0.5,1698) ; Store(GNETns3.2.E7)
GNETns3.2.E8<-cv5_GNET2(scal_mat3.2.E8,K.E8,training.E8$Score1,alpha=0.5,1698) ; Store(GNETns3.2.E8)
GNETns3.2.E9<-cv5_GNET2(scal_mat3.2.E9,K.E9,training.E9$Score1,alpha=0.5,1698) ; Store(GNETns3.2.E9)
GNETns3.2.E10<-cv5_GNET2(scal_mat3.2.E10,K.E10,training.E10$Score1,alpha=0.5,1698) ; Store(GNETns3.2.E10)

GNETns3.2<-Group(list(GNETns3.2.E1[[2]],GNETns3.2.E2[[2]],GNETns3.2.E3[[2]],GNETns3.2.E4[[2]],GNETns3.2.E5[[2]],
                      GNETns3.2.E6[[2]],GNETns3.2.E7[[2]],GNETns3.2.E8[[2]],GNETns3.2.E9[[2]],GNETns3.2.E10[[2]])) ; Store(GNETns3.2)

Spear_score(GNETns3.2)
graph1b(GNETns3.2)
Kappa_score(GNETns3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

GNETns3.IB.2.E1<-cv5_GNET2(scal.IB_mat3.2.E1,K.E1,training.E1$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.2.E1)
GNETns3.IB.2.E2<-cv5_GNET2(scal.IB_mat3.2.E2,K.E2,training.E2$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.2.E2)
GNETns3.IB.2.E3<-cv5_GNET2(scal.IB_mat3.2.E3,K.E3,training.E3$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.2.E3)
GNETns3.IB.2.E4<-cv5_GNET2(scal.IB_mat3.2.E4,K.E4,training.E4$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.2.E4)
GNETns3.IB.2.E5<-cv5_GNET2(scal.IB_mat3.2.E5,K.E5,training.E5$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.2.E5)
GNETns3.IB.2.E6<-cv5_GNET2(scal.IB_mat3.2.E6,K.E6,training.E6$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.2.E6)
GNETns3.IB.2.E7<-cv5_GNET2(scal.IB_mat3.2.E7,K.E7,training.E7$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.2.E7)
GNETns3.IB.2.E8<-cv5_GNET2(scal.IB_mat3.2.E8,K.E8,training.E8$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.2.E8)
GNETns3.IB.2.E9<-cv5_GNET2(scal.IB_mat3.2.E9,K.E9,training.E9$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.2.E9)
GNETns3.IB.2.E10<-cv5_GNET2(scal.IB_mat3.2.E10,K.E10,training.E10$Score1,alpha=0.5,1698) ; Store(GNETns3.IB.2.E10)

GNETns3.IB.2<-Group(list(GNETns3.IB.2.E1[[2]],GNETns3.IB.2.E2[[2]],GNETns3.IB.2.E3[[2]],GNETns3.IB.2.E4[[2]],GNETns3.IB.2.E5[[2]],
                         GNETns3.IB.2.E6[[2]],GNETns3.IB.2.E7[[2]],GNETns3.IB.2.E8[[2]],GNETns3.IB.2.E9[[2]],GNETns3.IB.2.E10[[2]])) ; Store(GNETns3.IB.2)

Spear_score(GNETns3.IB.2)
graph1b(GNETns3.IB.2)
Kappa_score(GNETns3.IB.2)

#####################################################################
# Train multinomial glmnet
#####################################################################

# function to train GNETcr
#####################################################################

CVGNETcr<-function(data,newdata,y,alpha,stand,seed) {
  set.seed(seed)
  out=list()
  y=factor(y,ordered=TRUE)
  out[[1]]<-glmnet.cr(data,y, alpha=alpha,maxit=500,standardize=stand)
  if (!is.null(newdata)) out[[2]]<-as.numeric(as.character(predict(out[[1]],newdata,type="class")$class[,95]))
  out
}
Store(CVGNETcr)

Cv5_GNETcr<-function(data,K,y,alpha,stand,seed) {
  set.seed(seed)
  out<-list()
  out[[1]]<-list()
  out[[2]]<-rep(0,nrow(data[[1]]))
  for (i in 1:6) {
    if (i<=5) temp<-CVGNETcr(data[[i]][-K[[i]],],data[[i]][K[[i]],],y[-K[[i]]],alpha,stand,seed) else
      temp<-CVGNETcr(data[[i]],NULL,y,alpha,stand,seed) 
    out[[1]][[i]]<-temp[[1]]
    if (i<=5) out[[2]][K[[i]]]<-temp[[2]]
    print(paste("set",i,"done"))
  }
  out
}
Store(Cv5_GNETcr)

library(glmnetcr)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

GNETcr3.E1<-Cv5_GNETcr(scal_mat3.E1,K.E1,training.E1$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.E1)
GNETcr3.E2<-Cv5_GNETcr(scal_mat3.E2,K.E2,training.E2$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.E2)
GNETcr3.E3<-Cv5_GNETcr(scal_mat3.E3,K.E3,training.E3$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.E3)
GNETcr3.E4<-Cv5_GNETcr(scal_mat3.E4,K.E4,training.E4$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.E4)
GNETcr3.E5<-Cv5_GNETcr(scal_mat3.E5,K.E5,training.E5$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.E5)
GNETcr3.E6<-Cv5_GNETcr(scal_mat3.E6,K.E6,training.E6$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.E6)
GNETcr3.E7<-Cv5_GNETcr(scal_mat3.E7,K.E7,training.E7$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.E7)
GNETcr3.E8<-Cv5_GNETcr(scal_mat3.E8,K.E8,training.E8$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.E8)
GNETcr3.E9<-Cv5_GNETcr(scal_mat3.E9,K.E9,training.E9$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.E9)
GNETcr3.E10<-Cv5_GNETcr(scal_mat3.E10,K.E10,training.E10$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.E10)

GNETcr3<-Group(list(GNETcr3.E1[[2]],GNETcr3.E2[[2]],GNETcr3.E3[[2]],GNETcr3.E4[[2]],GNETcr3.E5[[2]],
                    GNETcr3.E6[[2]],GNETcr3.E7[[2]],GNETcr3.E8[[2]],GNETcr3.E9[[2]],GNETcr3.E10[[2]])) ; Store(GNETcr3)

Spear_score(GNETcr3)
graph1b(GNETcr3)
Kappa_score(GNETcr3)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

GNETcr3.IB.E1<-Cv5_GNETcr(scal.IB_mat3.E1,K.E1,training.E1$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.E1)
GNETcr3.IB.E2<-Cv5_GNETcr(scal.IB_mat3.E2,K.E2,training.E2$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.E2)
GNETcr3.IB.E3<-Cv5_GNETcr(scal.IB_mat3.E3,K.E3,training.E3$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.E3)
GNETcr3.IB.E4<-Cv5_GNETcr(scal.IB_mat3.E4,K.E4,training.E4$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.E4)
GNETcr3.IB.E5<-Cv5_GNETcr(scal.IB_mat3.E5,K.E5,training.E5$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.E5)
GNETcr3.IB.E6<-Cv5_GNETcr(scal.IB_mat3.E6,K.E6,training.E6$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.E6)
GNETcr3.IB.E7<-Cv5_GNETcr(scal.IB_mat3.E7,K.E7,training.E7$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.E7)
GNETcr3.IB.E8<-Cv5_GNETcr(scal.IB_mat3.E8,K.E8,training.E8$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.E8)
GNETcr3.IB.E9<-Cv5_GNETcr(scal.IB_mat3.E9,K.E9,training.E9$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.E9)
GNETcr3.IB.E10<-Cv5_GNETcr(scal.IB_mat3.E10,K.E10,training.E10$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.E10)

GNETcr3.IB<-Group(list(GNETcr3.IB.E1[[2]],GNETcr3.IB.E2[[2]],GNETcr3.IB.E3[[2]],GNETcr3.IB.E4[[2]],GNETcr3.IB.E5[[2]],
                       GNETcr3.IB.E6[[2]],GNETcr3.IB.E7[[2]],GNETcr3.IB.E8[[2]],GNETcr3.IB.E9[[2]],GNETcr3.IB.E10[[2]])) ; Store(GNETcr3.IB)

Spear_score(GNETcr3.IB)
graph1b(GNETcr3.IB)
Kappa_score(GNETcr3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

GNETcr3.2.E1<-Cv5_GNETcr(scal_mat3.2.E1,K.E1,training.E1$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.2.E1)
GNETcr3.2.E2<-Cv5_GNETcr(scal_mat3.2.E2,K.E2,training.E2$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.2.E2)
GNETcr3.2.E3<-Cv5_GNETcr(scal_mat3.2.E3,K.E3,training.E3$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.2.E3)
GNETcr3.2.E4<-Cv5_GNETcr(scal_mat3.2.E4,K.E4,training.E4$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.2.E4)
GNETcr3.2.E5<-Cv5_GNETcr(scal_mat3.2.E5,K.E5,training.E5$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.2.E5)
GNETcr3.2.E6<-Cv5_GNETcr(scal_mat3.2.E6,K.E6,training.E6$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.2.E6)
GNETcr3.2.E7<-Cv5_GNETcr(scal_mat3.2.E7,K.E7,training.E7$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.2.E7)
GNETcr3.2.E8<-Cv5_GNETcr(scal_mat3.2.E8,K.E8,training.E8$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.2.E8)
GNETcr3.2.E9<-Cv5_GNETcr(scal_mat3.2.E9,K.E9,training.E9$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.2.E9)
GNETcr3.2.E10<-Cv5_GNETcr(scal_mat3.2.E10,K.E10,training.E10$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.2.E10)

GNETcr3.2<-Group(list(GNETcr3.2.E1[[2]],GNETcr3.2.E2[[2]],GNETcr3.2.E3[[2]],GNETcr3.2.E4[[2]],GNETcr3.2.E5[[2]],
                      GNETcr3.2.E6[[2]],GNETcr3.2.E7[[2]],GNETcr3.2.E8[[2]],GNETcr3.2.E9[[2]],GNETcr3.2.E10[[2]])) ; Store(GNETcr3.2)

Spear_score(GNETcr3.2)
graph1b(GNETcr3.2)
Kappa_score(GNETcr3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

GNETcr3.IB.2.E1<-Cv5_GNETcr(scal.IB_mat3.2.E1,K.E1,training.E1$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.2.E1)
GNETcr3.IB.2.E2<-Cv5_GNETcr(scal.IB_mat3.2.E2,K.E2,training.E2$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.2.E2)
GNETcr3.IB.2.E3<-Cv5_GNETcr(scal.IB_mat3.2.E3,K.E3,training.E3$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.2.E3)
GNETcr3.IB.2.E4<-Cv5_GNETcr(scal.IB_mat3.2.E4,K.E4,training.E4$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.2.E4)
GNETcr3.IB.2.E5<-Cv5_GNETcr(scal.IB_mat3.2.E5,K.E5,training.E5$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.2.E5)
GNETcr3.IB.2.E6<-Cv5_GNETcr(scal.IB_mat3.2.E6,K.E6,training.E6$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.2.E6)
GNETcr3.IB.2.E7<-Cv5_GNETcr(scal.IB_mat3.2.E7,K.E7,training.E7$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.2.E7)
GNETcr3.IB.2.E8<-Cv5_GNETcr(scal.IB_mat3.2.E8,K.E8,training.E8$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.2.E8)
GNETcr3.IB.2.E9<-Cv5_GNETcr(scal.IB_mat3.2.E9,K.E9,training.E9$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.2.E9)
GNETcr3.IB.2.E10<-Cv5_GNETcr(scal.IB_mat3.2.E10,K.E10,training.E10$Score1,alpha=0.5,stand=FALSE,1698) ; Store(GNETcr3.IB.2.E10)

GNETcr3.IB.2<-Group(list(GNETcr3.IB.2.E1[[2]],GNETcr3.IB.2.E2[[2]],GNETcr3.IB.2.E3[[2]],GNETcr3.IB.2.E4[[2]],GNETcr3.IB.2.E5[[2]],
                         GNETcr3.IB.2.E6[[2]],GNETcr3.IB.2.E7[[2]],GNETcr3.IB.2.E8[[2]],GNETcr3.IB.2.E9[[2]],GNETcr3.IB.2.E10[[2]])) ; Store(GNETcr3.IB.2)

Spear_score(GNETcr3.IB.2)
graph1b(GNETcr3.IB.2)
Kappa_score(GNETcr3.IB.2)

#####################################################################
#####################################################################
# Features Selection using GNET1R
#####################################################################
#####################################################################

cv5_DATAG<-function(data,FIT) {
  out<-list()
  for (i in 1:6) {
    fit<-FIT[[1]][[i]]
    VARSelect<-names(which(fit$glmnet.fit$beta[,which(fit$lambda==fit$lambda.min)]!=0))
    VARSelect<-as.numeric(gsub("V","",VARSelect))
    out[[i]]<-data[,VARSelect]
    print(paste("set",i,"done"))}    	
  out
}
Store(cv5_DATAG)

DATAG1.E1<-cv5_DATAG(bin_mat.E1,GNET1R.E1) ; Store(DATAG1.E1)
DATAG1.E2<-cv5_DATAG(bin_mat.E2,GNET1R.E2) ; Store(DATAG1.E2)
DATAG1.E3<-cv5_DATAG(bin_mat.E3,GNET1R.E3) ; Store(DATAG1.E3)
DATAG1.E4<-cv5_DATAG(bin_mat.E4,GNET1R.E4) ; Store(DATAG1.E4)
DATAG1.E5<-cv5_DATAG(bin_mat.E5,GNET1R.E5) ; Store(DATAG1.E5)
DATAG1.E6<-cv5_DATAG(bin_mat.E6,GNET1R.E6) ; Store(DATAG1.E6)
DATAG1.E7<-cv5_DATAG(bin_mat.E7,GNET1R.E7) ; Store(DATAG1.E7)
DATAG1.E8<-cv5_DATAG(bin_mat.E8,GNET1R.E8) ; Store(DATAG1.E8)
DATAG1.E9<-cv5_DATAG(bin_mat.E9,GNET1R.E9) ; Store(DATAG1.E9)
DATAG1.E10<-cv5_DATAG(bin_mat.E10,GNET1R.E10) ; Store(DATAG1.E10)

#####################################################################
#####################################################################
# Combine proxies with reduced term matrix
#####################################################################
#####################################################################

cv5_COMBGP<-function(datag,proxies) {
  out<-list()
  for (i in 1:6) {
    out[[i]]<-data.frame(proxies,datag[[i]])
    print(paste("set",i,"done"))}  		
  out
}
Store(cv5_COMBGP)

comb<-10:21
names(training)[comb]
COMBGP1.E1<-cv5_COMBGP(DATAG1.E1,training[training$EssaySet==1,comb]) ; Store(COMBGP1.E1)
COMBGP1.E2<-cv5_COMBGP(DATAG1.E2,training[training$EssaySet==2,comb]) ; Store(COMBGP1.E2)
COMBGP1.E3<-cv5_COMBGP(DATAG1.E3,training[training$EssaySet==3,comb]) ; Store(COMBGP1.E3)
COMBGP1.E4<-cv5_COMBGP(DATAG1.E4,training[training$EssaySet==4,comb]) ; Store(COMBGP1.E4)
COMBGP1.E5<-cv5_COMBGP(DATAG1.E5,training[training$EssaySet==5,comb]) ; Store(COMBGP1.E5)
COMBGP1.E6<-cv5_COMBGP(DATAG1.E6,training[training$EssaySet==6,comb]) ; Store(COMBGP1.E6)
COMBGP1.E7<-cv5_COMBGP(DATAG1.E7,training[training$EssaySet==7,comb]) ; Store(COMBGP1.E7)
COMBGP1.E8<-cv5_COMBGP(DATAG1.E8,training[training$EssaySet==8,comb]) ; Store(COMBGP1.E8)
COMBGP1.E9<-cv5_COMBGP(DATAG1.E9,training[training$EssaySet==9,comb]) ; Store(COMBGP1.E9)
COMBGP1.E10<-cv5_COMBGP(DATAG1.E10,training[training$EssaySet==10,comb]) ; Store(COMBGP1.E10)

#####################################################################
#####################################################################
# Train RF on reduced matrix with proxies
#####################################################################
#####################################################################

# function to train RF
#####################################################################

cv5_SEL_RF<-function(data,K,train_scores,seed) {
  set.seed(seed)
  out<-list()
  out[[1]]<-list()
  for (i in 1:6) {
    out[[1]][[i]]<-if (i<=5) 
      randomForest(x=data[[i]][-K[[i]],],y=train_scores[-K[[i]]],ntree=500) else
        randomForest(x=data[[i]][,],y=train_scores,ntree=500)
    print(paste("set",i,"done"))}    	
  out[[2]]<-rep(0,length(train_scores))
  for (i in 1:5) out[[2]][K[[i]]]<-predict(out[[1]][[i]],data[[i]][K[[i]],]) 
  out
}
Store(cv5_SEL_RF)

library(randomForest)

SEL_RF1.E1<-cv5_SEL_RF(COMBGP1.E1,K.E1,training.E1$Score1,  405) ; Store(SEL_RF1.E1)
SEL_RF1.E2<-cv5_SEL_RF(COMBGP1.E2,K.E2,training.E2$Score1,  405) ; Store(SEL_RF1.E2)
SEL_RF1.E3<-cv5_SEL_RF(COMBGP1.E3,K.E3,training.E3$Score1,  405) ; Store(SEL_RF1.E3)
SEL_RF1.E4<-cv5_SEL_RF(COMBGP1.E4,K.E4,training.E4$Score1,  405) ; Store(SEL_RF1.E4)
SEL_RF1.E5<-cv5_SEL_RF(COMBGP1.E5,K.E5,training.E5$Score1,  405) ; Store(SEL_RF1.E5)
SEL_RF1.E6<-cv5_SEL_RF(COMBGP1.E6,K.E6,training.E6$Score1,  405) ; Store(SEL_RF1.E6)
SEL_RF1.E7<-cv5_SEL_RF(COMBGP1.E7,K.E7,training.E7$Score1,  405) ; Store(SEL_RF1.E7)
SEL_RF1.E8<-cv5_SEL_RF(COMBGP1.E8,K.E8,training.E8$Score1,  405) ; Store(SEL_RF1.E8)
SEL_RF1.E9<-cv5_SEL_RF(COMBGP1.E9,K.E9,training.E9$Score1,  405) ; Store(SEL_RF1.E9)
SEL_RF1.E10<-cv5_SEL_RF(COMBGP1.E10,K.E10,training.E10$Score1,  405) ; Store(SEL_RF1.E10)

SEL_RF1<-Group(list(SEL_RF1.E1[[2]],SEL_RF1.E2[[2]],SEL_RF1.E3[[2]],SEL_RF1.E4[[2]],SEL_RF1.E5[[2]],
                    SEL_RF1.E6[[2]],SEL_RF1.E7[[2]],SEL_RF1.E8[[2]],SEL_RF1.E9[[2]],SEL_RF1.E10[[2]])) ; Store(SEL_RF1)

Spear_score(SEL_RF1)
graph1b(SEL_RF1)
Kappa_score(SEL_RF1)

#####################################################################
#####################################################################
# Combine proxies with 2nd set of bin_mat
#####################################################################
#####################################################################

comb<-10:21
names(training)[comb]
COMBGP1.2.E1<-cv5_COMBGP(bin_mat.2.E1,training[training$EssaySet==1,comb]) ; Store(COMBGP1.2.E1)
COMBGP1.2.E2<-cv5_COMBGP(bin_mat.2.E2,training[training$EssaySet==2,comb]) ; Store(COMBGP1.2.E2)
COMBGP1.2.E3<-cv5_COMBGP(bin_mat.2.E3,training[training$EssaySet==3,comb]) ; Store(COMBGP1.2.E3)
COMBGP1.2.E4<-cv5_COMBGP(bin_mat.2.E4,training[training$EssaySet==4,comb]) ; Store(COMBGP1.2.E4)
COMBGP1.2.E5<-cv5_COMBGP(bin_mat.2.E5,training[training$EssaySet==5,comb]) ; Store(COMBGP1.2.E5)
COMBGP1.2.E6<-cv5_COMBGP(bin_mat.2.E6,training[training$EssaySet==6,comb]) ; Store(COMBGP1.2.E6)
COMBGP1.2.E7<-cv5_COMBGP(bin_mat.2.E7,training[training$EssaySet==7,comb]) ; Store(COMBGP1.2.E7)
COMBGP1.2.E8<-cv5_COMBGP(bin_mat.2.E8,training[training$EssaySet==8,comb]) ; Store(COMBGP1.2.E8)
COMBGP1.2.E9<-cv5_COMBGP(bin_mat.2.E9,training[training$EssaySet==9,comb]) ; Store(COMBGP1.2.E9)
COMBGP1.2.E10<-cv5_COMBGP(bin_mat.2.E10,training[training$EssaySet==10,comb]) ; Store(COMBGP1.2.E10)

#####################################################################
#####################################################################
# Train RF on 2nd set of bin_mat with proxies
#####################################################################
#####################################################################

library(randomForest)

SEL_RF1.2.E1<-cv5_SEL_RF(COMBGP1.2.E1,K.E1,training.E1$Score1,  405) ; Store(SEL_RF1.2.E1)
SEL_RF1.2.E2<-cv5_SEL_RF(COMBGP1.2.E2,K.E2,training.E2$Score1,  405) ; Store(SEL_RF1.2.E2)
SEL_RF1.2.E3<-cv5_SEL_RF(COMBGP1.2.E3,K.E3,training.E3$Score1,  405) ; Store(SEL_RF1.2.E3)
SEL_RF1.2.E4<-cv5_SEL_RF(COMBGP1.2.E4,K.E4,training.E4$Score1,  405) ; Store(SEL_RF1.2.E4)
SEL_RF1.2.E5<-cv5_SEL_RF(COMBGP1.2.E5,K.E5,training.E5$Score1,  405) ; Store(SEL_RF1.2.E5)
SEL_RF1.2.E6<-cv5_SEL_RF(COMBGP1.2.E6,K.E6,training.E6$Score1,  405) ; Store(SEL_RF1.2.E6)
SEL_RF1.2.E7<-cv5_SEL_RF(COMBGP1.2.E7,K.E7,training.E7$Score1,  405) ; Store(SEL_RF1.2.E7)
SEL_RF1.2.E8<-cv5_SEL_RF(COMBGP1.2.E8,K.E8,training.E8$Score1,  405) ; Store(SEL_RF1.2.E8)
SEL_RF1.2.E9<-cv5_SEL_RF(COMBGP1.2.E9,K.E9,training.E9$Score1,  405) ; Store(SEL_RF1.2.E9)
SEL_RF1.2.E10<-cv5_SEL_RF(COMBGP1.2.E10,K.E10,training.E10$Score1,  405) ; Store(SEL_RF1.2.E10)

SEL_RF1.2<-Group(list(SEL_RF1.2.E1[[2]],SEL_RF1.2.E2[[2]],SEL_RF1.2.E3[[2]],SEL_RF1.2.E4[[2]],SEL_RF1.2.E5[[2]],
                      SEL_RF1.2.E6[[2]],SEL_RF1.2.E7[[2]],SEL_RF1.2.E8[[2]],SEL_RF1.2.E9[[2]],SEL_RF1.2.E10[[2]])) ; Store(SEL_RF1.2)

Spear_score(SEL_RF1.2)
graph1b(SEL_RF1.2)
Kappa_score(SEL_RF1.2)

#####################################################################
#####################################################################
# Compute simple averages
#####################################################################
#####################################################################

SEL_RF1.MIX<-(SEL_RF1+SEL_RF1.2)/2 ; Store(SEL_RF1.MIX)
GNET1R.MIX<-(GNET1R+GNET1R.2)/2 ; Store(GNET1R.MIX)
SVM3.MIX<-(SVM3+SVM3.2+SVM3.IB+SVM3.IB.2)/4 ; Store(SVM3.MIX)
RAD2.MIX<-(RAD2+RAD2.2+RAD2.IB+RAD2.IB.2)/4 ; Store(RAD2.MIX)
SVMc3.MIX<-(SVMc3b+SVMc3.2-SVMc3.IB-SVMc3b.IB.2)/4 ; Store(SVMc3.MIX)
RADc2.MIX<-(RADc2+RADc2.2-RADc2.IB-RADc2.IB.2)/4 ; Store(RADc2.MIX)
SVMm3.MIX<-(SVMm3+SVMm3.2+SVMm3.IB+SVMm3.IB.2)/4 ; Store(SVMm3.MIX)
RADm2.MIX<-(RADm2+RADm2.2+RADm2.IB+RADm2.IB.2)/4 ; Store(RADm2.MIX)
GNETns1.MIX<-(GNETns1+GNETns1.2+GNETns1.IB+GNETns1.IB.2)/4 ; Store(GNETns1.MIX)
GNETns3.MIX<-(GNETns3+GNETns3.2+GNETns3.IB+GNETns3.IB.2)/4 ; Store(GNETns3.MIX)
GNETcr3.MIX<-(GNETcr3+GNETcr3.2+GNETcr3.IB+GNETcr3.IB.2)/4 ; Store(GNETcr3.MIX)

Spear_score(SEL_RF1.MIX)
Spear_score(GNET1R.MIX)
Spear_score(SVM3.MIX)
Spear_score(RAD2.MIX)
Spear_score(SVMc3.MIX)
Spear_score(RADc2.MIX)
Spear_score(SVMm3.MIX)
Spear_score(RADm2.MIX)
Spear_score(GNETns1.MIX)
Spear_score(GNETns3.MIX)
Spear_score(GNETcr3.MIX)

#####################################################################
#####################################################################
#####################################################################
# Same but using Score2 to train models
#####################################################################
#####################################################################
#####################################################################

#####################################################################
#####################################################################
# train GNET (standardize=FALSE) on 
# 2 sets of bin_mat after Ridit transformation
#####################################################################
#####################################################################

# 1rst set of bin_mat (transformed with ridit coding) 
#####################################################################

library(glmnet)
S2.GNET1R.E1<-cv5_GNET(Ridit_mat.E1,K.E1,training.E1$Score2,alpha=0.5,FALSE,1698) ; Store(S2.GNET1R.E1)
S2.GNET1R.E2<-cv5_GNET(Ridit_mat.E2,K.E2,training.E2$Score2,alpha=0.5,FALSE,1698) ; Store(S2.GNET1R.E2)
S2.GNET1R.E3<-cv5_GNET(Ridit_mat.E3,K.E3,training.E3$Score2,alpha=0.5,FALSE,1698) ; Store(S2.GNET1R.E3)
S2.GNET1R.E4<-cv5_GNET(Ridit_mat.E4,K.E4,training.E4$Score2,alpha=0.5,FALSE,1698) ; Store(S2.GNET1R.E4)
S2.GNET1R.E5<-cv5_GNET(Ridit_mat.E5,K.E5,training.E5$Score2,alpha=0.5,FALSE,1698) ; Store(S2.GNET1R.E5)
S2.GNET1R.E6<-cv5_GNET(Ridit_mat.E6,K.E6,training.E6$Score2,alpha=0.5,FALSE,1698) ; Store(S2.GNET1R.E6)
S2.GNET1R.E7<-cv5_GNET(Ridit_mat.E7,K.E7,training.E7$Score2,alpha=0.5,FALSE,1698) ; Store(S2.GNET1R.E7)
S2.GNET1R.E8<-cv5_GNET(Ridit_mat.E8,K.E8,training.E8$Score2,alpha=0.5,FALSE,1698) ; Store(S2.GNET1R.E8)
S2.GNET1R.E9<-cv5_GNET(Ridit_mat.E9,K.E9,training.E9$Score2,alpha=0.5,FALSE,1698) ; Store(S2.GNET1R.E9)
S2.GNET1R.E10<-cv5_GNET(Ridit_mat.E10,K.E10,training.E10$Score2,alpha=0.5,FALSE,1698) ; Store(S2.GNET1R.E10)

S2.GNET1R<-Group(list(S2.GNET1R.E1[[2]],S2.GNET1R.E2[[2]],S2.GNET1R.E3[[2]],S2.GNET1R.E4[[2]],S2.GNET1R.E5[[2]],
                      S2.GNET1R.E6[[2]],S2.GNET1R.E7[[2]],S2.GNET1R.E8[[2]],S2.GNET1R.E9[[2]],S2.GNET1R.E10[[2]])) ; Store(S2.GNET1R)
Spear_score(S2.GNET1R)
graph1b(S2.GNET1R)
Kappa_score(S2.GNET1R)

# 2nd set of bin_mat (transformed with ridit coding) 
#####################################################################

library(glmnet)
S2.GNET1R.2.E1<-cv5_GNET2(Ridit_mat.2.E1,K.E1,training.E1$Score2,alpha=0.5,1698) ; Store(S2.GNET1R.2.E1)
S2.GNET1R.2.E2<-cv5_GNET2(Ridit_mat.2.E2,K.E2,training.E2$Score2,alpha=0.5,1698) ; Store(S2.GNET1R.2.E2)
S2.GNET1R.2.E3<-cv5_GNET2(Ridit_mat.2.E3,K.E3,training.E3$Score2,alpha=0.5,1698) ; Store(S2.GNET1R.2.E3)
S2.GNET1R.2.E4<-cv5_GNET2(Ridit_mat.2.E4,K.E4,training.E4$Score2,alpha=0.5,1698) ; Store(S2.GNET1R.2.E4)
S2.GNET1R.2.E5<-cv5_GNET2(Ridit_mat.2.E5,K.E5,training.E5$Score2,alpha=0.5,1698) ; Store(S2.GNET1R.2.E5)
S2.GNET1R.2.E6<-cv5_GNET2(Ridit_mat.2.E6,K.E6,training.E6$Score2,alpha=0.5,1698) ; Store(S2.GNET1R.2.E6)
S2.GNET1R.2.E7<-cv5_GNET2(Ridit_mat.2.E7,K.E7,training.E7$Score2,alpha=0.5,1698) ; Store(S2.GNET1R.2.E7)
S2.GNET1R.2.E8<-cv5_GNET2(Ridit_mat.2.E8,K.E8,training.E8$Score2,alpha=0.5,1698) ; Store(S2.GNET1R.2.E8)
S2.GNET1R.2.E9<-cv5_GNET2(Ridit_mat.2.E9,K.E9,training.E9$Score2,alpha=0.5,1698) ; Store(S2.GNET1R.2.E9)
S2.GNET1R.2.E10<-cv5_GNET2(Ridit_mat.2.E10,K.E10,training.E10$Score2,alpha=0.5,1698) ; Store(S2.GNET1R.2.E10)

S2.GNET1R.2<-Group(list(S2.GNET1R.2.E1[[2]],S2.GNET1R.2.E2[[2]],S2.GNET1R.2.E3[[2]],S2.GNET1R.2.E4[[2]],S2.GNET1R.2.E5[[2]],
                        S2.GNET1R.2.E6[[2]],S2.GNET1R.2.E7[[2]],S2.GNET1R.2.E8[[2]],S2.GNET1R.2.E9[[2]],S2.GNET1R.2.E10[[2]])) ; Store(S2.GNET1R.2)
Spear_score(S2.GNET1R.2)
graph1b(S2.GNET1R.2)
Kappa_score(S2.GNET1R.2)

#####################################################################
#####################################################################
# TRAIN SVM
# on 2 sets of bin mat
# scaled with bns using "isgood" or "isbadd"
# with linear and radial kernels
# regression, multinomial and binary classification
#####################################################################
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

S2.SVM3.E1<-cv5_SVM(scal_mat3.E1,K.E1,training.E1$Score2,0.1,267) ; Store(S2.SVM3.E1)
S2.SVM3.E2<-cv5_SVM(scal_mat3.E2,K.E2,training.E2$Score2,0.1,207) ; Store(S2.SVM3.E2)
S2.SVM3.E3<-cv5_SVM(scal_mat3.E3,K.E3,training.E3$Score2,0.1,167) ; Store(S2.SVM3.E3)
S2.SVM3.E4<-cv5_SVM(scal_mat3.E4,K.E4,training.E4$Score2,0.1,287) ; Store(S2.SVM3.E4)
S2.SVM3.E5<-cv5_SVM(scal_mat3.E5,K.E5,training.E5$Score2,0.1,232) ; Store(S2.SVM3.E5)
S2.SVM3.E6<-cv5_SVM(scal_mat3.E6,K.E6,training.E6$Score2,0.1,223) ; Store(S2.SVM3.E6)
S2.SVM3.E7<-cv5_SVM(scal_mat3.E7,K.E7,training.E7$Score2,0.1,478) ; Store(S2.SVM3.E7)
S2.SVM3.E8<-cv5_SVM(scal_mat3.E8,K.E8,training.E8$Score2,0.1,238) ; Store(S2.SVM3.E8)
S2.SVM3.E9<-cv5_SVM(scal_mat3.E9,K.E9,training.E9$Score2,0.1,108) ; Store(S2.SVM3.E9)
S2.SVM3.E10<-cv5_SVM(scal_mat3.E10,K.E10,training.E10$Score2,0.1,232) ; Store(S2.SVM3.E10)

S2.SVM3<-Group(list(S2.SVM3.E1[[2]],S2.SVM3.E2[[2]],S2.SVM3.E3[[2]],S2.SVM3.E4[[2]],S2.SVM3.E5[[2]],
                    S2.SVM3.E6[[2]],S2.SVM3.E7[[2]],S2.SVM3.E8[[2]],S2.SVM3.E9[[2]],S2.SVM3.E10[[2]])) ; Store(S2.SVM3)

Spear_score(S2.SVM3)
graph1b(S2.SVM3)
Kappa_score(S2.SVM3)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.SVM3.IB.E1<-cv5_SVM(scal.IB_mat3.E1,K.E1,training.E1$Score2,0.1,267) ; Store(S2.SVM3.IB.E1)
S2.SVM3.IB.E2<-cv5_SVM(scal.IB_mat3.E2,K.E2,training.E2$Score2,0.1,207) ; Store(S2.SVM3.IB.E2)
S2.SVM3.IB.E3<-cv5_SVM(scal.IB_mat3.E3,K.E3,training.E3$Score2,0.1,167) ; Store(S2.SVM3.IB.E3)
S2.SVM3.IB.E4<-cv5_SVM(scal.IB_mat3.E4,K.E4,training.E4$Score2,0.1,287) ; Store(S2.SVM3.IB.E4)
S2.SVM3.IB.E5<-cv5_SVM(scal.IB_mat3.E5,K.E5,training.E5$Score2,0.1,832) ; Store(S2.SVM3.IB.E5)
S2.SVM3.IB.E6<-cv5_SVM(scal.IB_mat3.E6,K.E6,training.E6$Score2,0.1,253) ; Store(S2.SVM3.IB.E6)
S2.SVM3.IB.E7<-cv5_SVM(scal.IB_mat3.E7,K.E7,training.E7$Score2,0.1,478) ; Store(S2.SVM3.IB.E7)
S2.SVM3.IB.E8<-cv5_SVM(scal.IB_mat3.E8,K.E8,training.E8$Score2,0.1,238) ; Store(S2.SVM3.IB.E8)
S2.SVM3.IB.E9<-cv5_SVM(scal.IB_mat3.E9,K.E9,training.E9$Score2,0.1,108) ; Store(S2.SVM3.IB.E9)
S2.SVM3.IB.E10<-cv5_SVM(scal.IB_mat3.E10,K.E10,training.E10$Score2,0.1,232) ; Store(S2.SVM3.IB.E10)

S2.SVM3.IB<-Group(list(S2.SVM3.IB.E1[[2]],S2.SVM3.IB.E2[[2]],S2.SVM3.IB.E3[[2]],S2.SVM3.IB.E4[[2]],S2.SVM3.IB.E5[[2]],
                       S2.SVM3.IB.E6[[2]],S2.SVM3.IB.E7[[2]],S2.SVM3.IB.E8[[2]],S2.SVM3.IB.E9[[2]],S2.SVM3.IB.E10[[2]])) ; Store(S2.SVM3.IB)

Spear_score(S2.SVM3.IB)
graph1b(S2.SVM3.IB)
Kappa_score(S2.SVM3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.SVM3.2.E1<-cv5_SVM(scal_mat3.2.E1,K.E1,training.E1$Score2,0.1,267) ; Store(S2.SVM3.2.E1)
S2.SVM3.2.E2<-cv5_SVM(scal_mat3.2.E2,K.E2,training.E2$Score2,0.1,207) ; Store(S2.SVM3.2.E2)
S2.SVM3.2.E3<-cv5_SVM(scal_mat3.2.E3,K.E3,training.E3$Score2,0.1,167) ; Store(S2.SVM3.2.E3)
S2.SVM3.2.E4<-cv5_SVM(scal_mat3.2.E4,K.E4,training.E4$Score2,0.1,287) ; Store(S2.SVM3.2.E4)
S2.SVM3.2.E5<-cv5_SVM(scal_mat3.2.E5,K.E5,training.E5$Score2,0.1,232) ; Store(S2.SVM3.2.E5)
S2.SVM3.2.E6<-cv5_SVM(scal_mat3.2.E6,K.E6,training.E6$Score2,0.1,223) ; Store(S2.SVM3.2.E6)
S2.SVM3.2.E7<-cv5_SVM(scal_mat3.2.E7,K.E7,training.E7$Score2,0.1,478) ; Store(S2.SVM3.2.E7)
S2.SVM3.2.E8<-cv5_SVM(scal_mat3.2.E8,K.E8,training.E8$Score2,0.1,238) ; Store(S2.SVM3.2.E8)
S2.SVM3.2.E9<-cv5_SVM(scal_mat3.2.E9,K.E9,training.E9$Score2,0.1,108) ; Store(S2.SVM3.2.E9)
S2.SVM3.2.E10<-cv5_SVM(scal_mat3.2.E10,K.E10,training.E10$Score2,0.1,232) ; Store(S2.SVM3.2.E10)

S2.SVM3.2<-Group(list(S2.SVM3.2.E1[[2]],S2.SVM3.2.E2[[2]],S2.SVM3.2.E3[[2]],S2.SVM3.2.E4[[2]],S2.SVM3.2.E5[[2]],
                      S2.SVM3.2.E6[[2]],S2.SVM3.2.E7[[2]],S2.SVM3.2.E8[[2]],S2.SVM3.2.E9[[2]],S2.SVM3.2.E10[[2]])) ; Store(S2.SVM3.2)

Spear_score(S2.SVM3.2)
graph1b(S2.SVM3.2)
Kappa_score(S2.SVM3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.SVM3.IB.2.E1<-cv5_SVM(scal.IB_mat3.2.E1,K.E1,training.E1$Score2,0.1,267) ; Store(S2.SVM3.IB.2.E1)
S2.SVM3.IB.2.E2<-cv5_SVM(scal.IB_mat3.2.E2,K.E2,training.E2$Score2,0.1,207) ; Store(S2.SVM3.IB.2.E2)
S2.SVM3.IB.2.E3<-cv5_SVM(scal.IB_mat3.2.E3,K.E3,training.E3$Score2,0.1,167) ; Store(S2.SVM3.IB.2.E3)
S2.SVM3.IB.2.E4<-cv5_SVM(scal.IB_mat3.2.E4,K.E4,training.E4$Score2,0.1,287) ; Store(S2.SVM3.IB.2.E4)
S2.SVM3.IB.2.E5<-cv5_SVM(scal.IB_mat3.2.E5,K.E5,training.E5$Score2,0.1,232) ; Store(S2.SVM3.IB.2.E5)
S2.SVM3.IB.2.E6<-cv5_SVM(scal.IB_mat3.2.E6,K.E6,training.E6$Score2,0.1,223) ; Store(S2.SVM3.IB.2.E6)
S2.SVM3.IB.2.E7<-cv5_SVM(scal.IB_mat3.2.E7,K.E7,training.E7$Score2,0.1,478) ; Store(S2.SVM3.IB.2.E7)
S2.SVM3.IB.2.E8<-cv5_SVM(scal.IB_mat3.2.E8,K.E8,training.E8$Score2,0.1,238) ; Store(S2.SVM3.IB.2.E8)
S2.SVM3.IB.2.E9<-cv5_SVM(scal.IB_mat3.2.E9,K.E9,training.E9$Score2,0.1,108) ; Store(S2.SVM3.IB.2.E9)
S2.SVM3.IB.2.E10<-cv5_SVM(scal.IB_mat3.2.E10,K.E10,training.E10$Score2,0.1,232) ; Store(S2.SVM3.IB.2.E10)

S2.SVM3.IB.2<-Group(list(S2.SVM3.IB.2.E1[[2]],S2.SVM3.IB.2.E2[[2]],S2.SVM3.IB.2.E3[[2]],S2.SVM3.IB.2.E4[[2]],S2.SVM3.IB.2.E5[[2]],
                         S2.SVM3.IB.2.E6[[2]],S2.SVM3.IB.2.E7[[2]],S2.SVM3.IB.2.E8[[2]],S2.SVM3.IB.2.E9[[2]],S2.SVM3.IB.2.E10[[2]])) ; Store(S2.SVM3.IB.2)

Spear_score(S2.SVM3.IB.2)
graph1b(S2.SVM3.IB.2)
Kappa_score(S2.SVM3.IB.2)

#####################################################################
# TRAIN SVM with radial kernel on scaled matrix
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

S2.RAD2.E1<-Cv5_RAD2(scal_mat3.E1,K.E1,training.E1$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.E1)
S2.RAD2.E2<-Cv5_RAD2(scal_mat3.E2,K.E2,training.E2$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.E2)
S2.RAD2.E3<-Cv5_RAD2(scal_mat3.E3,K.E3,training.E3$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.E3)
S2.RAD2.E4<-Cv5_RAD2(scal_mat3.E4,K.E4,training.E4$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.E4)
S2.RAD2.E5<-Cv5_RAD2(scal_mat3.E5,K.E5,training.E5$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.E5)
S2.RAD2.E6<-Cv5_RAD2(scal_mat3.E6,K.E6,training.E6$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.E6)
S2.RAD2.E7<-Cv5_RAD2(scal_mat3.E7,K.E7,training.E7$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.E7)
S2.RAD2.E8<-Cv5_RAD2(scal_mat3.E8,K.E8,training.E8$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.E8)
S2.RAD2.E9<-Cv5_RAD2(scal_mat3.E9,K.E9,training.E9$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.E9)
S2.RAD2.E10<-Cv5_RAD2(scal_mat3.E10,K.E10,training.E10$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.E10)

S2.RAD2<-Group(list(S2.RAD2.E1[[2]],S2.RAD2.E2[[2]],S2.RAD2.E3[[2]],S2.RAD2.E4[[2]],S2.RAD2.E5[[2]],
                    S2.RAD2.E6[[2]],S2.RAD2.E7[[2]],S2.RAD2.E8[[2]],S2.RAD2.E9[[2]],S2.RAD2.E10[[2]])) ; Store(S2.RAD2)
Spear_score(S2.RAD2)
graph1b(S2.RAD2)
Kappa_score(S2.RAD2)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.RAD2.IB.E1<-Cv5_RAD2(scal.IB_mat3.E1,K.E1,training.E1$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.IB.E1)
S2.RAD2.IB.E2<-Cv5_RAD2(scal.IB_mat3.E2,K.E2,training.E2$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.IB.E2)
S2.RAD2.IB.E3<-Cv5_RAD2(scal.IB_mat3.E3,K.E3,training.E3$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.IB.E3)
S2.RAD2.IB.E4<-Cv5_RAD2(scal.IB_mat3.E4,K.E4,training.E4$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.IB.E4)
S2.RAD2.IB.E5<-Cv5_RAD2(scal.IB_mat3.E5,K.E5,training.E5$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.IB.E5)
S2.RAD2.IB.E6<-Cv5_RAD2(scal.IB_mat3.E6,K.E6,training.E6$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.IB.E6)
S2.RAD2.IB.E7<-Cv5_RAD2(scal.IB_mat3.E7,K.E7,training.E7$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.IB.E7)
S2.RAD2.IB.E8<-Cv5_RAD2(scal.IB_mat3.E8,K.E8,training.E8$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.IB.E8)
S2.RAD2.IB.E9<-Cv5_RAD2(scal.IB_mat3.E9,K.E9,training.E9$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.IB.E9)
S2.RAD2.IB.E10<-Cv5_RAD2(scal.IB_mat3.E10,K.E10,training.E10$Score2,3,0.03,"radial",892) ; Store(S2.RAD2.IB.E10)

S2.RAD2.IB<-Group(list(S2.RAD2.IB.E1[[2]],S2.RAD2.IB.E2[[2]],S2.RAD2.IB.E3[[2]],S2.RAD2.IB.E4[[2]],S2.RAD2.IB.E5[[2]],
                       S2.RAD2.IB.E6[[2]],S2.RAD2.IB.E7[[2]],S2.RAD2.IB.E8[[2]],S2.RAD2.IB.E9[[2]],S2.RAD2.IB.E10[[2]])) ; Store(S2.RAD2.IB)
Spear_score(S2.RAD2.IB)
graph1b(S2.RAD2.IB)
Kappa_score(S2.RAD2.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.RAD2.2.E1<-Cv5_RAD2(scal_mat3.2.E1,K.E1,training.E1$Score2,1,0.01,"radial",892) ; Store(S2.RAD2.2.E1)
S2.RAD2.2.E2<-Cv5_RAD2(scal_mat3.2.E2,K.E2,training.E2$Score2,1,0.01,"radial",892) ; Store(S2.RAD2.2.E2)
S2.RAD2.2.E3<-Cv5_RAD2(scal_mat3.2.E3,K.E3,training.E3$Score2,1,0.01,"radial",892) ; Store(S2.RAD2.2.E3)
S2.RAD2.2.E4<-Cv5_RAD2(scal_mat3.2.E4,K.E4,training.E4$Score2,1,0.01,"radial",892) ; Store(S2.RAD2.2.E4)
S2.RAD2.2.E5<-Cv5_RAD2(scal_mat3.2.E5,K.E5,training.E5$Score2,1,0.01,"radial",892) ; Store(S2.RAD2.2.E5)
S2.RAD2.2.E6<-Cv5_RAD2(scal_mat3.2.E6,K.E6,training.E6$Score2,1,0.01,"radial",892) ; Store(S2.RAD2.2.E6)
S2.RAD2.2.E7<-Cv5_RAD2(scal_mat3.2.E7,K.E7,training.E7$Score2,1,0.01,"radial",892) ; Store(S2.RAD2.2.E7)
S2.RAD2.2.E8<-Cv5_RAD2(scal_mat3.2.E8,K.E8,training.E8$Score2,1,0.01,"radial",892) ; Store(S2.RAD2.2.E8)
S2.RAD2.2.E9<-Cv5_RAD2(scal_mat3.2.E9,K.E9,training.E9$Score2,1,0.01,"radial",892) ; Store(S2.RAD2.2.E9)
S2.RAD2.2.E10<-Cv5_RAD2(scal_mat3.2.E10,K.E10,training.E10$Score2,1,0.01,"radial",892) ; Store(S2.RAD2.2.E10)

S2.RAD2.2<-Group(list(S2.RAD2.2.E1[[2]],S2.RAD2.2.E2[[2]],S2.RAD2.2.E3[[2]],S2.RAD2.2.E4[[2]],S2.RAD2.2.E5[[2]],
                      S2.RAD2.2.E6[[2]],S2.RAD2.2.E7[[2]],S2.RAD2.2.E8[[2]],S2.RAD2.2.E9[[2]],S2.RAD2.2.E10[[2]])) ; Store(S2.RAD2.2)
Spear_score(S2.RAD2.2)
graph1b(S2.RAD2.2)
Kappa_score(S2.RAD2.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.RAD2.IB.2.E1<-Cv5_RAD2(scal.IB_mat3.2.E1,K.E1,training.E1$Score2,3,0.01,"radial",892) ; Store(S2.RAD2.IB.2.E1)
S2.RAD2.IB.2.E2<-Cv5_RAD2(scal.IB_mat3.2.E2,K.E2,training.E2$Score2,3,0.01,"radial",892) ; Store(S2.RAD2.IB.2.E2)
S2.RAD2.IB.2.E3<-Cv5_RAD2(scal.IB_mat3.2.E3,K.E3,training.E3$Score2,3,0.01,"radial",892) ; Store(S2.RAD2.IB.2.E3)
S2.RAD2.IB.2.E4<-Cv5_RAD2(scal.IB_mat3.2.E4,K.E4,training.E4$Score2,3,0.01,"radial",892) ; Store(S2.RAD2.IB.2.E4)
S2.RAD2.IB.2.E5<-Cv5_RAD2(scal.IB_mat3.2.E5,K.E5,training.E5$Score2,3,0.01,"radial",892) ; Store(S2.RAD2.IB.2.E5)
S2.RAD2.IB.2.E6<-Cv5_RAD2(scal.IB_mat3.2.E6,K.E6,training.E6$Score2,3,0.01,"radial",892) ; Store(S2.RAD2.IB.2.E6)
S2.RAD2.IB.2.E7<-Cv5_RAD2(scal.IB_mat3.2.E7,K.E7,training.E7$Score2,3,0.01,"radial",892) ; Store(S2.RAD2.IB.2.E7)
S2.RAD2.IB.2.E8<-Cv5_RAD2(scal.IB_mat3.2.E8,K.E8,training.E8$Score2,3,0.01,"radial",892) ; Store(S2.RAD2.IB.2.E8)
S2.RAD2.IB.2.E9<-Cv5_RAD2(scal.IB_mat3.2.E9,K.E9,training.E9$Score2,3,0.01,"radial",892) ; Store(S2.RAD2.IB.2.E9)
S2.RAD2.IB.2.E10<-Cv5_RAD2(scal.IB_mat3.2.E10,K.E10,training.E10$Score2,3,0.01,"radial",892) ; Store(S2.RAD2.IB.2.E10)

S2.RAD2.IB.2<-Group(list(S2.RAD2.IB.2.E1[[2]],S2.RAD2.IB.2.E2[[2]],S2.RAD2.IB.2.E3[[2]],S2.RAD2.IB.2.E4[[2]],S2.RAD2.IB.2.E5[[2]],
                         S2.RAD2.IB.2.E6[[2]],S2.RAD2.IB.2.E7[[2]],S2.RAD2.IB.2.E8[[2]],S2.RAD2.IB.2.E9[[2]],S2.RAD2.IB.2.E10[[2]])) ; Store(S2.RAD2.IB.2)
Spear_score(S2.RAD2.IB.2)
graph1b(S2.RAD2.IB.2)
Kappa_score(S2.RAD2.IB.2)

#####################################################################
# TRAIN SVM to model binary response good/poor essays
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

S2.SVMc3b.E1<-cv5_SVMc(scal_mat3.E1,K.E1,IsGood.E1$isgood2,0.03,267) ; Store(S2.SVMc3b.E1)
S2.SVMc3b.E2<-cv5_SVMc(scal_mat3.E2,K.E2,IsGood.E2$isgood2,0.03,207) ; Store(S2.SVMc3b.E2)
S2.SVMc3b.E3<-cv5_SVMc(scal_mat3.E3,K.E3,IsGood.E3$isgood2,0.03,167) ; Store(S2.SVMc3b.E3)
S2.SVMc3b.E4<-cv5_SVMc(scal_mat3.E4,K.E4,IsGood.E4$isgood2,0.03,287) ; Store(S2.SVMc3b.E4)
S2.SVMc3b.E5<-cv5_SVMc(scal_mat3.E5,K.E5,IsGood.E5$isgood2,0.03,232) ; Store(S2.SVMc3b.E5)
S2.SVMc3b.E6<-cv5_SVMc(scal_mat3.E6,K.E6,IsGood.E6$isgood2,0.03,223) ; Store(S2.SVMc3b.E6)
S2.SVMc3b.E7<-cv5_SVMc(scal_mat3.E7,K.E7,IsGood.E7$isgood2,0.03,478) ; Store(S2.SVMc3b.E7)
S2.SVMc3b.E8<-cv5_SVMc(scal_mat3.E8,K.E8,IsGood.E8$isgood2,0.03,238) ; Store(S2.SVMc3b.E8)
S2.SVMc3b.E9<-cv5_SVMc(scal_mat3.E9,K.E9,IsGood.E9$isgood2,0.03,208) ; Store(S2.SVMc3b.E9)
S2.SVMc3b.E10<-cv5_SVMc(scal_mat3.E10,K.E10,IsGood.E10$isgood2,0.03,138) ; Store(S2.SVMc3b.E10)

S2.SVMc3b<-Group(list(S2.SVMc3b.E1[[2]],S2.SVMc3b.E2[[2]],S2.SVMc3b.E3[[2]],S2.SVMc3b.E4[[2]],S2.SVMc3b.E5[[2]],
                      S2.SVMc3b.E6[[2]],S2.SVMc3b.E7[[2]],S2.SVMc3b.E8[[2]],S2.SVMc3b.E9[[2]],S2.SVMc3b.E10[[2]])) ; Store(S2.SVMc3b)

Spear_score(S2.SVMc3b)
graph1b(S2.SVMc3b)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

library(e1071)
S2.SVMc3.IB.E1<-cv5_SVMc(scal.IB_mat3.E1,K.E1,IsBad.E1$isbad2,0.1,267) ; Store(S2.SVMc3.IB.E1)
S2.SVMc3.IB.E2<-cv5_SVMc(scal.IB_mat3.E2,K.E2,IsBad.E2$isbad2,0.1,207) ; Store(S2.SVMc3.IB.E2)
S2.SVMc3.IB.E3<-cv5_SVMc(scal.IB_mat3.E3,K.E3,IsBad.E3$isbad2,0.1,167) ; Store(S2.SVMc3.IB.E3)
S2.SVMc3.IB.E4<-cv5_SVMc(scal.IB_mat3.E4,K.E4,IsBad.E4$isbad2,0.1,287) ; Store(S2.SVMc3.IB.E4)
S2.SVMc3.IB.E5<-cv5_SVMc(scal.IB_mat3.E5,K.E5,IsBad.E5$isbad2,0.1,232) ; Store(S2.SVMc3.IB.E5)
S2.SVMc3.IB.E6<-cv5_SVMc(scal.IB_mat3.E6,K.E6,IsBad.E6$isbad2,0.1,223) ; Store(S2.SVMc3.IB.E6)
S2.SVMc3.IB.E7<-cv5_SVMc(scal.IB_mat3.E7,K.E7,IsBad.E7$isbad2,0.1,478) ; Store(S2.SVMc3.IB.E7)
S2.SVMc3.IB.E8<-cv5_SVMc(scal.IB_mat3.E8,K.E8,IsBad.E8$isbad2,0.1,238) ; Store(S2.SVMc3.IB.E8)
S2.SVMc3.IB.E9<-cv5_SVMc(scal.IB_mat3.E9,K.E9,IsBad.E9$isbad2,0.1,208) ; Store(S2.SVMc3.IB.E9)
S2.SVMc3.IB.E10<-cv5_SVMc(scal.IB_mat3.E10,K.E10,IsBad.E10$isbad2,0.1,138) ; Store(S2.SVMc3.IB.E10)

S2.SVMc3.IB<-Group(list(S2.SVMc3.IB.E1[[2]],S2.SVMc3.IB.E2[[2]],S2.SVMc3.IB.E3[[2]],S2.SVMc3.IB.E4[[2]],S2.SVMc3.IB.E5[[2]],
                        S2.SVMc3.IB.E6[[2]],S2.SVMc3.IB.E7[[2]],S2.SVMc3.IB.E8[[2]],S2.SVMc3.IB.E9[[2]],S2.SVMc3.IB.E10[[2]])) ; Store(S2.SVMc3.IB)

Spear_score(S2.SVMc3.IB)
graph1b(S2.SVMc3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.SVMc3.2.E1<-cv5_SVMc(scal_mat3.2.E1,K.E1,IsGood.E1$isgood2,0.1,267) ; Store(S2.SVMc3.2.E1)
S2.SVMc3.2.E2<-cv5_SVMc(scal_mat3.2.E2,K.E2,IsGood.E2$isgood2,0.1,207) ; Store(S2.SVMc3.2.E2)
S2.SVMc3.2.E3<-cv5_SVMc(scal_mat3.2.E3,K.E3,IsGood.E3$isgood2,0.1,167) ; Store(S2.SVMc3.2.E3)
S2.SVMc3.2.E4<-cv5_SVMc(scal_mat3.2.E4,K.E4,IsGood.E4$isgood2,0.1,287) ; Store(S2.SVMc3.2.E4)
S2.SVMc3.2.E5<-cv5_SVMc(scal_mat3.2.E5,K.E5,IsGood.E5$isgood2,0.1,232) ; Store(S2.SVMc3.2.E5)
S2.SVMc3.2.E6<-cv5_SVMc(scal_mat3.2.E6,K.E6,IsGood.E6$isgood2,0.1,223) ; Store(S2.SVMc3.2.E6)
S2.SVMc3.2.E7<-cv5_SVMc(scal_mat3.2.E7,K.E7,IsGood.E7$isgood2,0.1,478) ; Store(S2.SVMc3.2.E7)
S2.SVMc3.2.E8<-cv5_SVMc(scal_mat3.2.E8,K.E8,IsGood.E8$isgood2,0.1,238) ; Store(S2.SVMc3.2.E8)
S2.SVMc3.2.E9<-cv5_SVMc(scal_mat3.2.E9,K.E9,IsGood.E9$isgood2,0.1,208) ; Store(S2.SVMc3.2.E9)
S2.SVMc3.2.E10<-cv5_SVMc(scal_mat3.2.E10,K.E10,IsGood.E10$isgood2,0.1,138) ; Store(S2.SVMc3.2.E10)

S2.SVMc3.2<-Group(list(S2.SVMc3.2.E1[[2]],S2.SVMc3.2.E2[[2]],S2.SVMc3.2.E3[[2]],S2.SVMc3.2.E4[[2]],S2.SVMc3.2.E5[[2]],
                       S2.SVMc3.2.E6[[2]],S2.SVMc3.2.E7[[2]],S2.SVMc3.2.E8[[2]],S2.SVMc3.2.E9[[2]],S2.SVMc3.2.E10[[2]])) ; Store(S2.SVMc3.2)

Spear_score(S2.SVMc3.2)
graph1b(S2.SVMc3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.SVMc3b.IB.2.E1<-cv5_SVMc(scal.IB_mat3.2.E1,K.E1,IsBad.E1$isbad2,0.03,267) ; Store(S2.SVMc3b.IB.2.E1)
S2.SVMc3b.IB.2.E2<-cv5_SVMc(scal.IB_mat3.2.E2,K.E2,IsBad.E2$isbad2,0.03,207) ; Store(S2.SVMc3b.IB.2.E2)
S2.SVMc3b.IB.2.E3<-cv5_SVMc(scal.IB_mat3.2.E3,K.E3,IsBad.E3$isbad2,0.03,167) ; Store(S2.SVMc3b.IB.2.E3)
S2.SVMc3b.IB.2.E4<-cv5_SVMc(scal.IB_mat3.2.E4,K.E4,IsBad.E4$isbad2,0.03,287) ; Store(S2.SVMc3b.IB.2.E4)
S2.SVMc3b.IB.2.E5<-cv5_SVMc(scal.IB_mat3.2.E5,K.E5,IsBad.E5$isbad2,0.03,232) ; Store(S2.SVMc3b.IB.2.E5)
S2.SVMc3b.IB.2.E6<-cv5_SVMc(scal.IB_mat3.2.E6,K.E6,IsBad.E6$isbad2,0.03,223) ; Store(S2.SVMc3b.IB.2.E6)
S2.SVMc3b.IB.2.E7<-cv5_SVMc(scal.IB_mat3.2.E7,K.E7,IsBad.E7$isbad2,0.03,478) ; Store(S2.SVMc3b.IB.2.E7)
S2.SVMc3b.IB.2.E8<-cv5_SVMc(scal.IB_mat3.2.E8,K.E8,IsBad.E8$isbad2,0.03,238) ; Store(S2.SVMc3b.IB.2.E8)
S2.SVMc3b.IB.2.E9<-cv5_SVMc(scal.IB_mat3.2.E9,K.E9,IsBad.E9$isbad2,0.03,208) ; Store(S2.SVMc3b.IB.2.E9)
S2.SVMc3b.IB.2.E10<-cv5_SVMc(scal.IB_mat3.2.E10,K.E10,IsBad.E10$isbad2,0.03,138) ; Store(S2.SVMc3b.IB.2.E10)

S2.SVMc3b.IB.2<-Group(list(S2.SVMc3b.IB.2.E1[[2]],S2.SVMc3b.IB.2.E2[[2]],S2.SVMc3b.IB.2.E3[[2]],S2.SVMc3b.IB.2.E4[[2]],S2.SVMc3b.IB.2.E5[[2]],
                           S2.SVMc3b.IB.2.E6[[2]],S2.SVMc3b.IB.2.E7[[2]],S2.SVMc3b.IB.2.E8[[2]],S2.SVMc3b.IB.2.E9[[2]],S2.SVMc3b.IB.2.E10[[2]])) ; Store(S2.SVMc3b.IB.2)

Spear_score(S2.SVMc3b.IB.2)
graph1b(S2.SVMc3b.IB.2)

#####################################################################
# TRAIN SVM to model binary response good/poor essays
# with radial kernel on scaled matrix
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

S2.RADc2.E1<-Cv5_RADc2(scal_mat3.E1,K.E1,IsGood.E1$isgood2,3,0.03,"radial",546) ; Store(S2.RADc2.E1)
S2.RADc2.E2<-Cv5_RADc2(scal_mat3.E2,K.E2,IsGood.E2$isgood2,3,0.03,"radial",546) ; Store(S2.RADc2.E2)
S2.RADc2.E3<-Cv5_RADc2(scal_mat3.E3,K.E3,IsGood.E3$isgood2,3,0.03,"radial",546) ; Store(S2.RADc2.E3)
S2.RADc2.E4<-Cv5_RADc2(scal_mat3.E4,K.E4,IsGood.E4$isgood2,3,0.03,"radial",546) ; Store(S2.RADc2.E4)
S2.RADc2.E5<-Cv5_RADc2(scal_mat3.E5,K.E5,IsGood.E5$isgood2,3,0.03,"radial",546) ; Store(S2.RADc2.E5)
S2.RADc2.E6<-Cv5_RADc2(scal_mat3.E6,K.E6,IsGood.E6$isgood2,3,0.03,"radial",546) ; Store(S2.RADc2.E6)
S2.RADc2.E7<-Cv5_RADc2(scal_mat3.E7,K.E7,IsGood.E7$isgood2,3,0.03,"radial",546) ; Store(S2.RADc2.E7)
S2.RADc2.E8<-Cv5_RADc2(scal_mat3.E8,K.E8,IsGood.E8$isgood2,3,0.03,"radial",546) ; Store(S2.RADc2.E8)
S2.RADc2.E9<-Cv5_RADc2(scal_mat3.E9,K.E9,IsGood.E9$isgood2,3,0.03,"radial",546) ; Store(S2.RADc2.E9)
S2.RADc2.E10<-Cv5_RADc2(scal_mat3.E10,K.E10,IsGood.E10$isgood2,3,0.03,"radial",546) ; Store(S2.RADc2.E10)

S2.RADc2<-Group(list(S2.RADc2.E1[[2]],S2.RADc2.E2[[2]],S2.RADc2.E3[[2]],S2.RADc2.E4[[2]],S2.RADc2.E5[[2]],
                     S2.RADc2.E6[[2]],S2.RADc2.E7[[2]],S2.RADc2.E8[[2]],S2.RADc2.E9[[2]],S2.RADc2.E10[[2]])) ; Store(S2.RADc2)
Spear_score(S2.RADc2)
graph1b(S2.RADc2)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.RADc2.IB.E1<-Cv5_RADc2(scal.IB_mat3.E1,K.E1,IsBad.E1$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.E1)
S2.RADc2.IB.E2<-Cv5_RADc2(scal.IB_mat3.E2,K.E2,IsBad.E2$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.E2)
S2.RADc2.IB.E3<-Cv5_RADc2(scal.IB_mat3.E3,K.E3,IsBad.E3$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.E3)
S2.RADc2.IB.E4<-Cv5_RADc2(scal.IB_mat3.E4,K.E4,IsBad.E4$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.E4)
S2.RADc2.IB.E5<-Cv5_RADc2(scal.IB_mat3.E5,K.E5,IsBad.E5$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.E5)
S2.RADc2.IB.E6<-Cv5_RADc2(scal.IB_mat3.E6,K.E6,IsBad.E6$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.E6)
S2.RADc2.IB.E7<-Cv5_RADc2(scal.IB_mat3.E7,K.E7,IsBad.E7$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.E7)
S2.RADc2.IB.E8<-Cv5_RADc2(scal.IB_mat3.E8,K.E8,IsBad.E8$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.E8)
S2.RADc2.IB.E9<-Cv5_RADc2(scal.IB_mat3.E9,K.E9,IsBad.E9$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.E9)
S2.RADc2.IB.E10<-Cv5_RADc2(scal.IB_mat3.E10,K.E10,IsBad.E10$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.E10)

S2.RADc2.IB<-Group(list(S2.RADc2.IB.E1[[2]],S2.RADc2.IB.E2[[2]],S2.RADc2.IB.E3[[2]],S2.RADc2.IB.E4[[2]],S2.RADc2.IB.E5[[2]],
                        S2.RADc2.IB.E6[[2]],S2.RADc2.IB.E7[[2]],S2.RADc2.IB.E8[[2]],S2.RADc2.IB.E9[[2]],S2.RADc2.IB.E10[[2]])) ; Store(S2.RADc2.IB)
Spear_score(S2.RADc2.IB)
graph1b(S2.RADc2.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.RADc2.2.E1<-Cv5_RADc2(scal_mat3.2.E1,K.E1,IsGood.E1$isgood2,1,0.01,"radial",546) ; Store(S2.RADc2.2.E1)
S2.RADc2.2.E2<-Cv5_RADc2(scal_mat3.2.E2,K.E2,IsGood.E2$isgood2,1,0.01,"radial",546) ; Store(S2.RADc2.2.E2)
S2.RADc2.2.E3<-Cv5_RADc2(scal_mat3.2.E3,K.E3,IsGood.E3$isgood2,1,0.01,"radial",546) ; Store(S2.RADc2.2.E3)
S2.RADc2.2.E4<-Cv5_RADc2(scal_mat3.2.E4,K.E4,IsGood.E4$isgood2,1,0.01,"radial",546) ; Store(S2.RADc2.2.E4)
S2.RADc2.2.E5<-Cv5_RADc2(scal_mat3.2.E5,K.E5,IsGood.E5$isgood2,1,0.01,"radial",546) ; Store(S2.RADc2.2.E5)
S2.RADc2.2.E6<-Cv5_RADc2(scal_mat3.2.E6,K.E6,IsGood.E6$isgood2,1,0.01,"radial",546) ; Store(S2.RADc2.2.E6)
S2.RADc2.2.E7<-Cv5_RADc2(scal_mat3.2.E7,K.E7,IsGood.E7$isgood2,1,0.01,"radial",546) ; Store(S2.RADc2.2.E7)
S2.RADc2.2.E8<-Cv5_RADc2(scal_mat3.2.E8,K.E8,IsGood.E8$isgood2,1,0.01,"radial",546) ; Store(S2.RADc2.2.E8)
S2.RADc2.2.E9<-Cv5_RADc2(scal_mat3.2.E9,K.E9,IsGood.E9$isgood2,1,0.01,"radial",546) ; Store(S2.RADc2.2.E9)
S2.RADc2.2.E10<-Cv5_RADc2(scal_mat3.2.E10,K.E10,IsGood.E10$isgood2,1,0.01,"radial",546) ; Store(S2.RADc2.2.E10)

S2.RADc2.2<-Group(list(S2.RADc2.2.E1[[2]],S2.RADc2.2.E2[[2]],S2.RADc2.2.E3[[2]],S2.RADc2.2.E4[[2]],S2.RADc2.2.E5[[2]],
                       S2.RADc2.2.E6[[2]],S2.RADc2.2.E7[[2]],S2.RADc2.2.E8[[2]],S2.RADc2.2.E9[[2]],S2.RADc2.2.E10[[2]])) ; Store(S2.RADc2.2)
Spear_score(S2.RADc2.2)
graph1b(S2.RADc2.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.RADc2.IB.2.E1<-Cv5_RADc2(scal.IB_mat3.2.E1,K.E1,IsBad.E1$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.2.E1)
S2.RADc2.IB.2.E2<-Cv5_RADc2(scal.IB_mat3.2.E2,K.E2,IsBad.E2$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.2.E2)
S2.RADc2.IB.2.E3<-Cv5_RADc2(scal.IB_mat3.2.E3,K.E3,IsBad.E3$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.2.E3)
S2.RADc2.IB.2.E4<-Cv5_RADc2(scal.IB_mat3.2.E4,K.E4,IsBad.E4$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.2.E4)
S2.RADc2.IB.2.E5<-Cv5_RADc2(scal.IB_mat3.2.E5,K.E5,IsBad.E5$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.2.E5)
S2.RADc2.IB.2.E6<-Cv5_RADc2(scal.IB_mat3.2.E6,K.E6,IsBad.E6$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.2.E6)
S2.RADc2.IB.2.E7<-Cv5_RADc2(scal.IB_mat3.2.E7,K.E7,IsBad.E7$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.2.E7)
S2.RADc2.IB.2.E8<-Cv5_RADc2(scal.IB_mat3.2.E8,K.E8,IsBad.E8$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.2.E8)
S2.RADc2.IB.2.E9<-Cv5_RADc2(scal.IB_mat3.2.E9,K.E9,IsBad.E9$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.2.E9)
S2.RADc2.IB.2.E10<-Cv5_RADc2(scal.IB_mat3.2.E10,K.E10,IsBad.E10$isbad2,3,0.03,"radial",546) ; Store(S2.RADc2.IB.2.E10)

S2.RADc2.IB.2<-Group(list(S2.RADc2.IB.2.E1[[2]],S2.RADc2.IB.2.E2[[2]],S2.RADc2.IB.2.E3[[2]],S2.RADc2.IB.2.E4[[2]],S2.RADc2.IB.2.E5[[2]],
                          S2.RADc2.IB.2.E6[[2]],S2.RADc2.IB.2.E7[[2]],S2.RADc2.IB.2.E8[[2]],S2.RADc2.IB.2.E9[[2]],S2.RADc2.IB.2.E10[[2]])) ; Store(S2.RADc2.IB.2)
Spear_score(S2.RADc2.IB.2)
graph1b(S2.RADc2.IB.2)

#####################################################################
# TRAIN Multinomial SVM
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

S2.SVMm3.E1<-cv5_SVMm(scal_mat3.E1,K.E1,training.E1$Score2,0.1,207) ; Store(S2.SVMm3.E1)
S2.SVMm3.E2<-cv5_SVMm(scal_mat3.E2,K.E2,training.E2$Score2,0.1,200) ; Store(S2.SVMm3.E2)
S2.SVMm3.E3<-cv5_SVMm(scal_mat3.E3,K.E3,training.E3$Score2,0.1,168) ; Store(S2.SVMm3.E3)
S2.SVMm3.E4<-cv5_SVMm(scal_mat3.E4,K.E4,training.E4$Score2,0.1,288) ; Store(S2.SVMm3.E4)
S2.SVMm3.E5<-cv5_SVMm(scal_mat3.E5,K.E5,training.E5$Score2,0.1,235) ; Store(S2.SVMm3.E5)
S2.SVMm3.E6<-cv5_SVMm(scal_mat3.E6,K.E6,training.E6$Score2,0.1,258) ; Store(S2.SVMm3.E6)
S2.SVMm3.E7<-cv5_SVMm(scal_mat3.E7,K.E7,training.E7$Score2,0.1,431) ; Store(S2.SVMm3.E7)
S2.SVMm3.E8<-cv5_SVMm(scal_mat3.E8,K.E8,training.E8$Score2,0.1,212) ; Store(S2.SVMm3.E8)
S2.SVMm3.E9<-cv5_SVMm(scal_mat3.E9,K.E9,training.E9$Score2,0.1,812) ; Store(S2.SVMm3.E9)
S2.SVMm3.E10<-cv5_SVMm(scal_mat3.E10,K.E10,training.E10$Score2,0.1,210) ; Store(S2.SVMm3.E10)

S2.SVMm3.E1[[2]]<-as.numeric(gsub("S","",S2.SVMm3.E1[[2]])) ; Store(S2.SVMm3.E1)
S2.SVMm3.E2[[2]]<-as.numeric(gsub("S","",S2.SVMm3.E2[[2]])) ; Store(S2.SVMm3.E2)
S2.SVMm3.E3[[2]]<-as.numeric(gsub("S","",S2.SVMm3.E3[[2]])) ; Store(S2.SVMm3.E3)
S2.SVMm3.E4[[2]]<-as.numeric(gsub("S","",S2.SVMm3.E4[[2]])) ; Store(S2.SVMm3.E4)
S2.SVMm3.E5[[2]]<-as.numeric(gsub("S","",S2.SVMm3.E5[[2]])) ; Store(S2.SVMm3.E5)
S2.SVMm3.E6[[2]]<-as.numeric(gsub("S","",S2.SVMm3.E6[[2]])) ; Store(S2.SVMm3.E6)
S2.SVMm3.E7[[2]]<-as.numeric(gsub("S","",S2.SVMm3.E7[[2]])) ; Store(S2.SVMm3.E7)
S2.SVMm3.E8[[2]]<-as.numeric(gsub("S","",S2.SVMm3.E8[[2]])) ; Store(S2.SVMm3.E8)
S2.SVMm3.E9[[2]]<-as.numeric(gsub("S","",S2.SVMm3.E9[[2]])) ; Store(S2.SVMm3.E9)
S2.SVMm3.E10[[2]]<-as.numeric(gsub("S","",S2.SVMm3.E10[[2]])) ; Store(S2.SVMm3.E10)

S2.SVMm3<-Group(list(S2.SVMm3.E1[[2]],S2.SVMm3.E2[[2]],S2.SVMm3.E3[[2]],S2.SVMm3.E4[[2]],S2.SVMm3.E5[[2]],
                     S2.SVMm3.E6[[2]],S2.SVMm3.E7[[2]],S2.SVMm3.E8[[2]],S2.SVMm3.E9[[2]],S2.SVMm3.E10[[2]])) ; Store(S2.SVMm3)

Spear_score(S2.SVMm3)
graph1b(S2.SVMm3)
Kappa_score(S2.SVMm3)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.SVMm3.IB.E1<-cv5_SVMm(scal.IB_mat3.E1,K.E1,training.E1$Score2,0.1,207) ; Store(S2.SVMm3.IB.E1)
S2.SVMm3.IB.E2<-cv5_SVMm(scal.IB_mat3.E2,K.E2,training.E2$Score2,0.1,200) ; Store(S2.SVMm3.IB.E2)
S2.SVMm3.IB.E3<-cv5_SVMm(scal.IB_mat3.E3,K.E3,training.E3$Score2,0.1,168) ; Store(S2.SVMm3.IB.E3)
S2.SVMm3.IB.E4<-cv5_SVMm(scal.IB_mat3.E4,K.E4,training.E4$Score2,0.1,288) ; Store(S2.SVMm3.IB.E4)
S2.SVMm3.IB.E5<-cv5_SVMm(scal.IB_mat3.E5,K.E5,training.E5$Score2,0.1,235) ; Store(S2.SVMm3.IB.E5)
S2.SVMm3.IB.E6<-cv5_SVMm(scal.IB_mat3.E6,K.E6,training.E6$Score2,0.1,258) ; Store(S2.SVMm3.IB.E6)
S2.SVMm3.IB.E7<-cv5_SVMm(scal.IB_mat3.E7,K.E7,training.E7$Score2,0.1,431) ; Store(S2.SVMm3.IB.E7)
S2.SVMm3.IB.E8<-cv5_SVMm(scal.IB_mat3.E8,K.E8,training.E8$Score2,0.1,212) ; Store(S2.SVMm3.IB.E8)
S2.SVMm3.IB.E9<-cv5_SVMm(scal.IB_mat3.E9,K.E9,training.E9$Score2,0.1,812) ; Store(S2.SVMm3.IB.E9)
S2.SVMm3.IB.E10<-cv5_SVMm(scal.IB_mat3.E10,K.E10,training.E10$Score2,0.1,210) ; Store(S2.SVMm3.IB.E10)

S2.SVMm3.IB.E1[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.E1[[2]])) ; Store(S2.SVMm3.IB.E1)
S2.SVMm3.IB.E2[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.E2[[2]])) ; Store(S2.SVMm3.IB.E2)
S2.SVMm3.IB.E3[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.E3[[2]])) ; Store(S2.SVMm3.IB.E3)
S2.SVMm3.IB.E4[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.E4[[2]])) ; Store(S2.SVMm3.IB.E4)
S2.SVMm3.IB.E5[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.E5[[2]])) ; Store(S2.SVMm3.IB.E5)
S2.SVMm3.IB.E6[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.E6[[2]])) ; Store(S2.SVMm3.IB.E6)
S2.SVMm3.IB.E7[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.E7[[2]])) ; Store(S2.SVMm3.IB.E7)
S2.SVMm3.IB.E8[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.E8[[2]])) ; Store(S2.SVMm3.IB.E8)
S2.SVMm3.IB.E9[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.E9[[2]])) ; Store(S2.SVMm3.IB.E9)
S2.SVMm3.IB.E10[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.E10[[2]])) ; Store(S2.SVMm3.IB.E10)

S2.SVMm3.IB<-Group(list(S2.SVMm3.IB.E1[[2]],S2.SVMm3.IB.E2[[2]],S2.SVMm3.IB.E3[[2]],S2.SVMm3.IB.E4[[2]],S2.SVMm3.IB.E5[[2]],
                        S2.SVMm3.IB.E6[[2]],S2.SVMm3.IB.E7[[2]],S2.SVMm3.IB.E8[[2]],S2.SVMm3.IB.E9[[2]],S2.SVMm3.IB.E10[[2]])) ; Store(S2.SVMm3.IB)

Spear_score(S2.SVMm3.IB)
graph1b(S2.SVMm3.IB)
Kappa_score(S2.SVMm3.IB)


# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.SVMm3.2.E1<-cv5_SVMm(scal_mat3.2.E1,K.E1,training.E1$Score2,0.1,207) ; Store(S2.SVMm3.2.E1)
S2.SVMm3.2.E2<-cv5_SVMm(scal_mat3.2.E2,K.E2,training.E2$Score2,0.1,200) ; Store(S2.SVMm3.2.E2)
S2.SVMm3.2.E3<-cv5_SVMm(scal_mat3.2.E3,K.E3,training.E3$Score2,0.1,168) ; Store(S2.SVMm3.2.E3)
S2.SVMm3.2.E4<-cv5_SVMm(scal_mat3.2.E4,K.E4,training.E4$Score2,0.1,288) ; Store(S2.SVMm3.2.E4)
S2.SVMm3.2.E5<-cv5_SVMm(scal_mat3.2.E5,K.E5,training.E5$Score2,0.1,235) ; Store(S2.SVMm3.2.E5)
S2.SVMm3.2.E6<-cv5_SVMm(scal_mat3.2.E6,K.E6,training.E6$Score2,0.1,258) ; Store(S2.SVMm3.2.E6)
S2.SVMm3.2.E7<-cv5_SVMm(scal_mat3.2.E7,K.E7,training.E7$Score2,0.1,431) ; Store(S2.SVMm3.2.E7)
S2.SVMm3.2.E8<-cv5_SVMm(scal_mat3.2.E8,K.E8,training.E8$Score2,0.1,212) ; Store(S2.SVMm3.2.E8)
S2.SVMm3.2.E9<-cv5_SVMm(scal_mat3.2.E9,K.E9,training.E9$Score2,0.1,812) ; Store(S2.SVMm3.2.E9)
S2.SVMm3.2.E10<-cv5_SVMm(scal_mat3.2.E10,K.E10,training.E10$Score2,0.1,210) ; Store(S2.SVMm3.2.E10)

S2.SVMm3.2.E1[[2]]<-as.numeric(gsub("S","",S2.SVMm3.2.E1[[2]])) ; Store(S2.SVMm3.2.E1)
S2.SVMm3.2.E2[[2]]<-as.numeric(gsub("S","",S2.SVMm3.2.E2[[2]])) ; Store(S2.SVMm3.2.E2)
S2.SVMm3.2.E3[[2]]<-as.numeric(gsub("S","",S2.SVMm3.2.E3[[2]])) ; Store(S2.SVMm3.2.E3)
S2.SVMm3.2.E4[[2]]<-as.numeric(gsub("S","",S2.SVMm3.2.E4[[2]])) ; Store(S2.SVMm3.2.E4)
S2.SVMm3.2.E5[[2]]<-as.numeric(gsub("S","",S2.SVMm3.2.E5[[2]])) ; Store(S2.SVMm3.2.E5)
S2.SVMm3.2.E6[[2]]<-as.numeric(gsub("S","",S2.SVMm3.2.E6[[2]])) ; Store(S2.SVMm3.2.E6)
S2.SVMm3.2.E7[[2]]<-as.numeric(gsub("S","",S2.SVMm3.2.E7[[2]])) ; Store(S2.SVMm3.2.E7)
S2.SVMm3.2.E8[[2]]<-as.numeric(gsub("S","",S2.SVMm3.2.E8[[2]])) ; Store(S2.SVMm3.2.E8)
S2.SVMm3.2.E9[[2]]<-as.numeric(gsub("S","",S2.SVMm3.2.E9[[2]])) ; Store(S2.SVMm3.2.E9)
S2.SVMm3.2.E10[[2]]<-as.numeric(gsub("S","",S2.SVMm3.2.E10[[2]])) ; Store(S2.SVMm3.2.E10)

S2.SVMm3.2<-Group(list(S2.SVMm3.2.E1[[2]],S2.SVMm3.2.E2[[2]],S2.SVMm3.2.E3[[2]],S2.SVMm3.2.E4[[2]],S2.SVMm3.2.E5[[2]],
                       S2.SVMm3.2.E6[[2]],S2.SVMm3.2.E7[[2]],S2.SVMm3.2.E8[[2]],S2.SVMm3.2.E9[[2]],S2.SVMm3.2.E10[[2]])) ; Store(S2.SVMm3.2)

Spear_score(S2.SVMm3.2)
graph1b(S2.SVMm3.2)
Kappa_score(S2.SVMm3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.SVMm3.IB.2.E1<-cv5_SVMm(scal.IB_mat3.2.E1,K.E1,training.E1$Score2,0.1,207) ; Store(S2.SVMm3.IB.2.E1)
S2.SVMm3.IB.2.E2<-cv5_SVMm(scal.IB_mat3.2.E2,K.E2,training.E2$Score2,0.1,200) ; Store(S2.SVMm3.IB.2.E2)
S2.SVMm3.IB.2.E3<-cv5_SVMm(scal.IB_mat3.2.E3,K.E3,training.E3$Score2,0.1,168) ; Store(S2.SVMm3.IB.2.E3)
S2.SVMm3.IB.2.E4<-cv5_SVMm(scal.IB_mat3.2.E4,K.E4,training.E4$Score2,0.1,288) ; Store(S2.SVMm3.IB.2.E4)
S2.SVMm3.IB.2.E5<-cv5_SVMm(scal.IB_mat3.2.E5,K.E5,training.E5$Score2,0.1,235) ; Store(S2.SVMm3.IB.2.E5)
S2.SVMm3.IB.2.E6<-cv5_SVMm(scal.IB_mat3.2.E6,K.E6,training.E6$Score2,0.1,258) ; Store(S2.SVMm3.IB.2.E6)
S2.SVMm3.IB.2.E7<-cv5_SVMm(scal.IB_mat3.2.E7,K.E7,training.E7$Score2,0.1,431) ; Store(S2.SVMm3.IB.2.E7)
S2.SVMm3.IB.2.E8<-cv5_SVMm(scal.IB_mat3.2.E8,K.E8,training.E8$Score2,0.1,212) ; Store(S2.SVMm3.IB.2.E8)
S2.SVMm3.IB.2.E9<-cv5_SVMm(scal.IB_mat3.2.E9,K.E9,training.E9$Score2,0.1,812) ; Store(S2.SVMm3.IB.2.E9)
S2.SVMm3.IB.2.E10<-cv5_SVMm(scal.IB_mat3.2.E10,K.E10,training.E10$Score2,0.1,210) ; Store(S2.SVMm3.IB.2.E10)

S2.SVMm3.IB.2.E1[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.2.E1[[2]])) ; Store(S2.SVMm3.IB.2.E1)
S2.SVMm3.IB.2.E2[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.2.E2[[2]])) ; Store(S2.SVMm3.IB.2.E2)
S2.SVMm3.IB.2.E3[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.2.E3[[2]])) ; Store(S2.SVMm3.IB.2.E3)
S2.SVMm3.IB.2.E4[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.2.E4[[2]])) ; Store(S2.SVMm3.IB.2.E4)
S2.SVMm3.IB.2.E5[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.2.E5[[2]])) ; Store(S2.SVMm3.IB.2.E5)
S2.SVMm3.IB.2.E6[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.2.E6[[2]])) ; Store(S2.SVMm3.IB.2.E6)
S2.SVMm3.IB.2.E7[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.2.E7[[2]])) ; Store(S2.SVMm3.IB.2.E7)
S2.SVMm3.IB.2.E8[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.2.E8[[2]])) ; Store(S2.SVMm3.IB.2.E8)
S2.SVMm3.IB.2.E9[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.2.E9[[2]])) ; Store(S2.SVMm3.IB.2.E9)
S2.SVMm3.IB.2.E10[[2]]<-as.numeric(gsub("S","",S2.SVMm3.IB.2.E10[[2]])) ; Store(S2.SVMm3.IB.2.E10)

S2.SVMm3.IB.2<-Group(list(S2.SVMm3.IB.2.E1[[2]],S2.SVMm3.IB.2.E2[[2]],S2.SVMm3.IB.2.E3[[2]],S2.SVMm3.IB.2.E4[[2]],S2.SVMm3.IB.2.E5[[2]],
                          S2.SVMm3.IB.2.E6[[2]],S2.SVMm3.IB.2.E7[[2]],S2.SVMm3.IB.2.E8[[2]],S2.SVMm3.IB.2.E9[[2]],S2.SVMm3.IB.2.E10[[2]])) ; Store(S2.SVMm3.IB.2)

Spear_score(S2.SVMm3.IB.2)
graph1b(S2.SVMm3.IB.2)
Kappa_score(S2.SVMm3.IB.2)

#####################################################################
# TRAIN SVM multinomial
# with radial kernel on scaled matrix
#####################################################################

library(e1071)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

S2.RADm2.E1<-Cv5_RADm2(scal_mat3.E1,K.E1,training.E1$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.E1)
S2.RADm2.E2<-Cv5_RADm2(scal_mat3.E2,K.E2,training.E2$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.E2)
S2.RADm2.E3<-Cv5_RADm2(scal_mat3.E3,K.E3,training.E3$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.E3)
S2.RADm2.E4<-Cv5_RADm2(scal_mat3.E4,K.E4,training.E4$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.E4)
S2.RADm2.E5<-Cv5_RADm2(scal_mat3.E5,K.E5,training.E5$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.E5)
S2.RADm2.E6<-Cv5_RADm2(scal_mat3.E6,K.E6,training.E6$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.E6)
S2.RADm2.E7<-Cv5_RADm2(scal_mat3.E7,K.E7,training.E7$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.E7)
S2.RADm2.E8<-Cv5_RADm2(scal_mat3.E8,K.E8,training.E8$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.E8)
S2.RADm2.E9<-Cv5_RADm2(scal_mat3.E9,K.E9,training.E9$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.E9)
S2.RADm2.E10<-Cv5_RADm2(scal_mat3.E10,K.E10,training.E10$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.E10)

S2.RADm2.E1[[2]]<-as.numeric(gsub("S","",S2.RADm2.E1[[2]])) ; Store(S2.RADm2.E1)
S2.RADm2.E2[[2]]<-as.numeric(gsub("S","",S2.RADm2.E2[[2]])) ; Store(S2.RADm2.E2)
S2.RADm2.E3[[2]]<-as.numeric(gsub("S","",S2.RADm2.E3[[2]])) ; Store(S2.RADm2.E3)
S2.RADm2.E4[[2]]<-as.numeric(gsub("S","",S2.RADm2.E4[[2]])) ; Store(S2.RADm2.E4)
S2.RADm2.E5[[2]]<-as.numeric(gsub("S","",S2.RADm2.E5[[2]])) ; Store(S2.RADm2.E5)
S2.RADm2.E6[[2]]<-as.numeric(gsub("S","",S2.RADm2.E6[[2]])) ; Store(S2.RADm2.E6)
S2.RADm2.E7[[2]]<-as.numeric(gsub("S","",S2.RADm2.E7[[2]])) ; Store(S2.RADm2.E7)
S2.RADm2.E8[[2]]<-as.numeric(gsub("S","",S2.RADm2.E8[[2]])) ; Store(S2.RADm2.E8)
S2.RADm2.E9[[2]]<-as.numeric(gsub("S","",S2.RADm2.E9[[2]])) ; Store(S2.RADm2.E9)
S2.RADm2.E10[[2]]<-as.numeric(gsub("S","",S2.RADm2.E10[[2]])) ; Store(S2.RADm2.E10)

S2.RADm2<-Group(list(S2.RADm2.E1[[2]],S2.RADm2.E2[[2]],S2.RADm2.E3[[2]],S2.RADm2.E4[[2]],S2.RADm2.E5[[2]],
                     S2.RADm2.E6[[2]],S2.RADm2.E7[[2]],S2.RADm2.E8[[2]],S2.RADm2.E9[[2]],S2.RADm2.E10[[2]])) ; Store(S2.RADm2)
Spear_score(S2.RADm2)
graph1b(S2.RADm2)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.RADm2.IB.E1<-Cv5_RADm2(scal.IB_mat3.E1,K.E1,training.E1$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.E1)
S2.RADm2.IB.E2<-Cv5_RADm2(scal.IB_mat3.E2,K.E2,training.E2$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.E2)
S2.RADm2.IB.E3<-Cv5_RADm2(scal.IB_mat3.E3,K.E3,training.E3$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.E3)
S2.RADm2.IB.E4<-Cv5_RADm2(scal.IB_mat3.E4,K.E4,training.E4$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.E4)
S2.RADm2.IB.E5<-Cv5_RADm2(scal.IB_mat3.E5,K.E5,training.E5$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.E5)
S2.RADm2.IB.E6<-Cv5_RADm2(scal.IB_mat3.E6,K.E6,training.E6$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.E6)
S2.RADm2.IB.E7<-Cv5_RADm2(scal.IB_mat3.E7,K.E7,training.E7$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.E7)
S2.RADm2.IB.E8<-Cv5_RADm2(scal.IB_mat3.E8,K.E8,training.E8$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.E8)
S2.RADm2.IB.E9<-Cv5_RADm2(scal.IB_mat3.E9,K.E9,training.E9$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.E9)
S2.RADm2.IB.E10<-Cv5_RADm2(scal.IB_mat3.E10,K.E10,training.E10$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.E10)

S2.RADm2.IB.E1[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.E1[[2]])) ; Store(S2.RADm2.IB.E1)
S2.RADm2.IB.E2[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.E2[[2]])) ; Store(S2.RADm2.IB.E2)
S2.RADm2.IB.E3[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.E3[[2]])) ; Store(S2.RADm2.IB.E3)
S2.RADm2.IB.E4[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.E4[[2]])) ; Store(S2.RADm2.IB.E4)
S2.RADm2.IB.E5[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.E5[[2]])) ; Store(S2.RADm2.IB.E5)
S2.RADm2.IB.E6[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.E6[[2]])) ; Store(S2.RADm2.IB.E6)
S2.RADm2.IB.E7[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.E7[[2]])) ; Store(S2.RADm2.IB.E7)
S2.RADm2.IB.E8[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.E8[[2]])) ; Store(S2.RADm2.IB.E8)
S2.RADm2.IB.E9[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.E9[[2]])) ; Store(S2.RADm2.IB.E9)
S2.RADm2.IB.E10[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.E10[[2]])) ; Store(S2.RADm2.IB.E10)

S2.RADm2.IB<-Group(list(S2.RADm2.IB.E1[[2]],S2.RADm2.IB.E2[[2]],S2.RADm2.IB.E3[[2]],S2.RADm2.IB.E4[[2]],S2.RADm2.IB.E5[[2]],
                        S2.RADm2.IB.E6[[2]],S2.RADm2.IB.E7[[2]],S2.RADm2.IB.E8[[2]],S2.RADm2.IB.E9[[2]],S2.RADm2.IB.E10[[2]])) ; Store(S2.RADm2.IB)
Spear_score(S2.RADm2.IB)
graph1b(S2.RADm2.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.RADm2.2.E1<-Cv5_RADm2(scal_mat3.2.E1,K.E1,training.E1$Score2,3,0.01,"radial",469) ; Store(S2.RADm2.2.E1)
S2.RADm2.2.E2<-Cv5_RADm2(scal_mat3.2.E2,K.E2,training.E2$Score2,3,0.01,"radial",469) ; Store(S2.RADm2.2.E2)
S2.RADm2.2.E3<-Cv5_RADm2(scal_mat3.2.E3,K.E3,training.E3$Score2,3,0.01,"radial",469) ; Store(S2.RADm2.2.E3)
S2.RADm2.2.E4<-Cv5_RADm2(scal_mat3.2.E4,K.E4,training.E4$Score2,3,0.01,"radial",469) ; Store(S2.RADm2.2.E4)
S2.RADm2.2.E5<-Cv5_RADm2(scal_mat3.2.E5,K.E5,training.E5$Score2,3,0.01,"radial",469) ; Store(S2.RADm2.2.E5)
S2.RADm2.2.E6<-Cv5_RADm2(scal_mat3.2.E6,K.E6,training.E6$Score2,3,0.01,"radial",469) ; Store(S2.RADm2.2.E6)
S2.RADm2.2.E7<-Cv5_RADm2(scal_mat3.2.E7,K.E7,training.E7$Score2,3,0.01,"radial",469) ; Store(S2.RADm2.2.E7)
S2.RADm2.2.E8<-Cv5_RADm2(scal_mat3.2.E8,K.E8,training.E8$Score2,3,0.01,"radial",469) ; Store(S2.RADm2.2.E8)
S2.RADm2.2.E9<-Cv5_RADm2(scal_mat3.2.E9,K.E9,training.E9$Score2,3,0.01,"radial",469) ; Store(S2.RADm2.2.E9)
S2.RADm2.2.E10<-Cv5_RADm2(scal_mat3.2.E10,K.E10,training.E10$Score2,3,0.01,"radial",469) ; Store(S2.RADm2.2.E10)

S2.RADm2.2.E1[[2]]<-as.numeric(gsub("S","",S2.RADm2.2.E1[[2]])) ; Store(S2.RADm2.2.E1)
S2.RADm2.2.E2[[2]]<-as.numeric(gsub("S","",S2.RADm2.2.E2[[2]])) ; Store(S2.RADm2.2.E2)
S2.RADm2.2.E3[[2]]<-as.numeric(gsub("S","",S2.RADm2.2.E3[[2]])) ; Store(S2.RADm2.2.E3)
S2.RADm2.2.E4[[2]]<-as.numeric(gsub("S","",S2.RADm2.2.E4[[2]])) ; Store(S2.RADm2.2.E4)
S2.RADm2.2.E5[[2]]<-as.numeric(gsub("S","",S2.RADm2.2.E5[[2]])) ; Store(S2.RADm2.2.E5)
S2.RADm2.2.E6[[2]]<-as.numeric(gsub("S","",S2.RADm2.2.E6[[2]])) ; Store(S2.RADm2.2.E6)
S2.RADm2.2.E7[[2]]<-as.numeric(gsub("S","",S2.RADm2.2.E7[[2]])) ; Store(S2.RADm2.2.E7)
S2.RADm2.2.E8[[2]]<-as.numeric(gsub("S","",S2.RADm2.2.E8[[2]])) ; Store(S2.RADm2.2.E8)
S2.RADm2.2.E9[[2]]<-as.numeric(gsub("S","",S2.RADm2.2.E9[[2]])) ; Store(S2.RADm2.2.E9)
S2.RADm2.2.E10[[2]]<-as.numeric(gsub("S","",S2.RADm2.2.E10[[2]])) ; Store(S2.RADm2.2.E10)

S2.RADm2.2<-Group(list(S2.RADm2.2.E1[[2]],S2.RADm2.2.E2[[2]],S2.RADm2.2.E3[[2]],S2.RADm2.2.E4[[2]],S2.RADm2.2.E5[[2]],
                       S2.RADm2.2.E6[[2]],S2.RADm2.2.E7[[2]],S2.RADm2.2.E8[[2]],S2.RADm2.2.E9[[2]],S2.RADm2.2.E10[[2]])) ; Store(S2.RADm2.2)
Spear_score(S2.RADm2.2)
graph1b(S2.RADm2.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.RADm2.IB.2.E1<-Cv5_RADm2(scal.IB_mat3.2.E1,K.E1,training.E1$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.2.E1)
S2.RADm2.IB.2.E2<-Cv5_RADm2(scal.IB_mat3.2.E2,K.E2,training.E2$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.2.E2)
S2.RADm2.IB.2.E3<-Cv5_RADm2(scal.IB_mat3.2.E3,K.E3,training.E3$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.2.E3)
S2.RADm2.IB.2.E4<-Cv5_RADm2(scal.IB_mat3.2.E4,K.E4,training.E4$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.2.E4)
S2.RADm2.IB.2.E5<-Cv5_RADm2(scal.IB_mat3.2.E5,K.E5,training.E5$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.2.E5)
S2.RADm2.IB.2.E6<-Cv5_RADm2(scal.IB_mat3.2.E6,K.E6,training.E6$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.2.E6)
S2.RADm2.IB.2.E7<-Cv5_RADm2(scal.IB_mat3.2.E7,K.E7,training.E7$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.2.E7)
S2.RADm2.IB.2.E8<-Cv5_RADm2(scal.IB_mat3.2.E8,K.E8,training.E8$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.2.E8)
S2.RADm2.IB.2.E9<-Cv5_RADm2(scal.IB_mat3.2.E9,K.E9,training.E9$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.2.E9)
S2.RADm2.IB.2.E10<-Cv5_RADm2(scal.IB_mat3.2.E10,K.E10,training.E10$Score2,3,0.03,"radial",469) ; Store(S2.RADm2.IB.2.E10)

S2.RADm2.IB.2.E1[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.2.E1[[2]])) ; Store(S2.RADm2.IB.2.E1)
S2.RADm2.IB.2.E2[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.2.E2[[2]])) ; Store(S2.RADm2.IB.2.E2)
S2.RADm2.IB.2.E3[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.2.E3[[2]])) ; Store(S2.RADm2.IB.2.E3)
S2.RADm2.IB.2.E4[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.2.E4[[2]])) ; Store(S2.RADm2.IB.2.E4)
S2.RADm2.IB.2.E5[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.2.E5[[2]])) ; Store(S2.RADm2.IB.2.E5)
S2.RADm2.IB.2.E6[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.2.E6[[2]])) ; Store(S2.RADm2.IB.2.E6)
S2.RADm2.IB.2.E7[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.2.E7[[2]])) ; Store(S2.RADm2.IB.2.E7)
S2.RADm2.IB.2.E8[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.2.E8[[2]])) ; Store(S2.RADm2.IB.2.E8)
S2.RADm2.IB.2.E9[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.2.E9[[2]])) ; Store(S2.RADm2.IB.2.E9)
S2.RADm2.IB.2.E10[[2]]<-as.numeric(gsub("S","",S2.RADm2.IB.2.E10[[2]])) ; Store(S2.RADm2.IB.2.E10)

S2.RADm2.IB.2<-Group(list(S2.RADm2.IB.2.E1[[2]],S2.RADm2.IB.2.E2[[2]],S2.RADm2.IB.2.E3[[2]],S2.RADm2.IB.2.E4[[2]],S2.RADm2.IB.2.E5[[2]],
                          S2.RADm2.IB.2.E6[[2]],S2.RADm2.IB.2.E7[[2]],S2.RADm2.IB.2.E8[[2]],S2.RADm2.IB.2.E9[[2]],S2.RADm2.IB.2.E10[[2]])) ; Store(S2.RADm2.IB.2)
Spear_score(S2.RADm2.IB.2)
graph1b(S2.RADm2.IB.2)

#####################################################################
#####################################################################
# TRAIN SVM with radial kernel on ridit scores
#####################################################################
#####################################################################

# 1rst set of bin_mat (transformed with ridit coding) 
#####################################################################

library(e1071)
S2.RAD.E1<-Cv5_RAD(Ridit_mat.E1,metrics.E1,0.25,K.E1,training.E1$Score2,2,0.01,"radial",892) ; Store(S2.RAD.E1)
S2.RAD.E2<-Cv5_RAD(Ridit_mat.E2,metrics.E2,0.25,K.E2,training.E2$Score2,2,0.01,"radial",892) ; Store(S2.RAD.E2)
S2.RAD.E3<-Cv5_RAD(Ridit_mat.E3,metrics.E3,0.25,K.E3,training.E3$Score2,2,0.01,"radial",892) ; Store(S2.RAD.E3)
S2.RAD.E4<-Cv5_RAD(Ridit_mat.E4,metrics.E4,0.4,K.E4,training.E4$Score2,2,0.01,"radial",892) ; Store(S2.RAD.E4)
S2.RAD.E5<-Cv5_RAD(Ridit_mat.E5,metrics.E5,0.25,K.E5,training.E5$Score2,2,0.01,"radial",892) ; Store(S2.RAD.E5)
S2.RAD.E6<-Cv5_RAD(Ridit_mat.E6,metrics.E6,0.25,K.E6,training.E6$Score2,2,0.01,"radial",892) ; Store(S2.RAD.E6)
S2.RAD.E7<-Cv5_RAD(Ridit_mat.E7,metrics.E7,0.25,K.E7,training.E7$Score2,2,0.01,"radial",892) ; Store(S2.RAD.E7)
S2.RAD.E8<-Cv5_RAD(Ridit_mat.E8,metrics.E8,0.8,K.E8,training.E8$Score2,2,0.01,"radial",892) ; Store(S2.RAD.E8)
S2.RAD.E9<-Cv5_RAD(Ridit_mat.E9,metrics.E9,0.3,K.E9,training.E9$Score2,2,0.01,"radial",892) ; Store(S2.RAD.E9)
S2.RAD.E10<-Cv5_RAD(Ridit_mat.E10,metrics.E10,0.25,K.E10,training.E10$Score2,2,0.01,"radial",892) ; Store(S2.RAD.E10)

S2.RAD<-Group(list(S2.RAD.E1[[2]],S2.RAD.E2[[2]],S2.RAD.E3[[2]],S2.RAD.E4[[2]],S2.RAD.E5[[2]],
                   S2.RAD.E6[[2]],S2.RAD.E7[[2]],S2.RAD.E8[[2]],S2.RAD.E9[[2]],S2.RAD.E10[[2]])) ; Store(S2.RAD)

Spear_score(S2.RAD)
graph1b(S2.RAD)
Kappa_score(S2.RAD)

#####################################################################
#####################################################################
# TRAIN Glmnet on scaled matrix
# scaled with delta tfidf and bns
# using isgood and isbad
# regression and multinomial
#####################################################################
#####################################################################

#####################################################################
# TRAIN Glmnet on scaled matrix with delta tfidf
#####################################################################

library(glmnet)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

S2.GNETns1.E1<-cv5_GNET2(scal_mat1.E1,K.E1,training.E1$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.E1)
S2.GNETns1.E2<-cv5_GNET2(scal_mat1.E2,K.E2,training.E2$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.E2)
S2.GNETns1.E3<-cv5_GNET2(scal_mat1.E3,K.E3,training.E3$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.E3)
S2.GNETns1.E4<-cv5_GNET2(scal_mat1.E4,K.E4,training.E4$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.E4)
S2.GNETns1.E5<-cv5_GNET2(scal_mat1.E5,K.E5,training.E5$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.E5)
S2.GNETns1.E6<-cv5_GNET2(scal_mat1.E6,K.E6,training.E6$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.E6)
S2.GNETns1.E7<-cv5_GNET2(scal_mat1.E7,K.E7,training.E7$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.E7)
S2.GNETns1.E8<-cv5_GNET2(scal_mat1.E8,K.E8,training.E8$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.E8)
S2.GNETns1.E9<-cv5_GNET2(scal_mat1.E9,K.E9,training.E9$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.E9)
S2.GNETns1.E10<-cv5_GNET2(scal_mat1.E10,K.E10,training.E10$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.E10)

S2.GNETns1<-Group(list(S2.GNETns1.E1[[2]],S2.GNETns1.E2[[2]],S2.GNETns1.E3[[2]],S2.GNETns1.E4[[2]],S2.GNETns1.E5[[2]],
                       S2.GNETns1.E6[[2]],S2.GNETns1.E7[[2]],S2.GNETns1.E8[[2]],S2.GNETns1.E9[[2]],S2.GNETns1.E10[[2]])) ; Store(S2.GNETns1)

Spear_score(S2.GNETns1)
graph1b(S2.GNETns1)
Kappa_score(S2.GNETns1)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.GNETns1.IB.E1<-cv5_GNET2(scal.IB_mat1.E1,K.E1,training.E1$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.E1)
S2.GNETns1.IB.E2<-cv5_GNET2(scal.IB_mat1.E2,K.E2,training.E2$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.E2)
S2.GNETns1.IB.E3<-cv5_GNET2(scal.IB_mat1.E3,K.E3,training.E3$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.E3)
S2.GNETns1.IB.E4<-cv5_GNET2(scal.IB_mat1.E4,K.E4,training.E4$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.E4)
S2.GNETns1.IB.E5<-cv5_GNET2(scal.IB_mat1.E5,K.E5,training.E5$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.E5)
S2.GNETns1.IB.E6<-cv5_GNET2(scal.IB_mat1.E6,K.E6,training.E6$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.E6)
S2.GNETns1.IB.E7<-cv5_GNET2(scal.IB_mat1.E7,K.E7,training.E7$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.E7)
S2.GNETns1.IB.E8<-cv5_GNET2(scal.IB_mat1.E8,K.E8,training.E8$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.E8)
S2.GNETns1.IB.E9<-cv5_GNET2(scal.IB_mat1.E9,K.E9,training.E9$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.E9)
S2.GNETns1.IB.E10<-cv5_GNET2(scal.IB_mat1.E10,K.E10,training.E10$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.E10)

S2.GNETns1.IB<-Group(list(S2.GNETns1.IB.E1[[2]],S2.GNETns1.IB.E2[[2]],S2.GNETns1.IB.E3[[2]],S2.GNETns1.IB.E4[[2]],S2.GNETns1.IB.E5[[2]],
                          S2.GNETns1.IB.E6[[2]],S2.GNETns1.IB.E7[[2]],S2.GNETns1.IB.E8[[2]],S2.GNETns1.IB.E9[[2]],S2.GNETns1.IB.E10[[2]])) ; Store(S2.GNETns1.IB)

Spear_score(S2.GNETns1.IB)
graph1b(S2.GNETns1.IB)
Kappa_score(S2.GNETns1.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.GNETns1.2.E1<-cv5_GNET2(scal_mat1.2.E1,K.E1,training.E1$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.2.E1)
S2.GNETns1.2.E2<-cv5_GNET2(scal_mat1.2.E2,K.E2,training.E2$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.2.E2)
S2.GNETns1.2.E3<-cv5_GNET2(scal_mat1.2.E3,K.E3,training.E3$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.2.E3)
S2.GNETns1.2.E4<-cv5_GNET2(scal_mat1.2.E4,K.E4,training.E4$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.2.E4)
S2.GNETns1.2.E5<-cv5_GNET2(scal_mat1.2.E5,K.E5,training.E5$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.2.E5)
S2.GNETns1.2.E6<-cv5_GNET2(scal_mat1.2.E6,K.E6,training.E6$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.2.E6)
S2.GNETns1.2.E7<-cv5_GNET2(scal_mat1.2.E7,K.E7,training.E7$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.2.E7)
S2.GNETns1.2.E8<-cv5_GNET2(scal_mat1.2.E8,K.E8,training.E8$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.2.E8)
S2.GNETns1.2.E9<-cv5_GNET2(scal_mat1.2.E9,K.E9,training.E9$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.2.E9)
S2.GNETns1.2.E10<-cv5_GNET2(scal_mat1.2.E10,K.E10,training.E10$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.2.E10)

S2.GNETns1.2<-Group(list(S2.GNETns1.2.E1[[2]],S2.GNETns1.2.E2[[2]],S2.GNETns1.2.E3[[2]],S2.GNETns1.2.E4[[2]],S2.GNETns1.2.E5[[2]],
                         S2.GNETns1.2.E6[[2]],S2.GNETns1.2.E7[[2]],S2.GNETns1.2.E8[[2]],S2.GNETns1.2.E9[[2]],S2.GNETns1.2.E10[[2]])) ; Store(S2.GNETns1.2)

Spear_score(S2.GNETns1.2)
graph1b(S2.GNETns1.2)
Kappa_score(S2.GNETns1.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.GNETns1.IB.2.E1<-cv5_GNET2(scal.IB_mat1.2.E1,K.E1,training.E1$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.2.E1)
S2.GNETns1.IB.2.E2<-cv5_GNET2(scal.IB_mat1.2.E2,K.E2,training.E2$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.2.E2)
S2.GNETns1.IB.2.E3<-cv5_GNET2(scal.IB_mat1.2.E3,K.E3,training.E3$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.2.E3)
S2.GNETns1.IB.2.E4<-cv5_GNET2(scal.IB_mat1.2.E4,K.E4,training.E4$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.2.E4)
S2.GNETns1.IB.2.E5<-cv5_GNET2(scal.IB_mat1.2.E5,K.E5,training.E5$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.2.E5)
S2.GNETns1.IB.2.E6<-cv5_GNET2(scal.IB_mat1.2.E6,K.E6,training.E6$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.2.E6)
S2.GNETns1.IB.2.E7<-cv5_GNET2(scal.IB_mat1.2.E7,K.E7,training.E7$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.2.E7)
S2.GNETns1.IB.2.E8<-cv5_GNET2(scal.IB_mat1.2.E8,K.E8,training.E8$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.2.E8)
S2.GNETns1.IB.2.E9<-cv5_GNET2(scal.IB_mat1.2.E9,K.E9,training.E9$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.2.E9)
S2.GNETns1.IB.2.E10<-cv5_GNET2(scal.IB_mat1.2.E10,K.E10,training.E10$Score2,alpha=0.5,1698) ; Store(S2.GNETns1.IB.2.E10)

S2.GNETns1.IB.2<-Group(list(S2.GNETns1.IB.2.E1[[2]],S2.GNETns1.IB.2.E2[[2]],S2.GNETns1.IB.2.E3[[2]],S2.GNETns1.IB.2.E4[[2]],S2.GNETns1.IB.2.E5[[2]],
                            S2.GNETns1.IB.2.E6[[2]],S2.GNETns1.IB.2.E7[[2]],S2.GNETns1.IB.2.E8[[2]],S2.GNETns1.IB.2.E9[[2]],S2.GNETns1.IB.2.E10[[2]])) ; Store(S2.GNETns1.IB.2)

Spear_score(S2.GNETns1.IB.2)
graph1b(S2.GNETns1.IB.2)
Kappa_score(S2.GNETns1.IB.2)

#####################################################################
# TRAIN Glmnet on scaled matrix with bns
#####################################################################

library(glmnet)

# 1rst set of scaled matrix 
# based on "isgood"
#####################################################################

S2.GNETns3.E1<-cv5_GNET2(scal_mat3.E1,K.E1,training.E1$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.E1)
S2.GNETns3.E2<-cv5_GNET2(scal_mat3.E2,K.E2,training.E2$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.E2)
S2.GNETns3.E3<-cv5_GNET2(scal_mat3.E3,K.E3,training.E3$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.E3)
S2.GNETns3.E4<-cv5_GNET2(scal_mat3.E4,K.E4,training.E4$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.E4)
S2.GNETns3.E5<-cv5_GNET2(scal_mat3.E5,K.E5,training.E5$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.E5)
S2.GNETns3.E6<-cv5_GNET2(scal_mat3.E6,K.E6,training.E6$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.E6)
S2.GNETns3.E7<-cv5_GNET2(scal_mat3.E7,K.E7,training.E7$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.E7)
S2.GNETns3.E8<-cv5_GNET2(scal_mat3.E8,K.E8,training.E8$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.E8)
S2.GNETns3.E9<-cv5_GNET2(scal_mat3.E9,K.E9,training.E9$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.E9)
S2.GNETns3.E10<-cv5_GNET2(scal_mat3.E10,K.E10,training.E10$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.E10)

S2.GNETns3<-Group(list(S2.GNETns3.E1[[2]],S2.GNETns3.E2[[2]],S2.GNETns3.E3[[2]],S2.GNETns3.E4[[2]],S2.GNETns3.E5[[2]],
                       S2.GNETns3.E6[[2]],S2.GNETns3.E7[[2]],S2.GNETns3.E8[[2]],S2.GNETns3.E9[[2]],S2.GNETns3.E10[[2]])) ; Store(S2.GNETns3)

Spear_score(S2.GNETns3)
graph1b(S2.GNETns3)
Kappa_score(S2.GNETns3)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.GNETns3.IB.E1<-cv5_GNET2(scal.IB_mat3.E1,K.E1,training.E1$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.E1)
S2.GNETns3.IB.E2<-cv5_GNET2(scal.IB_mat3.E2,K.E2,training.E2$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.E2)
S2.GNETns3.IB.E3<-cv5_GNET2(scal.IB_mat3.E3,K.E3,training.E3$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.E3)
S2.GNETns3.IB.E4<-cv5_GNET2(scal.IB_mat3.E4,K.E4,training.E4$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.E4)
S2.GNETns3.IB.E5<-cv5_GNET2(scal.IB_mat3.E5,K.E5,training.E5$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.E5)
S2.GNETns3.IB.E6<-cv5_GNET2(scal.IB_mat3.E6,K.E6,training.E6$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.E6)
S2.GNETns3.IB.E7<-cv5_GNET2(scal.IB_mat3.E7,K.E7,training.E7$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.E7)
S2.GNETns3.IB.E8<-cv5_GNET2(scal.IB_mat3.E8,K.E8,training.E8$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.E8)
S2.GNETns3.IB.E9<-cv5_GNET2(scal.IB_mat3.E9,K.E9,training.E9$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.E9)
S2.GNETns3.IB.E10<-cv5_GNET2(scal.IB_mat3.E10,K.E10,training.E10$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.E10)

S2.GNETns3.IB<-Group(list(S2.GNETns3.IB.E1[[2]],S2.GNETns3.IB.E2[[2]],S2.GNETns3.IB.E3[[2]],S2.GNETns3.IB.E4[[2]],S2.GNETns3.IB.E5[[2]],
                          S2.GNETns3.IB.E6[[2]],S2.GNETns3.IB.E7[[2]],S2.GNETns3.IB.E8[[2]],S2.GNETns3.IB.E9[[2]],S2.GNETns3.IB.E10[[2]])) ; Store(S2.GNETns3.IB)

Spear_score(S2.GNETns3.IB)
graph1b(S2.GNETns3.IB)
Kappa_score(S2.GNETns3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.GNETns3.2.E1<-cv5_GNET2(scal_mat3.2.E1,K.E1,training.E1$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.2.E1)
S2.GNETns3.2.E2<-cv5_GNET2(scal_mat3.2.E2,K.E2,training.E2$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.2.E2)
S2.GNETns3.2.E3<-cv5_GNET2(scal_mat3.2.E3,K.E3,training.E3$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.2.E3)
S2.GNETns3.2.E4<-cv5_GNET2(scal_mat3.2.E4,K.E4,training.E4$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.2.E4)
S2.GNETns3.2.E5<-cv5_GNET2(scal_mat3.2.E5,K.E5,training.E5$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.2.E5)
S2.GNETns3.2.E6<-cv5_GNET2(scal_mat3.2.E6,K.E6,training.E6$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.2.E6)
S2.GNETns3.2.E7<-cv5_GNET2(scal_mat3.2.E7,K.E7,training.E7$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.2.E7)
S2.GNETns3.2.E8<-cv5_GNET2(scal_mat3.2.E8,K.E8,training.E8$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.2.E8)
S2.GNETns3.2.E9<-cv5_GNET2(scal_mat3.2.E9,K.E9,training.E9$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.2.E9)
S2.GNETns3.2.E10<-cv5_GNET2(scal_mat3.2.E10,K.E10,training.E10$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.2.E10)

S2.GNETns3.2<-Group(list(S2.GNETns3.2.E1[[2]],S2.GNETns3.2.E2[[2]],S2.GNETns3.2.E3[[2]],S2.GNETns3.2.E4[[2]],S2.GNETns3.2.E5[[2]],
                         S2.GNETns3.2.E6[[2]],S2.GNETns3.2.E7[[2]],S2.GNETns3.2.E8[[2]],S2.GNETns3.2.E9[[2]],S2.GNETns3.2.E10[[2]])) ; Store(S2.GNETns3.2)

Spear_score(S2.GNETns3.2)
graph1b(S2.GNETns3.2)
Kappa_score(S2.GNETns3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.GNETns3.IB.2.E1<-cv5_GNET2(scal.IB_mat3.2.E1,K.E1,training.E1$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.2.E1)
S2.GNETns3.IB.2.E2<-cv5_GNET2(scal.IB_mat3.2.E2,K.E2,training.E2$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.2.E2)
S2.GNETns3.IB.2.E3<-cv5_GNET2(scal.IB_mat3.2.E3,K.E3,training.E3$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.2.E3)
S2.GNETns3.IB.2.E4<-cv5_GNET2(scal.IB_mat3.2.E4,K.E4,training.E4$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.2.E4)
S2.GNETns3.IB.2.E5<-cv5_GNET2(scal.IB_mat3.2.E5,K.E5,training.E5$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.2.E5)
S2.GNETns3.IB.2.E6<-cv5_GNET2(scal.IB_mat3.2.E6,K.E6,training.E6$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.2.E6)
S2.GNETns3.IB.2.E7<-cv5_GNET2(scal.IB_mat3.2.E7,K.E7,training.E7$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.2.E7)
S2.GNETns3.IB.2.E8<-cv5_GNET2(scal.IB_mat3.2.E8,K.E8,training.E8$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.2.E8)
S2.GNETns3.IB.2.E9<-cv5_GNET2(scal.IB_mat3.2.E9,K.E9,training.E9$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.2.E9)
S2.GNETns3.IB.2.E10<-cv5_GNET2(scal.IB_mat3.2.E10,K.E10,training.E10$Score2,alpha=0.5,1698) ; Store(S2.GNETns3.IB.2.E10)

S2.GNETns3.IB.2<-Group(list(S2.GNETns3.IB.2.E1[[2]],S2.GNETns3.IB.2.E2[[2]],S2.GNETns3.IB.2.E3[[2]],S2.GNETns3.IB.2.E4[[2]],S2.GNETns3.IB.2.E5[[2]],
                            S2.GNETns3.IB.2.E6[[2]],S2.GNETns3.IB.2.E7[[2]],S2.GNETns3.IB.2.E8[[2]],S2.GNETns3.IB.2.E9[[2]],S2.GNETns3.IB.2.E10[[2]])) ; Store(S2.GNETns3.IB.2)

Spear_score(S2.GNETns3.IB.2)
graph1b(S2.GNETns3.IB.2)
Kappa_score(S2.GNETns3.IB.2)

#####################################################################
# Train multinomial glmnet
#####################################################################

library(glmnetcr)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.GNETcr3.E1<-Cv5_GNETcr(scal_mat3.E1,K.E1,training.E1$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.E1)
S2.GNETcr3.E2<-Cv5_GNETcr(scal_mat3.E2,K.E2,training.E2$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.E2)
S2.GNETcr3.E3<-Cv5_GNETcr(scal_mat3.E3,K.E3,training.E3$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.E3)
S2.GNETcr3.E4<-Cv5_GNETcr(scal_mat3.E4,K.E4,training.E4$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.E4)
S2.GNETcr3.E5<-Cv5_GNETcr(scal_mat3.E5,K.E5,training.E5$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.E5)
S2.GNETcr3.E6<-Cv5_GNETcr(scal_mat3.E6,K.E6,training.E6$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.E6)
S2.GNETcr3.E7<-Cv5_GNETcr(scal_mat3.E7,K.E7,training.E7$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.E7)
S2.GNETcr3.E8<-Cv5_GNETcr(scal_mat3.E8,K.E8,training.E8$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.E8)
S2.GNETcr3.E9<-Cv5_GNETcr(scal_mat3.E9,K.E9,training.E9$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.E9)
S2.GNETcr3.E10<-Cv5_GNETcr(scal_mat3.E10,K.E10,training.E10$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.E10)

S2.GNETcr3<-Group(list(S2.GNETcr3.E1[[2]],S2.GNETcr3.E2[[2]],S2.GNETcr3.E3[[2]],S2.GNETcr3.E4[[2]],S2.GNETcr3.E5[[2]],
                       S2.GNETcr3.E6[[2]],S2.GNETcr3.E7[[2]],S2.GNETcr3.E8[[2]],S2.GNETcr3.E9[[2]],S2.GNETcr3.E10[[2]])) ; Store(S2.GNETcr3)

Spear_score(S2.GNETcr3)
graph1b(S2.GNETcr3)
Kappa_score(S2.GNETcr3)

# 1rst set of scaled matrix 
# based on "isbad"
#####################################################################

S2.GNETcr3.IB.E1<-Cv5_GNETcr(scal.IB_mat3.E1,K.E1,training.E1$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.E1)
S2.GNETcr3.IB.E2<-Cv5_GNETcr(scal.IB_mat3.E2,K.E2,training.E2$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.E2)
S2.GNETcr3.IB.E3<-Cv5_GNETcr(scal.IB_mat3.E3,K.E3,training.E3$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.E3)
S2.GNETcr3.IB.E4<-Cv5_GNETcr(scal.IB_mat3.E4,K.E4,training.E4$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.E4)
S2.GNETcr3.IB.E5<-Cv5_GNETcr(scal.IB_mat3.E5,K.E5,training.E5$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.E5)
S2.GNETcr3.IB.E6<-Cv5_GNETcr(scal.IB_mat3.E6,K.E6,training.E6$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.E6)
S2.GNETcr3.IB.E7<-Cv5_GNETcr(scal.IB_mat3.E7,K.E7,training.E7$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.E7)
S2.GNETcr3.IB.E8<-Cv5_GNETcr(scal.IB_mat3.E8,K.E8,training.E8$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.E8)
S2.GNETcr3.IB.E9<-Cv5_GNETcr(scal.IB_mat3.E9,K.E9,training.E9$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.E9)
S2.GNETcr3.IB.E10<-Cv5_GNETcr(scal.IB_mat3.E10,K.E10,training.E10$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.E10)

S2.GNETcr3.IB<-Group(list(S2.GNETcr3.IB.E1[[2]],S2.GNETcr3.IB.E2[[2]],S2.GNETcr3.IB.E3[[2]],S2.GNETcr3.IB.E4[[2]],S2.GNETcr3.IB.E5[[2]],
                          S2.GNETcr3.IB.E6[[2]],S2.GNETcr3.IB.E7[[2]],S2.GNETcr3.IB.E8[[2]],S2.GNETcr3.IB.E9[[2]],S2.GNETcr3.IB.E10[[2]])) ; Store(S2.GNETcr3.IB)

Spear_score(S2.GNETcr3.IB)
graph1b(S2.GNETcr3.IB)
Kappa_score(S2.GNETcr3.IB)

# 2nd set of scaled matrix 
# based on "isgood"
#####################################################################

S2.GNETcr3.2.E1<-Cv5_GNETcr(scal_mat3.2.E1,K.E1,training.E1$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.2.E1)
S2.GNETcr3.2.E2<-Cv5_GNETcr(scal_mat3.2.E2,K.E2,training.E2$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.2.E2)
S2.GNETcr3.2.E3<-Cv5_GNETcr(scal_mat3.2.E3,K.E3,training.E3$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.2.E3)
S2.GNETcr3.2.E4<-Cv5_GNETcr(scal_mat3.2.E4,K.E4,training.E4$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.2.E4)
S2.GNETcr3.2.E5<-Cv5_GNETcr(scal_mat3.2.E5,K.E5,training.E5$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.2.E5)
S2.GNETcr3.2.E6<-Cv5_GNETcr(scal_mat3.2.E6,K.E6,training.E6$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.2.E6)
S2.GNETcr3.2.E7<-Cv5_GNETcr(scal_mat3.2.E7,K.E7,training.E7$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.2.E7)
S2.GNETcr3.2.E8<-Cv5_GNETcr(scal_mat3.2.E8,K.E8,training.E8$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.2.E8)
S2.GNETcr3.2.E9<-Cv5_GNETcr(scal_mat3.2.E9,K.E9,training.E9$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.2.E9)
S2.GNETcr3.2.E10<-Cv5_GNETcr(scal_mat3.2.E10,K.E10,training.E10$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.2.E10)

S2.GNETcr3.2<-Group(list(S2.GNETcr3.2.E1[[2]],S2.GNETcr3.2.E2[[2]],S2.GNETcr3.2.E3[[2]],S2.GNETcr3.2.E4[[2]],S2.GNETcr3.2.E5[[2]],
                         S2.GNETcr3.2.E6[[2]],S2.GNETcr3.2.E7[[2]],S2.GNETcr3.2.E8[[2]],S2.GNETcr3.2.E9[[2]],S2.GNETcr3.2.E10[[2]])) ; Store(S2.GNETcr3.2)

Spear_score(S2.GNETcr3.2)
graph1b(S2.GNETcr3.2)
Kappa_score(S2.GNETcr3.2)

# 2nd set of scaled matrix 
# based on "isbad"
#####################################################################

S2.GNETcr3.IB.2.E1<-Cv5_GNETcr(scal.IB_mat3.2.E1,K.E1,training.E1$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.2.E1)
S2.GNETcr3.IB.2.E2<-Cv5_GNETcr(scal.IB_mat3.2.E2,K.E2,training.E2$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.2.E2)
S2.GNETcr3.IB.2.E3<-Cv5_GNETcr(scal.IB_mat3.2.E3,K.E3,training.E3$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.2.E3)
S2.GNETcr3.IB.2.E4<-Cv5_GNETcr(scal.IB_mat3.2.E4,K.E4,training.E4$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.2.E4)
S2.GNETcr3.IB.2.E5<-Cv5_GNETcr(scal.IB_mat3.2.E5,K.E5,training.E5$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.2.E5)
S2.GNETcr3.IB.2.E6<-Cv5_GNETcr(scal.IB_mat3.2.E6,K.E6,training.E6$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.2.E6)
S2.GNETcr3.IB.2.E7<-Cv5_GNETcr(scal.IB_mat3.2.E7,K.E7,training.E7$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.2.E7)
S2.GNETcr3.IB.2.E8<-Cv5_GNETcr(scal.IB_mat3.2.E8,K.E8,training.E8$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.2.E8)
S2.GNETcr3.IB.2.E9<-Cv5_GNETcr(scal.IB_mat3.2.E9,K.E9,training.E9$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.2.E9)
S2.GNETcr3.IB.2.E10<-Cv5_GNETcr(scal.IB_mat3.2.E10,K.E10,training.E10$Score2,alpha=0.5,stand=FALSE,1698) ; Store(S2.GNETcr3.IB.2.E10)

S2.GNETcr3.IB.2<-Group(list(S2.GNETcr3.IB.2.E1[[2]],S2.GNETcr3.IB.2.E2[[2]],S2.GNETcr3.IB.2.E3[[2]],S2.GNETcr3.IB.2.E4[[2]],S2.GNETcr3.IB.2.E5[[2]],
                            S2.GNETcr3.IB.2.E6[[2]],S2.GNETcr3.IB.2.E7[[2]],S2.GNETcr3.IB.2.E8[[2]],S2.GNETcr3.IB.2.E9[[2]],S2.GNETcr3.IB.2.E10[[2]])) ; Store(S2.GNETcr3.IB.2)

Spear_score(S2.GNETcr3.IB.2)
graph1b(S2.GNETcr3.IB.2)
Kappa_score(S2.GNETcr3.IB.2)

#####################################################################
#####################################################################
# Compute simple averages
#####################################################################
#####################################################################

S2.GNET1R.MIX<-(S2.GNET1R+S2.GNET1R.2)/2 ; Store(S2.GNET1R.MIX)
S2.SVM3.MIX<-(S2.SVM3+S2.SVM3.2+S2.SVM3.IB+S2.SVM3.IB.2)/4 ; Store(S2.SVM3.MIX)
S2.RAD2.MIX<-(S2.RAD2+S2.RAD2.2+S2.RAD2.IB+S2.RAD2.IB.2)/4 ; Store(S2.RAD2.MIX)
S2.SVMc3.MIX<-(S2.SVMc3b+S2.SVMc3.2-S2.SVMc3.IB-S2.SVMc3b.IB.2)/4 ; Store(S2.SVMc3.MIX)
S2.RADc2.MIX<-(S2.RADc2+S2.RADc2.2-S2.RADc2.IB-S2.RADc2.IB.2)/4 ; Store(S2.RADc2.MIX)
S2.SVMm3.MIX<-(S2.SVMm3+S2.SVMm3.2+S2.SVMm3.IB+S2.SVMm3.IB.2)/4 ; Store(S2.SVMm3.MIX)
S2.RADm2.MIX<-(S2.RADm2+S2.RADm2.2+S2.RADm2.IB+S2.RADm2.IB.2)/4 ; Store(S2.RADm2.MIX)
S2.GNETns1.MIX<-(S2.GNETns1+S2.GNETns1.2+S2.GNETns1.IB+S2.GNETns1.IB.2)/4 ; Store(S2.GNETns1.MIX)
S2.GNETns3.MIX<-(S2.GNETns3+S2.GNETns3.2+S2.GNETns3.IB+S2.GNETns3.IB.2)/4 ; Store(S2.GNETns3.MIX)
S2.GNETcr3.MIX<-(S2.GNETcr3+S2.GNETcr3.2+S2.GNETcr3.IB+S2.GNETcr3.IB.2)/4 ; Store(S2.GNETcr3.MIX)

Spear_score(S2.GNET1R.MIX)
Spear_score(S2.SVM3.MIX)
Spear_score(S2.RAD2.MIX)
Spear_score(S2.SVMc3.MIX)
Spear_score(S2.RADc2.MIX)
Spear_score(S2.SVMm3.MIX)
Spear_score(S2.RADm2.MIX)
Spear_score(S2.GNETns1.MIX)
Spear_score(S2.GNETns3.MIX)
Spear_score(S2.GNETcr3.MIX)

Spear_score(SEL_RF1.MIX)
Spear_score(S2.GNET1R.MIX+GNET1R.MIX)
Spear_score(S2.SVM3.MIX+SVM3.MIX)
Spear_score(S2.RAD2.MIX+RAD2.MIX)
Spear_score(S2.SVMc3.MIX+SVMc3.MIX)
Spear_score(S2.RADc2.MIX+RADc2.MIX)
Spear_score(S2.SVMm3.MIX+SVMm3.MIX)
Spear_score(S2.RADm2.MIX+RADm2.MIX)
Spear_score(S2.GNETns1.MIX+GNETns1.MIX)
Spear_score(S2.GNETns3.MIX+GNETns3.MIX)
Spear_score(S2.GNETcr3.MIX+GNETcr3.MIX)

#####################################################################
#####################################################################
# train GBM with early stop
#####################################################################
#####################################################################

# function to train GBM
#####################################################################

cv5_GBM.step<-function(data,y,K,maxtrees,treestep,complexity,shrink,bag.fraction,local_max_protection_threshold,seed) {
  library(gbm)
  library(Hmisc)
  out<-list()
  out$seed<-seed
  set.seed(seed)
  
  # 5 folds cross validation
  # first 100 trees
  out$yhatV<-rep(0,length(y))
  fit<-list()
  for (i in 1:5) {
    data2<-data.frame(y=y,data[[i]])
    xnam<-names(data2)
    fmla <- as.formula(paste("y~ ", paste(xnam[-1], collapse= "+")))
    fit[[i]]<-gbm(fmla,data2[-K[[i]],],distribution="gaussian",n.trees=treestep,interaction.depth=complexity, 
          shrinkage=shrink, bag.fraction=bag.fraction,verbose =F)
      out$yhatV[K[[i]]]<-predict(fit[[i]],newdata=data2[K[[i]],],n.trees=treestep) 
  }
  best_metric <- spearman(y,out$yhatV)
  best_ntrees <- treestep
  print(paste("ntrees=",treestep,"- metric now =",round(best_metric,6)))

  # add at each step 100 trees, stop when no more gain in accuracy
  for (ntrees in seq(100+treestep,maxtrees,treestep)) {
    for (i in 1:5) {
      fit[[i]]<-gbm.more(fit[[i]],treestep)
      out$yhatV[K[[i]]]<-predict(fit[[i]],data[[i]][K[[i]],],n.trees=ntrees) 
      }
    metric_now<-spearman(y,out$yhatV)
    print(paste("ntrees=",ntrees,"- metric now =",round(metric_now,6)))
    if (best_metric > metric_now+local_max_protection_threshold) break 
    if (metric_now>best_metric) {
      best_metric <- metric_now
      best_ntrees <- ntrees
    }
  }
  out$best_ntrees<-best_ntrees
  for (i in 1:5)  out$yhatV[K[[i]]]<-predict(fit[[i]],data[[i]][K[[i]],],n.trees=best_ntrees) 
  metric_now<-spearman(y,out$yhatV)
  print(paste("best ntrees =",best_ntrees,"- metric = ",round(metric_now,6)))
  
  # train of full training set
  data2<-data.frame(y=y,data[[6]])
  xnam<-names(data2)
  fmla <- as.formula(paste("y~ ", paste(xnam[-1], collapse= "+")))
  out$fit_full_set<-gbm(fmla,data2,distribution="gaussian",n.trees=best_ntrees,interaction.depth=complexity, 
          shrinkage=shrink, bag.fraction=bag.fraction,verbose =F)
  print(paste("Fit on full training set done - ntrees=",best_ntrees))

  out
}
Store(cv5_GBM.step)

#####################################################################
#####################################################################
# Train GBM on 1st set of bin_mat with proxies
#####################################################################
#####################################################################
    
SEL_GBM1.step.E1<-cv5_GBM.step(COMBGP1.E1,training.E1$Score1,K.E1,6000,100,8,0.005,0.75,0.0001,  405) ; Store(SEL_GBM1.step.E1)
SEL_GBM1.step.E2<-cv5_GBM.step(COMBGP1.E2,training.E2$Score1,K.E2,6000,100,8,0.005,0.75,0.0001,  405) ; Store(SEL_GBM1.step.E2)
SEL_GBM1.step.E3<-cv5_GBM.step(COMBGP1.E3,training.E3$Score1,K.E3,6000,100,8,0.005,0.75,0.0001,  405) ; Store(SEL_GBM1.step.E3)
SEL_GBM1.step.E4<-cv5_GBM.step(COMBGP1.E4,training.E4$Score1,K.E4,6000,100,8,0.005,0.75,0.0001,  405) ; Store(SEL_GBM1.step.E4)
SEL_GBM1.step.E5<-cv5_GBM.step(COMBGP1.E5,training.E5$Score1,K.E5,6000,100,8,0.005,0.75,0.0001,  405) ; Store(SEL_GBM1.step.E5)
SEL_GBM1.step.E6<-cv5_GBM.step(COMBGP1.E6,training.E6$Score1,K.E6,6000,100,8,0.005,0.75,0.0001,  405) ; Store(SEL_GBM1.step.E6)
SEL_GBM1.step.E7<-cv5_GBM.step(COMBGP1.E7,training.E7$Score1,K.E7,6000,100,8,0.005,0.75,0.0001,  405) ; Store(SEL_GBM1.step.E7)
SEL_GBM1.step.E8<-cv5_GBM.step(COMBGP1.E8,training.E8$Score1,K.E8,6000,100,8,0.005,0.75,0.0001,  405) ; Store(SEL_GBM1.step.E8)
SEL_GBM1.step.E9<-cv5_GBM.step(COMBGP1.E9,training.E9$Score1,K.E9,6000,100,8,0.005,0.75,0.0001,  405) ; Store(SEL_GBM1.step.E9)
SEL_GBM1.step.E10<-cv5_GBM.step(COMBGP1.E10,training.E10$Score1,K.E10,6000,100,8,0.005,0.75,0.0001,  405) ; Store(SEL_GBM1.step.E10)

SEL_GBM1.step<-Group(list(SEL_GBM1.step.E1[[2]],SEL_GBM1.step.E2[[2]],SEL_GBM1.step.E3[[2]],SEL_GBM1.step.E4[[2]],SEL_GBM1.step.E5[[2]],
                  SEL_GBM1.step.E6[[2]],SEL_GBM1.step.E7[[2]],SEL_GBM1.step.E8[[2]],SEL_GBM1.step.E9[[2]],SEL_GBM1.step.E10[[2]])) ; Store(SEL_GBM1.step)
Spear_score(SEL_GBM1.step)
graph1b(SEL_GBM1.step)

#####################################################################
#####################################################################
# Split into 5
#####################################################################
#####################################################################

Kbb.E1<-split_k(5006,training.E1) ; Store(Kbb.E1)
Kbb.E2<-split_k(1006,training.E2) ; Store(Kbb.E2)
Kbb.E3<-split_k(2270,training.E3) ; Store(Kbb.E3)
Kbb.E4<-split_k(1059,training.E4) ; Store(Kbb.E4)
Kbb.E5<-split_k(9038,training.E5) ; Store(Kbb.E5)
Kbb.E6<-split_k(1016,training.E6) ; Store(Kbb.E6)
Kbb.E7<-split_k(256,training.E7) ; Store(Kbb.E7)
Kbb.E8<-split_k(2100,training.E8) ; Store(Kbb.E8)
Kbb.E9<-split_k(8100,training.E9) ; Store(Kbb.E9)
Kbb.E10<-split_k(9100,training.E10) ; Store(Kbb.E10)

#####################################################################
#####################################################################
# Form a data frame with fits to blend
#####################################################################
#####################################################################

TO_Blend<-data.frame(y=training$Score1,
                     SEL_GBM1.step,SEL_RF1,GNET1R,SVM3,RAD,RAD2,SVMc3b,RADc2,SVMm3,RADm2,GNETns1,GNETns3,GNETcr3,
                     
                     SVM3.IB,RAD2.IB,SVMc3.IB,RADc2.IB,SVMm3.IB,RADm2.IB,GNETns1.IB,GNETns3.IB,GNETcr3.IB,
                     
                     SEL_RF1.2,GNET1R.2,SVM3.2,RAD2.2,SVMc3.2,RADc2.2,SVMm3.2,RADm2.2,
                     GNETns1.2,GNETns3.2,GNETcr3.2,
                     
                     SVM3.IB.2,RAD2.IB.2,SVMc3b.IB.2,RADc2.IB.2,SVMm3.IB.2,RADm2.IB.2,
                     GNETns1.IB.2,GNETns3.IB.2,GNETcr3.IB.2,
                     
                     SEL_RF1.MIX,GNET1R.MIX,SVM3.MIX,RAD2.MIX,SVMc3.MIX,RADc2.MIX,
                     SVMm3.MIX,RADm2.MIX,GNETns1.MIX,GNETns3.MIX,GNETcr3.MIX,
                     
                     S2.GNET1R,S2.SVM3,S2.RAD,S2.RAD2,S2.SVMc3b,S2.RADc2,S2.SVMm3,
                     S2.RADm2,S2.GNETns1,S2.GNETns3,S2.GNETcr3,
                     
                     S2.SVM3.IB,S2.RAD2.IB,S2.SVMc3.IB,S2.RADc2.IB,S2.SVMm3.IB,S2.RADm2.IB,
                     S2.GNETns1.IB,S2.GNETns3.IB,S2.GNETcr3.IB,
                     
                     S2.GNET1R.2,S2.SVM3.2,S2.RAD2.2,S2.SVMc3.2,S2.RADc2.2,S2.SVMm3.2,S2.RADm2.2,
                     S2.GNETns1.2,S2.GNETns3.2,S2.GNETcr3.2,
                     
                     S2.SVM3.IB.2,S2.RAD2.IB.2,S2.SVMc3b.IB.2,S2.RADc2.IB.2,S2.SVMm3.IB.2,S2.RADm2.IB.2,
                     S2.GNETns1.IB.2,S2.GNETns3.IB.2,S2.GNETcr3.IB.2,
                     
                     S2.GNET1R.MIX,S2.SVM3.MIX,S2.RAD2.MIX,S2.SVMc3.MIX,S2.RADc2.MIX,S2.SVMm3.MIX,
                     S2.RADm2.MIX,S2.GNETns1.MIX,S2.GNETns3.MIX,S2.GNETcr3.MIX)
Store(TO_Blend)

TO_Blend.E1<-TO_Blend[training$EssaySet==1,] ; Store(TO_Blend.E1)
TO_Blend.E2<-TO_Blend[training$EssaySet==2,] ; Store(TO_Blend.E2)
TO_Blend.E3<-TO_Blend[training$EssaySet==3,] ; Store(TO_Blend.E3)
TO_Blend.E4<-TO_Blend[training$EssaySet==4,] ; Store(TO_Blend.E4)
TO_Blend.E5<-TO_Blend[training$EssaySet==5,] ; Store(TO_Blend.E5)
TO_Blend.E6<-TO_Blend[training$EssaySet==6,] ; Store(TO_Blend.E6)
TO_Blend.E7<-TO_Blend[training$EssaySet==7,] ; Store(TO_Blend.E7)
TO_Blend.E8<-TO_Blend[training$EssaySet==8,] ; Store(TO_Blend.E8)
TO_Blend.E9<-TO_Blend[training$EssaySet==9,] ; Store(TO_Blend.E9)
TO_Blend.E10<-TO_Blend[training$EssaySet==10,] ; Store(TO_Blend.E10)

#####################################################################
# Blend with GNET
#####################################################################

library(glmnet)
alpha=0
BB_GNET1.E1<-cv5_GNET(TO_Blend.E1[,-1],Kbb.E1,training.E1$Score1,alpha=alpha,TRUE,5035) ; Store(BB_GNET1.E1)
BB_GNET1.E2<-cv5_GNET(TO_Blend.E2[,-1],Kbb.E2,training.E2$Score1,alpha=alpha,TRUE,5035) ; Store(BB_GNET1.E2)
BB_GNET1.E3<-cv5_GNET(TO_Blend.E3[,-1],Kbb.E3,training.E3$Score1,alpha=alpha,TRUE,5035) ; Store(BB_GNET1.E3)
BB_GNET1.E4<-cv5_GNET(TO_Blend.E4[,-1],Kbb.E4,training.E4$Score1,alpha=alpha,TRUE,5035) ; Store(BB_GNET1.E4)
BB_GNET1.E5<-cv5_GNET(TO_Blend.E5[,-1],Kbb.E5,training.E5$Score1,alpha=alpha,TRUE,5035) ; Store(BB_GNET1.E5)
BB_GNET1.E6<-cv5_GNET(TO_Blend.E6[,-1],Kbb.E6,training.E6$Score1,alpha=alpha,TRUE,5035) ; Store(BB_GNET1.E6)
BB_GNET1.E7<-cv5_GNET(TO_Blend.E7[,-1],Kbb.E7,training.E7$Score1,alpha=alpha,TRUE,5035) ; Store(BB_GNET1.E7)
BB_GNET1.E8<-cv5_GNET(TO_Blend.E8[,-1],Kbb.E8,training.E8$Score1,alpha=alpha,TRUE,5035) ; Store(BB_GNET1.E8)
BB_GNET1.E9<-cv5_GNET(TO_Blend.E9[,-1],Kbb.E9,training.E9$Score1,alpha=alpha,TRUE,5035) ; Store(BB_GNET1.E9)
BB_GNET1.E10<-cv5_GNET(TO_Blend.E10[,-1],Kbb.E10,training.E10$Score1,alpha=alpha,TRUE,5035) ; Store(BB_GNET1.E10)

BB_GNET1<-Group(list(BB_GNET1.E1[[2]],BB_GNET1.E2[[2]],BB_GNET1.E3[[2]],BB_GNET1.E4[[2]],BB_GNET1.E5[[2]],
                     BB_GNET1.E6[[2]],BB_GNET1.E7[[2]],BB_GNET1.E8[[2]],BB_GNET1.E9[[2]],BB_GNET1.E10[[2]])) ; Store(BB_GNET1)

Spear_score(BB_GNET1)
graph1b(BB_GNET1)
Kappa_score(BB_GNET1)

#####################################################################
# Blend with GAM
#####################################################################

# function to blend with GAM
#####################################################################

cv5_GAM<-function (data, K, gamma) {
  out <- list()
  out[[1]] <- list()
  out[[2]] <- rep(0, nrow(data))
  (fmla <- as.formula(paste("y~ ", paste("s(",names(data)[-1],",bs='cs')", collapse= "+"))))
  for (i in 1:6) { 
    out[[1]][[i]]<-if (i<=5) 
      gam(fmla, data = data[-K[[i]], ],family=gaussian,gamma=gamma) else 
        gam(fmla, data = data,family=gaussian,gamma=gamma)
    print(paste("set", i, "done"))}
  for (i in 1:5) 
    out[[2]][K[[i]]]<-predict(out[[1]][[i]],data[K[[i]],]) 
  out
}
Store(cv5_GAM)

# blend with GAM
#####################################################################

library(mgcv)
comb<-c("y","SEL_GBM1.step","SEL_RF1.MIX","GNET1R.MIX","SVM3.MIX","RAD2.MIX","SVMc3.MIX","GNETns1.MIX","GNETns3.MIX",
        "S2.GNET1R.MIX","S2.SVM3.MIX","S2.RAD2.MIX","S2.SVMc3.MIX","S2.GNETns1.MIX","S2.GNETns3.MIX")
gamma=2
BB_GAM1.E1<-cv5_GAM(TO_Blend.E1[,comb],Kbb.E1,gamma=gamma) ; Store(BB_GAM1.E1)
BB_GAM1.E2<-cv5_GAM(TO_Blend.E2[,comb],Kbb.E2,gamma=gamma) ; Store(BB_GAM1.E2)
BB_GAM1.E3<-cv5_GAM(TO_Blend.E3[,comb],Kbb.E3,gamma=gamma) ; Store(BB_GAM1.E3)
BB_GAM1.E4<-cv5_GAM(TO_Blend.E4[,comb],Kbb.E4,gamma=gamma) ; Store(BB_GAM1.E4)
BB_GAM1.E5<-cv5_GAM(TO_Blend.E5[,comb],Kbb.E5,gamma=gamma) ; Store(BB_GAM1.E5)
BB_GAM1.E6<-cv5_GAM(TO_Blend.E6[,comb],Kbb.E6,gamma=gamma) ; Store(BB_GAM1.E6)
BB_GAM1.E7<-cv5_GAM(TO_Blend.E7[,comb],Kbb.E7,gamma=gamma) ; Store(BB_GAM1.E7)
BB_GAM1.E8<-cv5_GAM(TO_Blend.E8[,comb],Kbb.E8,gamma=gamma) ; Store(BB_GAM1.E8)
BB_GAM1.E9<-cv5_GAM(TO_Blend.E9[,comb],Kbb.E9,gamma=gamma) ; Store(BB_GAM1.E9)
BB_GAM1.E10<-cv5_GAM(TO_Blend.E10[,comb],Kbb.E10,gamma=gamma) ; Store(BB_GAM1.E10)

#summary(BB_GAM1.E1[[1]][[6]])

BB_GAM1<-Group(list(BB_GAM1.E1[[2]],BB_GAM1.E2[[2]],BB_GAM1.E3[[2]],BB_GAM1.E4[[2]],BB_GAM1.E5[[2]],
                    BB_GAM1.E6[[2]],BB_GAM1.E7[[2]],BB_GAM1.E8[[2]],BB_GAM1.E9[[2]],BB_GAM1.E10[[2]])) ; Store(BB_GAM1)

Spear_score(BB_GAM1)
graph1b(BB_GAM1)
Kappa_score(BB_GAM1)

#####################################################################
#####################################################################
# Form a data frame with fits to blend
#####################################################################
#####################################################################

TO_Blendx2<-data.frame(y=training$Score1,BB_GNET1,BB_GAM1)
Store(TO_Blendx2)

TO_Blendx2.E1<-TO_Blendx2[training$EssaySet==1,] ; Store(TO_Blendx2.E1)
TO_Blendx2.E2<-TO_Blendx2[training$EssaySet==2,] ; Store(TO_Blendx2.E2)
TO_Blendx2.E3<-TO_Blendx2[training$EssaySet==3,] ; Store(TO_Blendx2.E3)
TO_Blendx2.E4<-TO_Blendx2[training$EssaySet==4,] ; Store(TO_Blendx2.E4)
TO_Blendx2.E5<-TO_Blendx2[training$EssaySet==5,] ; Store(TO_Blendx2.E5)
TO_Blendx2.E6<-TO_Blendx2[training$EssaySet==6,] ; Store(TO_Blendx2.E6)
TO_Blendx2.E7<-TO_Blendx2[training$EssaySet==7,] ; Store(TO_Blendx2.E7)
TO_Blendx2.E8<-TO_Blendx2[training$EssaySet==8,] ; Store(TO_Blendx2.E8)
TO_Blendx2.E9<-TO_Blendx2[training$EssaySet==9,] ; Store(TO_Blendx2.E9)
TO_Blendx2.E10<-TO_Blendx2[training$EssaySet==10,] ; Store(TO_Blendx2.E10)

# Split into 5
#####################################################################

Kbbb.E1<-split_k(125006,training.E1) ; Store(Kbbb.E1)
Kbbb.E2<-split_k(121006,training.E2) ; Store(Kbbb.E2)
Kbbb.E3<-split_k(122270,training.E3) ; Store(Kbbb.E3)
Kbbb.E4<-split_k(121059,training.E4) ; Store(Kbbb.E4)
Kbbb.E5<-split_k(129038,training.E5) ; Store(Kbbb.E5)
Kbbb.E6<-split_k(121016,training.E6) ; Store(Kbbb.E6)
Kbbb.E7<-split_k(12256,training.E7) ; Store(Kbbb.E7)
Kbbb.E8<-split_k(122100,training.E8) ; Store(Kbbb.E8)
Kbbb.E9<-split_k(128100,training.E9) ; Store(Kbbb.E9)
Kbbb.E10<-split_k(129100,training.E10) ; Store(Kbbb.E10)

#####################################################################
# Blend of Blend with LM
#####################################################################

# function to blend with LM
#####################################################################

cv5_LM<-function (data, K) {
  out <- list()
  out[[1]] <- list()
  out[[2]] <- rep(0, nrow(data))
  (fmla <- as.formula(paste("y~ ", paste(names(data)[-1], collapse= "+"))))
  for (i in 1:6) { 
    out[[1]][[i]]<-if (i<=5) lm(fmla, data = data[-K[[i]], ]) else lm(fmla, data = data)
    print(paste("set", i, "done"))}
  for (i in 1:5) 
    out[[2]][K[[i]]]<-predict(out[[1]][[i]],data[K[[i]],]) 
  out
}
Store(cv5_LM)

# blend with LM
#####################################################################

BBB_LM1.E1<-cv5_LM(TO_Blendx2.E1,Kbbb.E1) ; Store(BBB_LM1.E1)
BBB_LM1.E2<-cv5_LM(TO_Blendx2.E2,Kbbb.E2) ; Store(BBB_LM1.E2)
BBB_LM1.E3<-cv5_LM(TO_Blendx2.E3,Kbbb.E3) ; Store(BBB_LM1.E3)
BBB_LM1.E4<-cv5_LM(TO_Blendx2.E4,Kbbb.E4) ; Store(BBB_LM1.E4)
BBB_LM1.E5<-cv5_LM(TO_Blendx2.E5,Kbbb.E5) ; Store(BBB_LM1.E5)
BBB_LM1.E6<-cv5_LM(TO_Blendx2.E6,Kbbb.E6) ; Store(BBB_LM1.E6)
BBB_LM1.E7<-cv5_LM(TO_Blendx2.E7,Kbbb.E7) ; Store(BBB_LM1.E7)
BBB_LM1.E8<-cv5_LM(TO_Blendx2.E8,Kbbb.E8) ; Store(BBB_LM1.E8)
BBB_LM1.E9<-cv5_LM(TO_Blendx2.E9,Kbbb.E9) ; Store(BBB_LM1.E9)
BBB_LM1.E10<-cv5_LM(TO_Blendx2.E10,Kbbb.E10) ; Store(BBB_LM1.E10)

#summary(BBB_LM1.E1[[1]][[6]])

BBB_LM1<-Group(list(BBB_LM1.E1[[2]],BBB_LM1.E2[[2]],BBB_LM1.E3[[2]],BBB_LM1.E4[[2]],BBB_LM1.E5[[2]],
                    BBB_LM1.E6[[2]],BBB_LM1.E7[[2]],BBB_LM1.E8[[2]],BBB_LM1.E9[[2]],BBB_LM1.E10[[2]])) ; Store(BBB_LM1)

Spear_score(BBB_LM1)
graph1b(BBB_LM1)
Kappa_score(BBB_LM1)

#####################################################################
#####################################################################
# MAKE IT OPTIMAL FOR KAPPAS
#####################################################################
#####################################################################

#####################################################################
# new split into 5 folds
#####################################################################

Kx.E1<-split_k(102506,training.E1) ; Store(Kx.E1)
Kx.E2<-split_k(10606,training.E2) ; Store(Kx.E2)
Kx.E3<-split_k(10287,training.E3) ; Store(Kx.E3)
Kx.E4<-split_k(101540,training.E4) ; Store(Kx.E4)
Kx.E5<-split_k(10973,training.E5) ; Store(Kx.E5)
Kx.E6<-split_k(10306,training.E6) ; Store(Kx.E6)
Kx.E7<-split_k(10216,training.E7) ; Store(Kx.E7)
Kx.E8<-split_k(102358,training.E8) ; Store(Kx.E8)
Kx.E9<-split_k(105761,training.E9) ; Store(Kx.E9)
Kx.E10<-split_k(102022,training.E10) ; Store(Kx.E10)

#####################################################################
# function to adj with poly 1d
#####################################################################

OPT1d<-function(x,y,seed) {
  kappa<-function(xx) -SQWKappa(y,round(xx[1]+xx[2]*x,0))
  optim(c(0,1),kappa)
}
Store(OPT1d)

cv5_OPT1d<-function(x,y,K,seed) {
  set.seed(seed)
  out<-list()
  out[[1]]<-list()
  for (i in 1:6) {
    out[[1]][[i]]<- if (i<=5) OPT1d(x[-K[[i]]],y[-K[[i]]],seed*i+110) else OPT1d(x,y,seed*i+110)
    print(paste("set",i,"done"))}      
  out[[2]]<-rep(0,length(x))
  for (i in 1:5) {
    xx<-out[[1]][[i]]$par
    out[[2]][K[[i]]]<-round(xx[1]+ xx[2]*x[K[[i]]],0)}
  out
}
Store(cv5_OPT1d)

#####################################################################
# adjust BB_GNET1
#####################################################################

BB_GNET1_OPT1d.E1<-cv5_OPT1d(BB_GNET1.E1[[2]],training.E1$Score1,Kx.E1,458) ;Store(BB_GNET1_OPT1d.E1)
BB_GNET1_OPT1d.E2<-cv5_OPT1d(BB_GNET1.E2[[2]],training.E2$Score1,Kx.E2,1458) ;Store(BB_GNET1_OPT1d.E2)
BB_GNET1_OPT1d.E3<-cv5_OPT1d(BB_GNET1.E3[[2]],training.E3$Score1,Kx.E3,1188) ;Store(BB_GNET1_OPT1d.E3)
BB_GNET1_OPT1d.E4<-cv5_OPT1d(BB_GNET1.E4[[2]],training.E4$Score1,Kx.E4,1188) ;Store(BB_GNET1_OPT1d.E4)
BB_GNET1_OPT1d.E5<-cv5_OPT1d(BB_GNET1.E5[[2]],training.E5$Score1,Kx.E5,1188) ;Store(BB_GNET1_OPT1d.E5)
BB_GNET1_OPT1d.E6<-cv5_OPT1d(BB_GNET1.E6[[2]],training.E6$Score1,Kx.E6,1188) ;Store(BB_GNET1_OPT1d.E6)
BB_GNET1_OPT1d.E7<-cv5_OPT1d(BB_GNET1.E7[[2]],training.E7$Score1,Kx.E7,1188) ;Store(BB_GNET1_OPT1d.E7)
BB_GNET1_OPT1d.E8<-cv5_OPT1d(BB_GNET1.E8[[2]],training.E8$Score1,Kx.E8,1188) ;Store(BB_GNET1_OPT1d.E8)
BB_GNET1_OPT1d.E9<-cv5_OPT1d(BB_GNET1.E9[[2]],training.E9$Score1,Kx.E9,1188) ;Store(BB_GNET1_OPT1d.E9)
BB_GNET1_OPT1d.E10<-cv5_OPT1d(BB_GNET1.E10[[2]],training.E10$Score1,Kx.E10,1188) ;Store(BB_GNET1_OPT1d.E10)

#cap predicted values range with possible score range
#####################################################################

score_range<-rbind(c(0,3),c(0,3),c(0,2),c(0,2),c(0,3),c(0,3),c(0,2),c(0,2),c(0,2),c(0,2))
Store(score_range)
range_ADJ<- function(x,score_range) pmax(score_range[1],pmin(score_range[2],x)) 
Store(range_ADJ)

BB_GNET1_OPT1d.E1[[2]]<-range_ADJ(BB_GNET1_OPT1d.E1[[2]],score_range[1,]) ; Store(BB_GNET1_OPT1d.E1)
BB_GNET1_OPT1d.E2[[2]]<-range_ADJ(BB_GNET1_OPT1d.E2[[2]],score_range[2,]) ; Store(BB_GNET1_OPT1d.E2)
BB_GNET1_OPT1d.E3[[2]]<-range_ADJ(BB_GNET1_OPT1d.E3[[2]],score_range[3,]) ; Store(BB_GNET1_OPT1d.E3)
BB_GNET1_OPT1d.E4[[2]]<-range_ADJ(BB_GNET1_OPT1d.E4[[2]],score_range[4,]) ; Store(BB_GNET1_OPT1d.E4)
BB_GNET1_OPT1d.E5[[2]]<-range_ADJ(BB_GNET1_OPT1d.E5[[2]],score_range[5,]) ; Store(BB_GNET1_OPT1d.E5)
BB_GNET1_OPT1d.E6[[2]]<-range_ADJ(BB_GNET1_OPT1d.E6[[2]],score_range[6,]) ; Store(BB_GNET1_OPT1d.E6)
BB_GNET1_OPT1d.E7[[2]]<-range_ADJ(BB_GNET1_OPT1d.E7[[2]],score_range[7,]) ; Store(BB_GNET1_OPT1d.E7)
BB_GNET1_OPT1d.E8[[2]]<-range_ADJ(BB_GNET1_OPT1d.E8[[2]],score_range[8,]) ; Store(BB_GNET1_OPT1d.E8)
BB_GNET1_OPT1d.E9[[2]]<-range_ADJ(BB_GNET1_OPT1d.E9[[2]],score_range[9,]) ; Store(BB_GNET1_OPT1d.E9)
BB_GNET1_OPT1d.E10[[2]]<-range_ADJ(BB_GNET1_OPT1d.E10[[2]],score_range[10,]) ; Store(BB_GNET1_OPT1d.E10)

BB_GNET1_OPT1d<-Group(list(BB_GNET1_OPT1d.E1[[2]],BB_GNET1_OPT1d.E2[[2]],BB_GNET1_OPT1d.E3[[2]],BB_GNET1_OPT1d.E4[[2]],BB_GNET1_OPT1d.E5[[2]],
                           BB_GNET1_OPT1d.E6[[2]],BB_GNET1_OPT1d.E7[[2]],BB_GNET1_OPT1d.E8[[2]],BB_GNET1_OPT1d.E9[[2]],BB_GNET1_OPT1d.E10[[2]]))
Store(BB_GNET1_OPT1d)

Spear_score(BB_GNET1_OPT1d)
graph1b(BB_GNET1_OPT1d)
Kappa_score(BB_GNET1_OPT1d)
graph2(training$Score1,BB_GNET1_OPT1d)

#####################################################################
# adjust BB_GAM1
#####################################################################

BB_GAM1_OPT1d.E1<-cv5_OPT1d(BB_GAM1.E1[[2]],training.E1$Score1,Kx.E1,458) ;Store(BB_GAM1_OPT1d.E1)
BB_GAM1_OPT1d.E2<-cv5_OPT1d(BB_GAM1.E2[[2]],training.E2$Score1,Kx.E2,1458) ;Store(BB_GAM1_OPT1d.E2)
BB_GAM1_OPT1d.E3<-cv5_OPT1d(BB_GAM1.E3[[2]],training.E3$Score1,Kx.E3,1188) ;Store(BB_GAM1_OPT1d.E3)
BB_GAM1_OPT1d.E4<-cv5_OPT1d(BB_GAM1.E4[[2]],training.E4$Score1,Kx.E4,1188) ;Store(BB_GAM1_OPT1d.E4)
BB_GAM1_OPT1d.E5<-cv5_OPT1d(BB_GAM1.E5[[2]],training.E5$Score1,Kx.E5,1188) ;Store(BB_GAM1_OPT1d.E5)
BB_GAM1_OPT1d.E6<-cv5_OPT1d(BB_GAM1.E6[[2]],training.E6$Score1,Kx.E6,1188) ;Store(BB_GAM1_OPT1d.E6)
BB_GAM1_OPT1d.E7<-cv5_OPT1d(BB_GAM1.E7[[2]],training.E7$Score1,Kx.E7,1188) ;Store(BB_GAM1_OPT1d.E7)
BB_GAM1_OPT1d.E8<-cv5_OPT1d(BB_GAM1.E8[[2]],training.E8$Score1,Kx.E8,1188) ;Store(BB_GAM1_OPT1d.E8)
BB_GAM1_OPT1d.E9<-cv5_OPT1d(BB_GAM1.E9[[2]],training.E9$Score1,Kx.E9,1188) ;Store(BB_GAM1_OPT1d.E9)
BB_GAM1_OPT1d.E10<-cv5_OPT1d(BB_GAM1.E10[[2]],training.E10$Score1,Kx.E10,1188) ;Store(BB_GAM1_OPT1d.E10)

#cap predicted values range with possible score range
#####################################################################

BB_GAM1_OPT1d.E1[[2]]<-range_ADJ(BB_GAM1_OPT1d.E1[[2]],score_range[1,]) ; Store(BB_GAM1_OPT1d.E1)
BB_GAM1_OPT1d.E2[[2]]<-range_ADJ(BB_GAM1_OPT1d.E2[[2]],score_range[2,]) ; Store(BB_GAM1_OPT1d.E2)
BB_GAM1_OPT1d.E3[[2]]<-range_ADJ(BB_GAM1_OPT1d.E3[[2]],score_range[3,]) ; Store(BB_GAM1_OPT1d.E3)
BB_GAM1_OPT1d.E4[[2]]<-range_ADJ(BB_GAM1_OPT1d.E4[[2]],score_range[4,]) ; Store(BB_GAM1_OPT1d.E4)
BB_GAM1_OPT1d.E5[[2]]<-range_ADJ(BB_GAM1_OPT1d.E5[[2]],score_range[5,]) ; Store(BB_GAM1_OPT1d.E5)
BB_GAM1_OPT1d.E6[[2]]<-range_ADJ(BB_GAM1_OPT1d.E6[[2]],score_range[6,]) ; Store(BB_GAM1_OPT1d.E6)
BB_GAM1_OPT1d.E7[[2]]<-range_ADJ(BB_GAM1_OPT1d.E7[[2]],score_range[7,]) ; Store(BB_GAM1_OPT1d.E7)
BB_GAM1_OPT1d.E8[[2]]<-range_ADJ(BB_GAM1_OPT1d.E8[[2]],score_range[8,]) ; Store(BB_GAM1_OPT1d.E8)
BB_GAM1_OPT1d.E9[[2]]<-range_ADJ(BB_GAM1_OPT1d.E9[[2]],score_range[9,]) ; Store(BB_GAM1_OPT1d.E9)
BB_GAM1_OPT1d.E10[[2]]<-range_ADJ(BB_GAM1_OPT1d.E10[[2]],score_range[10,]) ; Store(BB_GAM1_OPT1d.E10)

BB_GAM1_OPT1d<-Group(list(BB_GAM1_OPT1d.E1[[2]],BB_GAM1_OPT1d.E2[[2]],BB_GAM1_OPT1d.E3[[2]],BB_GAM1_OPT1d.E4[[2]],BB_GAM1_OPT1d.E5[[2]],
                          BB_GAM1_OPT1d.E6[[2]],BB_GAM1_OPT1d.E7[[2]],BB_GAM1_OPT1d.E8[[2]],BB_GAM1_OPT1d.E9[[2]],BB_GAM1_OPT1d.E10[[2]]))
Store(BB_GAM1_OPT1d)

Spear_score(BB_GAM1_OPT1d)
graph1b(BB_GAM1_OPT1d)
Kappa_score(BB_GAM1_OPT1d)
graph2(training$Score1,BB_GAM1_OPT1d)

#####################################################################
# adjust BBB_LM1
#####################################################################

BBB_LM1_OPT1d.E1<-cv5_OPT1d(BBB_LM1.E1[[2]],training.E1$Score1,Kx.E1,458) ;Store(BBB_LM1_OPT1d.E1)
BBB_LM1_OPT1d.E2<-cv5_OPT1d(BBB_LM1.E2[[2]],training.E2$Score1,Kx.E2,1458) ;Store(BBB_LM1_OPT1d.E2)
BBB_LM1_OPT1d.E3<-cv5_OPT1d(BBB_LM1.E3[[2]],training.E3$Score1,Kx.E3,1188) ;Store(BBB_LM1_OPT1d.E3)
BBB_LM1_OPT1d.E4<-cv5_OPT1d(BBB_LM1.E4[[2]],training.E4$Score1,Kx.E4,1188) ;Store(BBB_LM1_OPT1d.E4)
BBB_LM1_OPT1d.E5<-cv5_OPT1d(BBB_LM1.E5[[2]],training.E5$Score1,Kx.E5,1188) ;Store(BBB_LM1_OPT1d.E5)
BBB_LM1_OPT1d.E6<-cv5_OPT1d(BBB_LM1.E6[[2]],training.E6$Score1,Kx.E6,1188) ;Store(BBB_LM1_OPT1d.E6)
BBB_LM1_OPT1d.E7<-cv5_OPT1d(BBB_LM1.E7[[2]],training.E7$Score1,Kx.E7,1188) ;Store(BBB_LM1_OPT1d.E7)
BBB_LM1_OPT1d.E8<-cv5_OPT1d(BBB_LM1.E8[[2]],training.E8$Score1,Kx.E8,1188) ;Store(BBB_LM1_OPT1d.E8)
BBB_LM1_OPT1d.E9<-cv5_OPT1d(BBB_LM1.E9[[2]],training.E9$Score1,Kx.E9,1188) ;Store(BBB_LM1_OPT1d.E9)
BBB_LM1_OPT1d.E10<-cv5_OPT1d(BBB_LM1.E10[[2]],training.E10$Score1,Kx.E10,1188) ;Store(BBB_LM1_OPT1d.E10)

#cap predicted values range with possible score range
#####################################################################

BBB_LM1_OPT1d.E1[[2]]<-range_ADJ(BBB_LM1_OPT1d.E1[[2]],score_range[1,]) ; Store(BBB_LM1_OPT1d.E1)
BBB_LM1_OPT1d.E2[[2]]<-range_ADJ(BBB_LM1_OPT1d.E2[[2]],score_range[2,]) ; Store(BBB_LM1_OPT1d.E2)
BBB_LM1_OPT1d.E3[[2]]<-range_ADJ(BBB_LM1_OPT1d.E3[[2]],score_range[3,]) ; Store(BBB_LM1_OPT1d.E3)
BBB_LM1_OPT1d.E4[[2]]<-range_ADJ(BBB_LM1_OPT1d.E4[[2]],score_range[4,]) ; Store(BBB_LM1_OPT1d.E4)
BBB_LM1_OPT1d.E5[[2]]<-range_ADJ(BBB_LM1_OPT1d.E5[[2]],score_range[5,]) ; Store(BBB_LM1_OPT1d.E5)
BBB_LM1_OPT1d.E6[[2]]<-range_ADJ(BBB_LM1_OPT1d.E6[[2]],score_range[6,]) ; Store(BBB_LM1_OPT1d.E6)
BBB_LM1_OPT1d.E7[[2]]<-range_ADJ(BBB_LM1_OPT1d.E7[[2]],score_range[7,]) ; Store(BBB_LM1_OPT1d.E7)
BBB_LM1_OPT1d.E8[[2]]<-range_ADJ(BBB_LM1_OPT1d.E8[[2]],score_range[8,]) ; Store(BBB_LM1_OPT1d.E8)
BBB_LM1_OPT1d.E9[[2]]<-range_ADJ(BBB_LM1_OPT1d.E9[[2]],score_range[9,]) ; Store(BBB_LM1_OPT1d.E9)
BBB_LM1_OPT1d.E10[[2]]<-range_ADJ(BBB_LM1_OPT1d.E10[[2]],score_range[10,]) ; Store(BBB_LM1_OPT1d.E10)

BBB_LM1_OPT1d<-Group(list(BBB_LM1_OPT1d.E1[[2]],BBB_LM1_OPT1d.E2[[2]],BBB_LM1_OPT1d.E3[[2]],BBB_LM1_OPT1d.E4[[2]],BBB_LM1_OPT1d.E5[[2]],
                          BBB_LM1_OPT1d.E6[[2]],BBB_LM1_OPT1d.E7[[2]],BBB_LM1_OPT1d.E8[[2]],BBB_LM1_OPT1d.E9[[2]],BBB_LM1_OPT1d.E10[[2]]))
Store(BBB_LM1_OPT1d)

Spear_score(BBB_LM1_OPT1d)
graph1b(BBB_LM1_OPT1d)
Kappa_score(BBB_LM1_OPT1d)

#####################################################################
# function to adj with poly 2d
#####################################################################

OPT2d<-function(x,y,seed) {
  kappa<-function(xx) -SQWKappa(y,round(xx[1]+xx[2]*x+xx[3]*x^2,0))
  #  optim(c(0,1,0),kappa,lower=c(-1,0.6,0),upper=c(lower=c(0,1.4,3)),method="L-BFGS-B")
  optim(c(0,1,0),kappa)
}
Store(OPT2d)

cv5_OPT2d<-function(x,y,K,seed) {
  set.seed(seed)
  out<-list()
  out[[1]]<-list()
  for (i in 1:6) {
    out[[1]][[i]]<- if (i<=5) OPT2d(x[-K[[i]]],y[-K[[i]]],seed*i+110) else OPT2d(x,y,seed*i+110)
    print(paste("set",i,"done"))}      
  out[[2]]<-rep(0,length(x))
  for (i in 1:5) {
    xx<-out[[1]][[i]]$par
    out[[2]][K[[i]]]<-round(xx[1]+ xx[2]*x[K[[i]]]+ xx[3]*x[K[[i]]]^2,0)}
  out
}
Store(cv5_OPT2d)

#####################################################################
# adjust BB_GNET1
#####################################################################

BB_GNET1_OPT2d.E1<-cv5_OPT2d(BB_GNET1.E1[[2]],training.E1$Score1,Kx.E1,458) ;Store(BB_GNET1_OPT2d.E1)
BB_GNET1_OPT2d.E2<-cv5_OPT2d(BB_GNET1.E2[[2]],training.E2$Score1,Kx.E2,1458) ;Store(BB_GNET1_OPT2d.E2)
BB_GNET1_OPT2d.E3<-cv5_OPT2d(BB_GNET1.E3[[2]],training.E3$Score1,Kx.E3,1188) ;Store(BB_GNET1_OPT2d.E3)
BB_GNET1_OPT2d.E4<-cv5_OPT2d(BB_GNET1.E4[[2]],training.E4$Score1,Kx.E4,1188) ;Store(BB_GNET1_OPT2d.E4)
BB_GNET1_OPT2d.E5<-cv5_OPT2d(BB_GNET1.E5[[2]],training.E5$Score1,Kx.E5,1188) ;Store(BB_GNET1_OPT2d.E5)
BB_GNET1_OPT2d.E6<-cv5_OPT2d(BB_GNET1.E6[[2]],training.E6$Score1,Kx.E6,1188) ;Store(BB_GNET1_OPT2d.E6)
BB_GNET1_OPT2d.E7<-cv5_OPT2d(BB_GNET1.E7[[2]],training.E7$Score1,Kx.E7,1188) ;Store(BB_GNET1_OPT2d.E7)
BB_GNET1_OPT2d.E8<-cv5_OPT2d(BB_GNET1.E8[[2]],training.E8$Score1,Kx.E8,1188) ;Store(BB_GNET1_OPT2d.E8)
BB_GNET1_OPT2d.E9<-cv5_OPT2d(BB_GNET1.E9[[2]],training.E9$Score1,Kx.E9,1188) ;Store(BB_GNET1_OPT2d.E9)
BB_GNET1_OPT2d.E10<-cv5_OPT2d(BB_GNET1.E10[[2]],training.E10$Score1,Kx.E10,1188) ;Store(BB_GNET1_OPT2d.E10)

#cap predicted values range with possible score range
#####################################################################

BB_GNET1_OPT2d.E1[[2]]<-range_ADJ(BB_GNET1_OPT2d.E1[[2]],score_range[1,]) ; Store(BB_GNET1_OPT2d.E1)
BB_GNET1_OPT2d.E2[[2]]<-range_ADJ(BB_GNET1_OPT2d.E2[[2]],score_range[2,]) ; Store(BB_GNET1_OPT2d.E2)
BB_GNET1_OPT2d.E3[[2]]<-range_ADJ(BB_GNET1_OPT2d.E3[[2]],score_range[3,]) ; Store(BB_GNET1_OPT2d.E3)
BB_GNET1_OPT2d.E4[[2]]<-range_ADJ(BB_GNET1_OPT2d.E4[[2]],score_range[4,]) ; Store(BB_GNET1_OPT2d.E4)
BB_GNET1_OPT2d.E5[[2]]<-range_ADJ(BB_GNET1_OPT2d.E5[[2]],score_range[5,]) ; Store(BB_GNET1_OPT2d.E5)
BB_GNET1_OPT2d.E6[[2]]<-range_ADJ(BB_GNET1_OPT2d.E6[[2]],score_range[6,]) ; Store(BB_GNET1_OPT2d.E6)
BB_GNET1_OPT2d.E7[[2]]<-range_ADJ(BB_GNET1_OPT2d.E7[[2]],score_range[7,]) ; Store(BB_GNET1_OPT2d.E7)
BB_GNET1_OPT2d.E8[[2]]<-range_ADJ(BB_GNET1_OPT2d.E8[[2]],score_range[8,]) ; Store(BB_GNET1_OPT2d.E8)
BB_GNET1_OPT2d.E9[[2]]<-range_ADJ(BB_GNET1_OPT2d.E9[[2]],score_range[9,]) ; Store(BB_GNET1_OPT2d.E9)
BB_GNET1_OPT2d.E10[[2]]<-range_ADJ(BB_GNET1_OPT2d.E10[[2]],score_range[10,]) ; Store(BB_GNET1_OPT2d.E10)

BB_GNET1_OPT2d<-Group(list(BB_GNET1_OPT2d.E1[[2]],BB_GNET1_OPT2d.E2[[2]],BB_GNET1_OPT2d.E3[[2]],BB_GNET1_OPT2d.E4[[2]],BB_GNET1_OPT2d.E5[[2]],
                           BB_GNET1_OPT2d.E6[[2]],BB_GNET1_OPT2d.E7[[2]],BB_GNET1_OPT2d.E8[[2]],BB_GNET1_OPT2d.E9[[2]],BB_GNET1_OPT2d.E10[[2]]))
Store(BB_GNET1_OPT2d)

Spear_score(BB_GNET1_OPT2d)
graph1b(BB_GNET1_OPT2d)
Kappa_score(BB_GNET1_OPT2d)
graph2(training$Score1,BB_GNET1_OPT2d)

#####################################################################
# adjust BB_GAM1
#####################################################################

BB_GAM1_OPT2d.E1<-cv5_OPT2d(BB_GAM1.E1[[2]],training.E1$Score1,Kx.E1,458) ;Store(BB_GAM1_OPT2d.E1)
BB_GAM1_OPT2d.E2<-cv5_OPT2d(BB_GAM1.E2[[2]],training.E2$Score1,Kx.E2,1458) ;Store(BB_GAM1_OPT2d.E2)
BB_GAM1_OPT2d.E3<-cv5_OPT2d(BB_GAM1.E3[[2]],training.E3$Score1,Kx.E3,1188) ;Store(BB_GAM1_OPT2d.E3)
BB_GAM1_OPT2d.E4<-cv5_OPT2d(BB_GAM1.E4[[2]],training.E4$Score1,Kx.E4,1188) ;Store(BB_GAM1_OPT2d.E4)
BB_GAM1_OPT2d.E5<-cv5_OPT2d(BB_GAM1.E5[[2]],training.E5$Score1,Kx.E5,1188) ;Store(BB_GAM1_OPT2d.E5)
BB_GAM1_OPT2d.E6<-cv5_OPT2d(BB_GAM1.E6[[2]],training.E6$Score1,Kx.E6,1188) ;Store(BB_GAM1_OPT2d.E6)
BB_GAM1_OPT2d.E7<-cv5_OPT2d(BB_GAM1.E7[[2]],training.E7$Score1,Kx.E7,1188) ;Store(BB_GAM1_OPT2d.E7)
BB_GAM1_OPT2d.E8<-cv5_OPT2d(BB_GAM1.E8[[2]],training.E8$Score1,Kx.E8,1188) ;Store(BB_GAM1_OPT2d.E8)
BB_GAM1_OPT2d.E9<-cv5_OPT2d(BB_GAM1.E9[[2]],training.E9$Score1,Kx.E9,1188) ;Store(BB_GAM1_OPT2d.E9)
BB_GAM1_OPT2d.E10<-cv5_OPT2d(BB_GAM1.E10[[2]],training.E10$Score1,Kx.E10,1188) ;Store(BB_GAM1_OPT2d.E10)

#cap predicted values range with possible score range
#####################################################################

BB_GAM1_OPT2d.E1[[2]]<-range_ADJ(BB_GAM1_OPT2d.E1[[2]],score_range[1,]) ; Store(BB_GAM1_OPT2d.E1)
BB_GAM1_OPT2d.E2[[2]]<-range_ADJ(BB_GAM1_OPT2d.E2[[2]],score_range[2,]) ; Store(BB_GAM1_OPT2d.E2)
BB_GAM1_OPT2d.E3[[2]]<-range_ADJ(BB_GAM1_OPT2d.E3[[2]],score_range[3,]) ; Store(BB_GAM1_OPT2d.E3)
BB_GAM1_OPT2d.E4[[2]]<-range_ADJ(BB_GAM1_OPT2d.E4[[2]],score_range[4,]) ; Store(BB_GAM1_OPT2d.E4)
BB_GAM1_OPT2d.E5[[2]]<-range_ADJ(BB_GAM1_OPT2d.E5[[2]],score_range[5,]) ; Store(BB_GAM1_OPT2d.E5)
BB_GAM1_OPT2d.E6[[2]]<-range_ADJ(BB_GAM1_OPT2d.E6[[2]],score_range[6,]) ; Store(BB_GAM1_OPT2d.E6)
BB_GAM1_OPT2d.E7[[2]]<-range_ADJ(BB_GAM1_OPT2d.E7[[2]],score_range[7,]) ; Store(BB_GAM1_OPT2d.E7)
BB_GAM1_OPT2d.E8[[2]]<-range_ADJ(BB_GAM1_OPT2d.E8[[2]],score_range[8,]) ; Store(BB_GAM1_OPT2d.E8)
BB_GAM1_OPT2d.E9[[2]]<-range_ADJ(BB_GAM1_OPT2d.E9[[2]],score_range[9,]) ; Store(BB_GAM1_OPT2d.E9)
BB_GAM1_OPT2d.E10[[2]]<-range_ADJ(BB_GAM1_OPT2d.E10[[2]],score_range[10,]) ; Store(BB_GAM1_OPT2d.E10)

BB_GAM1_OPT2d<-Group(list(BB_GAM1_OPT2d.E1[[2]],BB_GAM1_OPT2d.E2[[2]],BB_GAM1_OPT2d.E3[[2]],BB_GAM1_OPT2d.E4[[2]],BB_GAM1_OPT2d.E5[[2]],
                          BB_GAM1_OPT2d.E6[[2]],BB_GAM1_OPT2d.E7[[2]],BB_GAM1_OPT2d.E8[[2]],BB_GAM1_OPT2d.E9[[2]],BB_GAM1_OPT2d.E10[[2]]))
Store(BB_GAM1_OPT2d)

Spear_score(BB_GAM1_OPT2d)
graph1b(BB_GAM1_OPT2d)
Kappa_score(BB_GAM1_OPT2d)
graph2(training$Score1,BB_GAM1_OPT2d)


#####################################################################
# adjust BBB_LM1
#####################################################################

BBB_LM1_OPT2d.E1<-cv5_OPT2d(BBB_LM1.E1[[2]],training.E1$Score1,Kx.E1,458) ;Store(BBB_LM1_OPT2d.E1)
BBB_LM1_OPT2d.E2<-cv5_OPT2d(BBB_LM1.E2[[2]],training.E2$Score1,Kx.E2,1458) ;Store(BBB_LM1_OPT2d.E2)
BBB_LM1_OPT2d.E3<-cv5_OPT2d(BBB_LM1.E3[[2]],training.E3$Score1,Kx.E3,1188) ;Store(BBB_LM1_OPT2d.E3)
BBB_LM1_OPT2d.E4<-cv5_OPT2d(BBB_LM1.E4[[2]],training.E4$Score1,Kx.E4,1188) ;Store(BBB_LM1_OPT2d.E4)
BBB_LM1_OPT2d.E5<-cv5_OPT2d(BBB_LM1.E5[[2]],training.E5$Score1,Kx.E5,1188) ;Store(BBB_LM1_OPT2d.E5)
BBB_LM1_OPT2d.E6<-cv5_OPT2d(BBB_LM1.E6[[2]],training.E6$Score1,Kx.E6,1188) ;Store(BBB_LM1_OPT2d.E6)
BBB_LM1_OPT2d.E7<-cv5_OPT2d(BBB_LM1.E7[[2]],training.E7$Score1,Kx.E7,1188) ;Store(BBB_LM1_OPT2d.E7)
BBB_LM1_OPT2d.E8<-cv5_OPT2d(BBB_LM1.E8[[2]],training.E8$Score1,Kx.E8,1188) ;Store(BBB_LM1_OPT2d.E8)
BBB_LM1_OPT2d.E9<-cv5_OPT2d(BBB_LM1.E9[[2]],training.E9$Score1,Kx.E9,1188) ;Store(BBB_LM1_OPT2d.E9)
BBB_LM1_OPT2d.E10<-cv5_OPT2d(BBB_LM1.E10[[2]],training.E10$Score1,Kx.E10,1188) ;Store(BBB_LM1_OPT2d.E10)

#cap predicted values range with possible score range
#####################################################################

BBB_LM1_OPT2d.E1[[2]]<-range_ADJ(BBB_LM1_OPT2d.E1[[2]],score_range[1,]) ; Store(BBB_LM1_OPT2d.E1)
BBB_LM1_OPT2d.E2[[2]]<-range_ADJ(BBB_LM1_OPT2d.E2[[2]],score_range[2,]) ; Store(BBB_LM1_OPT2d.E2)
BBB_LM1_OPT2d.E3[[2]]<-range_ADJ(BBB_LM1_OPT2d.E3[[2]],score_range[3,]) ; Store(BBB_LM1_OPT2d.E3)
BBB_LM1_OPT2d.E4[[2]]<-range_ADJ(BBB_LM1_OPT2d.E4[[2]],score_range[4,]) ; Store(BBB_LM1_OPT2d.E4)
BBB_LM1_OPT2d.E5[[2]]<-range_ADJ(BBB_LM1_OPT2d.E5[[2]],score_range[5,]) ; Store(BBB_LM1_OPT2d.E5)
BBB_LM1_OPT2d.E6[[2]]<-range_ADJ(BBB_LM1_OPT2d.E6[[2]],score_range[6,]) ; Store(BBB_LM1_OPT2d.E6)
BBB_LM1_OPT2d.E7[[2]]<-range_ADJ(BBB_LM1_OPT2d.E7[[2]],score_range[7,]) ; Store(BBB_LM1_OPT2d.E7)
BBB_LM1_OPT2d.E8[[2]]<-range_ADJ(BBB_LM1_OPT2d.E8[[2]],score_range[8,]) ; Store(BBB_LM1_OPT2d.E8)
BBB_LM1_OPT2d.E9[[2]]<-range_ADJ(BBB_LM1_OPT2d.E9[[2]],score_range[9,]) ; Store(BBB_LM1_OPT2d.E9)
BBB_LM1_OPT2d.E10[[2]]<-range_ADJ(BBB_LM1_OPT2d.E10[[2]],score_range[10,]) ; Store(BBB_LM1_OPT2d.E10)

BBB_LM1_OPT2d<-Group(list(BBB_LM1_OPT2d.E1[[2]],BBB_LM1_OPT2d.E2[[2]],BBB_LM1_OPT2d.E3[[2]],BBB_LM1_OPT2d.E4[[2]],BBB_LM1_OPT2d.E5[[2]],
                          BBB_LM1_OPT2d.E6[[2]],BBB_LM1_OPT2d.E7[[2]],BBB_LM1_OPT2d.E8[[2]],BBB_LM1_OPT2d.E9[[2]],BBB_LM1_OPT2d.E10[[2]]))
Store(BBB_LM1_OPT2d)

Spear_score(BBB_LM1_OPT2d)
graph1b(BBB_LM1_OPT2d)
Kappa_score(BBB_LM1_OPT2d)
graph2(training$Score1,BBB_LM1_OPT2d)

#####################################################################
# BESTOFF
#####################################################################

Kappa_score(BB_GNET1_OPT1d)
Kappa_score(BB_GAM1_OPT1d)
Kappa_score(BBB_LM1_OPT1d)
Kappa_score(BB_GNET1_OPT2d)
Kappa_score(BB_GAM1_OPT2d)
Kappa_score(BBB_LM1_OPT2d)

BBB_BESTOFF1_23p<-rep(0,nrow(training))
tmp<-data.frame(BB_GNET1_OPT1d,BB_GAM1_OPT1d,BBB_LM1_OPT1d,BB_GNET1_OPT2d,BB_GAM1_OPT2d,BBB_LM1_OPT2d)
tmp2<-data.frame(Kappa_score(BB_GNET1_OPT1d),Kappa_score(BB_GAM1_OPT1d),Kappa_score(BBB_LM1_OPT1d),
                 Kappa_score(BB_GNET1_OPT2d),Kappa_score(BB_GAM1_OPT2d),Kappa_score(BBB_LM1_OPT2d))
tmp2$max<-pmax(tmp2[,1],tmp2[,2],tmp2[,3],tmp2[,4],tmp2[,5],tmp2[,6])
for (i in 1:10) BBB_BESTOFF1_23p[training$EssaySet==i]<-tmp[training$EssaySet==i,which(tmp2[i,1:6]==tmp2[i,7])]  
Store(BBB_BESTOFF1_23p)

Spear_score(BBB_BESTOFF1_23p)
graph1b(BBB_BESTOFF1_23p)
Kappa_score(BBB_BESTOFF1_23p)
graph2(training$Score1,BBB_BESTOFF1_23p)
