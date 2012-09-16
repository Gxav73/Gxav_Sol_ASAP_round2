Gxav_Sol_ASAP_round2
====================

1.  Install R 2.15.1 (2012-06-22) -- "Roasted Marshmallows". Note previous versions could produce slightly different results because of some changes in the mgcv package
2.	Open R
3.	Store external data files, training set and test set files in a directory 
4.	Select as a R working directory the directory where the files were stored
5.	Ensure all packages are installed
a.	SOAR # to manage memory usage
b.	Hmisc and MASS # for descriptive analysis
c.	Matrix # to handle matrix  
d.	RTextTools # to stem words
e.	tau # to produce n-grams
f.	tm # to remove duplicated blanks and punctuation marks
g.	clim.pact # to convert to lower case
h.	glmnet # to train a generalized linear model via penalized maximum likelihood
i.	glmnetcr # to train a multinomial generalized linear model via penalized maximum likelihood
j.	e1071 # to train Support Vector Machine
k.	gbm # to train Gradient Boosting Machine 
l.	randomForest # to train Random Forest
m.	mgcv # to train Generalized Additive Models
6.	Execute Gxav TRAINING CODE.R
7.	Execute Gxav TEST CODE.R
8.	1 csv file should be generated “Gxav_Sol.csv” equal to the solution selected for the private leaderboard of the competition
