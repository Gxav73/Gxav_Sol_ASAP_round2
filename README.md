Gxav_Sol_ASAP_round2
====================

0.  Unzip "Short_Answer_Gxav.zip". Read "Gxav Description.docx" to get familiar with the approach
1.  Install R 2.15.1 (2012-06-22) -- "Roasted Marshmallows". Note previous versions could produce slightly different results because of some changes in the mgcv package
2.	Open R
3.	Store external data files, training set and test set files in a directory 
4.	Select as a R working directory the directory where the files were stored
5.	Ensure all packages are installed
5a.	SOAR # to manage memory usage
5b.	Hmisc and MASS # for descriptive analysis
5c.	Matrix # to handle matrix  
5d.	RTextTools # to stem words
5e.	tau # to produce n-grams
5f.	tm # to remove duplicated blanks and punctuation marks
5g.	clim.pact # to convert to lower case
5h.	glmnet # to train a generalized linear model via penalized maximum likelihood
5i.	glmnetcr # to train a multinomial generalized linear model via penalized maximum likelihood
5j.	e1071 # to train Support Vector Machine
5k.	gbm # to train Gradient Boosting Machine 
5l.	randomForest # to train Random Forest
5m.	mgcv # to train Generalized Additive Models
6.	Execute Gxav TRAINING CODE.R
7.	Execute Gxav TEST CODE.R
8.	1 csv file should be generated “Gxav_Sol.csv” equal to the solution selected for the private leaderboard of the competition
