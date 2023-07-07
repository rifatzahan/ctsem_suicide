df <-  indata %>% select (ID, timeInStudy_hr, res3_Suicid, res1_Dep, res2_Irrit, res4_Connectd)

# Combined model (All variables) --------------------------------------

model_combined<-ctModel(type='stanct',
                        latentNames=c('Suicidality','Depression','Irritability', 'Connectedness'),
                        manifestNames=c('Suicidality','Depression','Irritability', 'Connectedness'),
                        LAMBDA=diag(4))


#set a narrow prior for the random scale
if(TRUE){ #or F
  model_combined$pars$indvarying <- TRUE
  model_combined$pars$sdscale[!model_combined$pars$matrix %in% c('T0MEANS','MANIFESTMEANS','CINT')] <- .05
}

#show model parameters
print(model_combined$pars)

ctModelLatex(model_combined)

fit_combined<-ctStanFit(datalong = df, ctstanmodel = model_combined, 
                        plot=10,verbose=0,cores=2,nopriors=F)



ctModelLatex(fit_combined)

#print the summary
ctsem:::ctSummarise(fit_combined, folder = 'frequency',cores=2,
                    lags = c(24, 72, 168),ctStanPlotPost = F,
                    nsamples = 10)

# Generating data from posterior mean
# Using 2/8 logical CPU cores
# starting worker pid=48035 on localhost:11719 at 22:50:26.497
# starting worker pid=48048 on localhost:11719 at 22:50:26.740
# Loading required package: data.table
# Loading required package: data.table
# Error in eval(parse(text = x), envir = globalenv()) : 
#   Exception: mdivide_left_spd: Matrix A is not positive definite  (in 'model_ctsmgen' at line 1100)
# 
# Error in eval(parse(text = x), envir = globalenv()) : 
#   Exception: mdivide_left_spd: Matrix A is not positive definite  (in 'model_ctsmgen' at line 1100)



plot(fit_combined)
ctStanContinuousPars(fit_combined)
ctStanDiscretePars(fit_combined,plot=T)

#PLOTTING THE MODEL OBSERVATIONS FOR SINGLE SUBJECTS
ctKalman(fit_combined,subjects=17:18,plot=T,kalmanvec=c('y','ysmooth'))#plot parameters for the first 4 subjects
ctKalman(fit_combined,subjects=4,plot=T,kalmanvec=c('y','ysmooth'))#plot parameters for subject n4

#EXTRACT INDIVIDUAL PARAMETERS#
?ctStanSubjectPars
indpars1 <- ctStanSubjectPars(fit_combined, cores = 2, nsamples = "all")[1,,]
indpars1
str(indpars1)
dimnames(indpars1)

#convert to a data-frame
ind.data1 <- data.frame(indpars1)
summary(ind.data1)

#check IDs
print(df$id)
fit_combined$setup$idmap
id<-data.frame(levels(df$id))
colnames(id)
ind.data1$ID<-as.factor(id$levels.data_combined.id.)
str(ind.data1)

#write a csv
write.csv(ind.data1, file="SubjectParametersCombined.csv")


#GRAPHS####
par(mfrow=c(2,2))
names(fit_combined)

#Distribution of continuous individual auto-effects
hist(ind.data1$drift_Suicidality,breaks=5,
     xlim=range (-1, 1), ylim=range(0, 30),freq=T,
     xlab= "Suicidality ", main=NA,ylab="n of participants")

hist(ind.data1$drift_Depression,breaks=5,
     xlim=range (-1, 1), ylim=range(0, 30),freq=T,
     xlab= "Depression ", main=NA,ylab="n of participants")

hist(ind.data1$drift_Irritability,breaks=5,
     xlim=range (-1, 1), ylim=range(0, 30),freq=T,
     xlab= "Irritability ", main=NA,ylab="n of participants")

hist(ind.data1$drift_Connectedness,breaks=5,
     xlim=range (-1, 1), ylim=range(0, 30),freq=T,
     xlab= "Connectedness ", main=NA,ylab="n of participants")


#Distribution of continuous individual cross-lagged effects
hist(ind.data1$drift_Depression_Suicidality,breaks=5,
     xlim=range (-1, 1), ylim=range(0, 30),freq=T,
     xlab= "Suicidality --> Depression", main=NA,ylab="n of participants")


