##### Combined model (All four measurements) #####

# create the model object
model_4measurements<-ctModel(type='stanct',
                       latentNames=c('Suicidality','Depression','Irritability', 'Connectedness'),
                       manifestNames=c('Suicidality','Depression','Irritability', 'Connectedness'),
                       LAMBDA=diag(4))


#set a narrow prior for the random scale
if(TRUE){ #or F
  model_4measurements$pars$indvarying <- TRUE
  model_4measurements$pars$sdscale[!model_4measurements$pars$matrix %in% c('T0MEANS','MANIFESTMEANS','CINT')] <- .05
}

#show model parameters
# print(model_4measurements$pars)
# generate model equation
# ctModelLatex(model_4measurements)

# create dataframe for the ctStanFit() object
df_4manifest <-  indata %>% select (id, time, Suicidality, Depression, Irritability, Connectedness)

fit_4manifest <- ctStanFit(datalong = df_4manifest, ctstanmodel = model_4measurements, 
                         plot=10,verbose=0,cores=2,nopriors=F)

# Save the model object
save(fit_4manifest, file = "fit_4manifest.RData")

# load("fit_4manifest.RData")

ctModelLatex(fit_4manifest)
summary.fit_4manifest <- summary(fit_4manifest)
summary.fit_4manifest$popmeans

# find correlation
corr_df <- as.data.frame(summary.fit_4manifest$rawpopcorr)
corr_df <- tibble::rownames_to_column(corr_df, "Parameter")
corr_df <- corr_df[grepl("^mm", corr_df$Parameter), ]
corr_df <- corr_df[!grepl("T0m", corr_df$Parameter), ]
rownames(corr_df) <- NULL

# Convert df to LaTeX code
latex_code <- xtable(corr_df)

# Print LaTeX code
print(latex_code, type = "latex", include.rownames=FALSE)

ctStanContinuousPars(fit_4manifest, plot = T)
ctStanDiscretePars(fit_4manifest,plot=T)

#PLOTTING THE MODEL OBSERVATIONS FOR SELECTED SUBJECTS
ctKalman(fit_4manifest,subjects=17:19,plot=T,kalmanvec=c('y','ysmooth'))#plot parameters for the selected subjects
ctKalman(fit_4manifest,subjects=4,plot=T,kalmanvec=c('y','ysmooth'))#plot parameters for subject#4

# check the chi-square test
ctChisqTest(fit_td_predc)

# get the fit of the model and log-likelihood
cf<-ctCheckFit(fit_4manifest, 
               postpred = TRUE,
               priorpred = TRUE,
               statepred = TRUE,
               residuals = TRUE)

# plot for diagnostic checking
plot(cf,wait=FALSE)

#compare randomly generated data from posterior to observed data
ctStanPostPredict(fit_4manifest, wait=FALSE) 

#calculate discrete time parameters
summary(fit_4manifest, timeinterval = 24)#1 day
summary(fit_4manifest, timeinterval = 72)#3 days
summary(fit_4manifest, timeinterval = 168)#1 week
summary(fit_4manifest, timeinterval = 336)#2 weeks

ctsem:::ctSummarise(fit_4manifest, folder = 'MODEL_4MANIFEST',cores=2,ctStanPlotPost = F,nsamples = 1000)
