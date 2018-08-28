###########################################################################################
##
## R source code to accompany Hoffman, Perretta, and Smith (2018), last updated 26 October 2017.
## Please contact Ava Hoffman (avamariehoffman@gmail.com) with questions.
##
## If you found this code useful, please cite the accompanying paper. Thank you! :)
## 
##
###########################################################################################

## SET YOUR WORKING DIRECTORY TO WHEREVER YOU HAVE DOWNLOADED ACCOMPANYING FILES
wd <- '<your path here>'
setwd(wd)
library(ggplot2)

###########################################################################################
## This code examines the difference in palatability. Code also generates Fig. 4b
###########################################################################################

hopper_data <- read.csv("preference.csv",header=T)
ggplot(hopper_data, aes(x=breed, y=C.N_diff, color=grasshopper_mass)) + geom_boxplot() +geom_jitter(size=1.5) + theme_bw() ## quick look at how the palatability differs

library(rstan)
options(mc.cores = parallel::detectCores())

grasshopper_model <- "
data{
int N; //number of datapoints, should correspond to total observations
vector[N] y; //response variable
vector[N] M; //covariate, in this case, grasshopper mass
}

parameters{
real b0; //intercept parameter
real<lower=0> sd_y; ////variance parameter for response variable
real c; //parameter for grasshopper mass
}

transformed parameters{
vector[N] yhat;
yhat = b0 + c*M;
}

model{
y ~ normal(yhat,sd_y);
//priors
//b0 ~ normal(0, 1); //weakly informative - assumption that ecological effect sizes are centered on zero - switch between this and the prior below to test effect of different prior
b0 ~ normal(0, 100); //very uninformative - switch between this and prior above
//c ~ normal(0, 1); //weakly informative
c ~ normal(0, 100); //very uninformative
sd_y ~ cauchy(0, 2);
}

generated quantities{
real data_mean;
real modeled_mean;
real modeled_mean_yhat;
modeled_mean_yhat = (yhat[1] + yhat[2] + yhat[3] + yhat[4] + yhat[5]) /5 ;
modeled_mean = b0 + c*((M[1]+M[2]+M[3]+M[4]+M[5])/5);
data_mean = (y[1] + y[2] +y[3] +y[4] +y[5]) /5 ;
}
"

comp = stan_model(model_code = grasshopper_model,model_name = 'Grasshopper Model')

library(coda)
stan2coda <- function(fit) { ## function to turn JAGS objects to Stan workable objects.. use Stan summary function instead if desired
  mcmc.list(lapply(1:ncol(fit), function(x)
    mcmc(as.array(fit)[,x,])))
}
library(mcmcplots)

#############################################################
#aboveground productivity
modeldat1 = list('N' = nrow(hopper_data),
                 'y' = hopper_data$C.N_diff, ##difference in mass consumed in cultivar over native
                 'M' = hopper_data$grasshopper_mass)
fit1 = sampling(comp,data = modeldat1,iter = 300000,chains = 4)
coda1 = stan2coda(fit1)
summ_coda = summary(coda1);summ_coda
#heidel.diag(coda1)  ## uncomment for convergence diagnostics


plotdat <- rbind(coda1[,1][[1]],coda1[,1][[2]],coda1[,1][[3]],coda1[,1][[4]])
plot(density(plotdat)) ## appears converged

ggdata <- data.frame() ## data for plotting
for(i in 1:15){
  ggdata[(i),1] <- summ_coda$statistics[i,1]
  ggdata[(i),2] <- summ_coda$quantiles[i,1]
  ggdata[(i),3] <- summ_coda$quantiles[i,5]
}
ghopper_labs <- row.names(summ_coda$statistics[1:15,])
ggdata <- cbind(ggdata,ghopper_labs)
justmeandata <- ggdata[15,]
labelplot <- expression(paste("Difference ",hat(w)))
pdf(file="preference.pdf",height=1.5,width=4)
ggplot(justmeandata, aes(y=ghopper_labs, x=V1, group=ghopper_labs, color=ghopper_labs)) +
  xlab("Mass consumed difference (mg)") +
  ylab("Preference") +
  geom_line(size=1) +
  theme_classic() +
  geom_point(pch=16,size=3) +
  scale_color_manual(values=c("black")) +
  scale_y_discrete(labels = labelplot)+
  theme(legend.position="none") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(aes(xmin=V2, xmax=V3), size=1)
dev.off()
