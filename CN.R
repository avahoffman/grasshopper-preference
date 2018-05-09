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
## This code examines the difference in palatability. Code also generates Fig. 4a
###########################################################################################

cn_df <- read.csv("CN.csv",header=T)
cn_df$CNratio <- cn_df$X.C / cn_df$X.N
summary(lm(CNratio~type,data=cn_df)) ## test for 'type' effect (frequentist)

library(rstan)
options(mc.cores = parallel::detectCores())



designmatrix=model.matrix(~as.factor(type), data=cn_df) #assigns 1s vs 0s to diff treatments.. need this because they are numeric, but factors
print(designmatrix, file="output_matrix", sep='\t')

CN_model <- "
data{
int N; //number of datapoints, should correspond to total observations
int K; //number of parameters (column number of design matrix)
vector[N] y; //response variable
matrix[N, K] X; //X is the design matrix
}

parameters{
vector[K] B; //B parameter for each column of the design matrix
real<lower=0> phi;
}

transformed parameters{
vector[N] yhat;
vector<lower=0>[N] rate;
vector<lower=0>[N] shape;

yhat =  exp(X*B); //designmatrix * parameter B, loglink function because gamma distribution
shape = yhat .* yhat / phi;
rate = yhat / phi;
}

model{
y ~ gamma(shape,rate); 
//priors
B ~ normal(0, 100); 
rate ~ cauchy(0, 10);
}

generated quantities{
vector[2] groupmean; // 2 group (type) means
vector[1] difference;
groupmean[1] = exp(B[1]) ;     // Cultivar
groupmean[2] = exp(B[1] + B[2]) ;     // Native
difference[1] = groupmean[2] - groupmean[1] ;
}
"

comp = stan_model(model_code = CN_model,model_name = 'CN_Model')

library(coda) 
stan2coda <- function(fit) { ## function to turn JAGS objects to Stan workable objects.. use Stan summary function instead if desired
  mcmc.list(lapply(1:ncol(fit), function(x)
    mcmc(as.array(fit)[,x,])))
}
## incorporate data into the model
modeldat1 = list('N' = nrow(cn_df),
                 'y' = cn_df$CNratio, 
                 'X' = designmatrix,
                 'K' = ncol(designmatrix) )
iter=300000
fit1 = sampling(comp,data = modeldat1, iter = iter, warmup = iter/2, thin = 1, chains = 4)
coda1 = stan2coda(fit1)
summ_coda = summary(coda1);summ_coda
#heidel.diag(coda1) ## uncomment for convergence diagnostics

plotdat <- rbind(coda1[,1][[1]],coda1[,1][[2]],coda1[,1][[3]],coda1[,1][[4]])
plot(density(plotdat)) ## appears converged

ggdata <- data.frame() #only want the mean and 95% CI
for(i in 64:65){
  ggdata[(i-63),1] <- summ_coda$statistics[i,1]
  ggdata[(i-63),2] <- summ_coda$quantiles[i,1]
  ggdata[(i-63),3] <- summ_coda$quantiles[i,5]
}
containerlabs <- c("Cultivar","Native")
ggdata <- cbind(ggdata,containerlabs)
pdf(file="CN.pdf",height=3,width=4)
cult_lab <- expression(paste("Cultivar ",hat(u)))
nat_lab <-  expression(paste("Native ",hat(u)))
ggplot(ggdata, aes(y=containerlabs, x=V1, group=containerlabs, color=containerlabs)) +
  xlab("C:N ratio") +
  ylab("") +
  geom_line(size=1) +
  theme_classic() +
  geom_point(pch=16,size=3) +
  scale_color_manual(values=c("springgreen3","darkgreen")) +
  theme(legend.position="none") +
  geom_errorbarh(aes(xmin=V2, xmax=V3), size=1)+
  scale_y_discrete(labels = c(cult_lab, nat_lab)) 
dev.off()

