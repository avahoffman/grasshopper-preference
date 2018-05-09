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

s_df <- read.csv("Silica.csv",header=T)
summary(lm(silica_percent~type,data=s_df)) ## test for 'type' effect (frequentist)

library(rstan)
options(mc.cores = parallel::detectCores())

designmatrix=model.matrix(~as.factor(type), data=s_df) ## generate model matrix for stan to use - also option to do a simple intercept and regression model since there are only two parameters
print(designmatrix, file="output_matrix", sep='\t')

Silica_model <- "
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

yhat =  exp(X*B); //designmatrix * parameter B, loglink function
shape = yhat .* yhat / phi;
rate = yhat / phi;
}

model{
y ~ gamma(shape,rate); 
//priors
B ~ normal(0, 100); //vague
rate ~ cauchy(0, 15); //flat
}

generated quantities{
vector[2] groupmean; // 2 group (type) means
vector[1] difference;

groupmean[1] = exp(B[1]) ;     // Cultivar
groupmean[2] = exp(B[1] + B[2]) ;     // Native
difference[1] = groupmean[2] - groupmean[1] ;

}
"

comp = stan_model(model_code = Silica_model,model_name = 'Silica_Model')

library(coda)
stan2coda <- function(fit) { ## function to turn JAGS objects to Stan workable objects.. use Stan summary function instead if desired
  mcmc.list(lapply(1:ncol(fit), function(x)
    mcmc(as.array(fit)[,x,])))
}
## incorporate data into the model
modeldat1 = list('N' = nrow(s_df),
                 'y' = s_df$silica_percent, 
                 'X' = designmatrix,
                 'K' = ncol(designmatrix) )
iter=300000
fit1 = sampling(comp,data = modeldat1, iter = iter, warmup = iter/2, thin = 1, chains = 4)
coda1 = stan2coda(fit1)
summ_coda = summary(coda1);summ_coda
#heidel.diag(coda1) ## uncomment for convergence diagnostics
#summary(fit1) ## Stan object

plotdat <- rbind(coda1[,34][[1]],coda1[,34][[2]],coda1[,34][[3]],coda1[,34][[4]])
plot(density(plotdat)) ## appears converged


ggdata <- data.frame() ## data for plotting
for(i in 34:35){
  ggdata[(i-33),1] <- summ_coda$statistics[i,1]
  ggdata[(i-33),2] <- summ_coda$quantiles[i,1]
  ggdata[(i-33),3] <- summ_coda$quantiles[i,5]
}
containerlabs <- c("Cultivar","Native")
ggdata <- cbind(ggdata,containerlabs)
pdf(file="silica.pdf",height=3,width=4)
cult_lab <- expression(paste("Cultivar ",hat(u)))
nat_lab <-  expression(paste("Native ",hat(u)))
ggplot(ggdata, aes(y=containerlabs, x=V1, group=containerlabs, color=containerlabs)) +
  xlab("% Silica") +
  ylab("") +
  geom_line(size=1) +
  theme_classic() +
  geom_point(pch=16,size=3) +
  scale_color_manual(values=c("springgreen3","darkgreen")) +
  theme(legend.position="none") +
  geom_errorbarh(aes(xmin=V2, xmax=V3), size=1)+
  scale_y_discrete(labels = c(cult_lab, nat_lab)) 
dev.off()

