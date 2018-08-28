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

hopper_data <- read.csv("palatability.csv",header=T)
summary(lm(diff~container_type + grasshopper_mass, data=hopper_data)) ## test for 'type' effect (frequentist)
ggplot(hopper_data, aes(x=container_type, y=diff, color=container_type)) + geom_boxplot() +geom_jitter(size=1.5) + theme_bw() ## quick look at how the palatability differs

library(rstan)
options(mc.cores = parallel::detectCores())

designmatrix=model.matrix(~as.factor(container_type), data=hopper_data) ## generate model matrix for stan to use - also option to do a simple intercept and regression model since there are only two parameters
print.table(designmatrix, file="output_matrix", sep='\t') ## view the designmatrix

grasshopper_model <- "
data{
int N; //number of datapoints, should correspond to total observations
int K; //number of parameters (column number of design matrix)
vector[N] y; //response variable
vector[N] M; //covariate, in this case, grasshopper mass
matrix[N, K] X; //X is the design matrix
}

parameters{
vector[K] B; //B parameter for each column of the design matrix
real<lower=0> sd_y; //variance parameter for response variable
real c; //parameter for grasshopper mass
}

transformed parameters{
vector[N] yhat;
yhat =  X*B + c*M ; //designmatrix * parameter B
}

model{
y ~ normal(yhat,sd_y);
//priors
B ~ normal(0, 100);
c ~ normal(0, 100); 
sd_y ~ cauchy(0, 2);
}

generated quantities{
vector[2] groupmean; // 2 group (type) means

groupmean[1] = ( B[1] + (c*M[3] + c*M[4] + c*M[7] + c*M[9] + c*M[11])/5 ) ;             // Cultivar - intercept
groupmean[2] = ( B[1] + B[2] + (c*M[2] + c*M[5] + c*M[8] + c*M[10] + c*M[13])/5 ) ;     // Native

}
"

comp = stan_model(model_code = grasshopper_model,model_name = 'Grasshopper Model')

library(coda) ## function to turn JAGS objects to Stan workable objects.. use Stan summary function instead if desired
stan2coda <- function(fit) {
  mcmc.list(lapply(1:ncol(fit), function(x)
    mcmc(as.array(fit)[,x,])))
}
## incorporate data into the model
modeldat1 = list('N' = nrow(hopper_data),
                 'y' = hopper_data$diff, 
                 'X' = designmatrix, 
                 'M' = hopper_data$grasshopper_mass,
                 'K' = ncol(designmatrix) )
fit1 = sampling(comp,data = modeldat1,iter = 300000,chains = 4)
coda1 = stan2coda(fit1)
summ_coda = summary(coda1);summ_coda
#heidel.diag(coda1) ## uncomment for convergence diagnostics
#summary(fit1) ## Stan object

plotdat <- rbind(coda1[,1][[1]],coda1[,1][[2]],coda1[,1][[3]],coda1[,1][[4]])
plot(density(plotdat)) ## appears converged


ggdata <- data.frame() ## data for plotting
for(i in 45:46){
  ggdata[(i-44),1] <- summ_coda$statistics[i,1]
  ggdata[(i-44),2] <- summ_coda$quantiles[i,1]
  ggdata[(i-44),3] <- summ_coda$quantiles[i,5]
}
containerlabs <- c("Cultivar","Native")
ggdata <- cbind(ggdata,containerlabs)
pdf(file="palatability.pdf",height=3,width=4)
cult_lab <- expression(paste("Cultivar ",hat(x)))
nat_lab <-  expression(paste("Native ",hat(x)))
ggplot(ggdata, aes(y=containerlabs, x=V1, group=containerlabs, color=containerlabs)) +
  xlab("Mass consumed (mg)") +
  ylab("Palatability") +
  geom_line(size=1) +
  theme_classic() +
  geom_point(pch=16,size=3) +
  scale_color_manual(values=c("springgreen3","darkgreen")) +
  theme(legend.position="none") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(aes(xmin=V2, xmax=V3), size=1)+
  scale_y_discrete(labels = c(cult_lab, nat_lab)) 
dev.off()

