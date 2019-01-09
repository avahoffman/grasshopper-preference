###########################################################################################
##
## R source code to accompany Hoffman, Perretta, Lemoine, and Smith (2019), last updated 14 December 2018.
## Please contact Ava Hoffman (avamariehoffman@gmail.com) with questions.
##
## If you found this code useful, please cite the accompanying paper. Thank you! :)
## 
##
###########################################################################################

## SET YOUR WORKING DIRECTORY TO WHEREVER YOU HAVE DOWNLOADED ACCOMPANYING FILES
wd <- '<your path here>'
setwd(wd)
setwd("/Users/avahoffman/Dropbox/Research/Mentoring/Holly/final_data")
library(ggplot2)

###########################################################################################
## This code examines the difference in palatability. Code also generates prelim. figures
###########################################################################################

hopper_data <- read.csv("palatability.csv",header=T)

## quick check for outliers
boxplot(diff ~ breed, data=hopper_data)
boxplot.stats(hopper_data$diff)$out

## appears to be one outlier - impute the mean for 'wild type' plants
outlier_value <- boxplot.stats(hopper_data$diff)$out 
hopper_data$diff[(hopper_data$diff == outlier_value)] <- NA ## replace with NA first
n_mean <- mean(na.omit(hopper_data$diff[(hopper_data$breed == 'W')])) ## calculate the mean
hopper_data$diff[(is.na(hopper_data$diff))] <- n_mean

## test for 'type/breed' effect (frequentist)
summary(lm(diff~ breed + grasshopper_mass, data=hopper_data))
ggplot(hopper_data, aes(x=container_type, y=diff, color=container_type)) + geom_boxplot() +geom_jitter(size=1.5) + theme_bw() ## quick look at how the palatability differs

ggplot(hopper_data) + geom_density(aes(x=diff,fill=breed),alpha=0.5) + theme_classic() +
  xlab("Mass consumed (mg)") +
  scale_fill_manual(values=c("springgreen3","darkgreen"),labels=c("Cultivar","Wild type"))

library(rstan)
options(mc.cores = parallel::detectCores())


grasshopper_model <- "
data {
int<lower=0> N; // number of observations
int<lower=0> J; // number of types : 2, cultivar and wild type
vector[N] y; // amount consumed
vector[N] x; // grasshopper mass
int type[N]; // cultivar or wild type
real m_x; // mean grasshopper mass
vector[N] mass; // wet mass
real m_m; // mean wet mass
}
parameters {
real<lower=0> sigma_a;
vector[J] a;
vector[J] b;
vector[J] c;
vector<lower=0>[J] sigma;
real mu_a;
}
transformed parameters{
vector[N] mu;
mu = a[type] + b[type].*x + c[type].*mass; 
}
model {
//priors
mu_a ~ normal(0,10);
sigma ~ cauchy(0,10);
//model
a ~ normal(mu_a, sigma_a);
b ~ normal(0,100);
c ~ normal(0,100);
y ~ normal(mu, sigma[type]);
}
generated quantities{
vector[N] draws1;
vector[J] palat;
vector[1] diff;
for(n in 1:N)
for(j in 1:J){
draws1[n] = normal_rng(mu[n], sigma[j]); //posterior draws
}
for(j in 1:J){
palat[j] = a[j] + b[j].*m_x + c[j].*m_m;
}
diff[1] = palat[1] - palat[2];
}
"


comp = stan_model(model_code = grasshopper_model,model_name = 'Grasshopper Model')

#############################################################
model_data = list(
  'N' = nrow(hopper_data),
  'J' = 2, 
  'y' = hopper_data$diff,
  'type' = as.numeric(as.factor(hopper_data$breed)),
  'x' =  hopper_data$grasshopper_mass,
  'm_x' = mean(hopper_data$grasshopper_mass),
  'mass' = hopper_data$wet_initial,
  'm_m' = mean(hopper_data$wet_initial)
)
## sampling
fit1 = sampling(
  comp,
  data = model_data,
  iter = 100000,
  warmup = 100000 / 2,
  thin = 1,
  chains = 2
)
summary_fit <- summary(fit1)
save(summary_fit,file="Palatability.output.R")
load("Palatability.output.R")

## plot posterior draws to ensure a good fit.
library(bayesplot)
list_of_draws <- extract(fit1)
yrep <- list_of_draws$draws1
ppc_dens_overlay(hopper_data$diff, yrep[1:500, ]) + 
  theme_minimal(base_size = 20)
  
## extract data for plotting
model_stats1 <-
  cbind(summary_fit$summary[, 4, drop = F],
        summary_fit$summary[, 8, drop = F],
        summary_fit$summary[, 1, drop = F],
        summary_fit$summary[, 10, drop = F])

  
#ggdata <- as.data.frame(model_stats1[89:90,])
ggdata <- as.data.frame(model_stats1[91:92,])
containerlabs <- c("Cultivar","Wild type")
ggdata <- cbind(ggdata,containerlabs)
cult_lab <- expression(paste("Cultivar"))
nat_lab <-  expression(paste("Wild type"))
ggplot(ggdata, aes(y=containerlabs, x=mean, group=containerlabs, color=containerlabs)) +
  xlab("Mass consumed (mg)") +
  ylab("Palatability") +
  geom_line(size=1) +
  theme_classic() +
  geom_point(pch=16,size=3) +
  scale_color_manual(values=c("springgreen3","darkgreen")) +
  theme(legend.position="none") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(aes(xmin=`2.5%`, xmax=`97.5%`), size=1)+
  scale_y_discrete(labels = c(cult_lab, nat_lab)) 
ggsave(file="palatability.jpg",height=3,width=4)


