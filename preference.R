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
## This code examines the difference in palatability. Code also generates Fig. 1
###########################################################################################

hopper_data <- read.csv("preference.csv",header=T)
ggplot(hopper_data, aes(x=breed, y=diff, color=grasshopper_mass)) + geom_boxplot() +geom_jitter(size=1.5) + theme_bw() ## quick look at how the palatability differs
summary(lm(diff~breed + grasshopper_mass, data=hopper_data)) ## test for grasshopper mass effect (frequentist)

library(rstan)
options(mc.cores = parallel::detectCores())


grasshopper_model <- "
data {
int<lower=0> N; // number of observations
int<lower=0> J; // number of types : 2, cultivar and native
vector[N] y; // amount consumed
vector[N] x; // grasshopper mass
int type[N]; // cultivar or native
real m_x; // mean grasshopper mass
}
parameters {
real<lower=0> sigma_a;
vector[J] a;
vector[J] b;
vector<lower=0>[J] sigma;
real mu_a;
}
transformed parameters{
vector[N] mu;
mu = a[type] + b[type].*x; 
}
model {
//priors
mu_a ~ normal(0,10);
sigma ~ cauchy(0,10);
//model
a ~ normal(mu_a, sigma_a);
b ~ normal(0,100);
y ~ normal(mu, sigma[type]);
}
generated quantities{
vector[N] draws1;
vector[J] palat;
vector[1] pref;
for(n in 1:N)
for(j in 1:J){
draws1[n] = normal_rng(mu[n], sigma[j]); //posterior draws
}
for(j in 1:J){
palat[j] = a[j] + b[j].*m_x;
}
pref[1] = palat[1] - palat[2];
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
  'm_x' = mean(hopper_data$grasshopper_mass)
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
save(summary_fit, file="Preference.output.R")
load("Preference.output.R")

## plot posterior draws to ensure a good fit. Looks okay but not great.
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


ggdata <- as.data.frame(model_stats1[51:52,])
ggdata <- ggdata[-2,]
labelplot <- expression(paste("Cultivar -\n Native"))
pdf(file="preference.pdf",height=1.5,width=4)
ggplot(ggdata, aes(y=rownames(ggdata), x=mean)) +
  xlab("Mass consumed difference (mg)") +
  ylab("Preference") +
  geom_line(size=1) +
  theme_classic() +
  geom_point(pch=16,size=3) +
  scale_color_manual(values=c("black")) +
  scale_y_discrete(labels = labelplot)+
  theme(legend.position="none") +
  geom_vline(xintercept = 0) +
  geom_errorbarh(aes(xmin=`2.5%`, xmax=`97.5%`), size=1)
dev.off()

#############################################################
## combine palatability and preference on one plot
## must run palatability code first!

load("Palatability.output.R") 
model_stats1 <-
  cbind(summary_fit$summary[, 4, drop = F],
        summary_fit$summary[, 8, drop = F],
        summary_fit$summary[, 1, drop = F],
        summary_fit$summary[, 10, drop = F])


ggdata1 <- as.data.frame(model_stats1[89:90,])

load("Preference.output.R")

model_stats2 <-
  cbind(summary_fit$summary[, 4, drop = F],
        summary_fit$summary[, 8, drop = F],
        summary_fit$summary[, 1, drop = F],
        summary_fit$summary[, 10, drop = F])


ggdata2 <- as.data.frame(model_stats2[90:91,]) ; ggdata2 <- ggdata2[-1,]

ggdat <- rbind(ggdata1,ggdata2) ; ggdat$facet.var <- c(rep("Palatability",2),"Preference") ; ggdat$type <- c("Cultivar","Native","Cultivar - \nNative") ; ggdat$lett <- c("a",NA,"b")
ggdat$x <- c(20,NA,33) ; 
ggdat$y <- c(2,NA,1.2)

ggplot(ggdat, aes(y=type, x=mean, color=type)) +
  xlab("Mass consumed (mg)") +
  ylab(NULL) +
  geom_line(size=1) +
  theme_classic() +
  geom_point(pch=16,size=3) +
  scale_color_manual(values=c("springgreen3","black","darkgreen")) +
  theme(legend.position="none") +
  geom_vline(xintercept = 0, lty=3) +
  facet_wrap(~facet.var,scales = "free", nrow=2) +
  geom_errorbarh(aes(xmin=`2.5%`, xmax=`97.5%`), size=1) +
  geom_text( mapping = aes(x = x, y = y, label = lett),
    hjust   = -0.1,
    vjust   = -1,
    color = "black",
    size=8
  )
ggsave(file="palatability.preference.jpg",height=5,width=3)


