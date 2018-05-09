setwd("/Users/avahoffman/Documents/CSU/Research/Mentoring/Holly")
hopper_water <- read.csv("water_weight_data.csv",header=T)

part <- hopper_water[!(hopper_water$type == "n-old"),]
part$ldmc <- part$dry / (part$wet /1000)
summary(lm(wet~dry + type, data=part))

c.part <- part[(part$type == 'c'),];hist(c.part$ldmc)
mean(c.part$ldmc);mean(c.part$ldmc)-(3*sd(c.part$ldmc));mean(c.part$ldmc)+(3*sd(c.part$ldmc))
n.part <- part[(part$type == 'n'),];hist(n.part$ldmc)
mean(n.part$ldmc);mean(n.part$ldmc)-(3*sd(n.part$ldmc));mean(n.part$ldmc)+(3*sd(n.part$ldmc))


library(ggplot2)

#pdf(file="waterweight.pdf",height=4,width=4)
ggplot(part, aes(x=wet, y=dry, color=type)) + 
  xlab("Wet mass (mg)") +
  ylab("Dry mass (mg)") +
  geom_point() + 
  theme_classic() +
  geom_smooth(method=lm, se=F) +
  scale_color_manual(values=c("springgreen3","darkgreen"),labels=c("Cultivar","Native")) +
  theme(legend.title = element_blank()) +
  theme(legend.position= c(.8,.2)) +
  theme(legend.background = element_rect(size=0.5, linetype="solid",colour="black"))
#dev.off()


library(rstan)
library(nlme)
library(graphics)
options(mc.cores = parallel::detectCores())

water_model <- "
data{
int N; //list of data
vector[N] y; //whatever the response variable is
vector[N] D; //dry mass
vector[N] T; //type
}

parameters{
real b0; //intercept param
real b1; //param for dry mass
real b2; //param for type  
real<lower=0> rate;
}

transformed parameters{
vector[N] yhat;
vector<lower=0>[N] theta;
vector<lower=0>[N] shape;

yhat = b0 + b1*D + b2*T; //
theta = exp(yhat); //
shape = theta*rate;
}

model{
y ~ gamma( shape , rate ); 
//priors
b0 ~ normal(0, 100); //weak
b1 ~ normal(0, 100);
b2 ~ normal(0, 100);
rate ~ cauchy(0, 2);
}

generated quantities{
}
"

comp = stan_model(model_code = water_model,model_name = 'Model')

library(coda)
stan2coda <- function(fit) { #got this function off the internet. Will turn stan object into a coda object.
  mcmc.list(lapply(1:ncol(fit), function(x)
    mcmc(as.array(fit)[,x,])))
}
library(mcmcplots)

#############################################################
#aboveground productivity
modeldat1 = list('N' = nrow(part),
                 'y' = part$dry,
                 'D' = part$wet, #nate said I should revers wet and dry.. so that's why the model annotations may not make sense
                 'T' = as.numeric(part$type) - 1)
fit1 = sampling(comp,data = modeldat1,iter = 150000,chains = 4)
coda1 = stan2coda(fit1)
summ_coda = summary(coda1);summ_coda


