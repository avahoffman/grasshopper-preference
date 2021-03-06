###########################################################################################
##
## R source code to accompany Hoffman, Perretta, Lemoine, and Smith (2019), last updated 22 Feb 2019.
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
## This code examines the difference in traits between cultivar and wild type.
## generates figure 2.
###########################################################################################

hopper_water <- read.csv("water_weight_data.csv",header=T)
part <- hopper_water[!(hopper_water$type == "w-old"),]

# calculate scaling equations
cultivated <- subset(part, type == "c")
wt <- subset(part, type == "w")
summary(lm(dry~wet, data=cultivated))
summary(lm(dry~wet, data=wt))

part$ldmc <- part$dry / (part$wet /1000)
c1 <- subset(part, type == "c")
n=w1 <- subset(part, type == "w")

## contrasting LDMC
shapiro.test(c1$ldmc)
shapiro.test(w1$ldmc)
wilcox.test( c1$ldmc , w1$ldmc)
## contrasting Mass
shapiro.test(c1$dry)
shapiro.test(w1$dry)
wilcox.test( c1$dry , w1$dry)

cn_df <- read.csv("CN.csv",header=T)
cn_df$CNratio <- cn_df$X.C / cn_df$X.N
summary(lm(CNratio~type,data=cn_df)) ## test for 'type' effect (frequentist)
c2 <- subset(cn_df, type == "cultivar")
w2 <- subset(cn_df, type == "wild type")

## contrasting C:N
shapiro.test(c2$CNratio)
shapiro.test(w2$CNratio)
wilcox.test( c2$CNratio , w2$CNratio)

s_df <- read.csv("Silica.csv",header=T)
summary(lm(silica_percent~type,data=s_df)) ## test for 'type' effect (frequentist)
c3 <- subset(s_df, type == "cultivar")
w3 <- subset(s_df, type == "wild type")

## contrasting Silica
shapiro.test(c3$silica_percent)
shapiro.test(w3$silica_percent)
wilcox.test( c3$silica_percent , w3$silica_percent)

d1 <- part[,c(3,4)] ; names(d1)[2] <- "trait" ; d1$cat <- rep("2LDMC",nrow(d1)) ; d1$lett <- rep("b",nrow(d1))
d2 <- part[,c(3,2)] ; names(d2)[2] <- "trait" ; d2$cat <- rep("1Mass",nrow(d2)) ; d2$lett <- rep("a",nrow(d2))
d3 <- cn_df[,c("type","CNratio")] ; d3$type <- gsub("cultivar","c",d3$type) ; d3$type <- gsub("wild type","w",d3$type) ; names(d3)[2] <- "trait"  ; d3$cat <- rep("3CN",nrow(d3)) ; d3$lett <- rep("c",nrow(d3))
d4 <- s_df[,c("type","silica_percent")] ; d4$type <- gsub("cultivar","c",d4$type) ; d4$type <- gsub("wild type","w",d4$type) ; names(d4)[2] <- "trait" ; d4$cat <- rep("4Silica",nrow(d4)) ; d4$lett <- rep("d",nrow(d4))

d1$x <- c("2.2",rep(NA,(nrow(d1)-1))) ; d1$y <- c("340",rep(NA,(nrow(d1)-1)))
d2$x <- c("2.2",rep(NA,(nrow(d2)-1))) ; d2$y <- c("250",rep(NA,(nrow(d2)-1)))
d3$x <- c("2.2",rep(NA,(nrow(d3)-1))) ; d3$y <- c("22",rep(NA,(nrow(d3)-1)))
d4$x <- c("2.2",rep(NA,(nrow(d4)-1))) ; d4$y <- c("4.1",rep(NA,(nrow(d4)-1)))

ggdat <- rbind( d1, d2, d3, d4)
ggdat$x <- as.numeric(ggdat$x) ; ggdat$y <- as.numeric(ggdat$y) 


trait_names <- list(
  '2LDMC'=expression(paste("DMC (mg ",g^-1,")")),
  '1Mass'="Dry mass (mg)",
  '3CN'="C:N",
  '4Silica'="Silica %"
)
trait_labeller <- function(variable,value){
  return(trait_names[value])
}



ggplot(ggdat, aes(y=trait, x=type, group=type, color=type)) +
  xlab(NULL) +
  ylab("Trait value") +
  theme_classic() +
  geom_boxplot(outlier.size = 0, alpha=1) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  scale_color_manual(values=c("springgreen3","darkgreen")) +
  theme(legend.position="none") +
  facet_wrap(~cat, nrow=4, scales = "free_y", labeller = trait_labeller) +
  scale_x_discrete(labels = c("Cultivar","Wild type")) +
  geom_text( mapping = aes(x = x, y = y, label = lett),
             hjust   = -0.1,
             vjust   = -1,
             color = "black",
             size=10
  )
ggsave(file="Traits.jpg",height=8,width=3)
