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
library(ggplot2)

###########################################################################################
## This code examines the difference in plant weight over time to determine optimal acclimation
## time for plants entering the palatability or preference assays. This code generates and 
## accompanies Fig. 1. 
###########################################################################################

cd1=c(150.8,130.5,148.5,108.4,40.9) ## differences in mass between time point 1 and 2, for cultivar
cd2=c(67.9,6.4,24.6,8,9.7) ## differences in mass between time point 2 and 3... and so on
cd3=c(11,11.5,40.6,6.3,8.1)
cd4=c(5.7,-18.8,0.9,-4.2,-8.8)
cd5=c(-42.6,7,20,12.7,4.2)

t.test(cd1,cd2,paired=T) ## differ
t.test(cd2,cd3,paired=T) ## do not differ
t.test(cd3,cd4,paired=T) ## differ
t.test(cd4,cd5,paired=T) ## do wot differ - mass difference has not changed between time 4 & 5 versus 5 & 6

wd1=c(22,6.4,23.7,-36.9,-42.1) ## differences in mass between time point 1 and 2, for native
wd2=c(-6.7,0.8,-19.6,49.3,8.5) ## differences in mass between time point 2 and 3... and so on
wd3=c(5.5,0.7,-8.5,-35.8,-16.8)
wd4=c(2.2,3,7.8,-2.6,0.7)
wd5=c(26.4,7.4,7.5,-45,-48)

t.test(wd1,wd2,paired=T) ## do not differ
t.test(wd2,wd3,paired=T) ## do not differ
t.test(wd3,wd4,paired=T) ## do not differ
t.test(wd4,wd5,paired=T) ## do not differ - mass difference has changed little for wild type, but since cultivar needs adjustment,
  ## both types will be acclimated

startdata <- rbind(cd1,cd2,cd3,cd4,cd5,wd1,wd2,wd3,wd4,wd5) ## combine data for plotting
ggdata <- data.frame()
for(i in 1:10){
  ggdata[i,1] <- mean(startdata[i,]) ## mean
  ggdata[i,2] <- (mean(startdata[i,])) - qt(0.975,df=4)*sd(startdata[i,])/sqrt(4) ## lower 95% interval bound
  ggdata[i,3] <- (mean(startdata[i,])) + qt(0.975,df=4)*sd(startdata[i,])/sqrt(4) ## upper 95% interval bound
}
names(ggdata) <- c("mean","lower95%","upper95%") ; ggdata
times <- c("1","2","3","4",'5',"1","2","3","4",'5') 
labs <- c("c","c","c","c",'c',"w","w","w","w",'w')
ggdata <- cbind(ggdata,times,labs)
ggdata$times <- as.factor(as.character(ggdata$times))

ggplot(ggdata, aes(y=mean, x=times, group=times, color=labs)) +
  xlab("Time point") +
  ylab("Mass change (mg)") +
  theme_classic() +
  geom_point(pch=16,size=3) +
  scale_color_manual(values=c("springgreen3","darkgreen"),labels=c("Cultivar","Wild type")) +
  scale_x_discrete(labels = c("0-30","30-60","60-90","90-120","120-150")) +
  theme(legend.position=c(.8,.8)) +
  geom_hline(yintercept = 0) +
  theme(legend.title = element_blank()) +
  geom_errorbar(aes(ymin=`lower95%`, ymax=`upper95%`), size=1) +
  theme(legend.background = element_rect(size=0.5, linetype="solid",colour="black"))
ggsave(file="water_acclimation.jpg",height=4,width=4)
