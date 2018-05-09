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
## This code examines the difference in plant weight over time to determine optimal acclimation
## time for plants entering the palatability or preference assays. This code generates and 
## accompanies Fig. 1. 
###########################################################################################

cd1=c(150.8,130.5,148.5,108.4,40.9) ## differences in mass between time point 1 and 2, for cultivar
cd2=c(67.9,6.4,24.6,8,9.7) ## differences in mass between time point 2 and 3... and so on
cd3=c(11,11.5,40.6,6.3,8.1)
cd4=c(5.7,-18.8,0.9,-4.2,-8.8)
cd5=c(-42.6,7,20,12.7,4.2)

t.test(cd1,cd2) ## differ
t.test(cd2,cd3) ## do not differ
t.test(cd3,cd4) ## differ
t.test(cd4,cd5) ## do not differ - mass difference has not changed between time 4 & 5 versus 5 & 6

nd1=c(22,6.4,23.7,-36.9,-42.1) ## differences in mass between time point 1 and 2, for native
nd2=c(-6.7,0.8,-19.6,49.3,8.5) ## differences in mass between time point 2 and 3... and so on
nd3=c(5.5,0.7,-8.5,-35.8,-16.8)
nd4=c(2.2,3,7.8,-2.6,0.7)
nd5=c(26.4,7.4,7.5,-45,-48)

t.test(nd1,nd2) ## do not differ
t.test(nd2,nd3) ## do not differ
t.test(nd3,nd4) ## do not differ
t.test(nd4,nd5) ## do not differ - mass difference has changed little for native, but since cultivar needs adjustment,
  ## both types will be acclimated

startdata <- rbind(cd1,cd2,cd3,cd4,cd5,nd1,nd2,nd3,nd4,nd5) ## combine data for plotting
ggdata <- data.frame()
for(i in 1:10){
  ggdata[i,1] <- mean(startdata[i,]) ## mean
  ggdata[i,2] <- (mean(startdata[i,])) - qt(0.975,df=4)*sd(startdata[i,])/sqrt(4) ## lower 95% interval bound
  ggdata[i,3] <- (mean(startdata[i,])) + qt(0.975,df=4)*sd(startdata[i,])/sqrt(4) ## upper 95% interval bound
}
names(ggdata) <- c("mean","lower95%","upper95%") ; ggdata
times <- c("1","2","3","4",'5',"1","2","3","4",'5') 
labs <- c("c","c","c","c",'c',"n","n","n","n",'n')
ggdata <- cbind(ggdata,times,labs)
ggdata$times <- as.factor(as.character(ggdata$times))

pdf(file="water_acclimation.pdf",height=4,width=4)
ggplot(ggdata, aes(y=mean, x=times, group=times, color=labs)) +
  xlab("Time point") +
  ylab("Mass change (mg)") +
  theme_classic() +
  geom_point(pch=16,size=3) +
  scale_color_manual(values=c("springgreen3","darkgreen"),labels=c("Cultivar","Native")) +
  scale_x_discrete(labels = c("0-30","30-60","60-90","90-120","120-150")) +
  theme(legend.position=c(.8,.8)) +
  geom_hline(yintercept = 0) +
  theme(legend.title = element_blank()) +
  geom_errorbar(aes(ymin=`lower95%`, ymax=`upper95%`), size=1) +
  theme(legend.background = element_rect(size=0.5, linetype="solid",colour="black"))
dev.off()