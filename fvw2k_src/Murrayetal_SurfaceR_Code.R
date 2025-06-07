
---- 
  title: "A new approach to identify heat treated silcrete"
  author: "John K. Murray"
  date: "10.15.2020"
-----
    
library(gridExtra)
library(ggplot2)

###Read files
dat <- read.csv("Murrayetal_SurfaceRoughness_RAW.csv", header = TRUE)
dat2 <- na.omit(dat)
peelsurf <- read.csv("Murrayetal_Silicon_Peel_Test.csv", header = TRUE)
dat3 <- read.csv("Murrayetal_SiliconPeels.csv", header = TRUE)

############################################################################
#####Comparing Surface Roughness Measurements of Experimental Data##########
###########################################################################



#Creating boxplots for each surface roughness variable

psq <- ggplot(dat2, aes(x = Treatment, y = Sq )) +
  geom_boxplot(fatten = 2, lwd = 1, notch = TRUE, outlier.colour = 'red', fill = c('darkorange','navajowhite2')) + theme_classic()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.title.y=element_blank()) + ggtitle("Sq")+theme(plot.title = element_text(hjust = .5))

psq

psku <- ggplot(dat2, aes(x = Treatment, y = Sku )) +
  geom_boxplot(fatten = 2, lwd = 1, notch = TRUE, outlier.colour = 'red', fill = c('darkorange','navajowhite2')) + theme_classic()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.title.y=element_blank()) + ggtitle("Sku")+theme(plot.title = element_text(hjust = .5))

psku

pssk <- ggplot(dat2, aes(x = Treatment, y = Ssk)) +
  geom_boxplot(fatten = 2, lwd = 1, notch = TRUE, outlier.colour = 'red', fill = c('darkorange','navajowhite2')) + theme_classic()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.title.y=element_blank()) + ggtitle("Ssk")+theme(plot.title = element_text(hjust = .5))

pssk

psv <- ggplot(dat2, aes(x = Treatment, y = Sv )) +
  geom_boxplot(fatten = 2, lwd = 1, notch = TRUE, outlier.colour = 'red', fill = c('darkorange','navajowhite2')) + theme_classic()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.title.y=element_blank()) + ggtitle("Sv")+theme(plot.title = element_text(hjust = .5))

psv


p <- ggplot(dat2, aes(x = Treatment, y = Sp )) +
  geom_boxplot(fatten = 2, lwd = 1, notch = TRUE, outlier.colour = 'red', fill = c('darkorange','navajowhite2')) + theme_classic()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.title.y=element_blank()) + ggtitle("Sp")+theme(plot.title = element_text(hjust = .5))

p

p2 <- ggplot(dat2, aes(x = Treatment, y= Sz, fill = Treatment)) +
  geom_boxplot(fatten = 2, lwd = 1, notch = TRUE, outlier.colour = 'red',fill = c('darkorange','navajowhite2')) + theme_classic() + theme(legend.position = 'none') +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank()) + ggtitle("Sz")+theme(plot.title = element_text(hjust = .5))
p2


p3 <- ggplot(dat2, aes(x = Treatment, y= Sa, fill = Treatment)) +
  geom_boxplot(aes(Treatment, Sa),fatten = 2, lwd = 1, notch = TRUE, outlier.colour = 'red') + theme_classic() +scale_fill_manual(values=c('darkorange','navajowhite2'))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank()) + ggtitle("Sa")+theme(plot.title = element_text(hjust = .5))+theme(legend.position=c(1.5,.5))+scale_color_manual(values = c('darkorange','navajowhite2'))
p3

#Visualizing all boxplots in one window
windows(width=10,height=6)
grid.arrange(psq,psku,pssk,psv,p,p2,p3, ncol = 4)



#################################################################
#####Comparing Surface Roughness Measurements of Peels##########
################################################################




#Creating boxplots for each surface roughness variable for the silicon peel dataset
peelsq<- ggplot(dat3, aes(x = Treatment, y = Sq)) +
  geom_boxplot(fatten = 2, lwd = 1, notch = FALSE, outlier.colour = 'red', fill = c('darkorange','navajowhite2')) + theme_classic()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.title.y=element_blank()) + ggtitle("Sq")+theme(plot.title = element_text(hjust = .5))

peelsq

peelsku <- ggplot(dat3, aes(x = Treatment, y = Sku )) +
  geom_boxplot(fatten = 2, lwd = 1, notch = FALSE, outlier.colour = 'red', fill = c('darkorange','navajowhite2')) + theme_classic()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.title.y=element_blank()) + ggtitle("Sku")+theme(plot.title = element_text(hjust = .5))

peelsku

peelssk <- ggplot(dat3, aes(x = Treatment, y = Ssk)) +
  geom_boxplot(fatten = 2, lwd = 1, notch = FALSE, outlier.colour = 'red', fill = c('darkorange','navajowhite2')) + theme_classic()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.title.y=element_blank()) + ggtitle("Ssk")+theme(plot.title = element_text(hjust = .5))

peelssk

peelsv <- ggplot(dat3, aes(x = Treatment, y = Sv )) +
  geom_boxplot(fatten = 2, lwd = 1, notch = FALSE, outlier.colour = 'red', fill = c('darkorange','navajowhite2')) + theme_classic()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.title.y=element_blank()) + ggtitle("Sv")+theme(plot.title = element_text(hjust = .5))

peelsv


peelsp <- ggplot(dat3, aes(x = Treatment, y = Sp )) +
  geom_boxplot(fatten = 2, lwd = 1, notch = FALSE, outlier.colour = 'red', fill = c('darkorange','navajowhite2')) + theme_classic()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.title.y=element_blank()) + ggtitle("Sp")+theme(plot.title = element_text(hjust = .5))

peelsp

peelsz <- ggplot(dat3, aes(x = Treatment, y= Sz, fill = Treatment)) +
  geom_boxplot(fatten = 2, lwd = 1, notch = FALSE, outlier.colour = 'red',fill = c('darkorange','navajowhite2')) + theme_classic() + theme(legend.position = 'none') +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank()) + ggtitle("Sz")+theme(plot.title = element_text(hjust = .5))
peelsz


peelsa <- ggplot(dat3, aes(x = Treatment, y= Sa, fill = Treatment)) +
  geom_boxplot(aes(Treatment, Sa),fatten = 2, lwd = 1, notch = FALSE, outlier.colour = 'red') + theme_classic() +scale_fill_manual(values=c('darkorange','navajowhite2'))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank()) + ggtitle("Sa")+theme(plot.title = element_text(hjust = .5))+theme(legend.position=c(1.5,.5))+scale_color_manual(values = c('darkorange','navajowhite2'))
peelsa

#Visualizing all boxplots for each surface roughness variable in one window
windows(width=10,height=6)
grid.arrange(peelsq,peelsku,peelssk,peelsv,peelsp,peelsz,peelsa, ncol = 4)


##################################################################
#####Comparing Surface Roughness Measurements of Sources##########
#################################################################

#Creating boxplots for selected surface roughness variables across sources

sourcesa <- ggplot(dat2, aes(x = Treatment, y=Sa, fill= Treatment)) + facet_wrap(~Source) +
geom_boxplot(fatten = 2, lwd = 1, notch = FALSE, outlier.colour = 'red') +scale_fill_manual(values=c('darkorange','navajowhite2'))+ 
  theme_classic() +   theme(legend.position= "none")+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) + theme(plot.title = element_text(hjust = .5))+scale_color_manual(values = c('darkorange','navajowhite2'))
sourcesa


sourcesp <- ggplot(dat2, aes(x = Treatment, y=Sp, fill= Treatment)) + facet_wrap(~Source) +
  geom_boxplot(fatten = 2, lwd = 1, notch = FALSE, outlier.colour = 'red') +scale_fill_manual(values=c('darkorange','navajowhite2'))+ 
  theme_classic() + theme(legend.position= "none") +theme(strip.background = element_blank()) + theme(strip.text = element_blank())+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) + theme(plot.title = element_text(hjust = .5))+theme(legend.position=c(1.5,.5))+scale_color_manual(values = c('darkorange','navajowhite2'))
sourcesp

sourcesz <- ggplot(dat2, aes(x = Treatment, y=Sz, fill= Treatment)) + facet_wrap(~Source) + 
  geom_boxplot(fatten = 2, lwd = 1, notch = FALSE, outlier.colour = 'red') +scale_fill_manual(values=c('darkorange','navajowhite2'))+ 
  theme_classic() + theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) + theme(plot.title = element_text(hjust = .5))+
  theme(legend.position= "bottom")+scale_color_manual(values = c('darkorange','navajowhite2')) +
  theme(strip.background = element_blank()) + theme(strip.text.x = element_blank()) + theme(strip.text = element_blank())
sourcesz

#Visualizing all boxplots for each source in one window.
windows(width=10,height=10)
grid.arrange(sourcesa,sourcesp,sourcesz, ncol = 1)


###############################################################################
#####Comparing silicon peel measurements to direct surface measurements########
###############################################################################


#Creating boxplot for comparison 

peelsurfplot <- ggplot(peelsurf, aes(x = Peel, y=Sa, fill= Peel))  +
  geom_boxplot(fatten = 2, lwd = 1, notch = FALSE, outlier.colour = 'red') +scale_fill_manual(values=c('gray','gray'))+ 
  theme_classic() +   theme(legend.position= "none")+ theme(axis.title.x=element_blank(), axis.ticks.x=element_blank())
peelsurfplot


#Creating subsets of data to conduct T-test
pls <- peelsurf$Sa[which(peelsurf$Peel=="Peel")]
srf <- peelsurf$Sa[which(peelsurf$Peel=="Surface")]


#T-test showing these values are not-significant
t.test(pls,srf)


#######################################
#####Logistic Regression Model#########
#######################################


#Creating null model and a logistic regression model based on all variables
lr.null <- glm(Treatment ~ 1, data = dat2, family = binomial(link="logit"))

lrm <- glm(Treatment ~ Sq + Ssk + Sku + Sp + Sv + Sz + Sa,data = dat2, family = binomial(link="logit"))

summary(lrm)


#Step testing the model to determine the model with the lowest AIC
step(lr.null, scope = list(upper=lrm), direction = "both", data = dat2)

#Model using variables with lowest AIC
lrm2 <- glm(Treatment ~ Ssk + Sku + Sp + Sz + Sa,data = dat2, family = binomial(link="logit"))
summary(lrm2)

#Model using suggested variables, but SSK removed as it was not a significant predictor
lrm3 <- glm(Treatment ~ Sku + Sp + Sz + Sa, data = dat2, family = binomial(link="logit"))
summary(lrm3)

#Models including and excluding SSK are not statistically different
anova(lrm2, lrm3, test = "LRT")


####Calculating Pseudo-McFaddens Rho - Thank you to Matt Peeples for providing us with this code######

mf.rho <- function (x){
  temp <- anova(x)
  iLL <- (temp[1,4])/-2
  LL <- logLik(x)[1]
  rho.out <- 1-(LL/iLL)
  return(rho.out)}

mf.rho(lrm3)
