require(ggplot2)
require(pscl)
require(MASS)
require(boot)
require(splines)

efficacy_data<-read.csv("C:\\Users\\sreek\\Documents\\bayer\\Scenario\\efficacy.csv",header = T)
subject_data<-read.csv("C:\\Users\\sreek\\Documents\\bayer\\Scenario\\subject.csv",header=T)
randomized_data<-read.csv("C:\\Users\\sreek\\Documents\\bayer\\Scenario\\randomization.csv",header=T)

first_merge=merge(efficacy_data,subject_data)
merged_data=merge(first_merge,randomized_data)

ds<-merged_data[,c("nosebleeds","mucus.viscosity","tissue.use","country",'previous.year','arm')]
bs$tissue.use=as.numeric(bs$tissue.use)
bs$country=as.numeric(bs$country)
summary( zingp<-zeroinfl(nosebleeds~bs(mucus.viscosity,df=3)+ tissue.use+country+previous.year*arm|bs(mucus.viscosity,df=3)+ tissue.use+country+previous.year*arm,data=ds,dist = "poisson"))
AIC(zingp)
zingp$coefficients
summary(p1 <- glm(nosebleeds ~ bs(mucus.viscosity,df=3,knots=c(0,3,5))+ tissue.use+country+previous.year*arm, family = poisson, data = ds))
summary.glm(p1)$dispersion
vuong(p1,zingp)

### finding chi square for goodness of test ###

pchisq(p1$deviance, df=p1$df.residual, lower.tail=FALSE)


### find p and lambda values 
prob<-predict(zingp,type='zero')
lamb_da<-predict(zingp,type='count')
ifelse(rbinom(444, size = 1, prob = prob) > 0, 0, rpois(444, lambda = lamb_da))




