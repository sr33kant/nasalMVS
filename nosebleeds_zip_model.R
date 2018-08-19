require(ggplot2)
require(pscl)
require(MASS)
require(boot)


efficacy_data<-read.csv("C:\\Users\\sreek\\Documents\\bayer\\Scenario\\efficacy.csv",header = T)
subject_data<-read.csv("C:\\Users\\sreek\\Documents\\bayer\\Scenario\\subject.csv",header=T)
randomized_data<-read.csv("C:\\Users\\sreek\\Documents\\bayer\\Scenario\\randomization.csv",header=T)

first_merge=merge(efficacy_data,subject_data)
merged_data=merge(first_merge,randomized_data)

ds<-merged_data[,c("nosebleeds","mucus.viscosity","tissue.use","country",'previous.year','arm')]
bs$tissue.use=as.numeric(bs$tissue.use)
bs$country=as.numeric(bs$country)
summary( zingp<-zeroinfl(nosebleeds~.|.,data=ds,dist = "poisson"))
summary(p1 <- glm(nosebleeds ~ mucus.viscosity + tissue.use+country, family = poisson, data = ds))

vuong(p1,zingp)

### finding chi square for goodness of test ###

pchisq(p1$deviance, df=p1$df.residual, lower.tail=FALSE)