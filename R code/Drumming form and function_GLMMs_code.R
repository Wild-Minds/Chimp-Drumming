
#Model 1: Do age, rank, context and group size influence use of drumming by chimpanzees?----
xdata=read.table(file="Drumming form and function_Model1.csv", header=T, sep=",", stringsAsFactors=T, dec=".") 

##Check data
str(xdata) 

##Check if data balanced
table(xdata$focal)

##Check frequencies of response drums 
table(xdata$drum, xdata$focal) 
table(xdata$drum, xdata$context) 
tablecontext=table(xdata$drum, xdata$context) 
write.table(tablecontext, file="table contexts.csv", row.names=T, col.names=T, sep="\t")
table(xdata$drum, xdata$period) 
table(xdata$drum, xdata$rank) 
table(xdata$drum, xdata$age) 
table(xdata$drum, xdata$group_size) 

##Check rank, age and group-size distributions
hist(xdata$rank)
hist(xdata$age) 
hist(xdata$group_size) 
str(xdata)

##Sorting categorical data 
xdata$context=relevel(xdata$context, ref="Resting") #relevel context for Resting as baseline
levels(xdata$context)

xdata$period=as.character(xdata$period=="2") #convert period to character
table(xdata$drum, xdata$period)
str(xdata)


##Choose random slopes
source("~/Desktop/R courses/Mundry R course 2020/course material/functions/diagnostic_fcns.r") #Function provided by Roger Mundry

xx.fe.re=fe.re.tab(fe.model="drum ~ age + rank + context + group_size + period",
                   re="(1|focal)", data=xdata) #this function will remove NAs and dummy code variables and return a list with 2 components: summary and data;
                                                #summary was used to choose random slopes as indicated by Roger Mundry

xx.fe.re$summary #include rs for context, group size, rank, age, period within focal 

##Center factors for random slopes
t.data=xx.fe.re$data 
str(t.data)
t.data$context.Feeding=t.data$context.Feeding-mean(t.data$context.Feeding) 
t.data$context.Display=t.data$context.Display-mean(t.data$context.Display)
t.data$context.Traveling=t.data$context.Traveling-mean(t.data$context.Traveling)
t.data$period.TRUE=t.data$period.TRUE-mean(t.data$period.TRUE) 

##Ztransform covariates to include in model
t.data$z.rank=as.vector(scale(t.data$rank))
t.data$z.age=as.vector(scale(t.data$age))
t.data$z.group_size=as.vector(scale(t.data$group_size))
str(t.data) 


##Fit model
library(lme4)

contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)) #used optimizer "bobyqa" for model convergence
full=glmer(drum ~ z.age + z.rank + z.group_size + period + context + 
             (1+z.age+z.rank+z.group_size+period.TRUE+context.Display+context.Feeding+context.Traveling|focal),
           data=t.data, family=binomial, control=contr) #model converged but Singularity message: may indicate that random intercepts and slopes may be correlated 

summary(full)$varcor #check correlations

ll.old=logLik(full) #stored ll.old for comparison below with ll of full model without correlations

full.wc=glmer(drum ~ z.age + z.rank + z.group_size + period + context + 
                (1+z.age+z.rank+z.group_size+period.TRUE+context.Display+context.Feeding+context.Traveling||focal),
              data=t.data, family=binomial, control=contr)  #fit model without correlations: converged but still isSingular message

round(ll.old, 3); logLik(full.wc) #ll don't vary much without correlations, so we can exclude the correlations

full=full.wc


##Assumptions
###Model stability
source("~/Desktop/R courses/Mundry R course 2020/course material/functions/glmm_stability.r") #function by Roger Mundry
m.stab=glmm.model.stab(model.res=full, contr=contr) 
round(m.stab$summary[, -1], 3) 
m.stab.plot(m.stab$summary[, -1]) 

###Collinearity
library(car)
xx=lm(drum ~ z.age + z.rank + context + z.group_size + period, 
      data=t.data) #run LM for VIF
vif(xx) 


##Full null model comparison
null=glmer(drum ~ period +
             (1+z.age+z.rank+z.group_size+period.TRUE+context.Display+context.Feeding+context.Traveling||focal),
           data=t.data, family=binomial, control=contr) 
as.data.frame(anova(null, full, test="Chisq")) 


##Results
###Coefficients
round(summary(full)$coefficients, 3) 
tests=as.data.frame(drop1(full, test="Chisq"))

library(multcomp) #for contexts p-values
cftest(full)

exp(fixef(full)["(Intercept)"])/
  (1+exp(fixef(full)["(Intercept)"])) #inverse logit transf: 19.2% is prob to drum at all preds being 0 

exp(fixef(full)["contextDisplay"])/
  (1+exp(fixef(full)["contextDisplay"])) #98.6% prob increase in drumming while displaying compared to resting

exp(fixef(full)["contextTraveling"])/
  (1+exp(fixef(full)["contextTraveling"])) #87% prob increase in drumming while traveling compared to resting

exp(fixef(full)["z.group_size"])/
  (1+exp(fixef(full)["z.group_size"])) #as number of individuals in party increase prob to drum decreases by 29.4% 


##CIs
source("~/Desktop/R courses/Mundry R course 2020/course material/functions/boot_glmm.r") #Function provided by Roger Mundry
boot.res=boot.glmm.pred(model.res=full, excl.warnings=F,
                        nboots=1000, para=F)
cis=round(boot.res$ci.estimates, 3)


##Effect sizes
library(MuMIn)
full.rsq=glmer(drum ~ z.age + z.rank + z.group_size + period + context + 
                 (1|focal)+(0+z.age|focal)+(0+z.rank|focal)+(0+z.group_size|focal)+(0+period.TRUE|focal)+(0+context.Display|focal)+(0+context.Feeding|focal)+(0+context.Traveling|focal),
               data=t.data, family=binomial, control=contr)  #to make r.sq work used 'double pipe' syntax for random slopes part of model 
r.sq=r.squaredGLMM(object=full.rsq) 


##Plot effect of group_size on frequency of drumming (Figure 6)
###Prepare data
xx=aggregate(x=xdata$drum, by=list(xdata$group_size), FUN=mean) 
xx
xx$N=aggregate(x=xdata$drum, by=list(xdata$group_size), FUN=length)$x 
xx


###Plot data
par(mar=c(3, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), las=1, tcl=-0.15, bty= "l") 
plot(x=xx$Group.1, y=xx$x, las=1, ylim=c(0,1),
     col=("black"), pch=19,
     xlab="group size", ylab="probability of drumming", xaxs=("r"), yaxs=("r"), cex=0.5*sqrt(xx$N))  

###Add model line and CIs in plot
plot.res.gs=glmer(drum ~ z.age + z.rank + z.group_size + period.TRUE + context.Display + context.Feeding + context.Traveling +
                    (1+z.age+z.rank+z.group_size+period.TRUE+context.Display+context.Feeding+context.Traveling||focal),
                  data=t.data, family=binomial, control=contr) #to plot all contexts and both periods use centered contexts and period

plot.xvals=seq(from=min(t.data$group_size), to=max(t.data$group_size), length.out=22) 

source("~/Desktop/R courses/Mundry R course 2020/course material/functions/boot_glmm.r") #use function provided
boot.res.plot.gs=boot.glmm.pred(model.res=plot.res.gs, 
                                excl.warnings=F, nboots=1000, para=F, resol=22, level=0.95, use="z.group_size") #get cis per z.group.size 
round(boot.res.plot.gs$ci.estimates, 3)

lines(x=plot.xvals, y=boot.res.plot.gs$ci.predicted$fitted, lty=2) #Add model line
lines(x=plot.xvals, y=boot.res.plot.gs$ci.predicted$lower.cl, lty=3) #Add lower ci
lines(x=plot.xvals, y=boot.res.plot.gs$ci.predicted$upper.cl, lty=3) #Add upper ci
dev.copy2pdf(file="plot groupsize-drumming.pdf", out.type = "pdf")


##Plot effect of context on frequency of drumming (Figure 7)
###Prepare data
xx=aggregate(x=xdata$drum, by=list(xdata$context), FUN=mean)
xx
xx$N=aggregate(x=xdata$drum, by=list(xdata$context), FUN=length)$x 
xx

###Plot data
plot.res.cx=glmer(drum ~ z.age + z.rank+z.group_size + period.TRUE + context +
                    (1+z.age+z.rank+z.group_size+period.TRUE+context.Display+context.Feeding+context.Traveling||focal),
                  data=t.data, family=binomial, control=contr) #to plot both periods use centered period

barplot(xx$x) 
par(mar=c(3, 3, 0.5, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1, lwd=3) 
barplot(xx$x, col="white", ylim=c(0, 1), yaxs="i", ylab="probability of drumming") 
x.at=barplot(xx$x, col="white", ylim=c(0, 1), xaxs=("r"), yaxs=("r"), ylab="probability of drumming") 
x.at
mtext(text=xx$Group.1, side=1, line=0.2, at=x.at) 

###Add model lines in plot
coefs=fixef(plot.res.cx) #extract coeffs from model
coefs
fv=rep(NA, times=4) #create vector with 4 entries for fixed values calculations for 4 contexts
fv[1]=coefs["(Intercept)"] #calculate fitted values for each context (here Resting is baseline context)
fv[2]=coefs["(Intercept)"]+coefs["contextDisplay"]
fv[3]=coefs["(Intercept)"]+coefs["contextFeeding"]
fv[4]=coefs["(Intercept)"]+coefs["contextTraveling"]
fv 
fv=exp(fv)/(1+exp(fv)) #convert fixed values into probabilities
fv

hll=mean(diff(x.at[, 1]))/4 
segments(x0=x.at-hll, x1=x.at+hll, y0=fv, y1=fv, lwd=3) 

###Add CIs to plot
source("~/Desktop/R courses/Mundry R course 2020/course material/functions/boot_glmm.r") #function provided by Roger Mundry
boot.res.plot.cx=boot.glmm.pred(model.res=plot.res.cx, 
                                excl.warnings=F, nboots=1000, para=F, resol=4, level=0.95, use="context") 
arrows(x0=x.at, x1=x.at, y0=boot.res.plot.cx$ci.predicted$lower.cl, y1=boot.res.plot.cx$ci.predicted$upper.cl, code=3, len=0.1, angle=90) 
dev.copy2pdf(file="plot context-drumming_final.pdf", out.type = "pdf")



#Model 2: Does presence of females in oestrous and preferred social partners affect use of drumming by chimpanzees?----
xdata=read.table(file="Drumming form and function_Model2.csv", header=T, sep=",", stringsAsFactors=T, dec=".") 

##Check data
str(xdata) 

##Check if data balanced
table(xdata$focal) 

##Check frequencies of response drums 
table(xdata$drum, xdata$focal) 

##Check frequencies of variables x drums
table(xdata$drum, xdata$estr_females) 
table(xdata$drum, xdata$psp) 
table(xdata$drum, xdata$context) 
table(xdata$drum, xdata$rank) 
table(xdata$psp, xdata$estr_females)

##Sorting categorical data 
xdata$estr_females=as.character(xdata$estr_females=="1") 
table(xdata$drum, xdata$estr_females)

xdata$psp=as.character(xdata$psp=="1") 
table(xdata$drum, xdata$psp)

xdata$context=relevel(xdata$context, ref="Resting") #relevel context with Resting as baseline
levels(xdata$context)
str(xdata)


##Choose random slopes
source("~/Desktop/R courses/Mundry R course 2020/course material/functions/diagnostic_fcns.r") #Function by Roger Mundry

xx.fe.re=fe.re.tab(fe.model="drum ~ estr_females + psp + rank + context + group_size",
                   re="(1|focal)", data=xdata)

xx.fe.re$summary #include all random slopes

##Center factors for random slopes
t.data=xx.fe.re$data

t.data$estr_females.TRUE=t.data$estr_females.TRUE-mean(t.data$estr_females.TRUE)
t.data$psp.TRUE=t.data$psp.TRUE-mean(t.data$psp.TRUE)

t.data$context.Feeding=t.data$context.Feeding-mean(t.data$context.Feeding)
t.data$context.Display=t.data$context.Display-mean(t.data$context.Display)
t.data$context.Traveling=t.data$context.Traveling-mean(t.data$context.Traveling)
str(t.data)

##Ztransform covariates to include in model
t.data$z.rank=as.vector(scale(t.data$rank))
t.data$z.group_size=as.vector(scale(t.data$group_size))
str(t.data) 


##Fit model
library(lme4)

full2=glmer(drum ~ estr_females + psp + z.rank + context + z.group_size +
              (1+estr_females.TRUE+psp.TRUE+z.rank+z.group_size+context.Display+context.Feeding+context.Traveling|focal),
            data=t.data, family=binomial, control=contr) #Again AsSingular message indicating correlation between random intercepts and slopes

round(summary(full2)$coefficients, 3)
summary(full2)$varcor #some close to 1 and -1

full2.wc=glmer(drum ~ estr_females + psp + z.rank + context + z.group_size +
                 (1+estr_females.TRUE+psp.TRUE+z.rank+z.group_size+context.Display+context.Feeding+context.Traveling||focal),
               data=t.data, family=binomial, control=contr) #Fit model without correlations

logLik(full2) #ll don't vary much
logLik(full2.wc)

full=full2.wc


##Assumptions
###Model stability
source("~/Desktop/R courses/Mundry R course 2020/course material/functions/glmm_stability.r") #Function provided by Roger 
m.stab=glmm.model.stab(model.res=full) 
round(m.stab$summary[, -1], 3)
m.stab.plot(m.stab$summary[, -1]) 

###Collinearity
library(car)
xx=lm(drum ~ estr_females + psp + z.rank + z.group_size + context,
      data=t.data) #run LM for VIF
vif(xx) 


##Full null model comparison
null=glmer(drum ~ z.rank + context + z.group_size +
             (1+estr_females.TRUE+psp.TRUE+z.rank+z.group_size+context.Display+context.Feeding+context.Traveling||focal),
           data=t.data, family=binomial, control=contr) 
as.data.frame(anova(null, full, test="Chisq")) #Non-significant


#Model 3: Does presence of higher-ranking males affect use of drumming by chimpanzees?----
xdata=read.table(file="Drumming form and function_Model3.csv", header=T, sep=",", stringsAsFactors=T, dec=".") 

##Check data
str(xdata) 

##Check if data balanced
table(xdata$focal) 

##Check frequencies of response drums 
table(xdata$drum, xdata$focal) 

##Check frequencies of variables x drum
table(xdata$drum, xdata$hrm) 
table(xdata$drum, xdata$context) 
table(xdata$drum, xdata$rank) 

##Sorting categorical data
xdata$hrm=as.character(xdata$hrm=="1") 
table(xdata$drum, xdata$hrm)

xdata$context=relevel(xdata$context, ref="Resting") #relevel context for Resting as baseline
levels(xdata$context)

str(xdata)


##Choose random slopes
source("~/Desktop/R courses/Mundry R course 2020/course material/functions/diagnostic_fcns.r") #Function by Roger Mundry

xx.fe.re=fe.re.tab(fe.model="drum ~ hrm + rank + context + group_size",
                   re="(1|focal)", data=xdata)

xx.fe.re$summary #include all random slopes

##Center factors for random slopes
t.data=xx.fe.re$data 
str(t.data)
t.data$hrm.TRUE=t.data$hrm.TRUE-mean(t.data$hrm.TRUE) 
t.data$context.Feeding=t.data$context.Feeding-mean(t.data$context.Feeding) 
t.data$context.Display=t.data$context.Display-mean(t.data$context.Display)
t.data$context.Traveling=t.data$context.Traveling-mean(t.data$context.Traveling)
str(t.data)

##Ztransform covariates (because glmms converges easier if covs ztrans)
t.data$z.rank=as.vector(scale(t.data$rank))
t.data$z.group_size=as.vector(scale(t.data$group_size))
str(t.data) 


##Fit model
library(lme4)

contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)) #used optimizer for model convergence
full=glmer(drum ~ hrm + z.rank + context + z.group_size +
             (1+hrm.TRUE+z.rank+z.group_size+context.Display+context.Feeding+context.Traveling|focal),
           data=t.data, family=binomial, control=contr) #converged but isSingular message indicating there might be correlation between random intercepts and slopes

summary(full)$varcor #some close to 1 and -1
full.wc=glmer(drum ~ hrm + z.rank + context + z.group_size +
                (1+hrm.TRUE+z.rank+z.group_size+context.Display+context.Feeding+context.Traveling||focal),
              data=t.data, family=binomial, control=contr)

logLik(full) 
logLik(full.wc)

full=full.wc 


##Assumptions
###Model stability
source("~/Desktop/R courses/Mundry R course 2020/course material/functions/glmm_stability.r") #Function provided by Roger 
m.stab=glmm.model.stab(model.res=full)
round(m.stab$summary[, -1], 3) 
m.stab.plot(m.stab$summary[, -1]) 

###Collinearity
library(car)
xx=lm(drum ~ hrm + z.rank + context + z.group_size,
      data=t.data) #run LM for VIF
vif(xx)


##Full null model comparison
null=glmer(drum ~ z.rank + context + z.group_size +
             (1+hrm.TRUE+z.rank+z.group_size+context.Display+context.Feeding+context.Traveling||focal),
           data=t.data, family=binomial, control=contr) 
as.data.frame(anova(null, full, test="Chisq")) #Non-significant


#ESM Model 4: Does presence of females in oestrous and preferred social partners affect use of drumming by chimpanzees?----
xdata=read.table(file="Drumming form and function_Model4.csv", header=T, sep=";", stringsAsFactors=T, dec=".") 

##Sorting categorical data 
xdata$period=as.character(xdata$period=="2") 
table(xdata$drum, xdata$period) 

xdata$estr_females=as.character(xdata$estr_females=="1") 
table(xdata$drum, xdata$estr_females)

xdata$psp=as.character(xdata$psp=="1") 
table(xdata$drum, xdata$psp)


##Choose random slopes
source("~/Desktop/Mundry R course 2020/course material/functions/diagnostic_fcns.r") 

xx.fe.re=fe.re.tab(fe.model="drum ~ estr_females + psp + period",
                   re="(1|focal)", data=xdata)

xx.fe.re$summary #include all random slopes

##Center factors for random slopes 
t.data=xx.fe.re$data 
str(t.data)
t.data$period.TRUE=t.data$period.TRUE-mean(t.data$period.TRUE) 
t.data$estr_females.TRUE=t.data$estr_females.TRUE-mean(t.data$estr_females.TRUE) 
t.data$psp.TRUE=t.data$psp.TRUE-mean(t.data$psp.TRUE) 
str(t.data)


##Fit model
library(lme4)

contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)) #"bobyqa" optimizer for model convergence
full=glmer(drum ~ estr_females + psp + period +
             (1+estr_females.TRUE+psp.TRUE+period.TRUE|focal),
           data=t.data, family=binomial, control=contr)  #model converged but isSingular message

summary(full)$varcor #check correlations

full.wc=glmer(drum ~ estr_females + psp + period +
                (1+estr_females.TRUE+psp.TRUE+period.TRUE||focal),
              data=t.data, family=binomial, control=contr) #model without correlations

logLik(full)
logLik(full.wc) #don't vary much so can remove correlations

full=full.wc 


##Assumptions
###Model stability
source("~/Desktop/Mundry R course 2020/course material/functions/glmm_stability.r") #Function provided by Roger Mundry
m.stab=glmm.model.stab(model.res=full) 
round(m.stab$summary[, -1], 3) 
m.stab.plot(m.stab$summary[, -1]) 

###Collinearity
library(car)
xx=lm(drum ~ estr_females + psp + period,
      data=xdata) #LM for VIF
vif(xx) 


##Full null model comparison
null=glmer(drum ~ period +
             (1+estr_females.TRUE+psp.TRUE+period.TRUE||focal),
           data=t.data, family=binomial, control=contr)
as.data.frame(anova(null, full, test="Chisq")) #Significant


##Results
round(summary(full)$coefficients, 3) 
tests=as.data.frame(drop1(full, test="Chisq")) 

exp(fixef(full)["(Intercept)"])/
  (1+exp(fixef(full)["(Intercept)"])) 

exp(fixef(full)["pspTRUE"])/
  (1+exp(fixef(full)["pspTRUE"])) #when psp are present probability to drum decreases by 24.5%  


##CIs
source("~/Desktop/R courses/Mundry R course 2020/course material/functions/boot_glmm.r") #use function provided for bootstrapped cis
boot.res=boot.glmm.pred(model.res=full, excl.warnings=F,
                        nboots=1000, para=F)
round(boot.res$ci.estimates, 3)


##Effect sizes
library(MuMIn)

full.rsq=glmer(drum ~ estr_females + psp + period +
                 (1|focal)+(0+estr_females.TRUE|focal)+(0+psp.TRUE|focal)+(0+period.TRUE|focal),
               data=t.data, family=binomial, control=contr) 
r.sq=r.squaredGLMM(object=full.rsq) 


#ESM Model 5: Does presence of higher-ranking males affect use of drumming by chimpanzees?----
xdata=read.table(file="Drumming form and function_Model5.csv", header=T, sep=";", stringsAsFactors=T, dec=".") 

##Sorting categorical data
xdata$period=as.character(xdata$period=="2") 
table(xdata$drum, xdata$period)

xdata$hrm=as.character(xdata$hrm=="1")
table(xdata$drum, xdata$hrm)
str(xdata)


##Choose random slopes
source("~/Desktop/Mundry R course 2020/course material/functions/diagnostic_fcns.r") 

xx.fe.re=fe.re.tab(fe.model="drum ~ hrm + period",
                   re="(1|focal)", data=xdata)

xx.fe.re$summary #include all random slopes

#Center factors for random slopes
t.data=xx.fe.re$data 
t.data$period.TRUE=t.data$period.TRUE-mean(t.data$period.TRUE) 
t.data$hrm.TRUE=t.data$hrm.TRUE-mean(t.data$hrm.TRUE) 
str(t.data)


##Fit model
library(lme4)

full=glmer(drum ~ hrm + period +
             (1+hrm.TRUE+period.TRUE|focal),
           data=t.data, family=binomial) #converged but isSingular message

summary(full)$varcor #check correlations 

full.wc=glmer(drum ~ hrm + period +
                (1+hrm.TRUE+period.TRUE||focal),
              data=t.data, family=binomial) #run model without period correlations

logLik(full) #ll don't vary much without corr, so can exclude correlations
logLik(full.wc)

full=full.wc


##Assumptions
###Model stability
source("~/Desktop/Mundry R course 2020/course material/functions/glmm_stability.r") #Function provided by Roger 
m.stab=glmm.model.stab(model.res=full) 
round(m.stab$summary[, -1], 3)
m.stab.plot(m.stab$summary[, -1])

###Collinearity
library(car)
xx=lm(drum ~ hrm + period,
      data=t.data) #LM for VIF
vif(xx)


##Full null model comparison
null=glmer(drum ~ period +
             (1+hrm.TRUE+period.TRUE||focal),
           data=t.data, family=binomial) 
as.data.frame(anova(null, full, test="Chisq")) #Significant 


##Results
round(summary(full)$coefficients, 3)
tests=as.data.frame(drop1(full, test="Chisq")) 

exp(fixef(full)["(Intercept)"])/
  (1+exp(fixef(full)["(Intercept)"]))  

exp(fixef(full)["hrmTRUE"])/
  (1+exp(fixef(full)["hrmTRUE"])) #when hrm present drumming prob decreases by 17.4%


##CIs
source("~/Desktop/Mundry R course 2020/course material/functions/boot_glmm.r") #Function provided by Roger Mundry
boot.res=boot.glmm.pred(model.res=full, excl.warnings=F,
                        nboots=1000, para=F)
round(boot.res$ci.estimates, 3)


##Effect sizes
library(MuMIn)

full.rsq=glmer(drum ~ hrm + period +
                 (1|focal)+(0+hrm.TRUE|focal)+(0+period.TRUE|focal),
               data=t.data, family=binomial) 
r.sq=r.squaredGLMM(object=full.rsq) 
