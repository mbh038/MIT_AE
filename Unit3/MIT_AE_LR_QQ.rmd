---
title: "MIT_AE_LR_QQ"
author: "mbh038"
date: "Tuesday, June 23, 2015"
output: html_document
---

##QQ2

```{r qq2}
beta0=-1.5
beta1=3
beta2=-0.5

x1=1
x2=5

P<-1/(1+exp(-(beta0+beta1*x1+beta2*x2))) # =P(y=1)
logit<-log(P/(1-P))
logit

odds<-exp(logit)
odds

P # =P(y=1)
```