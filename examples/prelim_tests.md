Preliminary polyreg tests
================
Bohdan Khomtchouk and Pete Mohanty
2/1/2018

``` r
library(partools)
library(nnet)
library(polyreg)      # version from github.com/rdrr1990/polyreg

getPE <- function(){
  data(prgeng)
  pe <- prgeng[,c(1,3,7:9)]
  # dummies for MS, PhD
  pe$ms <- as.integer(pe$educ == 14)
  pe$phd <- as.integer(pe$educ == 16)
  pe$educ <- NULL
  pe <<- pe
}
getPE() # code from partools, possibly on git version

rmse <- function(y, yhat){
  (mean((y - yhat)^2)^0.5)
}

loss <- matrix(nrow = 2, ncol = 4)
rownames(loss) <- c("Mean Abs Error", "Root Mean Squared Error")
colnames(loss) <- c("nnet", "lm", "plm2", "pl3")

pe1 <- scale(pe)
sdy <- attr(pe1,'scaled:scale')[3]
muy <- attr(pe1,'scaled:center')[3]
pe1 <- as.data.frame(pe1)
trnidxs <- sample(1:nrow(pe1),10000)
trn <- pe1[trnidxs,]
tst <- pe1[-trnidxs,]
nnout <- nnet(wageinc ~ .,trn,size=8,linout=T)
```

    # weights:  57
    initial  value 14147.553107 
    iter  10 value 7623.683451
    iter  20 value 7565.856394
    iter  30 value 7505.250071
    iter  40 value 7462.344945
    iter  50 value 7426.550185
    iter  60 value 7392.915119
    iter  70 value 7378.343718
    iter  80 value 7369.731044
    iter  90 value 7363.681276
    iter 100 value 7360.517115
    final  value 7360.517115 
    stopped after 100 iterations

``` r
nnpred <- predict(nnout,tst)[,1]
nnpred <- nnpred * sdy + muy
yorigtst <- pe$wageinc[-trnidxs]
loss[1, 1] <- mean(abs(nnpred-yorigtst))
loss[2, 1] <- rmse(nnpred, yorigtst)

lmout <- lm(wageinc ~ .,data=pe[trnidxs,])
lmpred <- predict(lmout,pe[-trnidxs,])
loss[1, 2] <- mean(abs(lmpred-yorigtst))
loss[2, 2] <- rmse(lmpred, yorigtst)

# srci('~/Research/PolyReg/polyreg/plm.R')
pe2 <- pe[,c(1,2,4:6,3)]
pe2.2 <- plm(pe2,2)
nms2.2 <- c(paste('a',1:17,sep=''),'wageinc')
names(pe2.2) <- nms2.2
lmout <- lm(wageinc ~ .,data=pe2.2[trnidxs,])
lmpred <- predict(lmout,pe2.2[-trnidxs,])
loss[1,3] <- mean(abs(lmpred-yorigtst))
loss[2,3] <- rmse(lmpred, yorigtst)

pe2.3 <- plm(pe2,3)
nms2.3 <- c(paste('a',1:37,sep=''),'wageinc')
names(pe2.3) <- nms2.3
lmout <- lm(wageinc ~ .,data=pe2.3[trnidxs,])
lmpred <- predict(lmout,pe2.3[-trnidxs,])
loss[1,4] <- mean(abs(lmpred-yorigtst))
loss[2, 4] <- rmse(lmpred, yorigtst)

loss
```

                                nnet       lm     plm2      pl3
    Mean Abs Error          24777.32 25788.76 25180.57 24806.14
    Root Mean Squared Error 42659.24 43428.54 42836.71 42662.41
