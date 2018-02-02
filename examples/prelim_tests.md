Preliminary polyreg tests
================
Pete Mohanty
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

rmse <- function(y, yhat){
  (mean((y - yhat)^2)^0.5)
}

loss <- matrix(nrow = 2, ncol = 4)
rownames(loss) <- c("Mean Abs Error", "Root Mean Squared Error")
colnames(loss) <- c("nnet", "lm", "plm2", "pl3")


getPE() # from partoools, possibly on git version
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
    initial  value 16883.823216 
    iter  10 value 7627.611814
    iter  20 value 7515.637661
    iter  30 value 7477.774074
    iter  40 value 7452.640136
    iter  50 value 7422.246083
    iter  60 value 7403.451529
    iter  70 value 7394.470334
    iter  80 value 7389.052621
    iter  90 value 7385.862852
    iter 100 value 7383.432738
    final  value 7383.432738 
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
    Mean Abs Error          24883.67 25886.99 25196.69 24827.72
    Root Mean Squared Error 42652.74 43431.08 42786.77 42578.32
