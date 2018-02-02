library(partools)
library(nnet)

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


getPE() # from partoools, possibly on git version
pe1 <- scale(pe)
sdy <- attr(pe1,'scaled:scale')[3]
muy <- attr(pe1,'scaled:center')[3]
pe1 <- as.data.frame(pe1)
trnidxs <- sample(1:nrow(pe1),10000)
trn <- pe1[trnidxs,]
tst <- pe1[-trnidxs,]
nnout <- nnet(wageinc ~ .,trn,size=8,linout=T)
nnpred <- predict(nnout,tst)[,1]
nnpred <- nnpred * sdy + muy
yorigtst <- pe$wageinc[-trnidxs]
mean(abs(nnpred-yorigtst))
rmse(nnpred, yorigtst)

lmout <- lm(wageinc ~ .,data=pe[trnidxs,])
lmpred <- predict(lmout,pe[-trnidxs,])
mean(abs(lmpred-yorigtst))
rmse(lmpred, yorigtst)

# srci('~/Research/PolyReg/polyreg/plm.R')
pe2 <- pe[,c(1,2,4:6,3)]
pe2.2 <- plm(pe2,2)
nms2.2 <- c(paste('a',1:17,sep=''),'wageinc')
names(pe2.2) <- nms2.2
lmout <- lm(wageinc ~ .,data=pe2.2[trnidxs,])
lmpred <- predict(lmout,pe2.2[-trnidxs,])
mean(abs(lmpred-yorigtst))
rmse(lmpred, yorigtst)

pe2.3 <- plm(pe2,3)
nms2.3 <- c(paste('a',1:37,sep=''),'wageinc')
names(pe2.3) <- nms2.3
lmout <- lm(wageinc ~ .,data=pe2.3[trnidxs,])
lmpred <- predict(lmout,pe2.3[-trnidxs,])
mean(abs(lmpred-yorigtst))
rmse(lmpred, yorigtst)


### Testing
d1 <- 2:4
d2 <- 7:9
d3 <- c(T, F, F)
d4 <- c(T, T, F)
dy <- c(1,2,4)
xy <- cbind(d1,d2,d3,d4,dy)
plm(xy,3)
# d1, d2, d3, d4
# d1^2, d2^2, d1*d2, d3*d4, d1*d3, d2*d3, d1*d4, d2*d4
# d1^3, d2^3, d1*d2^2, d1^2*d2, d1^2*d3, d2^2*d3, d1*d2*d3, d1^2*d4, d2^2*d4, d1*d2*d4, d1*d3*d4, d2*d3*d4

plm(xy,4)

