##########################
# Analysis
##########################

# clean environment
rm(list=ls())

# read in the simulation output
load("./Output/MC_Out.RData")

# function to compute RMSE
rmse <- function(pred, true=1){
  return(sqrt(mean((pred-true)^2)))
}


# create empty folder (if not existing already)
ifelse(!dir.exists(file.path("./Figs"))
       ,dir.create(file.path("./Figs")), FALSE)



### FIGURE 2
png("./Figs/Fig2",width=600,height=600)
par(mfcol=c(1,2),oma=c(0,0,0,0),mar=c(3,3,2,1))
for(g in unique(input$gamma2)){
  # empty plot
  plot(0,ylim=c(0,5),xlim=c(100,1000),axes=F,ann=F)
  # selector
  sel <- (input$corr_xe==.3 & input$corr_xz==.3 & input$corr_ze==0
          & input$gamma2==g & input$bw==.5 & input$exogenous==F)
  # OLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_ols[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_ols[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_ols[sel & input$n==1000],true=1))
        ,lty=1)
  # 2SLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_twoSLS[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_twoSLS[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_twoSLS[sel & input$n==1000],true=1))
        ,lty=3)
  # Kernel
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_kernel[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_kernel[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_kernel[sel & input$n==1000],true=1))
        ,lty=4)
  # LOWESS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_lowess[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_lowess[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_lowess[sel & input$n==1000],true=1))
        ,lty=2)
  # KRLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_krls[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_krls[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_krls[sel & input$n==1000],true=1))
        ,lty=5)
  # axes
  axis(1,at=c(100,500,1000))
  axis(2,las=1)
  if(g == 0){
    mtext("Linear First Stage",side=3, line=1)
  } else{
    mtext("Quadratic First Stage",side=3, line=1)
  }
  mtext("N",side=1,line=2)
  mtext("RMSE",side=2,line=2)
}
# Legend
legend("topright",legend=c("OLS","2SLS","Kernel","Lowess","KRLS")
       ,lty=c(1,3,4,2,5),cex=.8)
dev.off()





### FIGURE 3
png("./Figs/Fig3",width=600,height=600)
par(mfcol=c(1,2),oma=c(0,0,0,0),mar=c(3,3,2,1))
for(g in unique(input$gamma2)){
  # empty plot
  plot(0,ylim=c(0,5),xlim=c(100,1000),axes=F,ann=F)
  # selector
  sel <- (input$corr_xe==.6 & input$corr_xz==.6 & input$corr_ze==0
          & input$gamma2==g & input$bw==.5 & input$exogenous==F)
  # OLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_ols[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_ols[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_ols[sel & input$n==1000],true=1))
        ,lty=1)
  # 2SLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_twoSLS[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_twoSLS[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_twoSLS[sel & input$n==1000],true=1))
        ,lty=3)
  # Kernel
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_kernel[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_kernel[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_kernel[sel & input$n==1000],true=1))
        ,lty=4)
  # LOWESS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_lowess[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_lowess[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_lowess[sel & input$n==1000],true=1))
        ,lty=2)
  # KRLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_krls[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_krls[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_krls[sel & input$n==1000],true=1))
        ,lty=5)
  # axes
  axis(1,at=c(100,500,1000))
  axis(2,las=1)
  if(g == 0){
    mtext("Linear First Stage",side=3, line=1)
  } else{
    mtext("Quadratic First Stage",side=3, line=1)
  }
  mtext("N",side=1,line=2)
  mtext("RMSE",side=2,line=2)
}
# Legend
legend("topright",legend=c("OLS","2SLS","Kernel","Lowess","KRLS")
       ,lty=c(1,3,4,2,5),cex=.8)
dev.off()





### FIGURE 4
png("./Figs/Fig4",width=600,height=600)
par(mfcol=c(1,2),oma=c(0,0,0,0),mar=c(3,3,2,1))
for(g in unique(input$gamma2)){
  # empty plot
  plot(0,ylim=c(0,5),xlim=c(100,1000),axes=F,ann=F)
  # selector
  sel <- (input$corr_xe==.3 & input$corr_xz==.6 & input$corr_ze==0
          & input$gamma2==g & input$bw==.5 & input$exogenous==F)
  # OLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_ols[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_ols[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_ols[sel & input$n==1000],true=1))
        ,lty=1)
  # 2SLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_twoSLS[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_twoSLS[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_twoSLS[sel & input$n==1000],true=1))
        ,lty=3)
  # Kernel
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_kernel[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_kernel[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_kernel[sel & input$n==1000],true=1))
        ,lty=4)
  # LOWESS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_lowess[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_lowess[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_lowess[sel & input$n==1000],true=1))
        ,lty=2)
  # KRLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_krls[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_krls[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_krls[sel & input$n==1000],true=1))
        ,lty=5)
  # axes
  axis(1,at=c(100,500,1000))
  axis(2,las=1)
  if(g == 0){
    mtext("Linear First Stage",side=3, line=1)
  } else{
    mtext("Quadratic First Stage",side=3, line=1)
  }
  mtext("N",side=1,line=2)
  mtext("RMSE",side=2,line=2)
}
# Legend
legend("topright",legend=c("OLS","2SLS","Kernel","Lowess","KRLS")
       ,lty=c(1,3,4,2,5),cex=.8)
dev.off()




### FIGURE 5
png("./Figs/Fig5",width=600,height=600)
par(mfcol=c(1,2),oma=c(0,0,0,0),mar=c(3,3,2,1))
for(g in unique(input$gamma2)){
  # empty plot
  plot(0,ylim=c(0,5),xlim=c(100,1000),axes=F,ann=F)
  # selector
  sel <- (input$corr_xe==.6 & input$corr_xz==.3 & input$corr_ze==0
          & input$gamma2==g & input$bw==.5 & input$exogenous==F)
  # OLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_ols[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_ols[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_ols[sel & input$n==1000],true=1))
        ,lty=1)
  # 2SLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_twoSLS[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_twoSLS[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_twoSLS[sel & input$n==1000],true=1))
        ,lty=3)
  # Kernel
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_kernel[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_kernel[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_kernel[sel & input$n==1000],true=1))
        ,lty=4)
  # LOWESS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_lowess[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_lowess[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_lowess[sel & input$n==1000],true=1))
        ,lty=2)
  # KRLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_krls[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_krls[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_krls[sel & input$n==1000],true=1))
        ,lty=5)
  # axes
  axis(1,at=c(100,500,1000))
  axis(2,las=1)
  if(g == 0){
    mtext("Linear First Stage",side=3, line=1)
  } else{
    mtext("Quadratic First Stage",side=3, line=1)
  }
  mtext("N",side=1,line=2)
  mtext("RMSE",side=2,line=2)
}
# Legend
legend("topright",legend=c("OLS","2SLS","Kernel","Lowess","KRLS")
       ,lty=c(1,3,4,2,5),cex=.8)
dev.off()





### FIGURE 6
png("./Figs/Fig6",width=600,height=600)
par(mfcol=c(1,2),oma=c(0,0,0,0),mar=c(3,3,2,1))
for(g in unique(input$gamma2)){
  # empty plot
  plot(0,ylim=c(0,5),xlim=c(100,1000),axes=F,ann=F)
  # selector
  sel <- (input$corr_xe==.3 & input$corr_xz==.3 & input$corr_ze==0
          & input$gamma2==g & input$bw==.5 & input$exogenous==TRUE)
  # OLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_ols[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_ols[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_ols[sel & input$n==1000],true=1))
        ,lty=1)
  # 2SLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_twoSLS[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_twoSLS[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_twoSLS[sel & input$n==1000],true=1))
        ,lty=3)
  # Kernel
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_kernel[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_kernel[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_kernel[sel & input$n==1000],true=1))
        ,lty=4)
  # LOWESS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_lowess[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_lowess[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_lowess[sel & input$n==1000],true=1))
        ,lty=2)
  # KRLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_krls[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_krls[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_krls[sel & input$n==1000],true=1))
        ,lty=5)
  # axes
  axis(1,at=c(100,500,1000))
  axis(2,las=1)
  if(g == 0){
    mtext("Linear First Stage",side=3, line=1)
  } else{
    mtext("Quadratic First Stage",side=3, line=1)
  }
  mtext("N",side=1,line=2)
  mtext("RMSE",side=2,line=2)
}
# Legend
legend("topright",legend=c("OLS","2SLS","Kernel","Lowess","KRLS")
       ,lty=c(1,3,4,2,5),cex=.8)
dev.off()




### FIGURE 7
png("./Figs/Fig7",width=600,height=600)
par(mfcol=c(1,2),oma=c(0,0,0,0),mar=c(3,3,2,1))
for(g in unique(input$gamma2)){
  # empty plot
  plot(0,ylim=c(0,5),xlim=c(100,1000),axes=F,ann=F)
  # selector
  sel <- (input$corr_xe==.6 & input$corr_xz==.6 & input$corr_ze==0
          & input$gamma2==g & input$bw==.5 & input$exogenous==TRUE)
  # OLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_ols[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_ols[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_ols[sel & input$n==1000],true=1))
        ,lty=1)
  # 2SLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_twoSLS[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_twoSLS[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_twoSLS[sel & input$n==1000],true=1))
        ,lty=3)
  # Kernel
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_kernel[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_kernel[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_kernel[sel & input$n==1000],true=1))
        ,lty=4)
  # LOWESS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_lowess[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_lowess[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_lowess[sel & input$n==1000],true=1))
        ,lty=2)
  # KRLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_krls[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_krls[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_krls[sel & input$n==1000],true=1))
        ,lty=5)
  # axes
  axis(1,at=c(100,500,1000))
  axis(2,las=1)
  if(g == 0){
    mtext("Linear First Stage",side=3, line=1)
  } else{
    mtext("Quadratic First Stage",side=3, line=1)
  }
  mtext("N",side=1,line=2)
  mtext("RMSE",side=2,line=2)
}
# Legend
legend("topright",legend=c("OLS","2SLS","Kernel","Lowess","KRLS")
       ,lty=c(1,3,4,2,5),cex=.8)
dev.off()




### FIGURE 8
png("./Figs/Fig8",width=600,height=600)
par(mfcol=c(1,2),oma=c(0,0,0,0),mar=c(3,3,2,1))
for(g in unique(input$gamma2)){
  # empty plot
  plot(0,ylim=c(0,5),xlim=c(100,1000),axes=F,ann=F)
  # selector
  sel <- (input$corr_xe==.3 & input$corr_xz==.6 & input$corr_ze==0
          & input$gamma2==g & input$bw==.5 & input$exogenous==TRUE)
  # OLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_ols[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_ols[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_ols[sel & input$n==1000],true=1))
        ,lty=1)
  # 2SLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_twoSLS[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_twoSLS[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_twoSLS[sel & input$n==1000],true=1))
        ,lty=3)
  # Kernel
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_kernel[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_kernel[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_kernel[sel & input$n==1000],true=1))
        ,lty=4)
  # LOWESS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_lowess[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_lowess[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_lowess[sel & input$n==1000],true=1))
        ,lty=2)
  # KRLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_krls[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_krls[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_krls[sel & input$n==1000],true=1))
        ,lty=5)
  # axes
  axis(1,at=c(100,500,1000))
  axis(2,las=1)
  if(g == 0){
    mtext("Linear First Stage",side=3, line=1)
  } else{
    mtext("Quadratic First Stage",side=3, line=1)
  }
  mtext("N",side=1,line=2)
  mtext("RMSE",side=2,line=2)
}
# Legend
legend("topright",legend=c("OLS","2SLS","Kernel","Lowess","KRLS")
       ,lty=c(1,3,4,2,5),cex=.8)
dev.off()





### FIGURE 9
png("./Figs/Fig9",width=600,height=600)
par(mfcol=c(1,2),oma=c(0,0,0,0),mar=c(3,3,2,1))
for(g in unique(input$gamma2)){
  # empty plot
  plot(0,ylim=c(0,5),xlim=c(100,1000),axes=F,ann=F)
  # selector
  sel <- (input$corr_xe==.6 & input$corr_xz==.3 & input$corr_ze==0
          & input$gamma2==g & input$bw==.5 & input$exogenous==TRUE)
  # OLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_ols[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_ols[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_ols[sel & input$n==1000],true=1))
        ,lty=1)
  # 2SLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_twoSLS[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_twoSLS[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_twoSLS[sel & input$n==1000],true=1))
        ,lty=3)
  # Kernel
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_kernel[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_kernel[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_kernel[sel & input$n==1000],true=1))
        ,lty=4)
  # LOWESS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_lowess[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_lowess[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_lowess[sel & input$n==1000],true=1))
        ,lty=2)
  # KRLS
  lines(x=unique(input$n)
        ,y=c(rmse(pred=sim_out$beta_krls[sel & input$n==100],true=1)
             ,rmse(pred=sim_out$beta_krls[sel & input$n==500],true=1)
             ,rmse(pred=sim_out$beta_krls[sel & input$n==1000],true=1))
        ,lty=5)
  # axes
  axis(1,at=c(100,500,1000))
  axis(2,las=1)
  if(g == 0){
    mtext("Linear First Stage",side=3, line=1)
  } else{
    mtext("Quadratic First Stage",side=3, line=1)
  }
  mtext("N",side=1,line=2)
  mtext("RMSE",side=2,line=2)
}
# Legend
legend("topright",legend=c("OLS","2SLS","Kernel","Lowess","KRLS")
       ,lty=c(1,3,4,2,5),cex=.8)
dev.off()

