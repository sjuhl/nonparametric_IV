##########################
# Monte Carlo Simulations
##########################

# clean environment
rm(list=ls())

### load packages
# parallel computing
library(foreach)
library(doParallel)
library(doRNG)

# read simulation function
source("./SimFunc.R")


###################
# MONTE CARLO SIMULATION

# parameters
n <- c(100,500,1000)
corr_xe <- c(.3,.6) # degree of endogeneity in x
corr_xz <- c(.3,.6) # strength of instrument z
corr_ze <- c(0,.3,.6) # validity of instrument z
gamma2 <- c(0,.5) # how is gamma2 > 0 specified? For now: .5
bw <- c(.2,.5,.8)
exogenous <- c(FALSE,TRUE)
k <- 100 # number of bootstrap iterations

grid <- matrix(as.matrix(cbind(expand.grid(n,corr_xe,corr_xz,corr_ze,gamma2,bw,exogenous))),ncol=7
               ,dimnames=list(NULL, c("n", "corr_xe","corr_xz","corr_ze","gamma2","bw","exogenous")))
ninput <- nrow(grid)
nsim <- 1000

# specify function inputs
input <- data.frame(do.call(rbind,replicate(nsim,grid,simplify=F)))
input <- input[order(input$exogenous,input$n,input$bw,input$corr_xe
                     ,input$corr_xz,input$corr_ze,input$gamma2),]
input$exogenous <- input$exogenous==1
#unique(input)
rownames(input) <- 1:nrow(input)

# check
nrow(input)==ninput*nsim


# test
SimFunc(n=input$n[1],input$corr_xe[1],input$corr_xz[1],input$corr_ze[1]
        ,beta=1,gamma1=1,gamma2=input$gamma2[1],exogenous=input$exogenous[1]
        ,firststage="quadratic",k=k,kfunc="normal",bandwidth=input$bw[1])

# select scenario
#sel <- (input$corr_xe==.3 & input$corr_xz==.3 & input$corr_ze==0 & input$bw==.5
#        & input$exogenous==F & input$gamma2!=0)
#sum(sel)
#input <- input[sel,]


# parallel computing
(ncores <- detectCores())
nworkers <- ncores - 1
cl <- makeCluster(nworkers)
registerDoParallel(cl)
registerDoRNG(12345)

# start timer
start.time <- Sys.time()
# simulate
sim_out <- foreach(i=1:nrow(input), .combine=rbind,
                   .packages=c("MASS","KRLS")
                   ) %dopar% {
                     SimFunc(n=input$n[i],input$corr_xe[i],input$corr_xz[i],input$corr_ze[i]
                             ,beta=1,gamma1=.5,gamma2=input$gamma2[i],exogenous=input$exogenous[i]
                             ,firststage="quadratic",k=k,kfunc="normal",bandwidth=input$bw[i])
                     }
stopCluster(cl)

(time.taken <- Sys.time() - start.time)
session_info <- sessionInfo()


# create empty folder (if not existing already)
ifelse(!dir.exists(file.path("./Output"))
       ,dir.create(file.path("./Output")), FALSE)

# save output
save(input,sim_out,nsim,time.taken,session_info,file="./Output/MC_Out.RData")

