rm(list=ls())
##RUnning the experiment using the above tau leap function
##---------------------------------------------------------

library("spatstat")
library("dplyr")
library("ggplot2")
library("ggthemes")
library("RColorBrewer")
library("beepr")
## tau-leap Gillespie algorithm function
tauLeapG <- function(beta, # transmission rate
                     theta, # dispersal scale
                     b=1, # kernel shape parameter, 1 for exponential
                     sigma=0, # asymptomatic period, used for outputing the time series
                     q0=0, # starting incidence if ppp is without marks
                     q.end=1, # stoping condition 1: incidence lvl
                     t.end=Inf, # stoping condition 2: time after first simulated time step
                     area.host=10, # surface area occupied by one host
                     delta.t=1, # time step
                     ppp, # point pattern as a ppp object, optinally with marks 1/0 for infected/healthy
                     dist.mat=NULL){ # matrix distance if its computation is to be avoided here (for e.g. repeated calls)
  
  ## if the point pattern has no marks, generate some randomly that fits q0
  if (is.null(marks(ppp))){
    inf.start <- max(1, round(ppp$n * q0))
    marks(ppp) <- sample(c(rep(FALSE, ppp$n-inf.start), rep(TRUE, inf.start)))
  }
  
  ## compute distance matrix if not provided
  if (is.null(dist.mat)){ 
    ## add the kernel computation that can be added directly on the dist matrix to reduce comp time
    dist.mat <- exp(-pairdist(ppp)^b / theta^b)
    diag(dist.mat) <- NA
  }
  
  ## function that compute infection event probability, based on the dispersal kernel
  k.norm <- beta * area.host * (b/(2*pi*theta^2*gamma(2/b))) # constant part of the exponential power kernel
  infection <- function(infected, dist){
    inf <-  matrix(k.norm * dist[infected,!infected],
                   nrow=sum(infected), byrow=FALSE)
    inf[is.na(inf)] <- 0
    inf
  }
  
  ## starting time
  time <- 0
  ## inititate the heavy dataframe that will aggregate all changes
  df.big <- data.frame(time=0, who=which(ppp$marks), t(ppp$marks))
  
  ## computation loop
  while (any(!ppp$marks) & time <= t.end & mean(ppp$marks) < q.end){
    ## infection event probaility
    events <- infection(ppp$marks, dist=dist.mat)
    ## random proisson draws
    new.infected <- which(!ppp$marks)[rpois(n=sum(!ppp$marks), lambda=apply(events, 2, sum) * delta.t) > 0]
    ## change marks of newly infected
    ppp$marks[new.infected] <- TRUE
    ## increment time
    time <- time + delta.t
    ## if some infection, increment the big dataframe
    if (length(new.infected) > 0){
      df.big <- rbind(df.big, data.frame(time=time, who=new.infected, t(ppp$marks)))
    }
    ## print a dot per new infection
    # cat(paste0(rep('.', length(new.infected)), collapse = '')) ## comment for quiet
  }
  
  ## make compact, time only, version of the big dataframe
  times.i <- unique(df.big[,1])
  times.d <- times.i + sigma
  times <- sort(unique(c(times.i, times.d)))
  infected <- sapply(times, FUN=function(t) sum(t >= df.big[,1]))
  detectable <- sapply(times, FUN=function(t) sum(t >= df.big[,1] + sigma))
  df.small <- data.frame(time=times, infected=infected, detectable=detectable)
  
  ## out put the simplified time series, and the big one
  list(df.small[df.small$time <= max(df.big$time),], df.big) 
} 




## meta parameters
delta.t <- 50 # time step (ALEX-THIS IS BIGGER THAN THE EXPERIMENT BELOW BECAUSE IT IS TAKING SO MUCH LONGER!)
iterations <- 1000 # how many epidemic to simulate
hosts <- 887 # number of hosts
dim <- 100 # dimension of the landscape

## epidemic parameters
sigma <- 0 #this is the assymptomatic period, doesn't change yet

beta <- .2 ##The data I sent you, which is called data in R is the 1000 realisations of these parameters
theta <- 86
b <- 1
area.host<-1
infbegin<-5

##################################add a timer##############################################################

ts<-proc.time()

###########################################################################################################
##Concatenating a list of metric values
##-----------------------------------------
sim_par <- function(i=NULL){


  set.seed(seed=NULL)
  
  radiusCluster<-5
  lambdaParent<-1
  lambdaDaughter<-20
  randmod<-1
  
  
  numbparents<-rpois(1,lambdaParent*dim)
  
  xxParent<-runif(numbparents,0+radiusCluster,dim-radiusCluster)
  yyParent<-runif(numbparents,0+radiusCluster,dim-radiusCluster)
  
  numbdaughter<-rpois(numbparents,(lambdaDaughter))
  sumdaughter<-sum(numbdaughter)
  
  

  thetaLandscape<-2*pi*runif(sumdaughter)
  
  rho<-radiusCluster*sqrt(runif(sumdaughter))
  


  xx0=rho*cos(thetaLandscape)
  yy0=rho*sin(thetaLandscape)
  
  
  xx<-rep(xxParent,numbdaughter)
  yy<-rep(yyParent,numbdaughter)
  
  xx<-xx+xx0
  
  yy<-yy+yy0
  cds<-data.frame(xx,yy)
  is_outlier<-function(x){
    x > dim| x < 0
  }
  cds<-cds[!(is_outlier(cds$xx)|is_outlier(cds$yy)),]
  while (nrow(cds)<hosts){
    dif<-hosts-nrow(cds)
    extraparentxx<-sample(xxParent,dif,replace = TRUE)
    extraparentyy<-sample(yyParent,dif,replace = TRUE)
    extrathetaLandscape<-2*pi*runif(dif)
    extrarho<-radiusCluster*sqrt(runif(dif))
    newextracoodsxx<-extrarho*cos(extrathetaLandscape)
    newextracoodsyy<-extrarho*sin(extrathetaLandscape)
    extraxx<-extraparentxx+newextracoodsxx
    extrayy<-extraparentyy+newextracoodsyy
    cdsextra<-data.frame(xx=extraxx,yy=extrayy)
    cds<-rbind(cds,cdsextra)
  }
  
  sampleselect<-sample(1:nrow(cds),hosts,replace=F)
  cds<-cds%>%slice(sampleselect)
  
  randfunction<-function(x){
    x<-runif(length(x),0,dim)
  }
  randselect<-sample(1:nrow(cds),floor(hosts*randmod),replace=F)
  cds[randselect,]<-apply(cds[randselect,],1,randfunction)
  
  landscape<-ppp(x=cds$xx,y=cds$yy,window=owin(xrange=c(0,dim),yrange=c(0,dim)))
  
  print(length(landscape$marks))
  
  data <- data.frame(x=landscape$x, y=landscape$y, id=1:hosts)

## design a function that will be called
  
  
  set.seed(seed=NULL)
  marks(landscape)<- sample(c(rep(TRUE,infbegin), rep(FALSE, hosts-infbegin)))
  
  output <- tauLeapG(beta = beta, theta = theta, b = b,
                     sigma = sigma, delta.t = delta.t,
                     ppp = landscape)
  temp <- output[[2]][,1:2][order(output[[2]][,2]),]
  
  data.frame(time=temp$time, who=temp$who, x=landscape$x[temp$who], y=landscape$y[temp$who],sim=i) ## what it exports will be concatenated in a list
}

  
library("parallel")

## create a cluster with the set number of cores, say nmax-1
cl <- makeCluster(mc <- getOption("cl.cores", 3))
## call the library loading function in them
clusterCall(cl, function() library("spatstat"))
clusterCall(cl,function() library("ggplot2"))
clusterCall(cl,function() library("dplyr"))
## export all to the nodes, that's dirty, so run this with a clean environement otherwise your memory will be flooded
clusterExport(cl=cl, varlist=ls())
## call the function in a parallel lapply
par_results <- parLapply(1:1000, fun=sim_par, cl=cl) ## test with 10 first, but then replace 10 by 1000
## stop the cluster
stopCluster(cl)
## call cbind on your list of lines to find the matrix you expect
data <- do.call("rbind", par_results)

##################################add a timer############################################################
proc.end<-proc.time()-ts
proc.end
beep()
###################################plot your data###########################################################
t2<- proc.time()

head(data)
data<-data.frame(data)
times <- sort(unique(data$time))
data_logistic <- function(i=NULL){
data %>% group_by(sim) %>%
  do(data.frame(time=times, infected=sapply(times, function(x) sum(.$time <= x))))
}
## make a logistic df from this data
cl <- makeCluster(mc <- getOption("cl.cores", 3))
clusterCall(cl,function() library("dplyr"))
clusterExport(cl=cl, varlist=c("data","times"),envir = environment())
par_data_logistic<-parLapply(1,fun=data_logistic,cl=cl)
stopCluster(cl)
data_log<-data.frame(par_data_logistic)


## prepare a logistic function of r to fit
temp <- filter(data_log, infected < 1000)
temp$simdigit<-as.numeric(temp$sim)

  logis <- function(t, r, K=1, s=0, q0){
  pmin(
    K*q0*exp(r*(t+s)) / (K + q0*(exp(r*(t+s)) - 1)),
    K) # numerical errors can happen for high r and sigma
  }
  
  
r_calculate<-function(i=NULL){

eval <- function(r, df){
  sum((logis(r=r, t=df$time, K=1000, q0=1) - df$infected)^2) ## sum of square errors between predictions and observations
}

# sapply(unique(temp$sim), 
#               function(i) optimize(f = eval, interval = c(0, 0.5), df=filter(temp, sim==i))$minimum)
r <- sapply(unique(temp), 
             function(i) optimize(f = eval, interval = c(0, .1), df=filter(temp, sim==i))$minimum)
}
#another cluster
cl <- makeCluster(mc <- getOption("cl.cores", 3))
clusterCall(cl,function() library("dplyr"))
clusterExport(cl=cl, varlist=c("temp","logis"),envir = environment())
par_r<-parLapply(1,fun=r_calculate,cl=cl)
stopCluster(cl)
####################################getting mean r #########################################################

mean_r<-mean(unlist(par_r))

############################################################################################################
beta_an<-paste("beta ==", beta)
theta_an<-paste("theta ==", theta)
temptimemax<-temp%>%filter(infected<999)%>%filter(time==max(time))
temptimemax<-temptimemax[,"time"]
pred_data <- data.frame(time=times, infected=logis(r=mean_r, t=times, K=hosts, q0=1))
ggplot(data_log) + geom_line(aes(x=time, y=infected/hosts, group=sim), size=.2,colour="gray70") +
  geom_line(data=filter(pred_data, infected<1000), aes(x=time, y=infected/hosts), colour="red", size=1)+
  ggtitle("Epidemic growth curve for 1000 simulations")+theme_tufte()+xlim(0,max(times)) +
  annotate(parse=T, geom="text",label=beta_an, x = 100, y = .2) +
  annotate(parse=T, geom="text", label=theta_an, x= 100, y = .3) +
  ylab("Prevalence") +
  xlab("Time") 

ggprev<-ggplot(data_log) + geom_line(aes(x=time, y=infected/hosts, group=sim), size=.2,colour="gray70") +
  geom_line(data=filter(pred_data, infected<1000), aes(x=time, y=infected/hosts), colour="red", size=1)+
  ggtitle("Epidemic growth curve for 1000 simulations")+theme_tufte()+xlim(0,max(times)) +
  annotate(parse=T, geom="text",label=beta_an, x = 100, y = .2) +
  annotate(parse=T, geom="text", label=theta_an, x= 100, y = .3) +
  ylab("Prevalence") +
  xlab("Time") 
proc.end2<-proc.time()-t2
proc.end2
beep()
################################saving the data if it looks good!###########################################
prevfile<-paste0("ggprev",beta,"theta",theta,"delta.t",delta.t,"rf",randmod,".png")
ggsave(file=prevfile,ggprev)
wdspec1<-"C:/Users/owner/Documents/Uni stuff/PhD/R scripts/Chapter 1/Script for identifying parameter space/Raw Data/"
datanamefile<-paste0(wdspec1,"Theta", theta, " Beta", beta, " Rf", randmod, " Delta t", delta.t,".Rda")
save(data,file=datanamefile)
  
length(unique(unlist(par_r)))
mean_r
wdspec<-"C:/Users/owner/Documents/Uni stuff/PhD/R scripts/Chapter 1/Script for identifying parameter space/Raw Data/Growth rate table corresponding to raw data/"
parameter_table<-data.frame("Growth Rate"=mean_r, "Mean Dispersal Distance"= theta*2, "\u03b2" = beta, "Plant Distribution" = randmod, "\u03C4 leap"=delta.t)
parametertablenamefile<-paste0(wdspec, "Growth Rate ",mean_r, "Mean Dispersal Distance ", theta*2,"\u03b2 ",beta, "Plant Distribution " , randmod, "\u03C4-leap ", delta.t, ".Rda")
save(parameter_table,file=parametertablenamefile)


########################################added calculation of time finish####################################
t<-log((hosts^2-hosts*(infbegin+1)+infbegin)/infbegin)/mean_r
t
intinf<-(hosts^2-hosts)/(exp(mean_r*t)+hosts-1)
intinf

####################################translating from theoretical to data###################################
kfinallow<-max(which(pred_data$infected<=hosts-1))
intinflow<-(hosts^2-hosts)/(exp(mean_r*pred_data$time[kfinallow])+hosts-1)
intinflow

kfinalhigh<-min(which(pred_data$infected>=hosts-1))
intinfhigh<-(hosts^2-hosts)/(exp(mean_r*pred_data$time[kfinalhigh])+hosts-1)
intinfhigh
##########ALL BELOW IS GOING TO BE REWRITTEN INTO MORE SUITABLE FORMAT######################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
################using colour brewer#########################################################################

myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))

##################################plotting the simulation count per point#################################

ggtimestampplot<-ggplot(timestampdata)+geom_point(aes(x=x,y=y,colour=infected))+facet_grid(vars(time))+
  ggtitle(paste0("Simulated infection count per tree \n \u03b2 = ", beta, " \u03b8 = ", theta))+theme_tufte()+
  scale_color_gradientn(colours = rev(myPalette(1000)))

ggplot(timestampdata)+geom_point(aes(x=x,y=y,colour=infected))+facet_grid(vars(time))+
  ggtitle(paste0("Simulated infection count per tree \n \u03b2 = ", beta, " \u03b8 = ", theta))+theme_tufte()+
  scale_color_gradientn(colours = rev(myPalette(1000)))

timestampplotfile<-paste0("ggtimestampplotbeta",beta,"theta",theta,"delta.t",delta.t,"rf",randmod,".png")
ggsave(file=timestampplotfile,ggtimestampplot, width = 10, height = 30, units = "cm")
