################################parameters#############################################################

hosts<-1000
infbegin<-1:1000
mean_r<-0.002
####################################equations for plotting#############################################
log((hosts-1)*(hosts-1))/mean_r #equation for just 1 initial infective

t<-log((hosts^2-hosts*(infbegin+1)-infbegin)/infbegin)/mean_
t<-t[1:997]#eliminating bad data points
infbegin<-infbegin[1:997]#eliminating bad data points
######################################plot#############################################################
timeinfbeginplot<-plot(t,infbegin)
timeinfbeginplot


############################translating the optimisation function to my plot###########################


theta<-1
beta<-10
maxd<-100
eq<-function(x){
  discale<-exp(-x/theta)
  k.norm <- beta * (1/(2*pi*theta^2))
  f<-k.norm*discale
  return(f)}
ggplot(data.frame(x=c(0, maxd)), aes(x=x)) + 
  stat_function(fun=eq)+theme_minimal()+#theme(axis.text.y = element_blank())+
  ylab("Infection Likelihood")+
  xlab("Distance from Source")

intpd <- integrate(eq, 0, maxd)
intpdm <- intpd$value/maxd - 0

opt<-optim(50, fn = function(x) (eq(x) - intpdm)^2, 
           method = 'Brent', lower = 0, upper = maxd)$par

pdplot1<-ggplot(data.frame(x=c(0, maxd)), aes(x=x)) + 
  stat_function(fun=eq) +
  theme_minimal() +
  ylab("Infection Likelihood") +
  xlab("Distance from Source") +
  geom_hline(yintercept = intpdm) + 
  geom_vline(xintercept = opt) +
  ggtitle(expression(paste(theta," = 1")))