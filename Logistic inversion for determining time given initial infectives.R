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
