library(ggplot2)
library(GenomicRanges)

##create a GRangesList
gr<-GRanges("chr1", IRanges(c(1,20,50,60), width=c(5,10,6,7)))
combs<-combn(1:4,2)
grl<-do.call("GRangesList", apply(combs,2,function(x){
	res<-gr[x]
	values(res)<-data.frame(pvalue=runif(2))
	res
}))
values(grl)<-data.frame(counts=sample(1:100,size=6), score=rnorm(6))

#values(grl)$counts - support
#start(grl) - start values
#end(grl)  - end values

#Draws multiple arches.  
#startX= vector of start positions
#endX= vector of end positions
#y= single value for y position
#h= vector of hights of arches
#all vectors must be the same length or this will probably break. 
geom_arch<-function(data,...,startX, endX, y, h){
	xx<-c()
	yy<-c()
	#CREATES UNIT ARCH
	#only calculate points to draw a quarter of the curve, reduces time spent in for loop
	n=25 #number of points to draw quarter of curve, just has to be sufficiently high so curve looks smooth
	for(i in 1:n){
		ang<-i*pi/(2*n)
		xx[i]<-cos(ang)
		yy[i]<-sin(ang)
	}
	#takes the quarter of the curve calculated, flips a copy over the y axis
	#reduces time spent in for loop
	xx<-c(1,xx,rev(-xx),-1)
	yy<-c(0,yy,rev(yy), 0)
	
	#SETS UP DATAFRAME TO KEEP TRACK OF ALL POINTS TO DRAW ALL ARCHES
	apoint<-data.frame()
	jump<-abs(endX-startX)
	jumpAdj=max(jump)/max(abs(h))
	for(i in 1:length(startX)){
		temp<-data.frame(xx=xx*(abs(startX[i]-endX[i])/2)+(startX[i]+endX[i])/2,
						 yy=yy*h[i]+y,
						 junc=i,
						 s=((abs(h[i])-jump[i]/jumpAdj))/max(jump))
		apoint<-rbind(apoint,temp)	
	}
	ggplot()+geom_line(data=apoint, aes(xx,yy,group=junc,size=s,alpha=s))
}

#TESTING CODE
start<-c(rep(1, 25)); end<-c(2:26); height<-c(rep(c(1:5, 1:5*(-1)), 2), c(1:5)); y=0
height<-c(rep(1:5*5, 5))
start<-c(1)
end<-c(3)
height=(4)

ggplot()+
geom_arch(startX=start,endX=end, y=y, h=height)->p
p+scale_size(to = c(0.5, 2))
p+opts(legend.position="none") #Figure 3
p+scale_size(to = c(5))


