
######################图形基础
MyData<-read.table(file="美食餐馆食客评分数据.txt",header=TRUE,sep=" ",stringsAsFactors=FALSE)
hist(MyData$score_avg,xlab="平均打分",ylab="密度",main="食客平均打分的直方图",cex.lab=0.7,freq=FALSE,ylim=c(0,1))
lines(density(MyData$score_avg),lty=2,col=1)

#####################图形布局
MyLayout<-matrix(c(1,1,0,2),nrow=2,ncol=2,byrow=TRUE)
DrawLayout<-layout(MyLayout,widths=c(1,1),heights=c(1,2),respect=TRUE)
layout.show(DrawLayout)

#####################分类型变量的基本分析
MyData<-read.table(file="美食餐馆食客评分数据.txt",header=TRUE,sep=" ",stringsAsFactors=FALSE)
(freqT<-table(MyData$food_type))
addmargins(freqT)     #在频数分布表上增加合计
prop.table(freqT)*100  #计算百分比

T<-barplot(freqT,xlab="菜系",ylab="餐馆数",ylim=c(0,170),main="菜系分布柱形图")
box()
text(T,5,freqT,col=2)

Pct<-round(freqT/length(MyData$food_type)*100,2)
GLabs<-sapply(dimnames(freqT),FUN=function(x) paste(x,Pct,"%",sep=" "))
pie(freqT,cex=0.8,labels=GLabs,main="菜系分布饼图",cex.main=0.8)
library("plotrix")
fan.plot(freqT,labels=GLabs)
title(main="菜系分布扇形图")


##############数值型变量的基本分析
apply(MyData[,5:9],MARGIN = 2,mean)
Des.Fun<-function(x,...){
  Av<-mean(x,...)
  Sd<-sd(x,...)
  N<-length(x[!is.na(x)])
  Sk<-sum((x[!is.na(x)]-Av)^3/Sd^3)/N
  Ku<-sum((x[!is.na(x)]-Av)^4/Sd^4)/N-3
  result<-list(avg=Av,sd=Sd,skew=Sk,kurt=Ku)
  return(result)
}
DesRep<-sapply(MyData[,5:9],FUN=Des.Fun,na.rm=TRUE)
DesRep


par(mfrow=c(2,1),mar=c(4,6,4,4))
hist(MyData$score_avg,xlab="平均打分",ylab="密度",main="食客平均打分的直方图",cex.lab=0.7,freq=FALSE,ylim=c(0,1))
lines(density(MyData$score_avg),lty=2,col=1)
plot(density(MyData$taste),main="各细项打分的核密度曲线图",ylab="密度",cex.lab=0.7)
lines(density(MyData$environment),lty=2,col=2)
lines(density(MyData$service),lty=3,col=3)
legend("topright",title="细项",c("口味打分","环境打分","服务打分"),lty=c(1,2,3),col=c(1,2,3),cex=0.7)

par(mfrow=c(2,1),mar=c(4,6,4,4))
boxplot(MyData$score_avg~MyData$food_type,main="不同菜系食客平均打分的箱线图",ylab="平均打分",cex.main=0.7)
library("vioplot")
vioplot(MyData[,"score_avg"],MyData[MyData$region=="五道口","score_avg"],MyData[MyData$region=="北太平庄","score_avg"],
        names=c("全部","五道口","北太平庄"))
title(main="食客平均打分的小提琴图",cex.main=0.7,ylab="平均打分")


############################北京市空气质量监测数据基本分析
MyData<-read.table(file="空气质量.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
Data<-subset(MyData,(MyData$date<=20160315|MyData$date>=20161115))   #仅分析供暖季的空气质量数据
Data<-na.omit(Data)   #完整观测
library("psych")
describe(Data$PM2.5,IQR=TRUE)
par(mfrow=c(1,2))
hist(Data$PM2.5,xlab="PM2.5",ylab="密度",main="供暖季PM2.5浓度直方图",cex.lab=0.8,freq=FALSE,ylim=c(0,0.01))
lines(density(Data$PM2.5,na.rm=TRUE),col=2)
boxplot(Data$PM2.5,horizontal =TRUE,main="供暖季PM2.5浓度箱线图",xlab="PM2.5浓度",cex.lab=0.8)
dotchart(Data$PM2.5,main="供暖季PM2.5浓度克利夫兰点图",cex.main=0.8,xlab="PM2.5",ylab="观测编号",cex.lab=0.8)
abline(v=mean(Data$PM2.5),col=2)
(InDiff<-quantile(Data$PM2.5,0.75)-quantile(Data$PM2.5,0.25))
(threshold<-(quantile(Data$PM2.5,0.75)+1.5*InDiff))  #爆表临界值
exData<-Data[Data$PM2.5>threshold,]  #爆表数据
length(unique(exData$date))   #爆表天数
sort(table(exData[,"SiteName"]),decreasing = TRUE)  #各站点的爆表情况
unique(exData[,"SiteName"])   


#其他R内容
###################################学生成绩：正态分布
set.seed(123)
x1<-rnorm(1000,70,5)
summary(x1)
mean(x1)
var(x1)
sd(x1)

par(mfrow=c(2,1))
hist(x1,main="学生成绩(正态分布)",ylab="密度",freq=FALSE)
x1<-x1[order(x1)]
lines(x1,dnorm(x1,70,5),type="l",col=2,main="正态分布(学生成绩)",ylab="密度")
print(pnorm(60,70,5))
print(qnorm(0.25,70,5))
print(pnorm(80,70,5))
print(1-pnorm(80,70,5))  
print(pnorm(80,70,5)-pnorm(60,70,5))


x2<-rnorm(1000,0,1)
summary(x2)
x2<-x2[order(x2)]
plot(x2,dnorm(x2,0,1),type="l",col=2,main="标准正态分布",ylab="密度")
print(qnorm(0.05,0,1))
print(qnorm(0.025,0,1))
print(qnorm(0.95,0,1))
print(qnorm(0.975,0,1))

###################################正态分布和不同自由度下的t分布
set.seed(123)
x2<-rnorm(1000,0,1)
x2<-x2[order(x2)]
plot(x2,dnorm(x2,0,1),type="l",col=1,main="标准正态分布和t分布",ylab="密度")
x3<-rt(1000,5)
x3<-x3[order(x3)]
lines(x3,dt(x3,5),col=2)
x3<-rt(1000,30)
x3<-x3[order(x3)]
lines(x3,dt(x3,30),col=3)
legend("topright",title="自由度",c("标准正态分布","自由度=5","自由度=30"),lty=c(1,1,1),col=c(1,2,3))


###################################正态分布和不同自由度下的F分布
set.seed(12345)
x<-rnorm(1000,0,1)
Ord<-order(x,decreasing=FALSE)
x<-x[Ord]
y<-dnorm(x,0,1)
plot(x,y,xlim=c(-3,5),ylim=c(0,2),type="l",ylab="密度",main="标准正态分布与不同自由度下的F分布密度函数",lwd=2)
#######不同自由度的F分布
df1<-c(10,15,30,100)
df2<-c(10,20,25,110)
for(i in 1:4){
  x<-rf(1000,df1[i],df2[i])
  Ord<-order(x,decreasing=FALSE)
  x<-x[Ord]
  y<-df(x,df1[i],df2[i])
  lines(x,y,col=i+1,lty=i+1,lwd=2)
}
legend("topright",title="自由度",c("标准正态分布",paste(df1,df2,sep="-")),lty=1:5,col=1:5)

##############求解pi
Pi<-vector(length=500)
set.seed(123456)
for(i in 1:500){
  X<-runif(30000,0,1)
  Y<-runif(30000,0,1)
  Z<-X*X+Y*Y  #利用圆的标准方程(x-a)^2+(y-b)^2=r^2,a=b=0
  Data<-data.frame(x=X,y=Y,z=Z)
  if(i==1){  #只在第一次显示图
    plot(Data$x,Data$y,cex=0.2,col=ifelse(Data$z<=1,1,2))
  }
  SubSet<-subset(Data,Data$z<=1)
  Pi[i]<-4*length(SubSet$x)/30000
}
print(mean(Pi))

