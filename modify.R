load1997<-read.csv("E:/postgraduate/fuheData/competition-data/Load1997.csv",stringsAsFactors = F)
par(mfrow=c(1,1))
library(ggplot2)
dateSting<-as.Date(paste(load1997$Year,load1997$Month,load1997$Day),"%Y %m %d")
#将日期组织起来为后面转置用
rownames(load1997)<-dateSting
load1997.filter<-load1997[,-c(1,2,3)]
load1997.filter.T<-data.frame(t(load1997.filter))
load1997.filter.T$Time<-rownames(load1997.filter.T)

#效果不好，可略去
#install.packages("reshape")
#library(reshape)
#load1997.filter.T.Melt<-melt(load1997.filter.T,id.vars="Time")
#ggplot(data=load1997.filter.T.Melt, aes(x=Time, y=value,colour=variable))+geom_line()

#效果不好，图例太多，把整幅图覆盖了
#time<-seq(0.5,24,0.5)
#plot(x=time,y=load1997.filter.T[,1],type="l",xlab = "时间点",ylab="负荷",ylim=c(300,1000),main="1997年整年负荷数据")
#for(i in c(2:ncol(load1997.filter.T))){
#  lines(x=time,y=load1997.filter.T[,i])
#}


#进行凝聚层次聚类
load1997.filter.clu<-hclust(dist(load1997.filter,method = "euclidean"),method = "complete")
plot(load1997.filter.clu)
tt<-rect.hclust(load1997.filter.clu,k=4)

group_4<-cutree(load1997.filter.clu,k=4)
dd<-as.data.frame(group_4)
load1997.filter$type<-dd$group_4

#分别画出 此方法不通，需要调整
#par(mfrow=c(3,2))
#drawPic<-function(k){
#  print(-ncol(k))
#  plot(x=time,y=k[1,-ncol(k)],type="l",xlab = "时间点",ylab="负荷",ylim=c(300,1000),main=paste("第",k[1,ncol(k)],"类",sep=""))
#  for(i in c(2:nrow(k))){
#    lines(x=time,y=k[i,-ncol(k)])
#  }
#}
#load1997.filter$type<-as.factor(load1997.filter$type)
#ddply(data=load1997.filter,type1,drawPic)

#手动画出
load1997.filter_1<-load1997.filter[load1997.filter$type=="1",]
load1997.filter_2<-load1997.filter[load1997.filter$type=="2",]
load1997.filter_3<-load1997.filter[load1997.filter$type=="3",]
load1997.filter_4<-load1997.filter[load1997.filter$type=="4",]
par(mfrow=c(2,2))

plot(x=time,y=load1997.filter_1[1,-ncol(load1997.filter_1)],type="l",xlab = "时间点",ylab="负荷",ylim=c(300,1000),main="第一类")
for(i in c(2:nrow(load1997.filter_1))){
  lines(x=time,y=load1997.filter_1[i,-ncol(load1997.filter_1)])
}

plot(x=time,y=load1997.filter_2[1,-ncol(load1997.filter_2)],type="l",xlab = "时间点",ylab="负荷",ylim=c(300,1000),main="第二类")
for(i in c(2:nrow(load1997.filter_2))){
  lines(x=time,y=load1997.filter_2[i,-ncol(load1997.filter_2)])
}

plot(x=time,y=load1997.filter_3[1,-ncol(load1997.filter_3)],type="l",xlab = "时间点",ylab="负荷",ylim=c(300,1000),main="第三类")
for(i in c(2:nrow(load1997.filter_3))){
  lines(x=time,y=load1997.filter_3[i,-ncol(load1997.filter_3)])
}

plot(x=time,y=load1997.filter_4[1,-ncol(load1997.filter_4)],type="l",xlab = "时间点",ylab="负荷",ylim=c(300,1000),main="第四类")
for(i in c(2:nrow(load1997.filter_4))){
  lines(x=time,y=load1997.filter_4[i,-ncol(load1997.filter_4)])
}



#根据日和和类别分类
load1997.filter$Week<-weekdays(as.Date(rownames(load1997.filter)))

#将节假日引入
holiday<-read.csv("E:/postgraduate/fuheData/competition-data/Holidays.csv",stringsAsFactors = F)
holiday$HOLIDAY_1997<-as.Date(holiday$HOLIDAY_1997)
#构造6个指标
holiday_p1<-holiday$HOLIDAY_1997+1
holiday_s1<-holiday$HOLIDAY_1997-1
holiday_p2<-holiday$HOLIDAY_1997+2
holiday_s2<-holiday$HOLIDAY_1997-2
holiday_p3<-holiday$HOLIDAY_1997+3
holiday_s3<-holiday$HOLIDAY_1997-3
holiday_p4<-holiday$HOLIDAY_1997+4
holiday_s4<-holiday$HOLIDAY_1997-4
holiday_p5<-holiday$HOLIDAY_1997+5
holiday_s5<-holiday$HOLIDAY_1997-5
holiday_1<-unique(c(holiday_p1,holiday_s1,holiday$HOLIDAY_1997))
holiday_2<-unique(c(holiday_p2,holiday_s2,holiday_1))
holiday_3<-unique(c(holiday_p3,holiday_s3,holiday_2))
holiday_4<-unique(c(holiday_p4,holiday_s4,holiday_3))
holiday_5<-unique(c(holiday_p5,holiday_s5,holiday_4))

#添加相应的维度
load1997.filter$isholiday<-ifelse(as.Date(rownames(load1997.filter)) %in% holiday$HOLIDAY_1997,1,0)
load1997.filter$isholiday_1<-ifelse(as.Date(rownames(load1997.filter)) %in% holiday_1,1,0)
load1997.filter$isholiday_2<-ifelse(as.Date(rownames(load1997.filter)) %in% holiday_2,1,0)
load1997.filter$isholiday_3<-ifelse(as.Date(rownames(load1997.filter)) %in% holiday_3,1,0)
load1997.filter$isholiday_4<-ifelse(as.Date(rownames(load1997.filter)) %in% holiday_4,1,0)
load1997.filter$isholiday_5<-ifelse(as.Date(rownames(load1997.filter)) %in% holiday_5,1,0)

#查看各个维度的分布
table(load1997.filter$isholiday,load1997.filter$type)
table(load1997.filter$isholiday_1,load1997.filter$type)
table(load1997.filter$isholiday_2,load1997.filter$type)
table(load1997.filter$isholiday_3,load1997.filter$type)
table(load1997.filter$isholiday_4,load1997.filter$type)
table(load1997.filter$isholiday_5,load1997.filter$type)

#加载温度
temperature<-read.csv("E:/postgraduate/fuheData/competition-data/Temperature1997.csv",stringsAsFactors = F)
#绑定温度
load1997.filter$temperature<-temperature$Temperature..oC.
#求出负荷的均值
load1997.filter$fuheMean<-rowMeans(load1997.filter[,c(1:48)])
#将周转为数值
changeWeek<-function(k){
  if(k=="星期一"){
    1
  }else if(k=="星期二"){
    2
  }else if(k=="星期三"){
    3
  }else if(k=="星期四"){
    4
  }else if(k=="星期五"){
    5
  }else if(k=="星期六"){
    6
  }else if(k=="星期日"){
    7
  }
}
load1997.filter$weekNum<-sapply(load1997.filter$Week, changeWeek)
load1997.filter$Month<-load1997$Month
load1997.filter$Day<-load1997$Day

load1997.filter$fuheMean21<-scale(load1997.filter$fuheMean,center=FALSE,scale = T)
load1997.filter$weekNum21<-scale(load1997.filter$weekNum,center=FALSE,scale = T)
load1997.filter$temperature21<-scale(load1997.filter$temperature,center=FALSE,scale = T)
load1997.filter$fuheMean21<-scale(load1997.filter$fuheMean,center=FALSE,scale = T)
load1997.filter$Month21<-as.numeric(scale(load1997.filter$Month,center = F,scale = T))
load1997.filter$Day21<-as.numeric(scale(load1997.filter$Day,center = F,scale = T))
#求灰度关联
D01<-abs(load1997.filter$fuheMean21-load1997.filter$temperature21)
D02<-abs(load1997.filter$fuheMean21-load1997.filter$weekNum21)
D03<-abs(load1997.filter$fuheMean21-load1997.filter$isholiday)
D04<-abs(load1997.filter$fuheMean21-load1997.filter$isholiday_1)
D05<-abs(load1997.filter$fuheMean21-load1997.filter$isholiday_2)
D06<-abs(load1997.filter$fuheMean21-load1997.filter$isholiday_3)
D07<-abs(load1997.filter$fuheMean21-load1997.filter$isholiday_4)
D08<-abs(load1997.filter$fuheMean21-load1997.filter$isholiday_5)
D09<-abs(load1997.filter$fuheMean21-load1997.filter$Month21)
D10<-abs(load1997.filter$fuheMean21-load1997.filter$Day21)
Dmax<-max(c(D01,D02,D03,D04,D05,D06,D07,D08,D09,D10))
Dmin<-min(c(D01,D02,D03,D04,D05,D06,D07,D08,D09,D10))

#求关联系数
p01<-(0.001006463+0.5*2.076483)/(D01+0.5*2.076483)
p02<-(0.001006463+0.5*2.076483)/(D02+0.5*2.076483)
p03<-(0.001006463+0.5*2.076483)/(D03+0.5*2.076483)
p04<-(0.001006463+0.5*2.076483)/(D04+0.5*2.076483)
p05<-(0.001006463+0.5*2.076483)/(D05+0.5*2.076483)
p06<-(0.001006463+0.5*2.076483)/(D06+0.5*2.076483)
p07<-(0.001006463+0.5*2.076483)/(D07+0.5*2.076483)
p08<-(0.001006463+0.5*2.076483)/(D08+0.5*2.076483)
p09<-(0.001006463+0.5*2.076483)/(D09+0.5*2.076483)
p10<-(0.001006463+0.5*2.076483)/(D10+0.5*2.076483)

r01=mean(p01)
r02=mean(p02)
r03=mean(p03)
r04=mean(p04)
r05=mean(p05)
r06=mean(p06)
r07=mean(p07)
r08=mean(p08)
r09=mean(p09)
r10=mean(p10)

r01;r02;r03;r04;r05;r06;r07;r08;r09;r10

library(rpart.plot)
library(rpart)
library(maptree)
library(sampling)
a = round(1/4*sum(load1997.filter$type==1))
b = round(1/4*sum(load1997.filter$type==2))
c = round(1/4*sum(load1997.filter$type==3))
d = round(1/4*sum(load1997.filter$type==4))
a;b;c;d;
load1997.filter$type<-as.factor(load1997.filter$type)
load1997.filter$weekNum<-as.factor(load1997.filter$weekNum)

sub=strata(load1997.filter,stratanames = "type",size=c(33,19,21,18),method = "srswor")
train_data=load1997.filter[-sub$ID_unit,]
test_data=load1997.filter[sub$ID_unit,]

formula_load1997 = type~temperature+weekNum+isholiday+isholiday_1+isholiday_2+isholiday_3+isholiday_4+isholiday_5+Month+Day
rp_load1997_reg=rpart(formula_load1997,train_data,method = "class",minsplit=20)
par(mfrow=c(1,1))
rpart.plot(rp_load1997_reg,type=4,extra = 1)

pre_load1997_cla = predict(rp_load1997_reg,test_data,type = "class")
#错误率
(p=sum(as.numeric(pre_load1997_cla!=test_data$type))/nrow(test_data)) 
#混淆矩阵
table(test_data$type,pre_load1997_cla)


library(randomForest)
load1997.forest<-randomForest(formula_load1997,data=train_data)
pre.forest<-predict(load1997.forest,test_data)
(p=sum(as.numeric(pre.forest!=test_data$type))/nrow(test_data)) 

load1997.forest<-randomForest(formula_load1997,data=load1997.filter)
pre.forest<-predict(load1997.forest,load1997.filter)
(p=sum(as.numeric(pre.forest!=load1997.filter$type))/nrow(load1997.filter)) 


#利用神经网络进行第一种类的模型
load1997.type1<-load1997.filter[load1997.filter$type==1,]
load1997.type1<-load1997.type1[,-c(49,50,58,62,63,64,65,66)]
#tt<-test[,c(1:48)]
#property<-test[,c(49:58)]
#property$date<-rownames(property)

result=NULL
for(i in c(1:nrow(load1997.type1))){
  k = load1997.type1[i,]
  property<-as.data.frame(k[c(49:58)])
  for(i in c(1:48)){
     timeNumber=ifelse(i%%2==0,i/2,(i/2)-0.2)
     tk<-k[[i]]
     property$v=tk
     property$tn=timeNumber
     #singleResult<-as.data.frame(as.vector(c(v=tk,tn=timeNumber)),property)
     singleResult=property
     if(is.null(result)){
       result=singleResult
     }else{
       result=rbind(result,singleResult)
     }
  }
}
result$weekNum<-as.numeric(result$weekNum)
ascData<-result
trainData<-data.frame(scale(result))
xNum=nrow(trainData)
nn1<-nnet(v~.,trainData[1:(xNum-10),],size=10,decay=0.01,maxit=1000,linout=F,trace=F)

#第一种预测模式
pre={}
for(i in c(9:0)){
  nn1<-nnet(v~.,trainData[1:(xNum-(i+1)),],size=10,decay=0.01,maxit=1000,linout=F,trace=F)
  predict<-predict(nn1,trainData[xNum-i,])
  real=ascData[xNum-i,"v"]
  predict=predict*sd(ascData[,"v"])+mean(ascData[,"v"])
  pre<-cbind(pre,predict)
  percent <- (predict-real)*100/real
  res <- paste("预测值：",predict,"实际值：",real,"误差：",percent,"%")
  print(res)
}

#第二种预测模式
pre={}
for(i in c(9:0)){
  predict<-predict(nn1,trainData[xNum-i,])
  real=ascData[xNum-i,"v"]
  predict=predict*sd(ascData[,"v"])+mean(ascData[,"v"])
  pre<-cbind(pre,predict)
  percent <- (predict-real)*100/real
  res <- paste("预测值：",predict,"实际值：",real,"误差：",percent,"%")
  print(res)
}

#结束


#利用神经网络进行第二种类的模型
load1997.type2<-load1997.filter[load1997.filter$type==2,]
load1997.type2<-load1997.type2[,-c(49,50,58,62,63,64,65,66)]
#tt<-test[,c(1:48)]
#property<-test[,c(49:58)]
#property$date<-rownames(property)

result=NULL
for(i in c(1:nrow(load1997.type2))){
  k = load1997.type1[i,]
  property<-as.data.frame(k[c(49:58)])
  for(i in c(1:48)){
    timeNumber=ifelse(i%%2==0,i/2,(i/2)-0.2)
    tk<-k[[i]]
    property$v=tk
    property$tn=timeNumber
    #singleResult<-as.data.frame(as.vector(c(v=tk,tn=timeNumber)),property)
    singleResult=property
    if(is.null(result)){
      result=singleResult
    }else{
      result=rbind(result,singleResult)
    }
  }
}
result$weekNum<-as.numeric(result$weekNum)
ascData<-result
trainData<-data.frame(scale(result))
xNum=nrow(trainData)
nn2<-nnet(v~.,trainData[1:(xNum-10),],size=10,decay=0.01,maxit=1000,linout=F,trace=F)

#第一种预测模式
pre={}
for(i in c(9:0)){
  nn2<-nnet(v~.,trainData[1:(xNum-(i+1)),],size=10,decay=0.01,maxit=1000,linout=F,trace=F)
  predict<-predict(nn2,trainData[xNum-i,])
  real=ascData[xNum-i,"v"]
  predict=predict*sd(ascData[,"v"])+mean(ascData[,"v"])
  pre<-cbind(pre,predict)
  percent <- (predict-real)*100/real
  res <- paste("预测值：",predict,"实际值：",real,"误差：",percent,"%")
  print(res)
}

#第二种预测模式
pre={}
for(i in c(9:0)){
  predict<-predict(nn2,trainData[xNum-i,])
  real=ascData[xNum-i,"v"]
  predict=predict*sd(ascData[,"v"])+mean(ascData[,"v"])
  pre<-cbind(pre,predict)
  percent <- (predict-real)*100/real
  res <- paste("预测值：",predict,"实际值：",real,"误差：",percent,"%")
  print(res)
}

#结束

#利用神经网络进行第三种类的模型
load1997.type3<-load1997.filter[load1997.filter$type==3,]
load1997.type3<-load1997.type3[,-c(49,50,58,62,63,64,65,66)]
#tt<-test[,c(1:48)]
#property<-test[,c(49:58)]
#property$date<-rownames(property)

result=NULL
for(i in c(1:nrow(load1997.type3))){
  k = load1997.type1[i,]
  property<-as.data.frame(k[c(49:58)])
  for(i in c(1:48)){
    timeNumber=ifelse(i%%2==0,i/2,(i/2)-0.2)
    tk<-k[[i]]
    property$v=tk
    property$tn=timeNumber
    #singleResult<-as.data.frame(as.vector(c(v=tk,tn=timeNumber)),property)
    singleResult=property
    if(is.null(result)){
      result=singleResult
    }else{
      result=rbind(result,singleResult)
    }
  }
}
result$weekNum<-as.numeric(result$weekNum)
ascData<-result
trainData<-data.frame(scale(result))
xNum=nrow(trainData)
nn3<-nnet(v~.,trainData[1:(xNum-10),],size=10,decay=0.01,maxit=1000,linout=F,trace=F)

#第一种预测模式
pre={}
for(i in c(9:0)){
  nn3<-nnet(v~.,trainData[1:(xNum-(i+1)),],size=10,decay=0.01,maxit=1000,linout=F,trace=F)
  predict<-predict(nn3,trainData[xNum-i,])
  real=ascData[xNum-i,"v"]
  predict=predict*sd(ascData[,"v"])+mean(ascData[,"v"])
  pre<-cbind(pre,predict)
  percent <- (predict-real)*100/real
  res <- paste("预测值：",predict,"实际值：",real,"误差：",percent,"%")
  print(res)
}

#第二种预测模式
pre={}
for(i in c(9:0)){
  predict<-predict(nn3,trainData[xNum-i,])
  real=ascData[xNum-i,"v"]
  predict=predict*sd(ascData[,"v"])+mean(ascData[,"v"])
  pre<-cbind(pre,predict)
  percent <- (predict-real)*100/real
  res <- paste("预测值：",predict,"实际值：",real,"误差：",percent,"%")
  print(res)
}

#结束

#利用神经网络进行第四种类的模型
load1997.type4<-load1997.filter[load1997.filter$type==3,]
load1997.type4<-load1997.type4[,-c(49,50,58,62,63,64,65,66)]

result=NULL
for(i in c(1:nrow(load1997.type4))){
  k = load1997.type1[i,]
  property<-as.data.frame(k[c(49:58)])
  for(i in c(1:48)){
    timeNumber=ifelse(i%%2==0,i/2,(i/2)-0.2)
    tk<-k[[i]]
    property$v=tk
    property$tn=timeNumber
    #singleResult<-as.data.frame(as.vector(c(v=tk,tn=timeNumber)),property)
    singleResult=property
    if(is.null(result)){
      result=singleResult
    }else{
      result=rbind(result,singleResult)
    }
  }
}
result$weekNum<-as.numeric(result$weekNum)
ascData<-result
trainData<-data.frame(scale(result))
xNum=nrow(trainData)
nn4<-nnet(v~.,trainData[1:(xNum-10),],size=10,decay=0.01,maxit=1000,linout=F,trace=F)

#第一种预测模式
pre={}
for(i in c(9:0)){
  nn4<-nnet(v~.,trainData[1:(xNum-(i+1)),],size=10,decay=0.01,maxit=1000,linout=F,trace=F)
  predict<-predict(nn4,trainData[xNum-i,])
  real=ascData[xNum-i,"v"]
  predict=predict*sd(ascData[,"v"])+mean(ascData[,"v"])
  pre<-cbind(pre,predict)
  percent <- (predict-real)*100/real
  res <- paste("预测值：",predict,"实际值：",real,"误差：",percent,"%")
  print(res)
}

#第二种预测模式
pre={}
for(i in c(9:0)){
  predict<-predict(nn4,trainData[xNum-i,])
  real=ascData[xNum-i,"v"]
  predict=predict*sd(ascData[,"v"])+mean(ascData[,"v"])
  pre<-cbind(pre,predict)
  percent <- (predict-real)*100/real
  res <- paste("预测值：",predict,"实际值：",real,"误差：",percent,"%")
  print(res)
}

#结束


