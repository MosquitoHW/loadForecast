#加载1998年数据
load1998<-read.csv("D:/fuheData/competition-data/Load1998.csv",stringsAsFactors = F)
dateSting<-as.Date(paste(load1998$Year,load1998$Month,load1998$Day),"%Y %m %d")
#将日期组织起来为后面转置用
rownames(load1998)<-dateSting
load1998.filter<-load1998[,-c(1,2,3)]
load1998.filter.T<-data.frame(t(load1998.filter))
load1998.filter.T$Time<-rownames(load1998.filter.T)
#根据日和和类别分类
load1998.filter$Week<-weekdays(as.Date(rownames(load1998.filter)))
#将节假日引入
holiday<-read.csv("D:/fuheData/competition-data/Holidays.csv",stringsAsFactors = F)
holiday$HOLIDAY_1998<-as.Date(holiday$HOLIDAY_1998)

#构造6个指标
holiday_p1<-holiday$HOLIDAY_1998+1
holiday_s1<-holiday$HOLIDAY_1998-1
holiday_p2<-holiday$HOLIDAY_1998+2
holiday_s2<-holiday$HOLIDAY_1998-2
holiday_p3<-holiday$HOLIDAY_1998+3
holiday_s3<-holiday$HOLIDAY_1998-3
holiday_p4<-holiday$HOLIDAY_1998+4
holiday_s4<-holiday$HOLIDAY_1998-4
holiday_p5<-holiday$HOLIDAY_1998+5
holiday_s5<-holiday$HOLIDAY_1998-5
holiday_1<-unique(c(holiday_p1,holiday_s1,holiday$HOLIDAY_1998))
holiday_2<-unique(c(holiday_p2,holiday_s2,holiday_1))
holiday_3<-unique(c(holiday_p3,holiday_s3,holiday_2))
holiday_4<-unique(c(holiday_p4,holiday_s4,holiday_3))
holiday_5<-unique(c(holiday_p5,holiday_s5,holiday_4))

#添加相应的维度
load1998.filter$isholiday<-ifelse(as.Date(rownames(load1998.filter)) %in% holiday$HOLIDAY_1998,1,0)
load1998.filter$isholiday_1<-ifelse(as.Date(rownames(load1998.filter)) %in% holiday_1,1,0)
load1998.filter$isholiday_2<-ifelse(as.Date(rownames(load1998.filter)) %in% holiday_2,1,0)
load1998.filter$isholiday_3<-ifelse(as.Date(rownames(load1998.filter)) %in% holiday_3,1,0)
load1998.filter$isholiday_4<-ifelse(as.Date(rownames(load1998.filter)) %in% holiday_4,1,0)
load1998.filter$isholiday_5<-ifelse(as.Date(rownames(load1998.filter)) %in% holiday_5,1,0)

#加载温度
temperature<-read.csv("D:/fuheData/competition-data/Temperature1998.csv",stringsAsFactors = F)
#绑定温度
load1998.filter$temperature<-na.omit(temperature$Temperature..oC.)
#求出负荷的均值
load1998.filter$fuheMean<-rowMeans(load1998.filter[,c(1:48)])
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

load1998.filter$weekNum<-sapply(load1998.filter$Week, changeWeek)
load1998.filter$Month<-load1998$Month
load1998.filter$Day<-load1998$Day

load1998.filter$fuheMean21<-scale(load1998.filter$fuheMean,center=FALSE,scale = T)
load1998.filter$weekNum21<-scale(load1998.filter$weekNum,center=FALSE,scale = T)
load1998.filter$temperature21<-scale(load1998.filter$temperature,center=FALSE,scale = T)
load1998.filter$fuheMean21<-scale(load1998.filter$fuheMean,center=FALSE,scale = T)
load1998.filter$Month21<-as.numeric(scale(load1998.filter$Month,center = F,scale = T))
load1998.filter$Day21<-as.numeric(scale(load1998.filter$Day,center = F,scale = T))
load1998.filter$weekNum<-as.factor(load1998.filter$weekNum)
pre_load1998_cla = predict(rp_load1997_reg,load1998.filter,type = "class")
load1998.filter$type<-pre_load1998_cla

#利用神经网络进行第一种类的模型
load1998.type1<-load1998.filter[load1998.filter$type==1,]
load1998.type1<-load1998.type1[,-c(49,57,61,62,63,64,65,66)]
result1=NULL
for(i in c(1:nrow(load1998.type1))){
  k = load1998.type1[i,]
  property<-as.data.frame(k[c(49:58)])
  for(i in c(1:48)){
    timeNumber=ifelse(i%%2==0,i/2,(i/2)-0.2)
    tk<-k[[i]]
    property$v=tk
    property$tn=timeNumber
    #singleResult<-as.data.frame(as.vector(c(v=tk,tn=timeNumber)),property)
    singleResult=property
    if(is.null(result)){
      result1=singleResult
    }else{
      result1=rbind(result,singleResult)
    }
  }
}

result$weekNum<-as.numeric(result$weekNum)
ascData<-result
trainData<-data.frame(scale(result))
xNum=nrow(trainData)

pre={}
for(i in c(1:10)){
  predict<-predict(nn1,trainData[i,])
  real=ascData[i,"v"]
  predict=predict*sd(ascData[,"v"])+mean(ascData[,"v"])
  pre<-cbind(pre,predict)
  percent <- (predict-real)*100/real
  res <- paste("预测值：",predict,"实际值：",real,"误差：",percent,"%")
  print(res)
}