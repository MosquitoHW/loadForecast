library(nnet)
source <- c(10930,10318,10595,10972,7706,6756,9092,10551,9722,10913,11151,8186,6422,
            6337,11649,11652,10310,12043,7937,6476,9662,9570,9981,9331,9449,6773,6304,9355,10477,
            10148,10395,11261,8713,7299,10424,10795,11069,11602,11427,9095,7707,10767,12136,12812,
            12006,12528,10329,7818,11719,11683,12603,11495,13670,11337,10232,13261,13230,15535,
            16837,19598,14823,11622,19391,18177,19994,14723,15694,13248,9543,12872,13101,15053,
            12619,13749,10228,9725,14729,12518,14564,15085,14722,11999,9390,13481,14795,15845,
            15271,14686,11054,10395,14775,14618,16029,15231,14246,12095,10473,15323,15381,14947)
srcLen<-length(source)
for(i in 1:10){  #预测最后十个数；
  real <- source[srcLen-i+1] #实际值
  xNum=(srcLen-i+1)%/%7      #组数
  yNum=7                     #每组7个数
  data<-array(1:(xNum*yNum),c(xNum,yNum))
  pre=srcLen-i+1;
  for(x in 1:xNum){          #数组赋值
    for(y in 1:yNum){
      data[x,y]=source[pre]
      pre=pre-1;
    }
    if(pre<7){
      break;
    }
  }
  ascData<-array(1:(xNum*yNum),c(xNum,yNum)) #数组逆序
  for(x in 1:xNum){
    for(y in 1:yNum){
      ascData[x,y]=data[xNum-x+1,y]
    }
  }
  colnames(ascData) <- c("a","b","c","d","e","f","g") #每列列名
  trainData<-data.frame(scale(ascData[,c(1:7)]))  
  nn<-nnet(a~b+c+d+e+f+g,trainData[1:(xNum-1),],size=10,decay=0.01,maxit=1000,linout=F,trace=F)
  predict<-predict(nn,trainData[xNum,])
  
  predict=predict*sd(ascData[,1])+mean(ascData[,1])
  percent <- (predict-real)*100/real
  res <- paste("预测值：",predict,"实际值：",real,"误差：",percent)
  print(res)
}