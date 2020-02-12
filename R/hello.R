# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#获取数据集
#' @title getDataSet
#' @return da
#' @export
#' @author yx
getDataSet <- function(){
  #  读取api
  res <- GET("http://39.108.91.24:8099/query")
  #  获取raw数据
  data_raw <- res$content
  #  raw转字符串
  data <- rawToChar(data_raw)
  #  转化字符串编码
  Encoding(data) <- "UTF-8"
  #  转化为数据集
  da <- jsonlite::fromJSON(data)
  #  返回数据集
    return(da)
}

#导出excel
#' @title exportExcel
#' @return String
#' @export
#' @author yx
exportExcel <- function(data){
  columnName <- names(data)
  names(data) <- editColumnName(columnName)
  url <- getwd()
  write.csv(data,file=paste(url,"data.csv",sep = '/'))
  print(paste("文件保存地址：",url,"/data.csv",sep = "",collapse = ""))
  return('导出成功')
}

# data:数据集
# province：地区（省）
# item：列名（如data中time）
# name: 列名描述
# 获取柱状图
#' @title getHistogram
#' @return String
#' @export
#' @author hm
getHistogram <- function(data,province){
  x<-data

  t<-x[which(x$province==province),]

  ggplot(t, aes(format(as.Date(time),format="%m-%d"), as.numeric(confirmed_num))) +
    geom_col(fill='firebrick') + theme_minimal(base_size = 14) +
    xlab(NULL) + ylab(NULL)+ggtitle(paste(province,'确诊数柱状图',sep = "",collapse = ""))
}

#' @title initLib
#' @return String
#' @export
#' @author yx
initLib <- function(){
  library(httr)
  library(rjson)
  library(remotes)
  library(ggplot2)

  library(mapdata)
  library(maptools)
  library(plyr)
  library(sp)
  library(mapproj)
  return("包加载成功")
}

# 查看列名技巧描述
#' @title viewColumeAnddescribe
#' @return String
#' @export
#' @author yx
viewColumeAnddescribe <- function(){
  print("province 地区")
  print("remove_observation_num 解除医学观察数")
  print("touch_num 密切接触者数")
  print("confirmed_num 累计确诊数")
  print("severe_num 现有重症数")
  print("accept_num 接受医学观察数")
  print("time 发布日期")
  print("cure_num 累计治愈数")
  print("die_num 累计死亡数")
}

# 折线图
#' @title getLine
#' @return String
#' @export
#' @author yx
getLine <- function(data,colume_name='confirmed_num',address='全部',startTime='',endTime=''){
  if(address!='全部'){
    # 筛选地区
    data <- data[data$province==address,]
  }
  if(startTime!=''){
    data <- data[data$time>=startTime,]
  }
  if(endTime!=''){
    data <- data[data$time<=endTime,]
  }
  head(data)
  #去掉科学计数
  options(scipen=200)

  gg <- switch (colume_name,
    "remove_observation_num" = ggplot(data,aes(x=time,y=remove_observation_num,colour=province,group=province)),
    "touch_num" = ggplot(data,aes(x=time,y=touch_num,colour=province,group=province)),
    "severe_num" = ggplot(data,aes(x=time,y=severe_num,colour=province,group=province)),
    "accept_num" = ggplot(data,aes(x=time,y=accept_num,colour=province,group=province)),
    "cure_num" = ggplot(data,aes(x=time,y=cure_num,colour=province,group=province)),
    "die_num " = ggplot(data,aes(x=time,y=die_num,colour=province,group=province)),
    ggplot(data,aes(x=time,y=confirmed_num,colour=province,group=province))
  )

  desc <- switch(colume_name,
                 "remove_observation_num"="解除医学观察数",
                 "touch_num"="密切接触者数",
                 "severe_num"="现有重症数",
                 "accept_num"="接受医学观察数",
                 "cure_num"="累计治愈数",
                 "die_num"="累计死亡数",
                 "累计确诊数"
                 )
  gg+geom_line()+xlab("时间")+ylab(desc)+ggtitle(paste(address,desc,"折线图",sep = "",collapse = ""))
}

# 列名添加解释
editColumnName <- function(columnName){
  newColumnName <- c()
  for (i in columnName) {
    name <- switch (i,
      "province" = "province(地区)",
      "remove_observation_num" = "remove_observation_num(解除医学观察数)",
      "touch_num" = "touch_num(密切接触者数)",
      "confirmed_num" = "confirmed_num(累计确诊数)",
      "severe_num" = "severe_num(现有重症数)",
      "accept_num" = "accept_num(接受医学观察数)",
      "time" = "time(发布日期)",
      "cure_num" = "cure_num(累计治愈数)",
      "die_num" = "die_num(累计死亡数)"
    )
    newColumnName[length(newColumnName)+1] <- name
  }
  return(newColumnName)
}

# 热力图
#' @title getLine
#' @return String
#' @export
#' @author yx
getTd <- function(data,colume_name='confirmed_num',time='',low="white",high="red"){
  #去掉科学计数
  options(scipen=200)

  china_map <- readShapePoly(system.file("bou2_4p.shp", package="GzbdiDataSet"))
  x<-china_map@data
  xs<-data.frame(x,id=seq(0:924)-1)#地图中共计有925个地域信息
  china_map1<-fortify(china_map)
  china_map_data<-join(china_map1,xs,type="full")#基于id进行连接
  #unique(china_map@data$NAME)#查看地图数据中保存的地域名称，编辑自己的数据与其一致

  mydata <- getChinaData(data)
  names(mydata)[1] <- "NAME"
  # 筛选时间
  nowTime <- time
  if(time==''){
    nowTime <- format(Sys.time(),format = "%Y-%m-%d")
  }
  mydata <- mydata[mydata$time==nowTime,]

  china_data <- join(china_map_data, mydata, type="full")#基于NAME字段进行连接，NAME字段来自于地图文件中

  province_city<-read.csv(system.file("pcity.csv", package="GzbdiDataSet"),header=T,as.is=T)#获取省会城市坐标

  numPlot <- switch (colume_name,
             "remove_observation_num" = geom_polygon(aes(group=group,fill=remove_observation_num),colour="grey",size=0.01),
             "touch_num" = geom_polygon(aes(group=group,fill=touch_num),colour="grey",size=0.01),
             "severe_num" = geom_polygon(aes(group=group,fill=severe_num),colour="grey",size=0.01),
             "accept_num" = geom_polygon(aes(group=group,fill=accept_num),colour="grey",size=0.01),
             "cure_num" = geom_polygon(aes(group=group,fill=cure_num),colour="grey",size=0.01),
             "die_num" = geom_polygon(aes(group=group,fill=die_num),colour="grey",size=0.01),
             geom_polygon(aes(group=group,fill=confirmed_num),colour="grey",size=0.01)
          )
  desc <- switch(colume_name,
                 "remove_observation_num"="解除医学观察数",
                 "touch_num"="密切接触者数",
                 "severe_num"="现有重症数",
                 "accept_num"="接受医学观察数",
                 "cure_num"="累计治愈数",
                 "die_num"="累计死亡数",
                 "累计确诊数"
  )
  ggplot(china_data,aes(long,lat))+
    numPlot+
    scale_fill_gradient(name=desc,low=low,high=high)+
    coord_map("polyconic")+
    geom_text(aes(x=jd,y=wd,label=name),data=province_city,colour="black",size=2.5)+
    labs(title=paste(nowTime,desc,"热力图",seq="",collapse = ""))+
    theme(
      panel.grid=element_blank(),
      panel.background=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      axis.title=element_blank()
    )

}

# 提取出中国的信息
getChinaData <- function(data){

  china_area <- c("上海","广东","吉林","辽宁","福建","浙江","海南","广西","内蒙古",
                  "湖南","宁夏","安徽","澳门","湖北","北京","云南","江苏","河南",
                  "陕西","天津","江西","山东","黑龙江","四川","台湾","甘肃","山西",
                  "西藏","贵州","重庆","香港","青海","新疆","河北")

  newArea <- data[1,]
  m <- 1
  n <- 1
  for (i in data[,1]){
    if(data[n,]$province %in% china_area){
      newArea[m,] <- data[n,]
      newArea[m,1] <- getEditProvince(newArea[m,1])
      m <- m+1
    }
    n <- n+1
  }
  return(newArea)
}


# 获取完整的省份
getEditProvince <- function(area){
  address <- area
  if("上海"==area){
    address <- "上海市"
  }
  if("广东"==area){
    address <- "广东省"
  }
  if("吉林"==area){
    address <- "吉林省"
  }
  if("辽宁"==area){
    address <- "辽宁省"
  }
  if("福建"==area){
    address <- "福建省"
  }
  if("浙江"==area){
    address <- "浙江省"
  }
  if("海南"==area){
    address <- "海南省"
  }
  if("广西"==area){
    address <- "广西壮族自治区"
  }
  if("内蒙古"==area){
    address <- "内蒙古自治区"
  }
  if("湖南"==area){
    address <- "湖南省"
  }
  if("宁夏"==area){
    address <- "宁夏回族自治区"
  }
  if("安徽"==area){
    address <- "安徽省"
  }
  if("湖北"==area){
    address <- "湖北省"
  }
  if("北京"==area){
    address <- "北京市"
  }
  if("云南"==area){
    address <- "云南省"
  }
  if("江苏"==area){
    address <- "江苏省"
  }
  if("河南"==area){
    address <- "河南省"
  }
  if("陕西"==area){
    address <- "陕西省"
  }
  if("天津"==area){
    address <- "天津市"
  }
  if("江西"==area){
    address <- "江西省"
  }
  if("山东"==area){
    address <- "山东省"
  }
  if("黑龙江"==area){
    address <- "黑龙江省"
  }
  if("四川"==area){
    address <- "四川省"
  }
  if("台湾"==area){
    address <- "台湾省"
  }
  if("甘肃"==area){
    address <- "甘肃省"
  }
  if("山西"==area){
    address <- "山西省"
  }
  if("西藏"==area){
    address <- "西藏自治区"
  }
  if("贵州"==area){
    address <- "贵州省"
  }
  if("重庆"==area){
    address <- "重庆市"
  }
  if("香港"==area){
    address <- "香港特别行政区"
  }
  if("青海"==area){
    address <- "青海省"
  }
  if("新疆"==area){
    address <- "新疆维吾尔自治区"
  }
  if("河北"==area){
    address <- "河北省"
  }
  return(address)
}

# 热力图
#' @title getTdRatio
#' @return String
#' @export
#' @author yx
getTdRatio <- function(data,colume_name='cure_num',time='',low="white",high="red"){
  #去掉科学计数
  options(scipen=200)
  url <- system.file("bou2_4p.shp", package="GzbdiDataSet")
  china_map <- readShapePoly(url)
  x<-china_map@data
  xs<-data.frame(x,id=seq(0:924)-1)#地图中共计有925个地域信息
  china_map1<-fortify(china_map)
  china_map_data<-join(china_map1,xs,type="full")#基于id进行连接
  #unique(china_map@data$NAME)#查看地图数据中保存的地域名称，编辑自己的数据与其一致

  mydata <- getChinaData(data)
  names(mydata)[1] <- "NAME"
  # 筛选时间
  nowTime <- time
  if(time==''){
    nowTime <- format(Sys.time(),format = "%Y-%m-%d")
  }
  mydata <- mydata[mydata$time==nowTime,]

  china_data <- join(china_map_data, mydata, type="full")#基于NAME字段进行连接，NAME字段来自于地图文件中

  province_city<-read.csv(system.file("pcity.csv", package="GzbdiDataSet"),header=T,as.is=T)#获取省会城市坐标

  numPlot <- switch (colume_name,
                     "remove_observation_num" = geom_polygon(aes(group=group,fill=remove_observation_num/confirmed_num),colour="grey",size=0.01),
                     "touch_num" = geom_polygon(aes(group=group,fill=touch_num/confirmed_num),colour="grey",size=0.01),
                     "severe_num" = geom_polygon(aes(group=group,fill=severe_num/confirmed_num),colour="grey",size=0.01),
                     "accept_num" = geom_polygon(aes(group=group,fill=accept_num/confirmed_num),colour="grey",size=0.01),
                     "die_num" = geom_polygon(aes(group=group,fill=die_num/confirmed_num),colour="grey",size=0.01),
                     "cure_num" = geom_polygon(aes(group=group,fill=cure_num/confirmed_num),colour="grey",size=0.01)
  )
  desc <- switch(colume_name,
                 "remove_observation_num"="解除医学观察数/累计确诊数",
                 "touch_num"="密切接触者数/累计确诊数",
                 "severe_num"="现有重症数/累计确诊数",
                 "accept_num"="接受医学观察数/累计确诊数",
                 "die_num"="累计死亡数/累计确诊数",
                 "cure_num"="累计治愈数/累计确诊数"
  )
  ggplot(china_data,aes(long,lat))+
    numPlot+
    scale_fill_gradient(name=desc,low=low,high=high)+
    coord_map("polyconic")+
    geom_text(aes(x=jd,y=wd,label=name),data=province_city,colour="black",size=2.5)+
    labs(title=paste(nowTime,desc,"热力图",seq="",collapse = ""))+
    theme(
      panel.grid=element_blank(),
      panel.background=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      axis.title=element_blank()
    )

}


# 热力图
#' @title getTdRatioCustom
#' @return String
#' @export
#' @author yx
getTdRatioCustom <- function(data,dividend,divisor,desc='比例',time = '',low="white",high="red"){

  data1 <- dividend/divisor

  data[,length(data)+1] <- data1

  names(data)[length(data)] <- "newData"

  #去掉科学计数
  options(scipen=200)
  url <- system.file("bou2_4p.shp", package="GzbdiDataSet")
  china_map <- readShapePoly(url)
  x<-china_map@data
  xs<-data.frame(x,id=seq(0:924)-1)#地图中共计有925个地域信息
  china_map1<-fortify(china_map)
  china_map_data<-join(china_map1,xs,type="full")#基于id进行连接
  #unique(china_map@data$NAME)#查看地图数据中保存的地域名称，编辑自己的数据与其一致

  mydata <- getChinaData(data)
  names(mydata)[1] <- "NAME"
  # 筛选时间
  nowTime <- time
  if(time==''){
    nowTime <- format(Sys.time(),format = "%Y-%m-%d")
  }
  mydata <- mydata[mydata$time==nowTime,]

  china_data <- join(china_map_data, mydata, type="full")#基于NAME字段进行连接，NAME字段来自于地图文件中

  province_city<-read.csv(system.file("pcity.csv", package="GzbdiDataSet"),header=T,as.is=T)#获取省会城市坐标

  numPlot <- geom_polygon(aes(group=group,fill=newData),colour="grey",size=0.01)

  ggplot(china_data,aes(long,lat))+
    numPlot+
    scale_fill_gradient(name=desc,low=low,high=high)+
    coord_map("polyconic")+
    geom_text(aes(x=jd,y=wd,label=name),data=province_city,colour="black",size=2.5)+
    labs(title=paste(nowTime,desc,"热力图",seq="",collapse = ""))+
    theme(
      panel.grid=element_blank(),
      panel.background=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      axis.title=element_blank()
    )

}
