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
    xlab(NULL) + ylab(NULL)+ggtitle(paste(province,'确诊数柱状图',sep = "",collapse = "")
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
  return("包加载成功")
}

# 查看列名技巧描述
#' @title getLine
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

