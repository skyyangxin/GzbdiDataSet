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
    xlab(NULL) + ylab(NULL)+ggtitle(paste(province,'确诊数柱状图'))
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
