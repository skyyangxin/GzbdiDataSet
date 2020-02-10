exportExcel <- function(data){
  url <- getwd()
  write.csv(data,file=paste(url,"data.csv",sep = '/'))
  return('导出成功')
}
