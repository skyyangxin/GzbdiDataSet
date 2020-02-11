# GzbdiDataSet
这是一个数据集

第一步（首次加载）
需要在控制台先执行以下命令
install.packages("remotes")

第二步
library(remotes)

第三步(拉取包和初始化环境)
remotes::install_github("skyyangxin/GzbdiDataSet")
library(GzbdiDataSet)
initLib()

第四步(获取数据集)
x <- getDataSet()

获取柱状图
getHistogram(x,'贵州')

导出excel
exportExcel(x)

查看列名及其描述
viewColumeAnddescribe()

获取折线图
getLine(x)   全部地区确诊数/时间折线图
getLine(data=x,colume_name = 'cure_num',address = '贵州',startTime = '2020-02-01',endTime = '2020-02-10') 筛选折线图

