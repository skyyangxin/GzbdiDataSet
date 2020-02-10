# GzbdiDataSet
这是一个数据集

第一步（首次加载）\n
需要在控制台先执行以下命令\n
install.packages("httr")\n
install.packages("rjson")\n
install.packages("remotes")\n

第二步\n
library(httr)\n
library(rjson)\n
library(remotes)\n

第三步\n
remotes::install_github("skyyangxin/GzbdiDataSet")\n
library(GzbdiDataSet)\n

第四步(获取数据集)\n
x <- getDataSet()\n
