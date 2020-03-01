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
