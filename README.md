
# greyforecasting

灰色预测工具包开发 

## 工具加载方法
推荐使用工具：
- R语言[https://cloud.r-project.org/](https://cloud.r-project.org/ "R语言")
- Rstudio[https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/ "Rstudio")  
先安装R再安装Rstudio，R是计算平台，Rstudio是一个IDE工具，有点像辅助界面之类的工具。在Rstudio的环境中加载工具包  

首先，下载devtools包并在环境中加载
~~~{r}
install.package("devtools")
library("devtools")
~~~
然后，直接从github上在线加载该工具包
~~~{r}
install_github("exoplanetX/greyforecasting")
~~~
之后，以工具包方式正常加载之后便可以使用了
~~~{r}
library(greyforecasting)
~~~
## 灰色预测程序包

程序包中的代码使用S3类创建，与R语言其他模型格式保持一致。
灰色预测模型程序的使用基本示例如下

## 程序包中的函数一览 

greyforecasting包中的实用函数主要有： 

模型类： 

- gm： 经典gm模型以及在GM(1,1)模型基础上拓展的模型 
- gm_1: 优化背景值公式的灰色模型，参数采用辅助参数方式生成 
- gm_2: 优化背景值与响应式公式的灰色模型，参数采用辅助参数生成 
- roll： 滚动机制下的灰色模型 
- abgr: 缓冲适应性灰色预测模型框架，默认使用GM(1,1)、经典缓冲算子以及滚动机制
算子类： 

- operator: 经典弱化缓冲算子 
- svwbo： 平滑变权缓冲算子 

背景值： 

- background: 传统均值背景值生成公式 

可视化 

- plot: 输出一个灰预测类型变量的拟合结果 
- conplot: 输出多个灰预测类型变量的拟合结果 

结果分析：

- coef： 提取一个灰预测模型的参数估计值 
- summary: 查看一个灰预测模型计算结果简报 

以上所有函数的使用方法均可在R中调取使用说明，如 

~~~{r}
help(gm) #查看gm函数的功能、格式和使用帮助
~~~

## 创建基本GM(1,1)模型
关于模型的具体参数设置可以查看相关帮助文件
## 灰色预测基本模型

1. GM(1,1)模型的计算方法
直接计算模型
~~~
g<-gm(y) #对案例数据y建立灰预测模型
g<-gm(y,ntest=1) #对y建模，其中最后1个数据留作验证数据
~~~
变量g中记录了灰色模型的所有计算结果和参数，可以使用$符号按元素提取g中存储的计算记过，同时与r中建模命令格式保持一致，也可以使用诸如coef提取参数可以使用 

~~~
coef(g)
~~~ 
另外，直接计算y的灰色预测数据可以使用gmprocess函数

~~~{r}
y #案例数据
gmprocess(y) 
gmprocess(y, pattern="parameter") #输出模型参数
~~~ 

gmprocess()函数是GM(1,1)模型计算的集成函数，参数包括三个： 

- y:（不可缺省）原始数据，至少4个元素以上的向量； 

- pattern：（可缺省）预测类型，默认为“forecast",即仅返回GM(1,1)预测数值，当设为“parameter"时返回一个greyforecasting类型的所有参数值;  

2. 建模结果可视化 
直接对建模结果变量使用plot函数可以生成拟合图，即 

~~~{r}
g <- gm(y)
plot(g) #输出模型g的拟合图
plot(g,forecast=TRUE) #拟合图中包含预测部分
~~~

3. 缓冲算子
缓冲算子作为函数可以单独调用，但主要是用于建模中作为参数调用，直接施加于模型的预处理部分，如

~~~{r}
g<- gm(y,buff=operator,alpha=0.6) # 在gm模型中调用经典弱化缓冲算子，作用系数为0.6(默认为0.5)
~~~


### 未完待续......

