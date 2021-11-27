
# greyforecasting

灰色预测工具包开发 

## 工具加载方法
推荐使用工具：
- R语言[https://cloud.r-project.org/](https://cloud.r-project.org/ "R语言")
- Rstudio[https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/ "Rstudio")  
先安装R再安装Rstudio，R是计算平台，Rstudio是一个IDE工具，有点像辅助界面之类的工具。在Rstudio的环境中加载工具包  

首先，下载devtools包并在环境中加载

~~~{r}
install.packages("devtools")
library("devtools")
~~~

然后，直接从github上在线加载该工具包

~~~{r}
install_github("exoplanetX/greyforecasting")
~~~

之后，以工具包方式加载

~~~{r}
library(greyforecasting)
~~~

## 更新说明

2021.11.26 修订fgm()函数，对所有建模函数增加了模型名称属性,加入一个使用ggplot2画图的函数，可以同时绘制多个模型在同一张散点图中。
建模数据创建的向量默认元素名称是连续整数，类似年份。若数据名称非连续整数，请在创建向量时不要加元素名。

~~~{r}
md1=gm(y)
md2=dgm(y)
gsplot(md1,md2)
~~~

2021.5.23 修订了conplot——被遗忘了两年的函数，同时连带修订了plot参数，可以增加xlab和ylab标题，但如果想在标题中用非ASCII字符，例如中文，推荐用如下工具：

~~~{r}
install.packages("showtext")
library("showtext")
showtext_auto()
~~~

2021.5.22 近期对代码进行了一些修补，删除了一些无用参数，增加了verhulst等模型，加入了数据导出到excel文件功能。
## 灰色预测程序包

程序包中的代码使用S3类创建，与R语言其他模型格式保持一致。
灰色预测模型程序的使用基本示例如下

## 程序包中的函数一览 

greyforecasting包中的实用函数主要有： 

模型类： 

- gm： 经典gm模型以及在GM(1,1)模型基础上拓展的模型 
- dgm: 离散灰预测模型
- gm_1: 优化背景值公式的灰色模型，参数采用辅助参数方式生成 
- gm_2: 优化背景值与响应式公式的灰色模型，参数采用辅助参数生成 
- fgm:  分数阶GM(1,1)模型。
- verhulst: verhulst模型。
- pgm:  幂模型，默认参数r=2，即与verhulst相同。参数r需要手动调整，过大会导致计算出现奇异矩阵。
- roll： 滚动机制下的灰色模型 
- abgr: 缓冲适应性灰色预测模型框架，默认使用GM(1,1)、经典缓冲算子以及滚动机制

算子类： 

- operator: 经典弱化缓冲算子 
- svwbo： 平滑变权缓冲算子 
- ago: 累加生成算子，默认阶数参数r=1
- iago: 与ago对应的累减还原算子，默认r=1

背景值： 

- background: 传统均值背景值生成公式 

可视化 

- plot: 输出一个灰预测类型变量的拟合结果 
- conplot: 输出多个灰预测类型变量的拟合结果 
- gsplot: 使用ggplot2绘图散点图，可以保存图片。
- 
结果分析：

- coef： 提取一个灰预测模型的参数估计值 
- summary: 查看一个灰预测模型计算结果简报 

结果输出：

- gssave: 将一个模型计算结果存入excel文件中

以上所有函数的使用方法均可在R中调取使用说明，如 

~~~{r}
help(gm) #查看gm函数的功能、格式和使用帮助
~~~

## 创建基本GM(1,1)模型
关于模型的具体参数设置可以查看相关帮助文件
## 灰色预测基本模型

1. GM(1,1)模型的计算方法

直接计算模型，模型类函数保持一致的参数格式，返回计算结果和参数。
~~~
g<-gm(y) #对案例数据y建立灰预测模型
g<-gm(y,ntest=1,term=2) #对y建模，生成2期预测数据，样本内1个数据留作验证
~~~
变量g中记录了灰色模型的所有计算结果和参数，可以使用$符号按元素提取g中存储的计算结果，并与r中命令格式保持一致，可以使用诸如coef提取参数，如：

~~~
coef(g)
~~~ 

另外，直接计算y的预测值可以使用gmprocess函数。目前gmprocess推荐作为过渡函数，在二次开发时候方便使用，建模可以直接使用模型函数。

~~~{r}
y #案例数据
gmprocess(y) 
gmprocess(y, pattern="parameter") #输出模型参数
~~~ 

gmprocess()函数是GM(1,1)模型计算的集成函数，参数包括三个： 

- y:（不可缺省）原始数据，至少4个元素以上的向量； 

- pattern：（可缺省）预测类型，默认为“forecast",即仅返回GM(1,1)预测数值，当设为“parameter"时返回一个greyforecasting类型的所有参数值。

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

## 滚动建模机制 

~~~{r} 
model1<-roll(y,rollterm=3)
~~~ 

对序列y进行滚动建模，默认采用四数据为一个数据切片逐步滚动方式，模型采用GM(1,1)模型，生成3个外推预测值。roll函数返回值也是greyforecasting类，记录了整个算法的计算结果. 
roll函数的主要参数：
- y：建模序列 
- ntest: 序列后n位设置为样本外测试数据 
- rollterm： 滚动外推预测期数 
- model:用于对数据切片建模的基本模型
- buff: 对每个切片数据段上使用的缓冲算子，默认为NA 
- intensity: 缓冲算子的可变权重，默认为各缓冲算子的默认权值

具体参数请查阅 
~~~{r}
help(roll)
~~~ 
## 加入缓冲调节的滚动算法 

1.  roll函数调入缓冲算子

roll函数可以调入缓冲算子对数据切片进行调节，具体方法如下： 
~~~{r}
model1<-roll(y,rollterm=3,buff=operator,intensity=0.6)
~~~
在原滚动算法基础上，对数据切片加入平均弱化缓冲算子作用，调节系数为0.6。 

2.  自适应缓冲滚动预测算法 

在roll函数基础上，选取缓冲算子最优调节权重，实现对序列趋势自适应的预测算法。 
~~~{r} 
model2<-abgr(y,ntest=1,model=gm,buff=svwbo，term=1)
~~~ 

abgr是在roll基础上进行的二次开发，参数与roll函数参数基本相同，其中外推期数term对应roll中的rollterm。

## 模型结果的输出 

该工具包代码采用S3类编写，即所有计算输出均与R语言建模方法的查看保持一致。例如:

~~~{r}
model3<-gm(y,ntest=1,term=1) #建立GM(1,1)模型
model3 #直接打出保存模型的变量名，简要输出模型结果和参数值
summary(model3) #查看模型model3的计算结果
plot(model3) #做出model3的拟合图形
coef(model3) #提取model3的参数估计值
~~~

其中plot作图函数参数较多，列举几个重要参数： 
~~~{r} 
model3<-gm(y,ntest=1,term=1) #建立GM(1,1)模型，末尾预留1期测试数据，预测1期
plot(model3,xlab="year",forecast=TRUE) #做出model3拟合图，图中包含预测部分
~~~ 

conplot函数将多个模型放入同一张图内做对比，格式类似plot函数

~~~{r}
md1=gm(y)
md2=dgm(y)  #对比模型只考虑的等长拟合和预测的情况
md3=verhulst(y)
conplot(list(md1,md2,md3),forecast=TRUE) #模型以列表变量形式放入
~~~

其他参数请参考

~~~{r}
help(plot.greyforecasting) 
~~~ 

模型数据存入excel文件当中

~~~{r}
md=gm(y,term=2)
gssave(md) #默认存储路径为本地用户文件夹
~~~

## 模型参数的简要说明

由模型函数生成的计算结果存入'greyforecasting'类变量中，包含属性如下：

- data：建模用的原始数据。
- test: 原始数据中末尾留作检验的测试数据。
- parameter: 灰预测模型的参数，a 增长系数，b 灰做用量，ax 背景值组合参数(部分采用优化技术的模型有该参数，经典GM(1,1)模型等效设置为0.5)。
- fitted: 拟合数据。
- term: 外推预测长度。
- forecasts: 预测值。
- mape.in: 拟合数据的平均相对误差率。
- mape.out: 预测检验数据的平均相对误差率(仅当设置了ntest检验数据参数时生成)
- method:包含了模型所用的方法、名称等用于识别的属性。

## 提供了一些测试数据

- y :增长序列，中国温室气体排放数据，2000-2014年。
- yr :递减序列，由y生成的倒置序列，用于检验递减序列的建模效果。
- ghg :data.frame格式数据，国家统计局公布的中国能耗相关排放数据。
- ghg2 :ghg之后的校正数据。
- owners :中国机动车保有量数据，2006-2010。
- logiscost :中国食品冷链物流需求量，2014-2019。

### 未完待续......
The package now is  a brief tool in R, and plans to embrace all the algorithms of grey forecast theory.
Any problems please contact nuaa_xuning@163.com 
