**Matplotlib** 是一个 Python 的 2D绘图库，它以各种硬拷贝格式和跨平台的交互式环境生成出版质量级别的图形。

绘制线图、散点图、等高线图、条形图、柱状图、3D 图形、、甚至是图形动画等等。

- 一般使用如下语句导入:import matplotlib.pyplot as plt
- 绘图:[plt.plot(x,y)](https://matplotlib.org/api/_as_gen/matplotlib.pyplot.plot.html#matplotlib.pyplot.plot),可选color,marker,label等参数,默认的x坐标为从0开始且与y长度相同的数组,x坐标与y坐标一般使用numpy数组,也可以用列表
- 设置线条:[plt.setp()](https://matplotlib.org/api/_as_gen/matplotlib.pyplot.setp.html#matplotlib.pyplot.setp)
- 轴名称:plt.xlable('str'),plt.ylable('str)
- 添加文本:plt.txt(xpos,ypos,'str')
- 添加格子:plt.grid(True)
- 展示图片:plt.show()
- 图题:plt.title('str')
- 图示:plt.legend(),结合plot()中的label参数使用
- 获取子图:plt.sublot(nrows,ncols,index)或plt.subplot2grid((nrows,ncols),(rows,cols)),可选colspan和rowspan属性
- 创建画布:plt.figure()
- 数学表达式:[TeX表达式](https://matplotlib.org/users/mathtext.html#mathtext-tutorial)
- 非线性轴:plt.xscale('scale'),plt.yscale('scale'),可选参数log,symlog,logit等
- 填充颜色:plt.fill(x,y)和plt.fill_between(x,y,where=...)
- 条形图:plt.bar(x,y),注意多个条形图的默认颜色相同,应选择不同的颜色方便区分
- 直方图:plt.hist(x,bins),直方图是一种显示区段内数据数量的图像,x为数据,bins为数据区段,可选histtype,rwidth等属性
- 散点图:plt.scatter(x,y),散点图通常用于寻找相关性或分组,可选color,marker,label等属性
- 堆叠图:plt.stackplot(x,y1,y2,y3...),堆叠图用于显示部分对整体随时间的关系,通过利用plt.plot([],[],color,label)添加与堆叠图中颜色相同的空行,可以使堆叠图的意义更加清晰,可选colors等属性
- 饼图:plt.pie(slice),饼图用于显示部分对整体的关系,可选labels,colors,explode,autupct等属性

 



- [易百教程](https://www.yiibai.com/matplotlib/)

- [官方教程](https://matplotlib.org/users/pyplot_tutorial.html)

- 