

**NumPy**（Numeric Python）系统是Python的一种开源的**数值计算扩展**。这种工具可用来**存储和处理大型矩阵**，比Python自身的嵌套列表（nested list structure)结构要高效的多（该结构也可以用来表示矩阵（matrix））。

**NumPy**提供了许多高级的数值编程工具，如：矩阵数据类型、矢量处理，以及精密的运算库。专为进行严格的数字处理而产生。多为很多大型金融公司使用，以及核心的科学计算组织如：Lawrence Livermore，NASA用其处理一些本来使用C++，Fortran或Matlab等所做的任务。

- Numpy是一个用于进行数组运算的库
- Numpy中最重要的对象是称为ndarray的N维数组类型
- 一般使用如下语句导入:import numpy as np
- 创建数组:numpy.array(object, dtype = None, copy = True, order = None, subok = False, ndmin = 0)
- 可以用np.dtype()定义结构体
- 数组维度:ndarray.shape
- 数组维数:ndarray.ndim
- 调整数组维度:ndarray.reshape(shape)
- 创建未初始化数组:numpy.empty(shape, dtype = float, order = 'C')
- 创建零数组:numpy.zeros(shape, dtype = float, order = 'C')
- 创建一数组:numpy.ones(shape, dtype = float, order = 'C')
- 用现有数据创建数组:numpy.asarray(a, dtype = None, order = None)
- 按数值范围创建数组:numpy.arange(start = 0, stop, step = 1, dtype),类似的有linspace()和logspace()
- 切片:b=a[start:stop:step],可以用...代表剩余维度
- 整数索引:每个整数数组表示该维度的下标值,b=a[[r1, r2], [c1, c2]]
- 布尔索引:返回是布尔运算的结果的对象,可以用&或|连接()分隔的条件
- 在 NumPy 中可以对形状不相似的数组进行操作,因为它拥有广播功能,我的理解是,广播是一种维度的单方向拉伸
- 数组迭代:numpy.nditer(ndarray)或ndarray.flat
- 数组长度:len(arr)
- 访问第i个元素:一维数组用a[i],多维数组用a.flat[i]
- 数组转置:ndarray.T
- 数组分割:numpy.split(ary, indices_or_sections, axis),第二项的值为整数则表明要创建的等大小的子数组的数量,是一维数组则表明要创建新子数组的点。
- 追加值:numpy.append(arr, values, axis)
- 插入值:numpy.insert(arr, idx, values, axis)
- 删除值:numpy.delete(arr, values, axis)
- 去重数组:numpy.unique(arr, return_index, return_inverse, return_counts)
- 字符串函数:numpy.char类
- 三角函数:numpy.sin(arr),numpy.cos(arr),numpy.tan(arr)
- 四舍五入:numpy.around(arr,decimals)
- 向下取整:numpy.floor(arr)
- 向上取整:numpy.ceil(arr)
- 取倒数:numpy.reciprocal(arr),注意对于大于1的整数返回值为0
- 幂运算:numpy.power(arr,pow),pow可以是一个数,也可以是和arr对应的数组
- 取余:numpy.mod(a,b),b可以是一个数,也可以是和a对应是数组
- 最小值:numpy.amin(arr,axis)
- 最大值:numpy.amax(arr,axis)
- 数值跨度:numpy.ptp(arr,axis)
- 算术平均值:numpy.mean(arr,axis)
- 标准差:numpy.std(arr)
- 方差:numpy.var(arr)
- 副本的改变会影响原数组(赋值),视图的改变不会影响原数组(ndarray.view(),切片,ndarray.copy())
- 线性代数:numpy.linalg模块



- [菜鸟教程](https://www.runoob.com/numpy/numpy-tutorial.html)

- [菜鸟例子](https://www.runoob.com/w3cnote/matplotlib-tutorial.html)

- [易百教程](https://www.yiibai.com/numpy)