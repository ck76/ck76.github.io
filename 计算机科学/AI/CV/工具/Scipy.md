[TOC]

## Scipy简介

Scipy是一个用于数学、科学、工程领域的常用软件包，可以处理插值、积分、优化、图像处理、常微分方程数值解的求解、信号处理等问题。**它用于有效计算Numpy矩阵，使Numpy和Scipy协同工作，高效解决问题。**

Scipy是由针对特定任务的子模块组成：

| 模块名            | 应用领域           |
| ----------------- | ------------------ |
| scipy.cluster     | 向量计算/Kmeans    |
| scipy.constants   | 物理和数学常量     |
| scipy.fftpack     | 傅立叶变换         |
| scipy.integrate   | 积分程序           |
| scipy.interpolate | 插值               |
| scipy.io          | 数据输入输出       |
| scipy.linalg      | 线性代数程序       |
| scipy.ndimage     | n维图像包          |
| scipy.odr         | 正交距离回归       |
| scipy.optimize    | 优化               |
| scipy.signal      | 信号处理           |
| scipy.sparse      | 稀疏矩阵           |
| scipy.spatial     | 空间数据结构和算法 |
| scipy.special     | 一些特殊的数学函数 |
| scipy.stats       | 统计               |



```python
用Scipy的scipy.stats中的统计函数分析随机数
stats提供了产生连续性分布的函数
均匀分布（uniform）
                x=stats.uniform.rvs(size = 20) 生成20个[0,1]均匀分布随机数
-正态分布（norm）
                x=stats.norm.rvs(size = 20) 生成20个正态分布随机数
-贝塔分布（beta）
                x=stats.beta.rvs(size=20，a=3,b=4)生成20个服从参数a=3,b=4贝塔分布随机数
-离散分布
-伯努利分布（Bernoulli）
-几何分布（geom）
-泊松分布（poisson）
x=stats.poisson.rvs(0.6,loc=0,size = 20)生成20个服从泊松分布随机数
```



   python在科学计算领域有三个非常受欢迎库，numpy、SciPy、matplotlib。numpy是一个高性能的多维数组的计算库，SciPy是构建在numpy的基础之上的，它提供了许多的操作numpy的数组的函数。SciPy是一款方便、易于使用、专为科学和工程设计的python工具包，它包括了统计、优化、整合以及线性代数模块、傅里叶变换、信号和图像图例，常微分方差的求解等，SciPy完整的教程https://docs.scipy.org/doc/scipy/reference/index.html。

### 一、SciPy教程

1. 介绍
2. 基本功能
3. 特殊功能（scipy.special）
4. 整合（scipy.integrate）
5. 优化（scipy.optimize）
6. 插值（scipy.interpolate）
7. 傅立叶变换（scipy.fftpack）
8. 信号处理（scipy.signal）
9. 线性代数（scipy.linalg）
10. ARPACK的稀疏特征值问题
11. 压缩稀疏图例程（scipy.sparse.csgraph）
12. 空间数据结构和算法（scipy.spatial）
13. 统计数据（scipy.stats）
14. 多维图像处理（scipy.ndimage）
15. 文件IO（scipy.io）

###  二、API参考

所有函数和类的确切API，由docstrings给出。 API记录了所有函数的预期类型和允许的功能，以及算法可用的所有参数。

1. 集群包（scipy.cluster）
2. 常数（scipy.constants）
3. 离散傅立叶变换（scipy.fftpack）
4. 集成和ODE（scipy.integrate）
5. 插值（scipy.interpolate）
6. 输入和输出（scipy.io）
7. 线性代数（scipy.linalg）
8. [杂项例程（scipy.misc）](https://mp.csdn.net/postedit)
9. 多维图像处理（scipy.ndimage）
10. 正交距离回归（scipy.odr）
11. 优化和根查找（scipy.optimize）
12. 信号处理（scipy.signal）
13. 稀疏矩阵（scipy.sparse）
14. 稀疏线性代数（scipy.sparse.linalg）
15. 压缩稀疏图例程（scipy.sparse.csgraph）
16. 空间算法和数据结构（scipy.spatial）
17. 特殊功能（scipy.special）
18. 统计函数（scipy.stats）
19. 掩码数组的统计函数（scipy.stats.mstats）
20. 低级回调函数



- [易百教程](https://www.yiibai.com/scipy)

- <https://blog.csdn.net/wsp_1138886114/article/details/80444621>