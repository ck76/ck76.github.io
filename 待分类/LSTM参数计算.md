[TOC]

# LSTM的参数问题？

比如说，有100个句子，其中一个句子有8个词，然后所有的句子都被padding成20个，每个词的向量维度是128维，那么：1. lstm的cell就有20个 ？2. lstm的unit=128 ？3. 那超参数有多少个呢？据说是每个lstm单元的参数共享，怎么感觉没几个参数呀？4. 100这个值又对应的lstm这个函数的哪个参数呢？5. 输出的维度是自己定的吗，还是由哪个参数定的呢？6. lstm的输出向量和下一个词的向量 输入到损失函数中计算损失，然后更新参数是吗？

![image-20221123233519076](https://tva1.sinaimg.cn/large/008vxvgGgy1h8ffqmdpdwj30xq0dwabt.jpg)

作者：Peace
链接：https://www.zhihu.com/question/268956632/answer/523742738
来源：知乎
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。



**回答：**

\1. lstm的cell就有20个 ？

- 在深度学习网络框架中，一个lstm cell指的是一层的LSTM，所以按照这个理解，该网络的lstm cell数量为time-step。所以*lstm的cell就有20个。*



\2. lstm的unit=128 ？

- lstm网络的unit在深度学习网络框架中是指一层的output size(hidden size), 可不与 XtX_{t}X_{t} 的dim 相等。具体需要你在代码中设置。如：LSTM_cell(unit=128)。 因为LSTM cell还会有一个[非线性变换](https://www.zhihu.com/search?q=非线性变换&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A523742738})，即里面 WtW_{t}W_{t} 权重矩阵会变换input dim 为output size。 



\3. 那超参数有多少个呢？据说是每个lstm单元的参数共享，怎么感觉没几个参数呀？

- 你指的LSTM参数应该指包括[embedding](https://www.zhihu.com/search?q=embedding&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A523742738})层的整个网络的参数的数量。LSTM的参数计算公式：num(Embedding)+num(LSTM)=Word embedding dim * Vocab size  +(hidden size * (hidden size + x_dim ) + hidden size) *4
- 因为你未指定Vocab size，所以不便计算出结果。



\4. 100这个值又对应的lstm这个函数的哪个参数呢？

- 100为样本的数量，无需指定LSTM网络某个参数。



\5. 输出的维度是自己定的吗，还是由哪个参数定的呢？

- 一个（一层）LSTM cell输出的维度大小即output size(hidden size),具体需要你在代码中设置。如：LSTM_cell(unit=128)。 



\6. lstm的输出向量和下一个词的向量 输入到损失函数中计算损失，然后更新参数是吗？

- 你指的是在某个time_stepXtX_{t}X_{t}的（一层）[LSTM cell](https://www.zhihu.com/search?q=LSTM cell&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A523742738})，实际上对于LSTM网络，每个time_stepXtX_{t}X_{t}的计算会得到的 CtC_{t}C_{t} 和HtH_{t}H_{t}，都会参与到下一个time_step Xt+1X_{t+1}X_{t+1} 的计算。但是LSTM网络参数的更新时机是在整个网络从输入到输出完成后，通过计算 与ygroudtruth与ypredictedy_{groudtruth}与y_{predicted}y_{groudtruth}与y_{predicted} 的误差通过反向传播更新所有权重。并且LSTM的所有的time_step的cell都共享参数。这意味着，time_step上的计算不会改变参数大小。





**最后科普LSTM网络的参数计算问题。**

------

## **LSTM网络参数计算**



**一、LSTM cell**

 下面这个是Recurrent NN的结构图。

![img](https://tva1.sinaimg.cn/large/008vxvgGgy1h8ffqel4s6j30z80e9acb.jpg)

图1



如果将每个time_step的那个网络简称为A，并且每个网络换成LSTM的[gated cell](https://www.zhihu.com/search?q=gated cell&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A523742738}), 则得到以下两个图的LSTM结构。

这里LSTM cell（包括深度学习框架中的LSTM cell）指的是一层cells，即A；而不是MLP中的一个节点。.

![img](https://tva1.sinaimg.cn/large/008vxvgGgy1h8ffqeibnaj30z80e9acb.jpg)

图2

![img](https://tva1.sinaimg.cn/large/008vxvgGgy1h8ffqin527j31400k03zx.jpg)

图3



[Num_units](https://www.zhihu.com/search?q=Num_units&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A523742738}) 指的是一层的output size(hidden size), 可不与X_{t}的dim 相等。因为LSTM cell还会有一个非线性变换，即里面W_{t}权重矩阵会变换input dim 为output size。故权重矩阵使得在time_step_{t} 的x的dim 变换为output hidden size。如下图。 

![img](https://tva1.sinaimg.cn/large/008vxvgGgy1h8ffqgzzo3j30is07sgm8.jpg)

图4



强调一下，图2，图4的框框中是一个层，相当于图1，图3的框框。图1，图3里面的圈圈相当于把MLP的每个计算节点画出。

**二、LSTM的参数数量**

 现在用一个seq2seq的encoder-decoder结构的LSTM网络来举例。



![img](https://tva1.sinaimg.cn/large/008vxvgGgy1h8ffqhsiu8j30v809pdgk.jpg)

encoder-decoder



并假设：

encoder、decoder各只有一层LSTM ：

```text
cell的units_num(hidden size)=1000
steps = 2000
Word embedding dim (x_dim) = 500
Vocab size = 50000
```

Sequence2sequence 结构，encoder, decoder都为一层LSTM

则：

**1. Embedding:**

```text
Word embedding dim * Vocab size = 50,000 * 500 = 25M
```

**2. Encoder**

![img](https://tva1.sinaimg.cn/large/008vxvgGgy1h8ffqfxwqfj30k007jaah.jpg)

calculating lstm cell

 一层[LSTM](https://www.zhihu.com/search?q=LSTM&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A523742738})：

```text
(hidden size * (hidden size + x_dim ) + hidden size) *4 
= (1000 * 1500 + 1000) * 4 = 8M (4组gate)
```

 (hidden size + x_dim )这个即： [ht−1,xt][h_{t-1}, x_{t}][h_{t-1}, x_{t}] ，这是LSTM的结构所决定的，注意这里跟time_step无关，

**3. Decoder** 

```text
同encoder = 8M
```

**4. Output**

```text
Word embedding dim * Decoder output = Word embedding dim * Decoder hidden size
= 50,000 * 1000 = 50M
```

**所以总共有：**

```text
 25M + 8M + 8M + 50M = 91M
```

**总结：**

- 关键点：权重的计算与time_step无关，LSTM[权值共享](https://www.zhihu.com/search?q=权值共享&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A523742738}).
-  (hidden size + x_dim )这个亦即： [ht−1,xt][h_{t-1}, x_{t}][h_{t-1}, x_{t}] ，这是LSTM的结构所决定的，注意这里跟time_step无关
- 参数权重的数量，占大头的还是vocab size与embedding dim 以及output hidden size.
- LSTM的参数是RNN 的 一层的4倍的数量。

**三、[keras](https://www.zhihu.com/search?q=keras&search_source=Entity&hybrid_search_source=Entity&hybrid_search_extra={"sourceType"%3A"answer"%2C"sourceId"%3A523742738})举例**

***为了简单起见，用一个简单的LSTM，也不加emebdding.\***

 **1. input**

假设现有一个样本,Shape=(13,5),时间步是13,每个时间步的特征长度(dim)是5

**2. Codes**

```text
from keras.layers import LSTM 
from keras.models import Sequential
 
time_step=13 
feature_dim=5 
hidenfeatrue=10 
 
model=Sequential() 
model.add( LSTM(hidenfeatrue,input_shape=(time_step,featrue_dim))) 
model.summary()
```

 **3. 计算**

 因为没有embedding layer，所以只计算LSTM的参数即可。

```text
(10 * (10+5) + 10) * 4 = 640
```

**4. summary**

```text
_________________________________________________________________________________ 
Layer (type) Output Shape Param # 
========================================================== 
lstm_8 (LSTM) (None, 10)  640 
========================================================== 
Total params: 640 
Trainable params: 640 
Non-trainable params: 0 
_________________________________________________________________________________
 
```





------

Hope this can help.