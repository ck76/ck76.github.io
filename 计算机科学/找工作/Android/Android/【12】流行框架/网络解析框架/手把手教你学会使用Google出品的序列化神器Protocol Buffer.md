[TOC]

# 前言

- 习惯用 `Json、XML` 数据存储格式的你们，相信大多都没听过`Protocol Buffer`
- `Protocol Buffer` 其实 是 `Google`出品的一种轻量 & 高效的结构化数据存储格式，性能比 `Json、XML` 真的强！太！多！

> 由于 `Google`出品，我相信`Protocol Buffer`已经具备足够的吸引力

- 今天，我将详细介绍`Protocol Buffer`在`Android`平台 的具体使用

> 阅读本文前请先阅读：[快来看看Google出品的Protocol Buffer，别只会用Json和XML了](https://www.jianshu.com/p/1538bf85dad1)

------

# 目录

<img src="https://tva1.sinaimg.cn/large/008eGmZEly1gmwfpl352kj30u00ul75g.jpg" alt="img" style="zoom:33%;" />

示意图

------

# 1. 定义

一种 结构化数据 的数据存储格式（类似于 `XML、Json` ）

> 1. `Google` 出品 （开源）
> 2. `Protocol Buffer` 目前有两个版本：`proto2` 和 `proto3`
> 3. 因为`proto3` 还是beta 版，所以本次讲解是 `proto2`

------

# 2. 作用

通过将 结构化的数据 进行 串行化（**序列化**），从而实现 **数据存储 / RPC 数据交换**的功能

> 1. 序列化： 将 数据结构或对象 转换成 二进制串 的过程
> 2. 反序列化：将在序列化过程中所生成的二进制串 转换成 数据结构或者对象 的过程

------

# 3. 特点

- 对比于 常见的 `XML、Json` 数据存储格式，`Protocol Buffer`有如下特点：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfpjvv6zj30xc0fugq3.jpg)

Protocol Buffer 特点

------

# 4. 应用场景

传输数据量大 & 网络环境不稳定 的**数据存储、RPC 数据交换** 的需求场景

> 如 即时IM （QQ、微信）的需求场景

------

# 总结

在 **传输数据量较大**的需求场景下，`Protocol Buffer`比`XML、Json` 更小、更快、使用 & 维护更简单！

------

# 5. 使用流程

- 使用 `Protocol Buffer` 的流程如下：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfpgyz9nj30qe1k4jtx.jpg)

Protocol Buffer使用流程

- 今天主要讲解`Protocol Buffer`在`Android`平台 的具体使用

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfpfjk9rj30qe1k4dih.jpg)

示意图

------

# 6. 应用实例（Android平台）

- 具体步骤如下：

![img](//)

具体步骤

### 步骤1：将生成的 代码文件 放入到项目中

- 对于`Android（Java）平台`，即将编译`.proto`文件生成的`Java`包文件 整个复制到 `Android` 项目中
- 放置路径： `app/src/main/java的` 文件夹里

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfp66ghxj306809o0tu.jpg)

项目结构示意图

### 步骤2：在 `Gradle` 添加 `Protocol Buffer` 版本依赖



```csharp
compile 'com.google.protobuf:protobuf-java:2.6.1'
// 注：protobuf-java的版本 一定要和 安装protocobuffer的版本 一致
```

### 步骤3：具体在Android项目中使用

#### 3.1 消息对象类介绍

通过`.proto文件` 转换的 `Java`源代码 = `Protocol Buffer` 类 + 消息对象类（含`Builder`内部类）

> 消息对象类 是 `Protocol Buffer` 类的内部类

由于最常用的都是 消息对象类 和其内部类`Builder`类 的方法&成员变量，所以此处主要讲解这两者。

##### 3.1.1 消息对象类（`Message`类）

- 消息对象类 类通过 **二进制数组** 写 和 读 消息类型
- 使用方法包括：



```java
<-- 方式1：直接序列化和反序列化 消息 -->
protocolBuffer.toByteArray()；
// 序列化消息 并 返回一个包含它的原始字节的字节数组
protocolBuffer.parseFrom(byte[] data)；
// 从一个字节数组 反序列化（解析） 消息

<-- 方式2：通过输入/ 输出流（如网络输出流） 序列化和反序列化消息 -->
protocolBuffer.writeTo(OutputStream output)；
output.toByteArray();
// 将消息写入 输出流 ，然后再 序列化消息 

protocolBuffer.parseFrom(InputStream input)；
// 从一个 输入流 读取并 反序列化（解析）消息


// 只含包含字段的getters方法
// required string name = 1;
public boolean hasName();// 如果字段被设置，则返回true
public java.lang.String getName();

// required int32 id = 2;
public boolean hasId();
public int getId();

// optional string email = 3;
public boolean hasEmail();
public String getEmail();

// repeated .tutorial.Person.PhoneNumber phone = 4;
// 重复（repeated）字段有一些额外方法
public List<PhoneNumber> getPhoneList();
public int getPhoneCount();
// 列表大小的速记
// 作用：通过索引获取和设置列表的特定元素的getters和setters
```

常用的如上，更多请看[官方文档](https://links.jianshu.com/go?to=https%3A%2F%2Fdevelopers.google.com%2Fprotocol-buffers%2Fdocs%2Freference%2Fcpp%2Fgoogle.protobuf.message.html%23Message)

##### 3.1.2 `Builder`类

作用：创建 消息构造器 & 设置/ 获取消息对象的字段值 & 创建 消息类 实例

> 属于 消息对象类 的内部类

a. 创建 消息构造器



```undefined
Demo.Person.Builder person = Person.newBuilder();
```

b. 设置/ 获取 消息对象的字段值 具体方法如下：



```csharp
// 标准的JavaBeans风格：含getters和setters
// required string name = 1;
public boolean hasName();// 如果字段被设置，则返回true
public java.lang.String getName();
public Builder setName(String value);
public Builder clearName(); // 将字段设置回它的空状态

// required int32 id = 2;
public boolean hasId();
public int getId();
public Builder setId(int value);
public Builder clearId();

// optional string email = 3;
public boolean hasEmail();
public String getEmail();
public Builder setEmail(String value);
public Builder clearEmail();

// repeated .tutorial.Person.PhoneNumber phone = 4;
// 重复（repeated）字段有一些额外方法
public List<PhoneNumber> getPhoneList();
public int getPhoneCount();
// 列表大小的速记
// 作用：通过索引获取和设置列表的特定元素的getters和setters

public PhoneNumber getPhone(int index);
public Builder setPhone(int index, PhoneNumber value);

public Builder addPhone(PhoneNumber value);
// 将新元素添加到列表的末尾

public Builder addAllPhone(Iterable<PhoneNumber> value);
// 将一个装满元素的整个容器添加到列表中
public Builder clearPhone();

public Builder isInitialized() 
// 检查所有 required 字段 是否都已经被设置

public Builder toString() :
// 返回一个人类可读的消息表示（用于调试）

public Builder mergeFrom(Message other)
// 将 其他内容 合并到这个消息中，覆写单数的字段，附接重复的。

public Builder clear()
// 清空所有的元素为空状态。
```

#### 3.2 具体使用

- 使用步骤如下：
  **步骤1：**通过 消息类的内部类`Builder`类 构造 消息构造器
  **步骤2：**通过 消息构造器 设置 消息字段的值
  **步骤3：**通过 消息构造器 创建 消息类 对象
  **步骤4：**序列化 / 反序列化 消息
- 具体使用如下：（注释非常清晰）



```csharp
public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        // 步骤1:通过 消息类的内部类Builder类 构造 消息类的消息构造器
        Demo.Person.Builder personBuilder =  Demo.Person.newBuilder();

        // 步骤2:设置你想要设置的字段为你选择的值
        personBuilder.setName("Carson");// 在定义.proto文件时,该字段的字段修饰符是required,所以必须赋值
        personBuilder.setId(123);// 在定义.proto文件时,该字段的字段修饰符是required,所以必须赋值
        personBuilder.setEmail("carson.ho@foxmail.com"); // 在定义.proto文件时,该字段的字段修饰符是optional,所以可赋值 / 不赋值(不赋值时将使用默认值)

        Demo.Person.PhoneNumber.Builder phoneNumber =  Demo.Person.PhoneNumber.newBuilder();
        phoneNumber.setType( Demo.Person.PhoneType.HOME);// 直接采用枚举类型里的值进行赋值
        phoneNumber.setNumber("0157-23443276");
        // PhoneNumber消息是嵌套在Person消息里,可以理解为内部类
        // 所以创建对象时要通过外部类来创建

        // 步骤3:通过 消息构造器 创建 消息类 对象
        Demo.Person person = personBuilder.build();

        // 步骤4:序列化和反序列化消息(两种方式)

        /*方式1：直接 序列化 和 反序列化 消息 */
        // a.序列化
        byte[] byteArray1 = person.toByteArray();
        // 把 person消息类对象 序列化为 byte[]字节数组
        System.out.println(Arrays.toString(byteArray1));
        // 查看序列化后的字节流
      
        // b.反序列化
        try {

            Demo.Person person_Request = Demo.Person.parseFrom(byteArray1);
            // 当接收到字节数组byte[] 反序列化为 person消息类对象

            System.out.println(person_Request.getName());
            System.out.println(person_Request.getId());
            System.out.println(person_Request.getEmail());
            // 输出反序列化后的消息
        } catch (IOException e) {
            e.printStackTrace();
        }
       

        /*方式2：通过输入/ 输出流（如网络输出流） 序列化和反序列化消息 */
        // a.序列化
        ByteArrayOutputStream output = new ByteArrayOutputStream();
         try {

            person.writeTo(output);
            // 将消息序列化 并写入 输出流(此处用 ByteArrayOutputStream 代替)

        } catch (IOException e) {
            e.printStackTrace();
        }

        byte[] byteArray = output.toByteArray();
        // 通过 输出流 转化成二进制字节流

        // b. 反序列化
        ByteArrayInputStream input = new ByteArrayInputStream(byteArray);
        // 通过 输入流 接收消息流(此处用 ByteArrayInputStream 代替)

        try {

            Demo.Person person_Request = Demo.Person.parseFrom(input);
            // 通过输入流 反序列化 消息

            System.out.println(person_Request.getName());
            System.out.println(person_Request.getId());
            System.out.println(person_Request.getEmail());
            // 输出消息
        } catch (IOException e) {
            e.printStackTrace();
        }
            
    }
}
```

### Demo 地址

Carson_Ho的Github ：[https://github.com/Carson-Ho/ProtocolBuffer](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2FCarson-Ho%2FProtocolBuffer)

### 高级功能

- 贴心的Google还提供将`Protocol Buff` 编码方式 转化为 其他编码方式，如 `Json`、`XML`等等

> 即将 `Protocol Buff` 对象 转化为其他编码方式的数据存储对象

- 下面展示的是 将 `Protocol Buff` 对象 转化为 `Json`对象



```dart
// 步骤1：在Gradle加入依赖
compile 'com.googlecode.protobuf-java-format:protobuf-java-format:1.4'

// 步骤2：将`Protocol Buff` 对象 序列化 为 `Json`对象
JsonFormat jsonFormat = new JsonFormat();  
String person2json = jsonFormat.printToString(mProtoBuffer); 
```

至此， 关于`Protocol Buffer`的使用讲解完毕。

------

# 7. 总结

- 看完本文，你应该非常了解`Protocol Buffer` 在`Android`平台的使用

- 关于

  ```
  Protocol Buffer
  ```

  的系列文章请看：

  1. [手把手教你如何安装Protocol Buffer](https://www.jianshu.com/p/92dbe1ef0054)
  2. [这是一份很有诚意的 Protocol Buffer 语法详解](https://www.jianshu.com/p/e06ba6249edc)
  3. [快来看看Google出品的Protocol Buffer，别只会用Json和XML了](https://www.jianshu.com/p/1538bf85dad1)
  4. [Protocol Buffer 序列化原理大揭秘 - 为什么Protocol Buffer性能这么好？](https://www.jianshu.com/p/30ef9b3780d9)
  5. [Android：手把手带你分析 Protocol Buffer使用 源码](https://www.jianshu.com/p/2a5aa5ac6cf6)

- 下一篇文章我将对`Protocol Buffer` 的**源码**进行详细分析，感兴趣的同学可以继续关注本人运营的`Wechat Public Account`：

- [我想给你们介绍一个与众不同的Android微信公众号（福利回赠）](https://www.jianshu.com/p/2e92908af6ec)

- [我想邀请您和我一起写Android（福利回赠）](https://www.jianshu.com/p/2c5d57fb054d)

------

# 请点赞！因为你的鼓励是我写作的最大动力！



作者：Carson_Ho
链接：https://www.jianshu.com/p/4575342bc8ad
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。