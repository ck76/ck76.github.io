[TOC]

#### 基础类型

| .proto类型 | java类型   | C++类型 | 备注                                                         |
| ---------- | ---------- | ------- | ------------------------------------------------------------ |
| double     | double     | double  |                                                              |
| float      | float      | float   |                                                              |
| int32      | int        | int32   | 使用可变长编码方式。编码负数时不够高效——如果你的字段可能含有负数，那么请使用sint32。 |
| int64      | long       | int64   | 使用可变长编码方式。编码负数时不够高效——如果你的字段可能含有负数，那么请使用sint64。 |
| unit32     | int[1]     | unit32  | 总是4个字节。如果数值总是比总是比228大的话，这个类型会比uint32高效。 |
| unit64     | long[1]    | unit64  | 总是8个字节。如果数值总是比总是比256大的话，这个类型会比uint64高效。 |
| sint32     | int        | int32   | 使用可变长编码方式。有符号的整型值。编码时比通常的int32高效。 |
| sint64     | long       | int64   | 使用可变长编码方式。有符号的整型值。编码时比通常的int64高效。 |
| fixed32    | int[1]     | unit32  |                                                              |
| fixed64    | long[1]    | unit64  | 总是8个字节。如果数值总是比总是比256大的话，这个类型会比uint64高效。 |
| sfixed32   | int        | int32   | 总是4个字节。                                                |
| sfixed64   | long       | int64   | 总是8个字节。                                                |
| bool       | boolean    | bool    |                                                              |
| string     | String     | string  | 一个字符串必须是UTF-8编码或者7-bit ASCII编码的文本。         |
| bytes      | ByteString | string  | 可能包含任意顺序的字节数据                                   |

#### 特殊字段

| 英文     | 中文                                                | 备注                                           |
| -------- | --------------------------------------------------- | ---------------------------------------------- |
| enum     | 枚举(数字从零开始) 作用是为字段指定某”预定义值序列” | enum Type {MAN = 0;WOMAN = 1; OTHER= 3;}       |
| message  | 消息体                                              | message User{}                                 |
| repeated | 数组/集合                                           | repeated User users  = 1                       |
| import   | 导入定义                                            | import "protos/other_protos.proto"             |
| //       | 注释                                                | //用于注释                                     |
| extend   | 扩展                                                | extend User {}                                 |
| package  | 包名                                                | 相当于命名空间，用来防止不同消息类型的明明冲突 |



# 定义数据类型

 首先让我们看一个非常简单的例子。假设您想要定义搜索请求消息格式，其中每个搜索请求都有一个查询字符串、您感兴趣的特定结果页面以及每页的结果数量。这是用来定义消息类型的.proto文件。

```protobuf
syntax = "proto3";
 
message SearchRequest {
  string query = 1;
  int32 page_number = 2;
  int32 result_per_page = 3;
}
```

 文件的第一行指定您正在使用proto 3语法:如果不这样做，协议缓冲区编译器将假设您正在使用proto 2。这必须是文件的第一个非空的非注释行。

 SearchRequest消息定义指定了三个字段(名称/值对)，每一个字段对应于要包含在这种类型的消息中的数据。每个字段都有一个名称和一个类型。

### 指定字段类型

 在上例中，所有字段都是标量类型:两个整数(page_number和result_per_page)和一个字符串(query)。但是，您也可以为字段指定复合类型，包括枚举和其他消息类型。

### 分配字段编号

 如您所见，消息定义中的每个字段都有一个唯一的编号。这些字段编号用于以二进制格式标识您的字段，一旦您的消息类型被使用，就不应该被更改。请注意，1到15范围内的字段编号需要一个字节来编码，包括字段编号和字段类型(您可以在协议缓冲区编码中找到更多信息)。16到2047范围内的字段编号需要两个字节。因此，您应该为经常出现的消息元素保留数字1到15。记住为将来可能添加的频繁出现的元素留出一些空间。

 您可以指定的最小字段编号为1，最大字段编号为229 - 1，即536，870，911。但是不能使用数字19000到19999 ( FieldDescriptor::kFirstReservedNumber 到FieldDescriptor::kLastReservedNumber)，因为它们是为协议缓冲区实现而保留的-如果您在 .proto文件中使用这些保留的数字之一，协议缓冲区编译器就会报错。同样，您也不能使用任何保留字段。

### 指定字段规则

 消息字段可以是以下字段之一:

singular: 可以有零个或其中一个字段(但不超过一个)。

repeated: 该字段可以重复任意次数(包括零次)。重复值的顺序将被保留。

 在proto 3中，可扩展的repeated字段为数字类型的默认编码。

 您可以在协议缓冲区编码中找到更多关于打包编码的信息。

### 添加更多消息类型

 可以在单个.proto中定义多种消息类型。如果您要定义多个相关消息，这很有用——例如，如果您想定义与搜索响应消息类型相对应的回复消息格式，可以将其添加到该.proto中:

```
message SearchRequest {`` ``string query = 1;`` ``int32 page_number = 2;`` ``int32 result_per_page = 3;``}` `message SearchResponse {`` ``...``}
```

### 添加注释

为你的.proto 添加注释,使用 C/C++风格的 // 或者 /* ... */ 语法.

```
/* SearchRequest represents a search query, with pagination options to`` ``* indicate which results to include in the response. */` `message SearchRequest {`` ``string query = 1;`` ``int32 page_number = 2; // Which page number do we want?`` ``int32 result_per_page = 3; // Number of results to return per page.``}
```

### 保留字段

 如果通过完全删除某个字段或对其进行注释来更新消息类型，将来的用户可以在对该类型进行自己的更新时重用该字段编号。如果他们以后加载旧版本的相同.proto文件，这可能会导致严重的问题 ，包括数据损坏、隐私漏洞等。确保不会发生这种情况的解决方案是指定已删除字段的字段编号(and/or名称，这也可能导致JSON序列化问题)是保留的。如果将来有任何用户试图使用这些字段标识符，协议缓冲区编译器会报错。

```
message Foo {`` ``reserved 2, 15, 9 to 11;`` ``reserved "foo", "bar";``}
```

 请注意，不能在同一保留语句中混合字段名和字段编号。

### 从.proto文件中生成了什么?

 在.proto上运行协议缓冲区编译器时，编译器用您指定的编程语言生成代码，您需要使用您在文件中描述的消息类型，包括获取和设置字段值，将消息序列化为输出流，以及从输入流解析消息。

 对于C++,编译器会从每个 .proto文件生成一个 .h and .cc文件, 文件中描述的每种消息类型都有一个类。

 对于Java，编译器为每个消息类型生成一个Java文件，其中包括每个类的定义，以及用于创建消息类实例的特殊构建器类。

 Python有点不同，Python编译器生成一个模块，其中包含中每种消息类型的静态描述符，然后与元类一起使用，在运行时创建必要的Python数据访问类。

 对于Go, 编译器生成一个.pb.go文件，其中包含文件中每种消息类型的类型。

 对于Ruby, 编译器生成一个.rb文件带有包含消息类型的Ruby模块。

 对于Objective-C, 编译器为每个.proto文件中生成一个pbobjc.h和pbobjc.m文件，文件中描述的每种消息类型都有一个类。

 对于C#, 编译器为每个.proto文件生成一个.cs文件，文件中描述的每种消息类型都有一个类。

 通过所选语言的教程( proto 3版本即将推出)，您可以了解关于为每种语言使用API的更多信息。有关更多的API细节，请参见相关的API参考( proto 3版本也即将推出)。