[TOC]

## 一、Number

在实际开发过程中，我们经常会遇到需要使用对象，而不是内置数据类型的情形。为了解决这个问题，Java 语言为每一个内置数据类型提供了对应的包装类。

所有的包装类**（Integer、Long、Byte、Double、Float、Short）**都是抽象类 Number 的子类。

![Number类](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/htsz0GaC0Fo*8pkvD09by4n.b6eMmcwa5edePES6GUw!/r/dDYBAAAAAAAA)

## 二、Math

Java 的 Math 包含了用于执行基本数学运算的属性和方法，如初等指数、对数、平方根和三角函数。

Math 的方法都被定义为 static 形式，通过 Math 类可以在主函数中直接调用。

| 序号 | 方法与描述                                                   |
| ---- | ------------------------------------------------------------ |
| 1    | [xxxValue()](http://www.runoob.com/java/number-xxxvalue.html) 将 Number 对象转换为xxx数据类型的值并返回。 |
| 2    | [compareTo()](http://www.runoob.com/java/number-compareto.html) 将number对象与参数比较。 |
| 3    | [equals()](http://www.runoob.com/java/number-equals.html) 判断number对象是否与参数相等。 |
| 4    | [valueOf()](http://www.runoob.com/java/number-valueof.html) 返回一个 Number 对象指定的内置数据类型 |
| 5    | [toString()](http://www.runoob.com/java/number-tostring.html) 以字符串形式返回值。 |
| 6    | [parseInt()](http://www.runoob.com/java/number-parseInt.html) 将字符串解析为int类型。 |
| 7    | [abs()](http://www.runoob.com/java/number-abs.html) 返回参数的绝对值。 |
| 8    | [ceil()](http://www.runoob.com/java/number-ceil.html) 返回大于等于( >= )给定参数的的最小整数。 |
| 9    | [floor()](http://www.runoob.com/java/number-floor.html) 返回小于等于（<=）给定参数的最大整数 。 |
| 10   | [rint()](http://www.runoob.com/java/number-rint.html) 返回与参数最接近的整数。返回类型为double。 |
| 11   | [round()](http://www.runoob.com/java/number-round.html) 它表示**四舍五入**，算法为 Math.floor(x+0.5)，即将原来的数字加上 0.5 后再向下取整，所以，Math.round(11.5) 的结果为12，Math.round(-11.5) 的结果为-11。 |
| 12   | [min()](http://www.runoob.com/java/number-min.html) 返回两个参数中的最小值。 |
| 13   | [max()](http://www.runoob.com/java/number-max.html) 返回两个参数中的最大值。 |
| 14   | [exp()](http://www.runoob.com/java/number-exp.html) 返回自然数底数e的参数次方。 |
| 15   | [log()](http://www.runoob.com/java/number-log.html) 返回参数的自然数底数的对数值。 |
| 16   | [pow()](http://www.runoob.com/java/number-pow.html) 返回第一个参数的第二个参数次方。 |
| 17   | [sqrt()](http://www.runoob.com/java/number-sqrt.html) 求参数的算术平方根。 |
| 18   | [sin()](http://www.runoob.com/java/number-sin.html) 求指定double类型参数的正弦值。 |
| 19   | [cos()](http://www.runoob.com/java/number-cos.html) 求指定double类型参数的余弦值。 |
| 20   | [tan()](http://www.runoob.com/java/number-tan.html) 求指定double类型参数的正切值。 |
| 21   | [asin()](http://www.runoob.com/java/number-asin.html) 求指定double类型参数的反正弦值。 |
| 22   | [acos()](http://www.runoob.com/java/number-acos.html) 求指定double类型参数的反余弦值。 |
| 23   | [atan()](http://www.runoob.com/java/number-atan.html) 求指定double类型参数的反正切值。 |
| 24   | [atan2()](http://www.runoob.com/java/number-atan2.html) 将笛卡尔坐标转换为极坐标，并返回极坐标的角度值。 |
| 25   | [toDegrees()](http://www.runoob.com/java/number-todegrees.html) 将参数转化为角度。 |
| 26   | [toRadians()](http://www.runoob.com/java/number-toradians.html) 将角度转换为弧度。 |
| 27   | [random()](http://www.runoob.com/java/number-random.html) 返回0~1的伪随机数 |

## 三、String

| SN(序号) | 方法描述                                                     |
| -------- | ------------------------------------------------------------ |
| 1        | [char charAt(int index)](http://www.runoob.com/java/java-string-charat.html) 返回指定索引处的 char 值。 |
| 2        | [int compareTo(Object o)](http://www.runoob.com/java/java-string-compareto.html) 把这个字符串和另一个对象比较。 |
| 3        | [int compareTo(String anotherString)](http://www.runoob.com/java/java-string-compareto.html) 按字典顺序比较两个字符串。 |
| 4        | [int compareToIgnoreCase(String str)](http://www.runoob.com/java/java-string-comparetoignorecase.html) 按字典顺序比较两个字符串，不考虑大小写。 |
| 5        | **[String concat(String str)](http://www.runoob.com/java/java-string-concat.html) 将指定字符串连接到此字符串的结尾。** |
| 6        | [boolean contentEquals(StringBuffer sb)](http://www.runoob.com/java/java-string-contentequals.html) 当且仅当字符串与指定的StringBuffer有相同顺序的字符时候返回真。 |
| 7        | [static String copyValueOf(char[\] data)](http://www.runoob.com/java/java-string-copyvalueof.html) 返回指定数组中表示该字符序列的 String。 |
| 8        | [static String copyValueOf(char[\] data, int offset, int count)](http://www.runoob.com/java/java-string-copyvalueof.html) 返回指定数组中表示该字符序列的 String。 |
| 9        | **[boolean endsWith(String suffix)](http://www.runoob.com/java/java-string-endswith.html) 测试此字符串是否以指定的后缀结束。** |
| 10       | **[boolean equals(Object anObject)](http://www.runoob.com/java/java-string-equals.html) 将此字符串与指定的对象比较。** |
| 11       | [boolean equalsIgnoreCase(String anotherString)](http://www.runoob.com/java/java-string-equalsignorecase.html) 将此 String 与另一个 String 比较，不考虑大小写。 |
| 12****   | **[byte[\] getBytes()](http://www.runoob.com/java/java-string-getbytes.html)  使用平台的默认字符集将此 String 编码为 byte 序列，并将结果存储到一个新的 byte 数组中。** |
| 13****   | **[byte[\] getBytes(String charsetName)](http://www.runoob.com/java/java-string-getbytes.html) 使用指定的字符集将此 String 编码为 byte 序列，并将结果存储到一个新的 byte 数组中。** |
| 14       | [void getChars(int srcBegin, int srcEnd, char[\] dst, int dstBegin)](http://www.runoob.com/java/java-string-getchars.html) 将字符从此字符串复制到目标字符数组。 |
| 15****   | **[int hashCode()](http://www.runoob.com/java/java-string-hashcode.html) 返回此字符串的哈希码。** |
| 16       | [int indexOf(int ch)](http://www.runoob.com/java/java-string-indexof.html) 返回指定字符在此字符串中第一次出现处的索引。 |
| 17       | [int indexOf(int ch, int fromIndex)](http://www.runoob.com/java/java-string-indexof.html) 返回在此字符串中第一次出现指定字符处的索引，从指定的索引开始搜索。 |
| 18       | [int indexOf(String str)](http://www.runoob.com/java/java-string-indexof.html)  返回指定子字符串在此字符串中第一次出现处的索引。 |
| 19       | [int indexOf(String str, int fromIndex)](http://www.runoob.com/java/java-string-indexof.html) 返回指定子字符串在此字符串中第一次出现处的索引，从指定的索引开始。 |
| 20****   | **[String intern()](http://www.runoob.com/java/java-string-intern.html)  返回字符串对象的规范化表示形式。将字符串加入运行时常量池** |
| 21       | [int lastIndexOf(int ch)](http://www.runoob.com/java/java-string-lastindexof.html)  返回指定字符在此字符串中最后一次出现处的索引。 |
| 22       | [int lastIndexOf(int ch, int fromIndex)](http://www.runoob.com/java/java-string-lastindexof.html) 返回指定字符在此字符串中最后一次出现处的索引，从指定的索引处开始进行反向搜索。 |
| 23       | [int lastIndexOf(String str)](http://www.runoob.com/java/java-string-lastindexof.html) 返回指定子字符串在此字符串中最右边出现处的索引。 |
| 24       | [int lastIndexOf(String str, int fromIndex)](http://www.runoob.com/java/java-string-lastindexof.html)  返回指定子字符串在此字符串中最后一次出现处的索引，从指定的索引开始反向搜索。 |
| 25****   | **[int length()](http://www.runoob.com/java/java-string-length.html) 返回此字符串的长度。** |
| 26****   | **[boolean matches(String regex)](http://www.runoob.com/java/java-string-matches.html) 告知此字符串是否匹配给定的正则表达式。** |
| 27       | [boolean regionMatches(boolean ignoreCase, int toffset, String other, int ooffset, int len)](http://www.runoob.com/java/java-string-regionmatches.html) 测试两个字符串区域是否相等。 |
| 28       | [boolean regionMatches(int toffset, String other, int ooffset, int len)](http://www.runoob.com/java/java-string-regionmatches.html) 测试两个字符串区域是否相等。 |
| 29       | **[String replace(char oldChar, char newChar)](http://www.runoob.com/java/java-string-replace.html) 返回一个新的字符串，它是通过用 newChar 替换此字符串中出现的所有 oldChar 得到的。** |
| 30****   | **[String replaceAll(String regex, String replacement)](http://www.runoob.com/java/java-string-replaceall.html) 使用给定的 replacement 替换此字符串所有匹配给定的正则表达式的子字符串。** |
| 31       | [String replaceFirst(String regex, String replacement)](http://www.runoob.com/java/java-string-replacefirst.html)  使用给定的 replacement 替换此字符串匹配给定的正则表达式的第一个子字符串。 |
| 32       | **[String[\] split(String regex)](http://www.runoob.com/java/java-string-split.html) 根据给定正则表达式的匹配拆分此字符串。** |
| 33       | [String[\] split(String regex, int limit)](http://www.runoob.com/java/java-string-split.html) 根据匹配给定的正则表达式来拆分此字符串。 |
| 34       | [boolean startsWith(String prefix)](http://www.runoob.com/java/java-string-startswith.html) 测试此字符串是否以指定的前缀开始。 |
| 35       | [boolean startsWith(String prefix, int toffset)](http://www.runoob.com/java/java-string-startswith.html) 测试此字符串从指定索引开始的子字符串是否以指定前缀开始。 |
| 36       | [CharSequence subSequence(int beginIndex, int endIndex)](http://www.runoob.com/java/java-string-subsequence.html)  返回一个新的字符序列，它是此序列的一个子序列。 |
| 37       | [String substring(int beginIndex)](http://www.runoob.com/java/java-string-substring.html) 返回一个新的字符串，它是此字符串的一个子字符串。 |
| 38       | [String substring(int beginIndex, int endIndex)](http://www.runoob.com/java/java-string-substring.html) 返回一个新字符串，它是此字符串的一个子字符串。 |
| 39       | [char[\] toCharArray()](http://www.runoob.com/java/java-string-tochararray.html) 将此字符串转换为一个新的字符数组。 |
| 40       | [String toLowerCase()](http://www.runoob.com/java/java-string-tolowercase.html) 使用默认语言环境的规则将此 String 中的所有字符都转换为小写。 |
| 41       | [String toLowerCase(Locale locale)](http://www.runoob.com/java/java-string-tolowercase.html)  使用给定 Locale 的规则将此 String 中的所有字符都转换为小写。 |
| 42       | [String toString()](http://www.runoob.com/java/java-string-tostring.html)  返回此对象本身（它已经是一个字符串！）。 |
| 43       | [String toUpperCase()](http://www.runoob.com/java/java-string-touppercase.html) 使用默认语言环境的规则将此 String 中的所有字符都转换为大写。 |
| 44       | [String toUpperCase(Locale locale)](http://www.runoob.com/java/java-string-touppercase.html) 使用给定 Locale 的规则将此 String 中的所有字符都转换为大写。 |
| 45       | [String trim()](http://www.runoob.com/java/java-string-trim.html) 返回字符串的副本，忽略前导空白和尾部空白。 |
| 46       | **[static String valueOf(primitive data type x)](http://www.runoob.com/java/java-string-valueof.html) 返回给定data type类型x参数的字符串表示形式。** |



## 四、StringBuffer(线程安全)和StringBuilder(不安全)

当对字符串进行修改的时候，需要使用 StringBuffer 和 StringBuilder 类。

和 String 类不同的是，StringBuffer 和 StringBuilder 类的对象能够被多次的修改，**并且不产生新的未使用对象**。

StringBuilder 类在 Java 5 中被提出，它和 StringBuffer 之间的最大不同在于 StringBuilder 的方法不是线程安全的（不能同步访问）。

由于 StringBuilder 相较于 StringBuffer 有速度优势，所以多数情况下建议使用 StringBuilder 类。然而在应用程序要求线程安全的情况下，则必须使用 StringBuffer 类。

```java
public class Test{
  public static void main(String args[]){
    StringBuffer sBuffer = new StringBuffer("ck`Blog：");
    sBuffer.append("github.com/");
    sBuffer.append("ck76/");
    sBuffer.append("Blog");
    System.out.println(sBuffer);  
  }
}

//github.com/ck76/Blog
```

以下是 StringBuffer 类支持的主要方法：

| 序号 | 方法描述                                                     |
| ---- | ------------------------------------------------------------ |
| 1    | public StringBuffer **append**(String s) 将指定的字符串**追加**到此字符序列。 |
| 2    | public StringBuffer **reverse**()  将此字符序列用其反转形式取代。 |
| 3    | public **delete**(int start, int end) 移除此序列的子字符串中的字符。 |
| 4    | public **insert**(int offset, int i) 将 `int` 参数的字符串表示形式插入此序列中。 |
| 5    | **replace**(int start, int end, String str) 使用给定 `String` 中的字符替换此序列的子字符串中的字符。 |

## 五、Date

java.util.Date

日期类 月份从0-11(这就是老外 奇奇怪怪)

- 日期对象转成毫秒值

```java
Date d = new Date();
long time1 = d.getTime();
long time2 = System.currentTimeMillis(); //毫秒值
```

- 毫秒值转成具体的日期

```java
long time = 1322709921312l;
Date d = new Date();
d.setTime(time);
```

- 将日期字符串转换成日期对象 使用的就是DateFormat方法中的 Date parse(String source);

```java
public static void method() throws Exception {
    String str_time = "2011/10/25";
    DateFormat df = new SimpleDateFormat("yyyy/MM/dd"); //SimpleDateFormat作为可以指定用户自定义的格式来完成格式化
    Date d = df.parse(str_time);
}
```

- 将日期对象转换成字符串的方式 DateFormat类中的format方法,创建日期格式对象

`DateFormat df = new SimpleDateFormat(); `//该对象的建立内部会封装一个默认的日期格式 11-12-1 下午1:48

- 如果想要自定义日期格式的话 可使用SimpleDateFormat的构造函数 将具体的格式作为参数传入到构造函数中 必须要参与格式对象文档

`df = new SimpleDateFormat("yyyy年MM月dd日 HH:mm:ss");`

调用DateFormat中的format方法 对已有的日期对象进行格式化

String str_time = df.format(d);

 

| `boolean`  | `**after**(Date when)`             测试此日期是否在指定日期之后。 |
| ---------- | ------------------------------------------------------------ |
| ` boolean` | `**before**(Date when)`             测试此日期是否在指定日期之前。 |
| ` Object`  | `**clone**()`             返回此对象的副本。                 |
| ` int`     | `**compareTo**(Date anotherDate)`             比较两个日期的顺序。 |
| `boolean` | `**equals**(Object obj)`             比较两个日期的相等性。 |
| ` long` | `**getTime**()`             返回自 1970 年 1 月 1 日 00:00:00 GMT 以来此 `Date` 对象表示的毫秒数。 |
| ` void` | `**setTime**(long time)`             设置此 `Date` 对象，以表示 1970 年 1 月 1 日 00:00:00 GMT 以后  `time` 毫秒的时间点。 |
| `String` | `**toString**()`             把此 `Date` 对象转换为以下形式的 `String`： dow mon dd  hh:mm:ss zzz yyyy 其中： `dow` 是一周中的某一天 (`Sun, Mon, Tue, Wed, Thu, Fri,  Sat`)。 |

## 六、parse（）和valueOf（），toString（）的区别

1. parse()是SimpleDateFomat里面的方法，你说的应该是**parseInt**()或**parsefloat**()这种方法吧，

顾名思义 比如说parseInt()就是把String类型转化为**int**类型。

```java
如 String a= "123";

int b = Integer.parseInt(a);

这样b就等于123了。
```

2. ValueOf()方法比如说 Integer.valueOf() 是把String类型转化为**Integer**类型(注意：是Integer类型，而不是int类型，int类型是表示数字的简单类型，Integer类型是一个引用的复杂类型)如：

```java
String a= "123";

Integer c =Integer.valueOf(a);

//Integer类型可以用intValue方法转化为int类型

int b =c.intValue();

这时候这个b就等于123了
```
3. toString()可以把一个引用类型转化为**String**字符串类型。

  下面举个例子与2相反，把Integer转化为String类型：

  ```java
  Integer a = new Integer(123);
  
  String b =a.toString();
  
  这时候b就是 "123" 了
  ```
