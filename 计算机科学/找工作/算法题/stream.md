

[TOC]

## 概念

> `Stream`将要处理的元素集合看作一种流，在流的过程中，借助`Stream API`对流中的元素进行操作，比如：筛选、排序、聚合等。

![image-20210701194245361](https://tva1.sinaimg.cn/large/e6c9d24egy1h30twptzlpj20kc07mwf1.jpg)

> ```
> Stream` 的操作符大体上分为两种：`中间操作符`和`终止操作符
> ```

## 中间操作符

**对于数据流来说，中间操作符在执行制定处理程序后，数据流依然可以传递给下一级的操作符。**

**中间操作符包含8种(排除了parallel,sequential，这两个操作并不涉及到对数据流的加工操作)：**

1. map(mapToInt,mapToLong,mapToDouble) 转换操作符，把比如A->B，这里默认提供了转int，long，double的操作符。
2. flatmap(flatmapToInt,flatmapToLong,flatmapToDouble) 拍平操作比如把 int[]{2,3,4} 拍平 变成 2，3，4 也就是从原来的一个数据变成了3个数据，这里默认提供了拍平成int,long,double的操作符。
3. limit 限流操作，比如数据流中有10个 我只要出前3个就可以使用。
4. distint 去重操作，对重复元素去重，底层使用了equals方法。
5. filter 过滤操作，把不想要的数据过滤。
6. peek 挑出操作，如果想对数据进行某些操作，如：读取、编辑修改等。
7. skip 跳过操作，跳过某些元素。
8. sorted(unordered) 排序操作，对元素排序，前提是实现Comparable接口，当然也可以自定义比较器。

## 终止操作符

**数据经过中间加工操作，就轮到终止操作符上场了；**

**终止操作符就是用来对数据进行收集或者消费的，数据到了终止操作这里就不会向下流动了，终止操作符只能使用一次。**

1. collect 收集操作，将所有数据收集起来，这个操作非常重要，官方的提供的Collectors 提供了非常多收集器，可以说Stream 的核心在于Collectors。
2. count 统计操作，统计最终的数据个数。
3. findFirst、findAny 查找操作，查找第一个、查找任何一个 返回的类型为Optional。
4. noneMatch、allMatch、anyMatch 匹配操作，数据流中是否存在符合条件的元素 返回值为bool 值。
5. min、max 最值操作，需要自定义比较器，返回数据流中最大最小的值。
6. reduce 规约操作，将整个数据流的值规约为一个值，count、min、max底层就是使用reduce。
7. forEach、forEachOrdered 遍历操作，这里就是对最终的数据进行消费了。
8. toArray 数组操作，将数据流的元素转换成数组。

## Stream的创建

1、通过 `java.util.Collection.stream()` 方法用集合创建流

```java
List<String> list = Arrays.asList("a", "b", "c");
// 创建一个顺序流
Stream<String> stream = list.stream();
// 创建一个并行流
Stream<String> parallelStream = list.parallelStream();
```

2、使用`java.util.Arrays.stream(T[] array)`方法用数组创建流

```java
int[] array={1,3,5,6,8};
IntStream stream = Arrays.stream(array);
```

3、使用`Stream`的静态方法：`of()、iterate()、generate()`

```java
Stream<Integer> stream = Stream.of(1, 2, 3, 4, 5, 6);

Stream<Integer> stream2 = Stream.iterate(0, (x) -> x + 3).limit(4);
stream2.forEach(System.out::println); // 0 3 6 9

Stream<Double> stream3 = Stream.generate(Math::random).limit(3);
stream3.forEach(System.out::println);
```

输出结果：

```awk
3
6
9
0.8106623442686114
0.11554643727388458
0.1404645961428974

Process finished with exit code 0
```

> `stream`和`parallelStream`的简单区分：

`stream`是顺序流，由主线程按顺序对流执行操作；
`parallelStream`是并行流，内部以多线程并行执行的方式对流进行操作，但前提是流中的数据处理没有顺序要求。

例如筛选集合中的奇数，两者的处理不同之处：

![image-20210701230623951](https://tva1.sinaimg.cn/large/e6c9d24egy1h30twui91jj20kc0ef0u3.jpg)

## Stream使用

### 遍历/匹配（foreach/find/match）

`Stream`也是支持类似集合的遍历和匹配元素的，只是`Stream`中的元素是以`Optional`类型存在的。`Stream`的遍历、匹配非常简单。

```java
public class StreamTest {

    public static void main(String[] args) {
        List<Integer> list = Arrays.asList(7, 6, 9, 3, 8, 2, 1);
        // 遍历输出符合条件的元素
        list.stream().filter(x -> x > 6).forEach(System.out::println);
        // 匹配第一个
        Optional<Integer> findFirst = list.stream().filter(x -> x > 6).findFirst();
        // 匹配任意（适用于并行流）
        Optional<Integer> findAny = list.parallelStream().filter(x -> x > 6).findAny();
        // 是否包含符合特定条件的元素
        boolean anyMatch = list.stream().anyMatch(x -> x < 6);
        System.out.println("匹配第一个值：" + findFirst.get());
        System.out.println("匹配任意一个值：" + findAny.get());
        System.out.println("是否存在大于6的值：" + anyMatch);
        
    }
}
```

输出结果：

```awk
7
9
8
匹配第一个值：7
匹配任意一个值：8
是否存在大于6的值：true

Process finished with exit code 0
```

### 筛选（filter）

筛选，是按照一定的规则校验流中的元素，将符合条件的元素提取到新的流中的操作。

**筛选出`Integer`集合中大于7的元素，并打印出来**

```java
public class StreamTest {

    public static void main(String[] args) {
        List<Integer> list = Arrays.asList(6, 7, 3, 8, 1, 2, 9);
        Stream<Integer> stream = list.stream();
        stream.filter(x -> x > 7).forEach(System.out::println);
    }
}
```

输出结果：

```awk
8
9

Process finished with exit code 0
```

### 聚合（max/min/count)

`max`、`min`、`count`这些字眼你一定不陌生，没错，在mysql中我们常用它们进行数据统计。Java stream中也引入了这些概念和用法，极大地方便了我们对集合、数组的数据统计工作。

**案例一：获取`String`集合中最长的元素。**

```java
public class StreamTest {

    public static void main(String[] args) {
        List<String> list = Arrays.asList("adnm", "admmt", "pot", "xbangd", "weoujgsd");
        Optional<String> max = list.stream().max(Comparator.comparing(String::length));
        System.out.println("最长的字符串：" + max.get());
    }
}
```

输出结果：

```awk
最长的字符串：weoujgsd

Process finished with exit code 0
```

**案例二：获取`Integer`集合中的最大值。**

```java
public class StreamTest {

    public static void main(String[] args) {
        List<Integer> list = Arrays.asList(7, 6, 9, 4, 11, 6);
        // 自然排序
        Optional<Integer> max = list.stream().max(Integer::compareTo);
        // 自定义排序
        Optional<Integer> max2 = list.stream().max(new Comparator<Integer>() {
            @Override
            public int compare(Integer o1, Integer o2) {
                return o1.compareTo(o2);
            }
        });
        System.out.println("自然排序的最大值：" + max.get());
        System.out.println("自定义排序的最大值：" + max2.get());
    }
}
```

输出结果：

```awk
自然排序的最大值：11
自定义排序的最大值：11

Process finished with exit code 0
```

**案例三：计算`Integer`集合中大于6的元素的个数。**

```java
public class StreamTest {

    public static void main(String[] args) {
        List<Integer> list = Arrays.asList(7, 6, 4, 8, 2, 11, 9);
        long count = list.stream().filter(x -> x > 6).count();
        System.out.println("list中大于6的元素个数：" + count);
    }
}
```

输出结果：

```awk
list中大于6的元素个数：4

Process finished with exit code 0
```

### 映射(map/flatMap)

映射，可以将一个流的元素按照一定的映射规则映射到另一个流中。分为`map`和`flatMap`：

- `map`：接收一个函数作为参数，该函数会被应用到每个元素上，并将其映射成一个新的元素。
- `flatMap`：接收一个函数作为参数，将流中的每个值都换成另一个流，然后把所有流连接成一个流。

**案例一：英文字符串数组的元素全部改为大写。整数数组每个元素+3。**

```java
public class StreamTest {

    public static void main(String[] args) {
        String[] strArr = { "abcd", "bcdd", "defde", "fTr" };
        List<String> strList = Arrays.stream(strArr).map(String::toUpperCase).collect(Collectors.toList());
        System.out.println("每个元素大写：" + strList);
        List<Integer> intList = Arrays.asList(1, 3, 5, 7, 9, 11);
        List<Integer> intListNew = intList.stream().map(x -> x + 3).collect(Collectors.toList());
        System.out.println("每个元素+3：" + intListNew);
    }
}
```

输出结果：

```awk
每个元素大写：[ABCD, BCDD, DEFDE, FTR]
每个元素+3：[4, 6, 8, 10, 12, 14]

Process finished with exit code 0
```

**案例二：将两个字符数组合并成一个新的字符数组。**

```java
public class StreamTest {

    public static void main(String[] args) {
        List<String> list = Arrays.asList("m,k,l,a", "1,3,5,7");
        List<String> listNew = list.stream().flatMap(s -> {
            // 将每个元素转换成一个stream
            String[] split = s.split(",");
            Stream<String> s2 = Arrays.stream(split);
            return s2;
        }).collect(Collectors.toList());
        System.out.println("处理前的集合：" + list);
        System.out.println("处理后的集合：" + listNew);
    }
}
```

输出结果：

```awk
处理前的集合：[m,k,l,a, 1,3,5,7]
处理后的集合：[m, k, l, a, 1, 3, 5, 7]

Process finished with exit code 0
```

### 归约(reduce)

归约，也称缩减，顾名思义，是把一个流缩减成一个值，能实现对集合求和、求乘积和求最值操作。

**案例一：求`Integer`集合的元素之和、乘积和最大值。**

```java
public class StreamTest {

    public static void main(String[] args) {
        List<Integer> list = Arrays.asList(1, 3, 2, 8, 11, 4);
        // 求和方式1
        Optional<Integer> sum = list.stream().reduce(Integer::sum);
        // 求和方式2
        Optional<Integer> sum2 = list.stream().reduce(Integer::sum);
        // 求和方式3
        Integer sum3 = list.stream().reduce(0, Integer::sum);
        // 求乘积
        Optional<Integer> product = list.stream().reduce((x, y) -> x * y);
        // 求最大值方式1
        Optional<Integer> max = list.stream().reduce((x, y) -> x > y ? x : y);
        // 求最大值写法2
        Integer max2 = list.stream().reduce(1, Integer::max);
        System.out.println("list求和：" + sum.get() + "," + sum2.get() + "," + sum3);
        System.out.println("list求积：" + product.get());
        System.out.println("list求和：" + max.get() + "," + max2);
    }
}
```

输出结果：

```awk
list求和：29,29,29
list求积：2112
list求和：11,11

Process finished with exit code 0
```

### 归集(toList/toSet/toMap)

因为流不存储数据，那么在流中的数据完成处理后，需要将流中的数据重新归集到新的集合里。`toList`、`toSet`和`toMap`比较常用，另外还有`toCollection`、`toConcurrentMap`等复杂一些的用法。

下面用一个案例演示`toList`、`toSet`和`toMap`：

```java
public class Person {

    private String name;  // 姓名
    private int salary; // 薪资
    private int age; // 年龄
    private String sex; //性别
    private String area;  // 地区

    // 构造方法
    public Person(String name, int salary, int age,String sex,String area) {
        this.name = name;
        this.salary = salary;
        this.age = age;
        this.sex = sex;
        this.area = area;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getSalary() {
        return salary;
    }

    public void setSalary(int salary) {
        this.salary = salary;
    }

    public int getAge() {
        return age;
    }

    public void setAge(int age) {
        this.age = age;
    }

    public String getSex() {
        return sex;
    }

    public void setSex(String sex) {
        this.sex = sex;
    }

    public String getArea() {
        return area;
    }

    public void setArea(String area) {
        this.area = area;
    }

    @Override
    public String toString() {
        return "Person{" +
                "name='" + name + '\'' +
                ", salary=" + salary +
                ", age=" + age +
                ", sex='" + sex + '\'' +
                ", area='" + area + '\'' +
                '}';
    }
}
public class StreamTest {

    public static void main(String[] args) {
        List<Integer> list = Arrays.asList(1, 6, 3, 4, 6, 7, 9, 6, 20);
        List<Integer> listNew = list.stream().filter(x -> x % 2 == 0).collect(Collectors.toList());
        Set<Integer> set = list.stream().filter(x -> x % 2 == 0).collect(Collectors.toSet());
        List<Person> personList = new ArrayList<Person>();
        personList.add(new Person("Tom", 8900, 23, "male", "New York"));
        personList.add(new Person("Jack", 7000, 25, "male", "Washington"));
        personList.add(new Person("Lily", 7800, 21, "female", "Washington"));
        personList.add(new Person("Anni", 8200, 24, "female", "New York"));
        Map<?, Person> map = personList.stream().filter(p -> p.getSalary() > 8000)
                .collect(Collectors.toMap(Person::getName, p -> p));
        System.out.println("toList:" + listNew);
        System.out.println("toSet:" + set);
        System.out.println("toMap:" + map);
    }
}
```

输出结果：

```routeros
toList:[6, 4, 6, 6, 20]
toSet:[4, 20, 6]
toMap:{Tom=Person{name='Tom', salary=8900, age=23, sex='male', area='New York'}, Anni=Person{name='Anni', salary=8200, age=24, sex='female', area='New York'}}

Process finished with exit code 0
```

### 统计(count/averaging)

`Collectors`提供了一系列用于数据统计的静态方法：

- 计数：`count`
- 平均值：`averagingInt`、`averagingLong`、`averagingDouble`
- 最值：`maxBy`、`minBy`
- 求和：`summingInt`、`summingLong`、`summingDouble`
- 统计以上所有：`summarizingInt`、`summarizingLong`、`summarizingDouble`

**案例：统计员工人数、平均工资、工资总额、最高工资。**

```java
public class StreamTest {

    public static void main(String[] args) {
        List<Person> personList = new ArrayList<Person>();
        personList.add(new Person("Tom", 8900, 23, "male", "New York"));
        personList.add(new Person("Jack", 7000, 25, "male", "Washington"));
        personList.add(new Person("Lily", 7800, 21, "female", "Washington"));
        // 求总数
        long count = personList.size();
        // 求平均工资
        Double average = personList.stream().collect(Collectors.averagingDouble(Person::getSalary));
        // 求最高工资
        Optional<Integer> max = personList.stream().map(Person::getSalary).max(Integer::compare);
        // 求工资之和
        int sum = personList.stream().mapToInt(Person::getSalary).sum();
        // 一次性统计所有信息
        DoubleSummaryStatistics collect = personList.stream().collect(Collectors.summarizingDouble(Person::getSalary));
        System.out.println("员工总数：" + count);
        System.out.println("员工平均工资：" + average);
        System.out.println("员工最高工资：" + max.get());
        System.out.println("员工工资总和：" + sum);
        System.out.println("员工工资所有统计：" + collect);
    }
}
```

输出结果：

```awk
员工总数：3
员工平均工资：7900.0
员工最高工资：8900
员工工资总和：23700
员工工资所有统计：DoubleSummaryStatistics{count=3, sum=23700.000000, min=7000.000000, average=7900.000000, max=8900.000000}

Process finished with exit code 0
```

### 分组(partitioningBy/groupingBy)

- 分区：将`stream`按条件分为两个`Map`，比如员工按薪资是否高于8000分为两部分。
- 分组：将集合分为多个Map，比如员工按性别分组。有单级分组和多级分组。

**案例：将员工按薪资是否高于8000分为两部分；将员工按性别和地区分组**

```java
public class StreamTest {

    public static void main(String[] args) {
        List<Person> personList = new ArrayList<Person>();
        personList.add(new Person("Tom", 8900, 23, "male", "Washington"));
        personList.add(new Person("Jack", 7000, 25, "male", "Washington"));
        personList.add(new Person("Lily", 7800, 21, "female", "New York"));
        personList.add(new Person("Anni", 8200, 24, "female", "New York"));
        // 将员工按薪资是否高于8000分组
        Map<Boolean, List<Person>> part = personList.stream().collect(Collectors.partitioningBy(x -> x.getSalary() > 8000));
        // 将员工按性别分组
        Map<String, List<Person>> group = personList.stream().collect(Collectors.groupingBy(Person::getSex));
        // 将员工先按性别分组，再按地区分组
        Map<String, Map<String, List<Person>>> group2 = personList.stream().collect(Collectors.groupingBy(Person::getSex, Collectors.groupingBy(Person::getArea)));
        System.out.println("员工按薪资是否大于8000分组情况：" + part);
        System.out.println("员工按性别分组情况：" + group);
        System.out.println("员工按性别、地区：" + group2);
    }
}
```

输出结果：

```routeros
员工按薪资是否大于8000分组情况：{false=[Person{name='Jack', salary=7000, age=25, sex='male', area='Washington'}, Person{name='Lily', salary=7800, age=21, sex='female', area='New York'}], true=[Person{name='Tom', salary=8900, age=23, sex='male', area='Washington'}, Person{name='Anni', salary=8200, age=24, sex='female', area='New York'}]}
员工按性别分组情况：{female=[Person{name='Lily', salary=7800, age=21, sex='female', area='New York'}, Person{name='Anni', salary=8200, age=24, sex='female', area='New York'}], male=[Person{name='Tom', salary=8900, age=23, sex='male', area='Washington'}, Person{name='Jack', salary=7000, age=25, sex='male', area='Washington'}]}
员工按性别、地区：{female={New York=[Person{name='Lily', salary=7800, age=21, sex='female', area='New York'}, Person{name='Anni', salary=8200, age=24, sex='female', area='New York'}]}, male={Washington=[Person{name='Tom', salary=8900, age=23, sex='male', area='Washington'}, Person{name='Jack', salary=7000, age=25, sex='male', area='Washington'}]}}

Process finished with exit code 0
```

### 接合(joining)

`joining`可以将stream中的元素用特定的连接符（没有的话，则直接连接）连接成一个字符串。

```java
public class StreamTest {

    public static void main(String[] args) {
        List<Person> personList = new ArrayList<Person>();
        personList.add(new Person("Tom", 8900, 23, "male", "New York"));
        personList.add(new Person("Jack", 7000, 25, "male", "Washington"));
        personList.add(new Person("Lily", 7800, 21, "female", "Washington"));
        String names = personList.stream().map(Person::getName).collect(Collectors.joining(","));
        System.out.println("所有员工的姓名：" + names);
        List<String> list = Arrays.asList("A", "B", "C");
        String string = list.stream().collect(Collectors.joining("-"));
        System.out.println("拼接后的字符串：" + string);
    }
}
```

输出结果：

```awk
所有员工的姓名：Tom,Jack,Lily
拼接后的字符串：A-B-C

Process finished with exit code 0
```

### 排序(sorted)

`sorted`，中间操作。有两种排序：

- `sorted()`：自然排序，流中元素需实现`Comparable`接口
- `sorted(Comparator com)`：`Comparator`排序器自定义排序

**案例：将员工按工资由高到低（工资一样则按年龄由大到小）排序**

```java
public class StreamTest {

    public static void main(String[] args) {
        List<Person> personList = new ArrayList<Person>();
        personList.add(new Person("Sherry", 9000, 24, "female", "New York"));
        personList.add(new Person("Tom", 8900, 22, "male", "Washington"));
        personList.add(new Person("Jack", 9000, 25, "male", "Washington"));
        personList.add(new Person("Lily", 8800, 26, "male", "New York"));
        personList.add(new Person("Alisa", 9000, 26, "female", "New York"));
        // 按工资升序排序（自然排序）
        List<String> newList = personList.stream().sorted(Comparator.comparing(Person::getSalary)).map(Person::getName)
                .collect(Collectors.toList());
        // 按工资倒序排序
        List<String> newList2 = personList.stream().sorted(Comparator.comparing(Person::getSalary).reversed())
                .map(Person::getName).collect(Collectors.toList());
        // 先按工资再按年龄升序排序
        List<String> newList3 = personList.stream()
                .sorted(Comparator.comparing(Person::getSalary).thenComparing(Person::getAge)).map(Person::getName)
                .collect(Collectors.toList());
        // 先按工资再按年龄自定义排序（降序）
        List<String> newList4 = personList.stream().sorted((p1, p2) -> {
            if (p1.getSalary() == p2.getSalary()) {
                return p2.getAge() - p1.getAge();
            } else {
                return p2.getSalary() - p1.getSalary();
            }
        }).map(Person::getName).collect(Collectors.toList());
        System.out.println("按工资升序排序：" + newList);
        System.out.println("按工资降序排序：" + newList2);
        System.out.println("先按工资再按年龄升序排序：" + newList3);
        System.out.println("先按工资再按年龄自定义降序排序：" + newList4);
    }
}
```

输出结果：

```awk
按工资升序排序：[Lily, Tom, Sherry, Jack, Alisa]
按工资降序排序：[Sherry, Jack, Alisa, Tom, Lily]
先按工资再按年龄升序排序：[Lily, Tom, Sherry, Jack, Alisa]
先按工资再按年龄自定义降序排序：[Alisa, Jack, Sherry, Tom, Lily]

Process finished with exit code 0
```

### 提取/组合

流也可以进行合并、去重、限制、跳过等操作。

```java
public class StreamTest {

    public static void main(String[] args) {
        String[] arr1 = { "a", "b", "c", "d" };
        String[] arr2 = { "d", "e", "f", "g" };
        Stream<String> stream1 = Stream.of(arr1);
        Stream<String> stream2 = Stream.of(arr2);
        // concat:合并两个流 distinct：去重
        List<String> newList = Stream.concat(stream1, stream2).distinct().collect(Collectors.toList());
        // limit：限制从流中获得前n个数据
        List<Integer> collect = Stream.iterate(1, x -> x + 2).limit(10).collect(Collectors.toList());
        // skip：跳过前n个数据
        List<Integer> collect2 = Stream.iterate(1, x -> x + 2).skip(1).limit(5).collect(Collectors.toList());
        System.out.println("流合并：" + newList);
        System.out.println("limit：" + collect);
        System.out.println("skip：" + collect2);
    }
}
```

输出结果：

```awk
流合并：[a, b, c, d, e, f, g]
limit：[1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
skip：[3, 5, 7, 9, 11]

Process finished with exit code 0
```

### 分页操作

stream api 的强大之处还不仅仅是对集合进行各种组合操作，还支持分页操作。

例如，将如下的数组从小到大进行排序，排序完成之后，从第1行开始，查询10条数据出来，操作如下：

```java
//需要查询的数据
List<Integer> numbers = Arrays.asList(3, 2, 2, 3, 7, 3, 5, 10, 6, 20, 30, 40, 50, 60, 100);
List<Integer> dataList = numbers.stream().sorted(Integer::compareTo).skip(0).limit(10).collect(Collectors.toList());
System.out.println(dataList.toString());
```

输出结果：

```awk
[2, 2, 3, 3, 3, 5, 6, 7, 10, 20]

Process finished with exit code 0
```

### 并行操作

所谓并行，指的是多个任务在同一时间点发生，并由不同的cpu进行处理，不互相抢占资源；而并发，指的是多个任务在同一时间点内同时发生了，但由同一个cpu进行处理，互相抢占资源。

stream api 的并行操作和串行操作，只有一个方法区别，其他都一样，例如下面我们使用parallelStream来输出空字符串的数量：

```java
List<String> strings = Arrays.asList("abc", "", "bc", "efg", "abcd", "", "jkl");
// 采用并行计算方法，获取空字符串的数量
long count = strings.parallelStream().filter(String::isEmpty).count();
System.out.println(count);
```

**在实际使用的时候，`并行操作`不一定比`串行操作`快！对于简单操作，数量非常大，同时服务器是多核的话，建议使用Stream并行！反之，采用串行操作更可靠！**

### 集合转Map操作

在实际的开发过程中，还有一个使用最频繁的操作就是，将集合元素中某个主键字段作为key，元素作为value，来实现集合转map的需求，这种需求在数据组装方面使用的非常多。

```java
public static void main(String[] args) {
    List<Person> personList = new ArrayList<>();
    personList.add(new Person("Tom",7000,25,"male","安徽"));
    personList.add(new Person("Jack",8000,30,"female","北京"));
    personList.add(new Person("Lucy",9000,40,"male","上海"));
    personList.add(new Person("Airs",10000,40,"female","深圳"));
    Map<Integer, Person> collect = personList.stream().collect(Collectors.toMap(Person::getAge, v -> v, (k1, k2) -> k1));
    System.out.println(collect);
}
```

输出结果：

```routeros
{40=Person{name='Lucy', salary=9000, age=40, sex='male', area='上海'}, 25=Person{name='Tom', salary=7000, age=25, sex='male', area='安徽'}, 30=Person{name='Jack', salary=8000, age=30, sex='female', area='北京'}}

Process finished with exit code 0
```

打开`Collectors.toMap`方法源码，一起来看看。

```java
public static <T, K, U>
    Collector<T, ?, Map<K,U>> toMap(Function<? super T, ? extends K> keyMapper,
                                    Function<? super T, ? extends U> valueMapper,
                                    BinaryOperator<U> mergeFunction) {
    return toMap(keyMapper, valueMapper, mergeFunction, HashMap::new);
}
```

从参数表可以看出：

- 第一个参数：表示 key
- 第二个参数：表示 value
- 第三个参数：表示某种规则

上文中的`Collectors.toMap(Person::getAge, v -> v, (k1,k2) -> k1)`，表达的意思就是将`age`的内容作为`key`，`v -> v`是表示将元素`person`作为`value`，其中`(k1,k2) -> k1`表示如果存在相同的`key`，将第一个匹配的元素作为内容，第二个舍弃！

- https://segmentfault.com/a/1190000040276586