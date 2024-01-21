[TOC]

## 简介

APT+JavaPoet 是一把利剑，可以将很多模板代码在编译期间直接生成，即通过注解收集信息，然后将这些信息形成一些固定代码；特别是在写框架的时候，可以将一些“脏活、累活”通过这种方式处理掉，然后提供给用户一个干净的API接口使用，目前常用在

- 路由 如[ARouter](https://github.com/alibaba/ARouter)
- ButterKnife、Dagger等
- JsBridge (Hanlder或者Actioon)
- 权限
  如 [MPermissions](https://github.com/hongyangAndroid/MPermissions)、[PermissionsDispatcher](https://github.com/permissions-dispatcher/PermissionsDispatcher)
- 工厂模式相关 [工厂模式简化](https://race604.com/annotation-processing/)

一些复杂类型的Adapter 等等，这些都可以找到相关的开源库



## 一些技巧

### 老生常谈的面向接口编程

比如在[MPermissions](https://github.com/hongyangAndroid/MPermissions)中，提供了

```java
public interface PermissionProxy<T> {
    void grant(T source, int requestCode);
    void denied(T source, int requestCode);
    void rationale(T source, int requestCode);
    boolean needShowRationale(int requestCode);
}
```

然后APT生成的代码实现该接口

```java
public class MainActivity$$PermissionProxy implements PermissionProxy<MainActivity> {
    ……
}
```

实际逻辑操作中直接使用该接口

```java
public static boolean shouldShowRequestPermissionRationale(Activity activity, String permission, int requestCode) {
        //核心逻辑只关注接口
        PermissionProxy proxy = findPermissionProxy(activity);
        if (!proxy.needShowRationale(requestCode)) return false;
        if (ActivityCompat.shouldShowRequestPermissionRationale(activity,
                permission)) {
            proxy.rationale(activity, requestCode);
            return true;
        }
        return false;
}
```

### 信息注入分离

比如在ARouter中有一个`仓库`WearHouse类里面就是一些空壳容器，用来盛放路由的元信息，

```java
class Warehouse {
    // Cache route and metas
    static Map<String, Class<? extends IRouteGroup>> groupsIndex = new HashMap<>();
    static Map<String, RouteMeta> routes = new HashMap<>();

    // Cache provider
    static Map<Class, IProvider> providers = new HashMap<>();
    static Map<String, RouteMeta> providersIndex = new HashMap<>();

    // Cache interceptor
    static Map<Integer, Class<? extends IInterceptor>> interceptorsIndex = new UniqueKeyTreeMap<>("More than one interceptors use same priority [%s]");
    static List<IInterceptor> interceptors = new ArrayList<>();
    ……
}
```

我们知道具体的信息是在注解之中，APT+JavaPoet负责将信息收集，在ARouter中体现如下

```java
public class ARouter$$Group$$service implements IRouteGroup {
  @Override
  public void loadInto(Map<String, RouteMeta> atlas) {
    atlas.put("/service/hello", RouteMeta.build(RouteType.PROVIDER, HelloServiceImpl.class, "/service/hello", "service", null, -1, -2147483648));
    atlas.put("/service/json", RouteMeta.build(RouteType.PROVIDER, JsonServiceImpl.class, "/service/json", "service", null, -1, -2147483648));
    atlas.put("/service/single", RouteMeta.build(RouteType.PROVIDER, SingleService.class, "/service/single", "service", null, -1, -2147483648));
  }
}
```

可以看出只有一个接受注入的信息的函数,然后在实际逻辑处理中,将此处的信息load到WareHouse中对应map

LogisticsCenter#completion

```java
IRouteGroup iGroupInstance = groupMeta.getConstructor().newInstance();
iGroupInstance.loadInto(Warehouse.routes);
Warehouse.groupsIndex.remove(postcard.getGroup());
```

干净清爽

### 常用的JavaPoet

基本操作可以查看官方文档[JavaPoet](https://github.com/square/javapoet) 这里讲一下一些难点所在不过一般都是纠结在 获取类、接口、Map、带泛型的Map，下面将一一说明

#### 获取类

有两种方式

- ClassName.bestGuess(“类全名称”) 返回ClassName对象，这里的类全名称表示的类必须要存在，会自动导入相应的包
- ClassName.get(“包名”，”类名”) 返回ClassName对象，不检查该类是否存在

因此需要注意获取类全名的类在以后重构时候`改名类名`或者`移动了位置`需要对应修改这里

#### 占位符

- $L 字面常量（Literals）
- $S 字符串常量（String）
- $T 类型(Types)
  该占位符最大特点就是会自动导包
- $N 命名(Names),通常指我们自己生成的方法名或者变量名等等

#### 复杂类型

稍微复杂点的类型 比如泛型 、Map之类的，需要了解下JavaPoet定义的几种专门描述类型的类

[![复杂类型](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/pcUmSHs8rEWmnjBzHmzViRE6YxdeWylIkMEccEFbS4k!/r/dLYAAAAAAAAA)

常见的有

| 分类                                   | 生成的类型       | JavaPoet 写法                                       | 也可以这么写 （等效的 Java 写法） |
| :------------------------------------- | :--------------- | :-------------------------------------------------- | :-------------------------------- |
| 内置类型                               | int              | TypeName.INT                                        | int.class                         |
| 数组类型                               | int[]            | ArrayTypeName.of(int.class)                         | int[].class                       |
| 需要引入包名的类型                     | java.io.File     | ClassName.get(“java.io”, “File”)                    | java.io.File.class                |
| 参数化类型 （ParameterizedType         | List             | ParameterizedTypeName.get(List.class, String.class) | -                                 |
| 类型变量 （WildcardType） 用于声明泛型 | T                | TypeVariableName.get(“T”)                           | -                                 |
| 通配符类型                             | ? extends String | WildcardTypeName.subtypeOf(String.class)            | -                                 |

通过ARouter中的一段代码，就可以解释的很清楚

RouterProcessor

```java
//参数化类型 Map<String, Class<? extends IRouteGroup>>
ParameterizedTypeName inputMapTypeOfRoot = ParameterizedTypeName.get(
            ClassName.get(Map.class),
            ClassName.get(String.class),
            ParameterizedTypeName.get(
                    ClassName.get(Class.class),
                    WildcardTypeName.subtypeOf(ClassName.get(type_IRouteGroup))
            )
    );
    
//参数化类型 Map<String, RouteMeta>
ParameterizedTypeName inputMapTypeOfGroup = ParameterizedTypeName.get(
            ClassName.get(Map.class),
            ClassName.get(String.class),
            ClassName.get(RouteMeta.class)
    );        
    
    /*
     *Build input param name.
     */
    ParameterSpec rootParamSpec = ParameterSpec.builder(inputMapTypeOfRoot, "routes").build();
    ParameterSpec groupParamSpec = ParameterSpec.builder(inputMapTypeOfGroup, "atlas").build();
```

**生成代码**

```java
public class ARouter$$Root$$app implements IRouteRoot {
  @Override
  public void loadInto(Map<String, Class<? extends IRouteGroup>> routes) {
    routes.put("service", ARouter$$Group$$service.class);
    routes.put("test", ARouter$$Group$$test.class);
  }
}


public class ARouter$$Group$$service implements IRouteGroup {
  @Override
  public void loadInto(Map<String, RouteMeta> atlas) {
    atlas.put("/service/hello", RouteMeta.build(RouteType.PROVIDER, HelloServiceImpl.class, "/service/hello", "service", null, -1, -2147483648));
    atlas.put("/service/json", RouteMeta.build(RouteType.PROVIDER, JsonServiceImpl.class, "/service/json", "service", null, -1, -2147483648));
    atlas.put("/service/single", RouteMeta.build(RouteType.PROVIDER, SingleService.class, "/service/single", "service", null, -1, -2147483648));
  }
}
```

或者直接先把你需要的泛型都写出来

```java
final ClassName java_lang_Class = ClassName.get(Class.class);
        final ClassName java_util_Collections = ClassName.get("java.util", "Collections");
        final ClassName java_util_Map = ClassName.get("java.util", "Map");
        final ClassName java_util_Set = ClassName.get("java.util", "Set");
        final ClassName java_util_LinkedHashMap = ClassName.get("java.util", "LinkedHashMap");
        final ClassName java_util_LinkedHashSet = ClassName.get("java.util", "LinkedHashSet");
        final ClassName instantiator = ClassName.get("java.util.concurrent", "Callable");
        final TypeName classOfAny = ParameterizedTypeName.get(java_lang_Class, any);
        final TypeName instantiatorOfAny = ParameterizedTypeName.get(instantiator, any);
        final TypeName instantiatorOfP = ParameterizedTypeName.get(instantiator, p);
        final TypeName classOfS = ParameterizedTypeName.get(java_lang_Class, s);
        final TypeName classOfP = ParameterizedTypeName.get(java_lang_Class, p);
        final TypeName classOfSubTypeOfS = ParameterizedTypeName.get(java_lang_Class, subTypeOfS);
        final TypeName setOfClass = ParameterizedTypeName.get(java_util_Set, classOfAny);
        final TypeName setOfClassOfSubTypeOfS = ParameterizedTypeName.get(java_util_Set, classOfSubTypeOfS);
        final TypeName linkedHashSetOfClass = ParameterizedTypeName.get(java_util_LinkedHashSet, classOfAny);
        final TypeName mapOfClassToSetOfClass = ParameterizedTypeName.get(java_util_Map, classOfAny, setOfClass);
        final TypeName mapOfClassToInstantiator = ParameterizedTypeName.get(java_util_Map, classOfAny, instantiatorOfAny);
        final TypeName linkedHashMapOfClassToSetOfClass = ParameterizedTypeName.get(java_util_LinkedHashMap, classOfAny, setOfClass);
        final TypeName linkedHashMapOfClassToInstantializer = ParameterizedTypeName.get(java_util_LinkedHashMap, classOfAny, instantiatorOfAny);
```

然后在需要时直接拿到即可,这里是作为一个变量(Field使用)

```java
.addField(FieldSpec.builder(mapOfClassToSetOfClass, "sServices")
                        .addModifiers(Modifier.PRIVATE, Modifier.STATIC, Modifier.FINAL)
                        .initializer("new $T()", linkedHashMapOfClassToSetOfClass)
                        .build())
                        
.addField(FieldSpec.builder(mapOfClassToInstantiator, "sInstantiators")
                        .addModifiers(Modifier.PRIVATE, Modifier.STATIC, Modifier.FINAL)
                        .initializer("new $T()", linkedHashMapOfClassToInstantializer)
                        .build())
```

## 要点分析

### Element和TypeMirror

这个点还是非常重要的，我们的java代码在对于APT处理时只不过各种的Element的结构化文本，当我们需要进行细致的逻辑判断时候，比如是否是某个类的子类，就需要操作他们了

```java
package com.example;    // PackageElement

public class Test {        // TypeElement

    private int a;      // VariableElement
    private Test other;  // VariableElement

    public Test () {}    // ExecuteableElement
    public void setA (  // ExecuteableElement
                     int newA   // TypeElement
                     ) {}
}
```

`Element`代表java源文件中的程序构建元素，例如包、类、方法等。Element接口有5个子类。

| PackageElement       | 表示一个包程序元素，可以获取到包名等                         |
| :------------------- | :----------------------------------------------------------- |
| TypeParameterElement | 表示一般类、接口、方法或构造方法元素的泛型参数               |
| TypeElement          | 表示一个类或接口程序元素                                     |
| VariableElement      | 表示一个字段、enum 常量、方法或构造方法参数、局部变量或异常参数 |
| ExecutableElement    | 表示某个类或接口的方法、构造方法或初始化程序（静态或实例），包括注解类型元素 |

开发中Element可根据实际情况强转为以上5种中的一种

- 当你有一个注解是以@Target(ElementType.METHOD)定义时，表示该注解只能修饰方法。
  那么这个时候你为了生成代码，而需要获取一些基本信息：包名、类名、方法名、参数类型、返回值,如何做？

```java
//@Target(ElementType.METHOD)修饰
for (Element element : roundEnv.getElementsAnnotatedWith(Inject.class)) {
    //对于Element直接强转
    ExecutableElement executableElement = (ExecutableElement) element;

    //非对应的Element，通过getEnclosingElement转换获取
    TypeElement classElement = (TypeElement) element
                .getEnclosingElement();

    //当(ExecutableElement) element成立时，使用(PackageElement) element
    //            .getEnclosingElement();将报错。
    //需要使用elementUtils来获取
    Elements elementUtils = processingEnv.getElementUtils();
    PackageElement packageElement = elementUtils.getPackageOf(classElement);

    //全类名
    String fullClassName = classElement.getQualifiedName().toString();
    //类名
    String className = classElement.getSimpleName().toString();
    //包名
    String packageName = packageElement.getQualifiedName().toString();
    //方法名
    String methodName = executableElement.getSimpleName().toString();

    //取得方法参数列表
    List<? extends VariableElement> methodParameters = executableElement.getParameters();
    //参数类型列表
    List<String> types = new ArrayList<>();
    for (VariableElement variableElement : methodParameters) {
        TypeMirror methodParameterType = variableElement.asType();
        if (methodParameterType instanceof TypeVariable) {
            TypeVariable typeVariable = (TypeVariable) methodParameterType;
            methodParameterType = typeVariable.getUpperBound();

        }
        //参数名
        String parameterName = variableElement.getSimpleName().toString();
        //参数类型
        String parameteKind = methodParameterType.toString();
        types.add(methodParameterType.toString());
    }
}
```

- 当你有一个注解是以@Target(ElementType.FIELD)定义时，表示该注解只能修饰属性、类成员。那么这个时候你为了生成代码，而需要获取一些基本信息：包名、类名、类成员类型、类成员名,如何获取?

```java
for (Element element : roundEnv.getElementsAnnotatedWith(IdProperty.class)) {
    //ElementType.FIELD注解可以直接强转VariableElement
    VariableElement variableElement = (VariableElement) element;
    //非对应的Element，通过getEnclosingElement转换获取
    TypeElement classElement = (TypeElement) element
            .getEnclosingElement();
    PackageElement packageElement = elementUtils.getPackageOf(classElement);
    //类名
    String className = classElement.getSimpleName().toString();
    //包名
    String packageName = packageElement.getQualifiedName().toString();
    //类成员名
    String variableName = variableElement.getSimpleName().toString();

    //类成员类型
    TypeMirror typeMirror = variableElement.asType();
    String type = typeMirror.toString();

}
```

- 当你有一个注解是以@Target(ElementType.TYPE)定义时，表示该注解只能修饰类、接口、枚举。那么这个时候你为了生成代码，而需要获取一些基本信息：包名、类名、全类名、父类，如何获取？

```java
for (Element element : roundEnv.getElementsAnnotatedWith(xxx.class)) {
    //ElementType.TYPE注解可以直接强转TypeElement
    TypeElement classElement = (TypeElement) element;
    //非对应的Element，通过getEnclosingElement转换获取
    PackageElement packageElement = (PackageElement) element
                .getEnclosingElement();

    //全类名
    String fullClassName = classElement.getQualifiedName().toString();
    //类名
    String className = classElement.getSimpleName().toString();
    //包名
    String packageName = packageElement.getQualifiedName().toString();
    //父类名
    String superClassName = classElement.getSuperclass().toString();

}
```

Element代表的是源代码。TypeElement代表的是源代码中的类型元素，例如类。然而，TypeElement并不包含类本身的信息。你可以从TypeElement中获取类的名字，但是你获取不到类的信息，例如它的父类。这种信息需要通过TypeMirror获取。`TypeMirror`用与描述Java程序中元素的信息，即Elment的元信息。通过通过`Element.asType()`接口可以获取Element的TypeMirror，结构比较复杂
![typemirror](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/SEPhQMCdrw43brj4u5TDp82fg7h.lRLpcCGXRn2LPQM!/r/dLYAAAAAAAAA)

常用的TypeMirror，如下

| PrimitiveType  | 原始数据类型，boolean,byte,short int,long,float,char,double |
| :------------- | :---------------------------------------------------------- |
| ReferenceType  | 引用类型                                                    |
| ArrayType      | 数组类型                                                    |
| DeclaredType   | 声明的类型，例如类、接口、枚举、注解类型                    |
| AnnotationType | 注解类型                                                    |
| ClassType      | 类类型                                                      |
| EnumType       | 枚举类型                                                    |
| InterfaceType  | 接口类型                                                    |
| TypeVariable   | 类型变量类型                                                |
| VoidType       | void 类型                                                   |
| WildcardType   | 通配符类型                                                  |

当TypeMirror是DeclaredType或者TypeVariable时，TypeMirror可以转化成Element:

```
Element element = processingEviroment.getTypeUtils().asElement(typeMirror);
```

在ARouter中 为了区分是否是某个类的子类使用到了TypeMirro

RouterProcessor # parseRoutes

```java
 TypeMirror type_Activity = elements.getTypeElement(ACTIVITY).asType();
            TypeMirror type_Service = elements.getTypeElement(SERVICE).asType();
            TypeMirror fragmentTm = elements.getTypeElement(FRAGMENT).asType();
            TypeMirror fragmentTmV4 = elements.getTypeElement(Consts.FRAGMENT_V4).asType();
            
if (types.isSubtype(tm, type_Activity)) {                 // Activity
                    ……
} else if (types.isSubtype(tm, iProvider)) {         // IProvider
                ……
} else if (types.isSubtype(tm, fragmentTm) || types.isSubtype(tm, fragmentTmV4)) {
                 ……
}
```

### 调试

- 使用Messager

`Messager`提供给注解处理器一个报告错误、警告以及提示信息的途径。它不是注解处理器开发者的日志工具，而是用来写一些信息给使用此注解器的第三方开发者的。
在官方文档中描述了消息的不同级别中非常重要的是Kind.ERROR，因为这种类型的信息用来表示我们的注解处理器处理失败了。很有可能是第三方开发者错误的使用了注解。这个概念和传统的Java应用有点不一样，在传统Java应用中我们可能就抛出一个异常Exception。如果你在process()中抛出一个异常，那么运行注解处理器的JVM将会崩溃（就像其他Java应用一样），使用我们注解处理器第三方开发者将会从javac中得到非常难懂的出错信息，因为它包含注解处理器的堆栈跟踪（Stacktace）信息。因此，注解处理器就有一个Messager类，它能够打印非常优美的错误信息。除此之外，你还可以连接到出错的元素。在像现在的IDE（集成开发环境）中，第三方开发者可以直接点击错误信息，IDE将会直接跳转到第三方开发者项目的出错的源文件的相应的行。
因此我们通常封装一个Logger去打印关键点，具体可以参考ARouter的[Logger](https://github.com/alibaba/ARouter/blob/master/arouter-compiler/src/main/java/com/alibaba/android/arouter/compiler/utils/Logger.java)

![断点调试](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/1LvShu.N.Bnr8fwQkzJH*woyDFzB.cxZ5Tf.lzxvIB4!/r/dL8AAAAAAAAA)

## 结语

APT + JavaPoet 固然比较强大，但是也有其局限性，比如它无法扫描 AAR、JAR包，在一些大型app上分模块最终以jar包形式提供的话，就不能扫描到注解了，那这时就需要借助于更为强大的技术了，可以通过自定义Gradle Plugin + JavaAssist在dex之前扫描class方式去生成我们想要的代码,这是后话了。

## 参考

- https://juejin.im/entry/58fefebf8d6d810058a610de/
- https://cloud.tencent.com/developer/article/1006210
- https://race604.com/annotation-processing/
- https://lizhaoxuan.github.io/2016/07/17/apt-Grammar-explanation/

 