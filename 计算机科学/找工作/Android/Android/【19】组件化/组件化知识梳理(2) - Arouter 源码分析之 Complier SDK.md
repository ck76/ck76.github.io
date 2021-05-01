[TOC]

# 组件化知识梳理目录

[组件化知识梳理(1) - Arouter 的基本使用](https://www.jianshu.com/p/5c109c51d7ba)
[组件化知识梳理(2) - Arouter 源码分析之 Complier SDK](https://www.jianshu.com/p/a1f6db686b17)
[组件化知识梳理(3) - Arouter 源码分析之运行时 SDK](https://www.jianshu.com/p/a662c0b8edd2)

# 一、概述

在`Arouter`的源码 [https://github.com/alibaba/ARouter](https://link.jianshu.com/?t=https%3A%2F%2Fgithub.com%2Falibaba%2FARouter) 当中，最主要的就是下面这三个部分：注解`arouter-annotation`、运行时`arouter-api`和编译时`arouter-complier`。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymivqtezj309s01n0sr.jpg)

Arouter 源码



## 1.1 注解 arouter-annotation

`arouter-annotation`定义了我们之前在 [组件化知识梳理(1) - Arouter 的基本使用](https://www.jianshu.com/p/5c109c51d7ba)
中使用过的注解：

- `Router`：路径路由，`2.3.1`。
- `intercepter`：拦截器，`2.3.6`。
- `AutoWired`：依赖注入，`2.3.7`。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymitw1wgj309s05cjrw.jpg)

注解 arouter-annotation

## 1.2 编译时 arouter-complier

`arouter-complier`的内部包含了三个注解处理器，用于处理`1.1`中使用的三种注解，它们都继承于`AbstractProcessor`，其原理和 [Java&Android 基础知识梳理(1) - 注解](https://www.jianshu.com/p/2585d2a7cd97) 中介绍的 **编译时处理注解** 原理是相同的，最终目的就是通过这三个注解处理器，在 **编译期** 扫描被标注的文件，然后按照不同的源文件进行分类，按照 **固定的命名格式生成映射文件**。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymisext9j309t04adgd.jpg)

编译时 arouter-complier

以第一篇文章中看到的`module-home`模块为例，其会通过解析注解创建下面的`.java`映射文件，并打包到最终的`apk`当中：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymirehjvj30cw092t9t.jpg)

module-home 创建的映射文件



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymiq48jhj30cr08njsp.jpg)

module-other 创建的映射文件

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyminrmobj30cq072758.jpg)

module-store 创建的映射文件

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymimdj3yj30ch07k75c.jpg)

lib-base 创建的映射文件

## 1.3 运行时 arouter-api

运行时`arouter-api`所包含的组件可以用下面这张图来表示。
[图片上传失败...(image-9d4212-1523552507081)]
从上之下分为四层：

- `Launcher`层，包含了开发者可以直接使用的`API`，例如`Arouter.getInstance()...`。

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymilametj308501mglj.jpg)

  Launcher 层

  

- `Frossard`层，包含了三个部分：

  - `Service`，从意义上讲是将一定的功能和组件封装成接口，并对外提供能力。

  - `Callback`，回调接口。

  - ```
    Template
    ```

    ，用于处理在编译期

    ```
    SDK
    ```

    生成的映射文件，这些映射文件会按照

    ```
    Template
    ```

    组件中提供的模板来生成，这样按照一定规则和约束生成的映射文件也方便

    ```
    ARouter
    ```

    在运行时进行读取。

    ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymijff70j30800c6405.jpg)

    Frossard 层

- `SDK`内部实现：

  - `Ware House`，主要存储了`ARouter`在运行期间加载的一些配置文件以及映射关系。

  - `Thread`，提供异步加载的线程池。

  - ```
    Class Tool
    ```

    ，解决不同类型的

    ```
    APK
    ```

    兼容问题。

    ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymihjop0j30970cugnh.jpg)

    SDK 内部实现

- `Logistics Center`，物流中心，整个`SDK`的流转以及内部调用都会下沉到这一层，并会按照功能模块进行划分。

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymigok8bj309a05dgmd.jpg)

  Logistics Center

  

# 二、arouter-complier 详解

今天这篇文章，我们就来分析编译时的`arouter-complier`的实现，看一下映射文件是如何生成的。

## 2.1 基础知识

按照注解的处理时期，分为两种类型：运行时和编译时，运行时注解处理会引起性能问题，编译时注解依赖`APT(Annotation Processing Tools)`实现，其原理是在类、函数、字段上添加注解，在编译时，编译器会去检查`AbstractProcessor`的子类，并调用它实现的`process`函数，然后将添加了注解的所有元素都传递到`process`函数中，使得开发人员可以在编译期进行处理，主要就是生成新的`Java`类。

关于编译时注解的处理，我们在 [Java&Android 基础知识梳理(1) - 注解](https://www.jianshu.com/p/2585d2a7cd97) 文中用一个详细的例子说明了，大家可以看一下，这里我们简要说一下在代码中用到的类的作用：

- ```
  ProcessingEnvironment
  ```

  ：用于提供实用的工具类，主要是

  ```
  Elements
  ```

  、

  ```
  Types
  ```

  和

  ```
  Filer
  ```

  这三个。

  - `Elements`：用来处理`Element`的工具类，源码中的每个部分都是`Element`的一个特定类型，其代码程序中的元素，例如包、类、方法，每一个元素代表一个静态的，语言级别的结构，`Element`是一个`interface`，它定义了如下的接口：

    ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymievyggj30cj089wfe.jpg)

    Element 定义的接口

    根据类、成员变量等不同，有如下的继承接口 & 实现类：

    ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymidcqt4j30ni09etb0.jpg)

    Element 的实现类

    

  - `Types`：用来处理`TypeMirror`的工具类。从`Element`的实现类中，例如`TypeElement`，我们可以获取类的名称，但你不能获取类的信息，可以通过`element.asType()`来获取一个`Element`的`TypeMirror`，然后获取类的信息。

  - `Filer`：用来创建文件。

我们在 **创建映射文件** 的时候，包括以下几步，后一步的结果依赖于前一步：

- 函数形参的类型，使用`ParameterizedTypeName`。
- 函数形参的类型 & 名称，使用`ParameterSpec`。
- 函数的声明，使用`MethodSpec.Builder`。
- 类的声明，使用`JavaFile.builder`，最后调用`JavaFile`的`writeTo(mFiler)`方法写入到磁盘当中，这里的`mFiler`就是通过`ProcessingEnvironment`获取到的。

## 2.2 @Route 注解的解析

下面是对于`RouteProcessor`的 解析，详细的可以看注释里面的说明。



```java
/**
 * A processor used for find route.
 *
 * @author Alex <a href="mailto:zhilong.liu@aliyun.com">Contact me.</a>
 * @version 1.0
 * @since 16/8/15 下午10:08
 */
@AutoService(Processor.class)
@SupportedOptions(KEY_MODULE_NAME)
@SupportedSourceVersion(SourceVersion.RELEASE_7)
@SupportedAnnotationTypes({ANNOTATION_TYPE_ROUTE, ANNOTATION_TYPE_AUTOWIRED})
public class RouteProcessor extends AbstractProcessor {

    /**
     * 以组为 key，value 对应于该组下解析出的所有注解信息，Set 的实现为 TreeSet，会根据 path 来进行排序。
     */
    private Map<String, Set<RouteMeta>> groupMap = new HashMap<>();

    /**
     * 以根节点为 key，value 对应于创建的文件。
     */
    private Map<String, String> rootMap = new TreeMap<>();
    private Filer mFiler;
    private Logger logger;
    private Types types;
    private Elements elements;
    private TypeUtils typeUtils;
    private String moduleName = null;
    private TypeMirror iProvider = null;

    /**
     * 通过 ProcessingEnvironment 初始化后面需要使用到的工具类，例如 Filer，Types 和 Elements。
     * @param processingEnv 提供解析注解所需的工具类。
     */
    @Override
    public synchronized void init(ProcessingEnvironment processingEnv) {
        super.init(processingEnv);

        //Filer：用于创建 Java 文件的工具类。
        mFiler = processingEnv.getFiler();
        //Types：用于操作类型的工具类。
        types = processingEnv.getTypeUtils();
        //Elements：用于处理 Element 的工具类。
        elements = processingEnv.getElementUtils();

        //将两个工具类进行封装。
        typeUtils = new TypeUtils(types, elements);
        //日志工具类。
        logger = new Logger(processingEnv.getMessager());

        //解析我们在 android 节点下配置的 moduleName，其为对应模块的名字。
        Map<String, String> options = processingEnv.getOptions();
        if (MapUtils.isNotEmpty(options)) {
            moduleName = options.get(KEY_MODULE_NAME);
        }

        //对 moduleName 进行处理。
        if (StringUtils.isNotEmpty(moduleName)) {
            moduleName = moduleName.replaceAll("[^0-9a-zA-Z_]+", "");
            logger.info("The user has configuration the module name, it was [" + moduleName + "]");
        //如果没有配置 moduleName，那么会抛出异常。
        } else {
            logger.error("These no module name, at 'build.gradle', like :\n" +
                    "apt {\n" +
                    "    arguments {\n" +
                    "        moduleName project.getName();\n" +
                    "    }\n" +
                    "}\n");
            throw new RuntimeException("ARouter::Compiler >>> No module name, for more information, look at gradle log.");
        }

        //获取 IProvider 的 TypeMirror，它包含了 IProvider 的所有信息。
        iProvider = elements.getTypeElement(Consts.IPROVIDER).asType();

        logger.info(">>> RouteProcessor init. <<<");
    }

    /**
     * {@inheritDoc}
     *
     * @param annotations
     * @param roundEnv
     */
    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        if (CollectionUtils.isNotEmpty(annotations)) {
            //获得所有被 @Route 注解的元素。
            Set<? extends Element> routeElements = roundEnv.getElementsAnnotatedWith(Route.class);
            try {
                logger.info(">>> Found routes, start... <<<");
                //开始处理。
                this.parseRoutes(routeElements);

            } catch (Exception e) {
                logger.error(e);
            }
            return true;
        }

        return false;
    }

    private void parseRoutes(Set<? extends Element> routeElements) throws IOException {
        if (CollectionUtils.isNotEmpty(routeElements)) {

            logger.info(">>> Found routes, size is " + routeElements.size() + " <<<");
            //清空信息。
            rootMap.clear();

            //1.获得 Activity、Service、Fragment 和 FragmentV4 的信息，这些是定义在 Android SDK 当中的。
            TypeMirror type_Activity = elements.getTypeElement(ACTIVITY).asType();
            TypeMirror type_Service = elements.getTypeElement(SERVICE).asType();
            TypeMirror fragmentTm = elements.getTypeElement(FRAGMENT).asType();
            TypeMirror fragmentTmV4 = elements.getTypeElement(Consts.FRAGMENT_V4).asType();

            //2.获得 IRouteGroup 和 IProviderGroup 的信息，这些接口是定义在 arouter-api 当中的模板。
            TypeElement type_IRouteGroup = elements.getTypeElement(IROUTE_GROUP);
            TypeElement type_IProviderGroup = elements.getTypeElement(IPROVIDER_GROUP);

            //3.RouteMeta 和 RouteType 的信息。
            ClassName routeMetaCn = ClassName.get(RouteMeta.class);
            ClassName routeTypeCn = ClassName.get(RouteType.class);

            //4. 定义函数形参的类型为 Map<String, Class<? extends IRouteGroup>>。
            ParameterizedTypeName inputMapTypeOfRoot = ParameterizedTypeName.get(
                    ClassName.get(Map.class),
                    ClassName.get(String.class),
                    ParameterizedTypeName.get(
                            ClassName.get(Class.class),
                            WildcardTypeName.subtypeOf(ClassName.get(type_IRouteGroup))
                    )
            );

            //5. 定义函数形参的类型为 Map<String, RouteMeta>。
            ParameterizedTypeName inputMapTypeOfGroup = ParameterizedTypeName.get(
                    ClassName.get(Map.class),
                    ClassName.get(String.class),
                    ClassName.get(RouteMeta.class)
            );

            //6. 定义函数的形参类型 & 形参名字。
            //6.1 Map<String, Class<? extends IRouteGroup>> routes
            ParameterSpec rootParamSpec = ParameterSpec.builder(inputMapTypeOfRoot, "routes").build();
            //6.2 Map<String, RouteMeta> atlas
            ParameterSpec groupParamSpec = ParameterSpec.builder(inputMapTypeOfGroup, "atlas").build();
            //6.3 Map<String, RouteMeta> providers
            ParameterSpec providerParamSpec = ParameterSpec.builder(inputMapTypeOfGroup, "providers").build();

            //7.定义函数的声明为 public void loadInto(Map<String, Class<? extends IRouteGroup>> routes)。
            MethodSpec.Builder loadIntoMethodOfRootBuilder = MethodSpec.methodBuilder(METHOD_LOAD_INTO)
                    .addAnnotation(Override.class)
                    .addModifiers(PUBLIC)
                    .addParameter(rootParamSpec);

            //8.遍历所有的被 @Route 注解的 Element
            for (Element element : routeElements) {
                //获得该元素的类型信息。
                TypeMirror tm = element.asType();
                //获得该元素的注解。
                Route route = element.getAnnotation(Route.class);
                RouteMeta routeMeta = null;
                //Activity 的子类。
                if (types.isSubtype(tm, type_Activity)) {
                    logger.info(">>> Found activity route: " + tm.toString() + " <<<");
                    Map<String, Integer> paramsType = new HashMap<>();
                    //获得其所有被 @Autowired 注解的成员变量。
                    for (Element field : element.getEnclosedElements()) {
                        //不处理 IProvider 的子类。
                        if (field.getKind().isField() && field.getAnnotation(Autowired.class) != null && !types.isSubtype(field.asType(), iProvider)) {
                            Autowired paramConfig = field.getAnnotation(Autowired.class);
                            //将所有被 @Autowired 注解的相关信息放到 map 当中。
                            paramsType.put(StringUtils.isEmpty(paramConfig.name()) ? field.getSimpleName().toString() : paramConfig.name(), typeUtils.typeExchange(field));
                        }
                    }
                    routeMeta = new RouteMeta(route, element, RouteType.ACTIVITY, paramsType);
                //IProvider 的子类。
                } else if (types.isSubtype(tm, iProvider)) {
                    logger.info(">>> Found provider route: " + tm.toString() + " <<<");
                    routeMeta = new RouteMeta(route, element, RouteType.PROVIDER, null);
                //Service 的子类。
                } else if (types.isSubtype(tm, type_Service)) {
                    logger.info(">>> Found service route: " + tm.toString() + " <<<");
                    routeMeta = new RouteMeta(route, element, RouteType.parse(SERVICE), null);
                //Fragment 或者 FragmentV4 的子类。
                } else if (types.isSubtype(tm, fragmentTm) || types.isSubtype(tm, fragmentTmV4)) {
                    logger.info(">>> Found fragment route: " + tm.toString() + " <<<");
                    routeMeta = new RouteMeta(route, element, RouteType.parse(FRAGMENT), null);
                } else {
                    throw new RuntimeException("ARouter::Compiler >>> Found unsupported class type, type = [" + types.toString() + "].");
                }
                //对其按 group 进行分组。
                categories(routeMeta);
            }

            //9. 定义函数的声明为 public void loadInto(Map<String, RouteMeta> providers)。
            MethodSpec.Builder loadIntoMethodOfProviderBuilder = MethodSpec.methodBuilder(METHOD_LOAD_INTO)
                    .addAnnotation(Override.class)
                    .addModifiers(PUBLIC)
                    .addParameter(providerParamSpec);

            //10. 创建 Java 源代码，遍历所有的 group。
            for (Map.Entry<String, Set<RouteMeta>> entry : groupMap.entrySet()) {
                String groupName = entry.getKey();

                //定义函数的声明为 public void loadInto(Map<String, RouteMeta> atlas)。
                MethodSpec.Builder loadIntoMethodOfGroupBuilder = MethodSpec.methodBuilder(METHOD_LOAD_INTO)
                        .addAnnotation(Override.class)
                        .addModifiers(PUBLIC)
                        .addParameter(groupParamSpec);

                //遍历该 group 下所有的 RouteMeta。
                Set<RouteMeta> groupData = entry.getValue();

                for (RouteMeta routeMeta : groupData) {
                    switch (routeMeta.getType()) {
                        //填充函数体。
                        case PROVIDER:
                            List<? extends TypeMirror> interfaces = ((TypeElement) routeMeta.getRawType()).getInterfaces();
                            for (TypeMirror tm : interfaces) {
                                //IProvider。
                                if (types.isSameType(tm, iProvider)) {
                                    //添加 providers.put(..) 函数体。
                                    loadIntoMethodOfProviderBuilder.addStatement(
                                            "providers.put($S, $T.build($T." + routeMeta.getType() + ", $T.class, $S, $S, null, " + routeMeta.getPriority() + ", " + routeMeta.getExtra() + "))",
                                            (routeMeta.getRawType()).toString(),
                                            routeMetaCn,
                                            routeTypeCn,
                                            ClassName.get((TypeElement) routeMeta.getRawType()),
                                            routeMeta.getPath(),
                                            routeMeta.getGroup());
                                //IProvider 的子类。
                                } else if (types.isSubtype(tm, iProvider)) {
                                    //添加 providers.put(..) 函数体。
                                    loadIntoMethodOfProviderBuilder.addStatement(
                                            "providers.put($S, $T.build($T." + routeMeta.getType() + ", $T.class, $S, $S, null, " + routeMeta.getPriority() + ", " + routeMeta.getExtra() + "))",
                                            tm.toString(),
                                            routeMetaCn,
                                            routeTypeCn,
                                            ClassName.get((TypeElement) routeMeta.getRawType()),
                                            routeMeta.getPath(),
                                            routeMeta.getGroup());
                                }
                            }
                            break;
                        default:
                            break;
                    }

                    StringBuilder mapBodyBuilder = new StringBuilder();
                    Map<String, Integer> paramsType = routeMeta.getParamsType();
                    if (MapUtils.isNotEmpty(paramsType)) {
                        for (Map.Entry<String, Integer> types : paramsType.entrySet()) {
                            mapBodyBuilder.append("put(\"").append(types.getKey()).append("\", ").append(types.getValue()).append("); ");
                        }
                    }
                    String mapBody = mapBodyBuilder.toString();
                    //填充函数体。
                    loadIntoMethodOfGroupBuilder.addStatement(
                            "atlas.put($S, $T.build($T." + routeMeta.getType() + ", $T.class, $S, $S, " + (StringUtils.isEmpty(mapBody) ? null : ("new java.util.HashMap<String, Integer>(){{" + mapBodyBuilder.toString() + "}}")) + ", " + routeMeta.getPriority() + ", " + routeMeta.getExtra() + "))",
                            routeMeta.getPath(),
                            routeMetaCn,
                            routeTypeCn,
                            ClassName.get((TypeElement) routeMeta.getRawType()),
                            routeMeta.getPath().toLowerCase(),
                            routeMeta.getGroup().toLowerCase());
                }

                //关键点1：每一个 group 创建一个 Java 文件，其类名为 Arouter$$Group$$组名，函数名为 public void loadInto(Map<String, RouteMeta> atlas)
                String groupFileName = NAME_OF_GROUP + groupName;
                JavaFile.builder(PACKAGE_OF_GENERATE_FILE,
                        TypeSpec.classBuilder(groupFileName)
                                .addJavadoc(WARNING_TIPS)
                                .addSuperinterface(ClassName.get(type_IRouteGroup))
                                .addModifiers(PUBLIC)
                                .addMethod(loadIntoMethodOfGroupBuilder.build())
                                .build()
                ).build().writeTo(mFiler);

                logger.info(">>> Generated group: " + groupName + "<<<");
                rootMap.put(groupName, groupFileName);
            }

            if (MapUtils.isNotEmpty(rootMap)) {
                for (Map.Entry<String, String> entry : rootMap.entrySet()) {
                    loadIntoMethodOfRootBuilder.addStatement("routes.put($S, $T.class)", entry.getKey(), ClassName.get(PACKAGE_OF_GENERATE_FILE, entry.getValue()));
                }
            }

            //关键点2：创建 Java 文件，类名为 Arouter$$Providers$$moduleName，函数名为 public void loadInto(Map<String, RouteMeta> providers)，存放 PROVIDER 类型的节点。
            String providerMapFileName = NAME_OF_PROVIDER + SEPARATOR + moduleName;
            JavaFile.builder(PACKAGE_OF_GENERATE_FILE,
                    TypeSpec.classBuilder(providerMapFileName)
                            .addJavadoc(WARNING_TIPS)
                            .addSuperinterface(ClassName.get(type_IProviderGroup))
                            .addModifiers(PUBLIC)
                            .addMethod(loadIntoMethodOfProviderBuilder.build())
                            .build()
            ).build().writeTo(mFiler);

            logger.info(">>> Generated provider map, name is " + providerMapFileName + " <<<");

            //关键点3：创建 Java 文件，类名为 Arouter$$Root$$moduleName，函数名为 public void loadInto(Map<String, Class<? extends IRouteGroup>> routes)
            String rootFileName = NAME_OF_ROOT + SEPARATOR + moduleName;
            JavaFile.builder(PACKAGE_OF_GENERATE_FILE,
                    TypeSpec.classBuilder(rootFileName)
                            .addJavadoc(WARNING_TIPS)
                            .addSuperinterface(ClassName.get(elements.getTypeElement(ITROUTE_ROOT)))
                            .addModifiers(PUBLIC)
                            .addMethod(loadIntoMethodOfRootBuilder.build())
                            .build()
            ).build().writeTo(mFiler);

            logger.info(">>> Generated root, name is " + rootFileName + " <<<");
        }
    }

    /**
     * 对 @Route 注解的类进行分类。
     * @param routeMete
     */
    private void categories(RouteMeta routeMete) {
        if (routeVerify(routeMete)) {
            logger.info(">>> Start categories, group = " + routeMete.getGroup() + ", path = " + routeMete.getPath() + " <<<");
            Set<RouteMeta> routeMetas = groupMap.get(routeMete.getGroup());
            if (CollectionUtils.isEmpty(routeMetas)) {
                Set<RouteMeta> routeMetaSet = new TreeSet<>(new Comparator<RouteMeta>() {
                    @Override
                    public int compare(RouteMeta r1, RouteMeta r2) {
                        try {
                            return r1.getPath().compareTo(r2.getPath());
                        } catch (NullPointerException npe) {
                            logger.error(npe.getMessage());
                            return 0;
                        }
                    }
                });
                routeMetaSet.add(routeMete);
                groupMap.put(routeMete.getGroup(), routeMetaSet);
            } else {
                routeMetas.add(routeMete);
            }
        } else {
            logger.warning(">>> Route meta verify error, group is " + routeMete.getGroup() + " <<<");
        }
    }

    /**
     * 验证 RouteMeta，要求 @Route 指定的 name 不为空，并且要使用 / 作为开头。
     *
     * 并解析 RouteMeta 中的 group，默认是使用 / 后的第一个字段，如果指定了 group，那么就使用 group 的值。
     * @param meta
     * @return 验证正确。
     */
    private boolean routeVerify(RouteMeta meta) {
        String path = meta.getPath();

        if (StringUtils.isEmpty(path) || !path.startsWith("/")) {
            return false;
        }

        if (StringUtils.isEmpty(meta.getGroup())) {
            try {
                String defaultGroup = path.substring(1, path.indexOf("/", 1));
                if (StringUtils.isEmpty(defaultGroup)) {
                    return false;
                }

                meta.setGroup(defaultGroup);
                return true;
            } catch (Exception e) {
                logger.error("Failed to extract default group! " + e.getMessage());
                return false;
            }
        }

        return true;
    }
}
```

大家注意看上面的三个关键点，其最终目的就是创建三个`Java`文件，对于第一章中的`module-other`，就是创建了下面三个文件：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymia6px9j30ay02cjre.jpg)

module-other 创建的文件



- `ARouter$$Root$$` + **模块名**，由于`Arouter`对于一个模块下的所有组件都是 **采用分组加载** 的机制，因此该文件存储的是组名，对应的`value`为该组下所有的组件。



```java
/**
 * DO NOT EDIT THIS FILE!!! IT WAS GENERATED BY AROUTER. */
public class ARouter$$Root$$moduleother implements IRouteRoot {
  @Override
  public void loadInto(Map<String, Class<? extends IRouteGroup>> routes) {
    routes.put("other", ARouter$$Group$$other.class);
  }
}
```

- `ARouter$$Group$$` + **组名**：对应该组下所有的组件，可以看到这里面包含了`@Route`中的`path`对应的组件的映射，这些信息都保存在`RouteMeta`当中。



```java
/**
 * DO NOT EDIT THIS FILE!!! IT WAS GENERATED BY AROUTER. */
public class ARouter$$Group$$other implements IRouteGroup {
  @Override
  public void loadInto(Map<String, RouteMeta> atlas) {
    atlas.put("/other/event_bus", RouteMeta.build(RouteType.ACTIVITY, EventBusActivity.class, "/other/event_bus", "other", null, -1, -2147483648));
    atlas.put("/other/inject", RouteMeta.build(RouteType.ACTIVITY, InjectActivity.class, "/other/inject", "other", new java.util.HashMap<String, Integer>(){{put("inject_object", 10); put("inject_age", 3); }}, -1, -2147483648));
    atlas.put("/other/inter_middle", RouteMeta.build(RouteType.ACTIVITY, InterMiddleActivity.class, "/other/inter_middle", "other", null, -1, -2147483648));
    atlas.put("/other/inter_target", RouteMeta.build(RouteType.ACTIVITY, InterTargetActivity.class, "/other/inter_target", "other", null, -1, 2));
    atlas.put("/other/no_result", RouteMeta.build(RouteType.ACTIVITY, NoResultActivity.class, "/other/no_result", "other", null, -1, -2147483648));
    atlas.put("/other/result_server", RouteMeta.build(RouteType.ACTIVITY, ResultServerActivity.class, "/other/result_server", "other", null, -1, -2147483648));
  }
}
```

- `Arouter$$Providers$$` + **模块名**：对应于该模块下所有`IProvider`的子类。



```java
/**
 * DO NOT EDIT THIS FILE!!! IT WAS GENERATED BY AROUTER. */
public class ARouter$$Providers$$moduleother implements IProviderGroup {
  @Override
  public void loadInto(Map<String, RouteMeta> providers) {
  }
}
```

## 2.3 @Interceptor 注解的解析

`@Interceptor`注解的解析是通过`InterceptorProcessor`处理的，其说明如下面代码的注释，原理和`2.2`中`@Route`相同，最终的目标也是创建`Java`文件。



```java
@AutoService(Processor.class)
@SupportedOptions(KEY_MODULE_NAME)
@SupportedSourceVersion(SourceVersion.RELEASE_7)
@SupportedAnnotationTypes(ANNOTATION_TYPE_INTECEPTOR)
public class InterceptorProcessor extends AbstractProcessor {
    private Map<Integer, Element> interceptors = new TreeMap<>();
    private Filer mFiler;       // File util, write class file into disk.
    private Logger logger;
    private Elements elementUtil;
    private String moduleName = null;   // Module name, maybe its 'app' or others
    private TypeMirror iInterceptor = null;

    @Override
    public synchronized void init(ProcessingEnvironment processingEnv) {
        super.init(processingEnv);
        
        //工具类。
        mFiler = processingEnv.getFiler();                  // Generate class.
        elementUtil = processingEnv.getElementUtils();      // Get class meta.
        logger = new Logger(processingEnv.getMessager());   // Package the log utils.

        //获取模块名。
        Map<String, String> options = processingEnv.getOptions();
        if (MapUtils.isNotEmpty(options)) {
            moduleName = options.get(KEY_MODULE_NAME);
        }

        if (StringUtils.isNotEmpty(moduleName)) {
            moduleName = moduleName.replaceAll("[^0-9a-zA-Z_]+", "");
            logger.info("The user has configuration the module name, it was [" + moduleName + "]");
        } else {
            logger.error("These no module name, at 'build.gradle', like :\n" +
                    "apt {\n" +
                    "    arguments {\n" +
                    "        moduleName project.getName();\n" +
                    "    }\n" +
                    "}\n");
            throw new RuntimeException("ARouter::Compiler >>> No module name, for more information, look at gradle log.");
        }
        
        //获得 IInterceptor 的类信息。
        iInterceptor = elementUtil.getTypeElement(Consts.IINTERCEPTOR).asType();

        logger.info(">>> InterceptorProcessor init. <<<");
    }

    /**
     * 系统调用的处理类。
     * @param annotations 所有被注解的元素。
     * @param roundEnv 运行时环境。
     * @return 是否成功处理。
     */
    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        if (CollectionUtils.isNotEmpty(annotations)) {
            Set<? extends Element> elements = roundEnv.getElementsAnnotatedWith(Interceptor.class);
            try {
                parseInterceptors(elements);
            } catch (Exception e) {
                logger.error(e);
            }
            return true;
        }

        return false;
    }

    /**
     * 解析注解，创建 JavaFile
     * @param elements 所有被 @Interceptor 注解的元素。
     * @throws IOException 抛出异常。
     */
    private void parseInterceptors(Set<? extends Element> elements) throws IOException {
        if (CollectionUtils.isNotEmpty(elements)) {
            logger.info(">>> Found interceptors, size is " + elements.size() + " <<<");

            //1.遍历所有被 @Interceptor 注解的元素。
            for (Element element : elements) {
                //必须要实现 IInterceptor 接口。
                if (verify(element)) {
                    logger.info("A interceptor verify over, its " + element.asType());
                    Interceptor interceptor = element.getAnnotation(Interceptor.class);
                    //按照优先级作为 key，存储到 map 当中，必须要保证一个模块内的优先级不会重复，否则会抛出异常。
                    Element lastInterceptor = interceptors.get(interceptor.priority());
                    if (null != lastInterceptor) { // Added, throw exceptions
                        throw new IllegalArgumentException(
                                String.format(Locale.getDefault(), "More than one interceptors use same priority [%d], They are [%s] and [%s].",
                                        interceptor.priority(),
                                        lastInterceptor.getSimpleName(),
                                        element.getSimpleName())
                        );
                    }

                    interceptors.put(interceptor.priority(), element);
                } else {
                    logger.error("A interceptor verify failed, its " + element.asType());
                }
            }
            
            TypeElement type_ITollgate = elementUtil.getTypeElement(IINTERCEPTOR);
            TypeElement type_ITollgateGroup = elementUtil.getTypeElement(IINTERCEPTOR_GROUP);

            //2.函数形参的类型为 Map<Integer, Class<? extends IInterceptor>>
            ParameterizedTypeName inputMapTypeOfTollgate = ParameterizedTypeName.get(
                    ClassName.get(Map.class),
                    ClassName.get(Integer.class),
                    ParameterizedTypeName.get(
                            ClassName.get(Class.class),
                            WildcardTypeName.subtypeOf(ClassName.get(type_ITollgate))
                    )
            );

            //3.函数形参的类型 & 名字为 Map<Integer, Class<? extends IInterceptor>> interceptors
            ParameterSpec tollgateParamSpec = ParameterSpec.builder(inputMapTypeOfTollgate, "interceptors").build();

            //4.函数名为 loadInto
            MethodSpec.Builder loadIntoMethodOfTollgateBuilder = MethodSpec.methodBuilder(METHOD_LOAD_INTO)
                    .addAnnotation(Override.class)
                    .addModifiers(PUBLIC)
                    .addParameter(tollgateParamSpec);

            //5.利用前面遍历出来的列表，填充函数体。
            if (null != interceptors && interceptors.size() > 0) {
                // Build method body
                for (Map.Entry<Integer, Element> entry : interceptors.entrySet()) {
                    loadIntoMethodOfTollgateBuilder.addStatement("interceptors.put(" + entry.getKey() + ", $T.class)", ClassName.get((TypeElement) entry.getValue()));
                }
            }

            //6.关键点：写入 JavaFile，类名为 Arouter$$Interceptors$$moduleName
            JavaFile.builder(PACKAGE_OF_GENERATE_FILE,
                    TypeSpec.classBuilder(NAME_OF_INTERCEPTOR + SEPARATOR + moduleName)
                            .addModifiers(PUBLIC)
                            .addJavadoc(WARNING_TIPS)
                            .addMethod(loadIntoMethodOfTollgateBuilder.build())
                            .addSuperinterface(ClassName.get(type_ITollgateGroup))
                            .build()
            ).build().writeTo(mFiler);

            logger.info(">>> Interceptor group write over. <<<");
        }
    }

    /**
     * 验证，必须要保证实现了 IInterceptor 接口。
     * @param element 元素。
     * @return 验证陈宫。
     */
    private boolean verify(Element element) {
        Interceptor interceptor = element.getAnnotation(Interceptor.class);
        // It must be implement the interface IInterceptor and marked with annotation Interceptor.
        return null != interceptor && ((TypeElement) element).getInterfaces().contains(iInterceptor);
    }
}
```

最终会创建`Java`文件，例如我们在第一章的`Demo`中的`lib-base`，创建的文件为：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymi70j17j309f02pt9b.jpg)

@Interceptor 注解创建的文件


其命名的规则为：`Arouter$$Interceptors` + **模块名**。





```java
/**
 * DO NOT EDIT THIS FILE!!! IT WAS GENERATED BY AROUTER. */
public class ARouter$$Interceptors$$libbase implements IInterceptorGroup {
  @Override
  public void loadInto(Map<Integer, Class<? extends IInterceptor>> interceptors) {
    interceptors.put(1, BaseInterceptor.class);
  }
}
```

## 2.4 @Autowired 注解的解析

`@Autowired`注解的解析依赖于`AutowiredProcessor`，它的原理和上面类似，都是最终创建一个`JavaFile`：



```java
@AutoService(Processor.class)
@SupportedOptions(KEY_MODULE_NAME)
@SupportedSourceVersion(SourceVersion.RELEASE_7)
@SupportedAnnotationTypes({ANNOTATION_TYPE_AUTOWIRED})
public class AutowiredProcessor extends AbstractProcessor {
    private Filer mFiler;       // File util, write class file into disk.
    private Logger logger;
    private Types types;
    private TypeUtils typeUtils;
    private Elements elements;
    private Map<TypeElement, List<Element>> parentAndChild = new HashMap<>();   // Contain field need autowired and his super class.
    private static final ClassName ARouterClass = ClassName.get("com.alibaba.android.arouter.launcher", "ARouter");
    private static final ClassName AndroidLog = ClassName.get("android.util", "Log");

    @Override
    public synchronized void init(ProcessingEnvironment processingEnvironment) {
        super.init(processingEnvironment);

        mFiler = processingEnv.getFiler();                  // Generate class.
        types = processingEnv.getTypeUtils();            // Get type utils.
        elements = processingEnv.getElementUtils();      // Get class meta.

        typeUtils = new TypeUtils(types, elements);

        logger = new Logger(processingEnv.getMessager());   // Package the log utils.

        logger.info(">>> AutowiredProcessor init. <<<");
    }

    @Override
    public boolean process(Set<? extends TypeElement> set, RoundEnvironment roundEnvironment) {
        if (CollectionUtils.isNotEmpty(set)) {
            try {
                logger.info(">>> Found autowired field, start... <<<");
                categories(roundEnvironment.getElementsAnnotatedWith(Autowired.class));
                generateHelper();

            } catch (Exception e) {
                logger.error(e);
            }
            return true;
        }

        return false;
    }

    /**
     * 开始进行解析。
     * @throws IOException IO 异常。
     * @throws IllegalAccessException 不满足状态的异常。
     */
    private void generateHelper() throws IOException, IllegalAccessException {
        TypeElement type_ISyringe = elements.getTypeElement(ISYRINGE);
        TypeElement type_JsonService = elements.getTypeElement(JSON_SERVICE);
        TypeMirror iProvider = elements.getTypeElement(Consts.IPROVIDER).asType();
        TypeMirror activityTm = elements.getTypeElement(Consts.ACTIVITY).asType();
        TypeMirror fragmentTm = elements.getTypeElement(Consts.FRAGMENT).asType();
        TypeMirror fragmentTmV4 = elements.getTypeElement(Consts.FRAGMENT_V4).asType();

        //1. 函数的形参类型为 Object，形参名为 target。
        ParameterSpec objectParamSpec = ParameterSpec.builder(TypeName.OBJECT, "target").build();

        //2.遍历所有拥有 @Autowired 注解的元素。
        if (MapUtils.isNotEmpty(parentAndChild)) {
            for (Map.Entry<TypeElement, List<Element>> entry : parentAndChild.entrySet()) {
                //2.1 方法名为 inject
                MethodSpec.Builder injectMethodBuilder = MethodSpec.methodBuilder(METHOD_INJECT)
                        .addAnnotation(Override.class)
                        .addModifiers(PUBLIC)
                        .addParameter(objectParamSpec);

                TypeElement parent = entry.getKey();
                List<Element> childs = entry.getValue();

                String qualifiedName = parent.getQualifiedName().toString();
                String packageName = qualifiedName.substring(0, qualifiedName.lastIndexOf("."));
                String fileName = parent.getSimpleName() + NAME_OF_AUTOWIRED;

                logger.info(">>> Start process " + childs.size() + " field in " + parent.getSimpleName() + " ... <<<");
                //2.2 类继承于 ISyringe。
                TypeSpec.Builder helper = TypeSpec.classBuilder(fileName)
                        .addJavadoc(WARNING_TIPS)
                        .addSuperinterface(ClassName.get(type_ISyringe))
                        .addModifiers(PUBLIC);

                //2.3 类中包含一个类型为 SerializationService，名称为 serializationService 的成员变量。
                FieldSpec jsonServiceField = FieldSpec.builder(TypeName.get(type_JsonService.asType()), "serializationService", Modifier.PRIVATE).build();
                helper.addField(jsonServiceField);
                
                //2.4 对 Object 类型的形参进行强制转型。
                injectMethodBuilder.addStatement("serializationService = $T.getInstance().navigation($T.class)", ARouterClass, ClassName.get(type_JsonService));
                injectMethodBuilder.addStatement("$T substitute = ($T)target", ClassName.get(parent), ClassName.get(parent));

                //2.5 开始对成员变量遍历进行赋值。
                for (Element element : childs) {
                    Autowired fieldConfig = element.getAnnotation(Autowired.class);
                    String fieldName = element.getSimpleName().toString();
                    //2.5.1 如果是 IProvider 的子类，那么需要获取服务。
                    if (types.isSubtype(element.asType(), iProvider)) { 
                        //没有指定名称，通过类型来获取。
                        if ("".equals(fieldConfig.name())) {

                            // Getter
                            injectMethodBuilder.addStatement(
                                    "substitute." + fieldName + " = $T.getInstance().navigation($T.class)",
                                    ARouterClass,
                                    ClassName.get(element.asType())
                            );
                        //指定了名称，通过名称来获取。    
                        } else {
                            injectMethodBuilder.addStatement(
                                    "substitute." + fieldName + " = ($T)$T.getInstance().build($S).navigation();",
                                    ClassName.get(element.asType()),
                                    ARouterClass,
                                    fieldConfig.name()
                            );
                        }

                        //异常情况处理。
                        if (fieldConfig.required()) {
                            injectMethodBuilder.beginControlFlow("if (substitute." + fieldName + " == null)");
                            injectMethodBuilder.addStatement(
                                    "throw new RuntimeException(\"The field '" + fieldName + "' is null, in class '\" + $T.class.getName() + \"!\")", ClassName.get(parent));
                            injectMethodBuilder.endControlFlow();
                        }
                    //2.5.2 普通的成员变量。    
                    } else { 
                        String originalValue = "substitute." + fieldName;
                        String statement = "substitute." + fieldName + " = substitute.";
                        boolean isActivity = false;
                        //对 Activity 和 Fragment 进行不同的处理，分别使用 getIntent 和 getArguments 来获取参数。
                        if (types.isSubtype(parent.asType(), activityTm)) {  // Activity, then use getIntent()
                            isActivity = true;
                            statement += "getIntent().";
                        } else if (types.isSubtype(parent.asType(), fragmentTm) || types.isSubtype(parent.asType(), fragmentTmV4)) {   // Fragment, then use getArguments()
                            statement += "getArguments().";
                        } else {
                            throw new IllegalAccessException("The field [" + fieldName + "] need autowired from intent, its parent must be activity or fragment!");
                        }

                        statement = buildStatement(originalValue, statement, typeUtils.typeExchange(element), isActivity);
                        //需要通过序列化进行赋值的对象。
                        if (statement.startsWith("serializationService.")) {   // Not mortals
                            injectMethodBuilder.beginControlFlow("if (null != serializationService)");
                            injectMethodBuilder.addStatement(
                                    "substitute." + fieldName + " = " + statement,
                                    (StringUtils.isEmpty(fieldConfig.name()) ? fieldName : fieldConfig.name()),
                                    ClassName.get(element.asType())
                            );
                            injectMethodBuilder.nextControlFlow("else");
                            injectMethodBuilder.addStatement(
                                    "$T.e(\"" + Consts.TAG + "\", \"You want automatic inject the field '" + fieldName + "' in class '$T' , then you should implement 'SerializationService' to support object auto inject!\")", AndroidLog, ClassName.get(parent));
                            injectMethodBuilder.endControlFlow();
                        //普通赋值的对象。    
                        } else {
                            injectMethodBuilder.addStatement(statement, StringUtils.isEmpty(fieldConfig.name()) ? fieldName : fieldConfig.name());
                        }

                        //异常情况。
                        if (fieldConfig.required() && !element.asType().getKind().isPrimitive()) {  // Primitive wont be check.
                            injectMethodBuilder.beginControlFlow("if (null == substitute." + fieldName + ")");
                            injectMethodBuilder.addStatement(
                                    "$T.e(\"" + Consts.TAG + "\", \"The field '" + fieldName + "' is null, in class '\" + $T.class.getName() + \"!\")", AndroidLog, ClassName.get(parent));
                            injectMethodBuilder.endControlFlow();
                        }
                    }
                }

                helper.addMethod(injectMethodBuilder.build());

                //2.6 写入文件。
                JavaFile.builder(packageName, helper.build()).build().writeTo(mFiler);

                logger.info(">>> " + parent.getSimpleName() + " has been processed, " + fileName + " has been generated. <<<");
            }

            logger.info(">>> Autowired processor stop. <<<");
        }
    }

    /**
     * 根据成员变量的类型进行区分。
     * @param originalValue
     * @param statement
     * @param type
     * @param isActivity
     * @return
     */
    private String buildStatement(String originalValue, String statement, int type, boolean isActivity) {
        if (type == TypeKind.BOOLEAN.ordinal()) {
            statement += (isActivity ? ("getBooleanExtra($S, " + originalValue + ")") : ("getBoolean($S)"));
        } else if (type == TypeKind.BYTE.ordinal()) {
            statement += (isActivity ? ("getByteExtra($S, " + originalValue + "") : ("getByte($S)"));
        } else if (type == TypeKind.SHORT.ordinal()) {
            statement += (isActivity ? ("getShortExtra($S, " + originalValue + ")") : ("getShort($S)"));
        } else if (type == TypeKind.INT.ordinal()) {
            statement += (isActivity ? ("getIntExtra($S, " + originalValue + ")") : ("getInt($S)"));
        } else if (type == TypeKind.LONG.ordinal()) {
            statement += (isActivity ? ("getLongExtra($S, " + originalValue + ")") : ("getLong($S)"));
        }else if(type == TypeKind.CHAR.ordinal()){
            statement += (isActivity ? ("getCharExtra($S, " + originalValue + ")") : ("getChar($S)"));
        } else if (type == TypeKind.FLOAT.ordinal()) {
            statement += (isActivity ? ("getFloatExtra($S, " + originalValue + ")") : ("getFloat($S)"));
        } else if (type == TypeKind.DOUBLE.ordinal()) {
            statement += (isActivity ? ("getDoubleExtra($S, " + originalValue + ")") : ("getDouble($S)"));
        } else if (type == TypeKind.STRING.ordinal()) {
            statement += (isActivity ? ("getStringExtra($S)") : ("getString($S)"));
        } else if (type == TypeKind.PARCELABLE.ordinal()) {
            statement += (isActivity ? ("getParcelableExtra($S)") : ("getParcelable($S)"));
        } else if (type == TypeKind.OBJECT.ordinal()) {
            statement = "serializationService.parseObject(substitute." + (isActivity ? "getIntent()." : "getArguments().") + (isActivity ? "getStringExtra($S)" : "getString($S)") + ", new com.alibaba.android.arouter.facade.model.TypeWrapper<$T>(){}.getType())";
        }

        return statement;
    }

    /**
     * 进行分类处理。
     * @param elements
     * @throws IllegalAccessException
     */
    private void categories(Set<? extends Element> elements) throws IllegalAccessException {
        if (CollectionUtils.isNotEmpty(elements)) {
            for (Element element : elements) {
                TypeElement enclosingElement = (TypeElement) element.getEnclosingElement();
                //不允许声明为 private。
                if (element.getModifiers().contains(Modifier.PRIVATE)) {
                    throw new IllegalAccessException("The inject fields CAN NOT BE 'private'!!! please check field ["
                            + element.getSimpleName() + "] in class [" + enclosingElement.getQualifiedName() + "]");
                }

                if (parentAndChild.containsKey(enclosingElement)) {
                    parentAndChild.get(enclosingElement).add(element);
                } else {
                    List<Element> childs = new ArrayList<>();
                    childs.add(element);
                    parentAndChild.put(enclosingElement, childs);
                }
            }

            logger.info("categories finished.");
        }
    }
}
```

最终创建的文件，其命名规则为 **要注入的 Activity/Fragment** + `$$ARouter$$Autowired`，以`module-other`的`InjectActivity`为例，最终生成的文件为：



```java
/**
 * DO NOT EDIT THIS FILE!!! IT WAS GENERATED BY AROUTER. */
public class InjectActivity$$ARouter$$Autowired implements ISyringe {
  private SerializationService serializationService;

  @Override
  public void inject(Object target) {
    serializationService = ARouter.getInstance().navigation(SerializationService.class);
    InjectActivity substitute = (InjectActivity)target;
    substitute.age = substitute.getIntent().getIntExtra("inject_age", substitute.age);
    if (null != serializationService) {
      substitute.bean = serializationService.parseObject(substitute.getIntent().getStringExtra("inject_object"), new com.alibaba.android.arouter.facade.model.TypeWrapper<SerialBean>(){}.getType());
    } else {
      Log.e("ARouter::", "You want automatic inject the field 'bean' in class 'InjectActivity' , then you should implement 'SerializationService' to support object auto inject!");
    }
  }
}
```

当我们调用下面的方法时，就会通过反射创建该类的一个对象，然后调用它的`inject`方法，依次对各成员变量进行赋值。



```java
ARouter.getInstance().inject(this);
```

# 三、总结

以上就是对于`Arouter`的编译时`SDK`的分析，这篇文章理解起来可能比较难，因为要求对编译期注解的处理有一定的了解。

通过分析源码，我们学习到了如何在编译期处理注解，并了解到了`Arouter`分组加载，拦截器以及依赖注入在编译期处理的原理，了解了这些，是我们之后学习`Arouter`的运行时`SDK`的基础，也可以借此机会学习它的思想，以后我们有遇到需要在编译期做一些事情的时候，可以拿出来参考。



作者：泽毛
链接：https://www.jianshu.com/p/a1f6db686b17
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。ƒ