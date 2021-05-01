[TOC]

# 一、概述

`Lint`是`Android Studio`中提供的代码分析工具，它能够检查出代码当中存在的问题，定义该问题的严重程度，并给出相应的解决方案，这样我们就可以快速地定位和修复问题。
整个`lint`检查的架构如下图所示：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynbqno7sj30jo07y74u.jpg)



从图中来看它包含以下几个组件：

- `App Source Files`：对应于我们工程当中的源文件，包括`Java`代码、`XML`文件、`Icons`图片、`ProGuard configuration files`。

- `lint.xml`：定义了需要检查的问题，以及该问题对应的等级。

- `lint tool`：静态的代码扫描工具，我们可以从命令行或者`Android Studio`中启动，它会根据`lint.xml`中定义的规则，来检查`App Source Files`中的代码。

- ```
  lint
  ```

  检查结果：把

  ```
  lint tool
  ```

  检测出的问题分为不同的类别，方便开发者修复，目前问题分为以下几类：

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynbq5h8hj30ls0bumy3.jpg)

下面，我们就分以下几部分来介绍`lint`的使用：

- 定义`lint`检查的范围
- 如何使用`lint`
- 使用`lint`检查无用资源
- `Gradle`配置

# 二、`Lint`使用

## 2.1 `Lint`配置

选`File -> Settings`，在其中搜索`lint`，可以得到如下的界面：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynbmv7n6j30tf0j6tb7.jpg)


通过这个界面，我们可以配置在开发的过程中需要检测哪些问题，以及这些问题所对应的严重程度。
当我们配置完成之后，将它保存为一个新的`Profile`，同时可以将这个配置文件导出用于之后的项目：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynbly441j30lh0fn75z.jpg)


它会保存成为`xml`文件，假如我们相对于默认的配置有所修改，那么会在文件中添加一条记录：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynbl1fk5j30td02raag.jpg)



## 2.2 使用`Lint`

## 2.2.1 配置检查的范围

使用`Lint`的步骤如下：
第一步：点击`Analyze -> Inspect Code`，之后，会弹出下面的界面：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynbjm1bsj30m10a9gm4.jpg)


上半部分是定义需要检查的范围：



- `Whole Project`：整个工程
- `Module Browser-Browser`：当前我们所处的模块
- `File xxx`：某个文件
- `Custom Space`：自定义的范围，下拉列表中一般包括：
- `Project Files`：所有项目文件
- `Project Production Files`：项目的代码文件
- `Project Test Files`：项目的测试文件
- `OpenFiles`：当前打开的文件
- `Module xx`：某个模块
- `Current File`：当前文件

点击右边的`……`，可以定义自己的`Scope`：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynbhbjc0j30rr0jtmyq.jpg)


在这里，我们可以选择需要分析的文件。



### 2.2.2 结果分析

我们在上面选择对整个项目进行分析，之后会在下面得到分析的结果：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynbgdqe5j30xc0aadjd.jpg)

- 区域的最左边提供了一组操作，我们可以通过它来改变结果展现的形式
- 区域的中心列出了检查的问题，当我们点击某条分析结果之后，会在右边展现更为具体的信息
- 区域的右边列出了分析的具体信息
- `Name`：文件名
- `Location`：文件所处位置
- `Problem synopsis` ：问题的具体描述，通常会给出解决的方法
- `Problen resolution`：提供了一些快速修复问题的途径，只需要点一下链接，就会执行它所描述的操作。
- `Suppress`：如果想要忽略这条错误，那么需要怎么做。

## 2.3 只分析某个具体的问题

在上面的操作当中，我们是分析了所有的问题，有时候，我们只想处理某一方面的问题，那么可以通过另一个入口来分析，`Analyze -> Run inspection by name`，之后会弹出一个窗口，然我们选择需要检查的问题类型：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynbewh7yj30dt0843z3.jpg)


选择好之后，点回车，那么就会弹出和`2.2`一样的选择检查文件范围的窗口：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynbdzbm7j30mb0abaam.jpg)


假如我们选择了`Unused resources`，那么最后的结果是这样的，我们只会看到和这个问题相关的代码：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynbd61rtj30pb07v3yz.jpg)



# 三、使用`lint`删除无用资源文件

在经过一段时间的开发之后，我们项目中难免会出现无用的资源文件，而`Lint`就提供了很好的方式，平常，我们对于资源的无效引用方式主要有以下几种：

## 3.1 `drawable`在任何地方都没有用到

这种方式很好理解，`Lint`一定会为我们检查出来

## 3.2 `drawable`仅仅被`style.xml`引用，但是`style`没有被`Java`代码，或者`xml`文件引用：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynbawbt6j30hz04idfq.jpg)


此时检测的结果为`style`和`drawable`没有被引用：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynbad5prj30ju0333yi.jpg)



## 3.3 `drawable`被`style.xml`引用，`style`又被`layout`引用，但是`layout`没有被引用：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynb7ekypj30n6040mx3.jpg)


此时检测的结果为`layout`、`drawable`和`style`没有被引用

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynb6hz85j30k003pjrg.jpg)



## 3.4 前面和`3.3`相同，但是`layout`被`Java`代码所引用，而`Java`代码没有被引用：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynb54we1j30og04mdft.jpg)


这种情况，只通过`Unused resources`是检测不出来的，

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynb46g43j30jy01b0rx.jpg)


这时候，就需要用到另外一个分类，`Unused declaration`，此时的检测结果为：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynb39ss3j30ll079t91.jpg)


我们在上面点击右键，然后选择`Safe delete`就可以删除这个类了。



## 3.5 小结

通过`Unused declaration`和`Unused resources`结合，就可以删除我们大多数无用的资源，而小部分由于代码引用到，但是因为业务逻辑变了，导致不可能走到那一路的逻辑这种情况，就只能通过开发者自己处理了。

# 四、使用`gradle`构建时的配置

当我们使用`Gradle`构建时，可以通过`lintoptions`配置选项，并定义是否需要停止编译，强制让开发者处理较为严重的问题，这些选项如下表：



```dart
android {
    lintOptions {
        // true--关闭lint报告的分析进度
        quiet true
        // true--错误发生后停止gradle构建
        abortOnError false
        // true--只报告error
        ignoreWarnings true
        // true--忽略有错误的文件的全/绝对路径(默认是true)
        //absolutePaths true
        // true--检查所有问题点，包含其他默认关闭项
        checkAllWarnings true
        // true--所有warning当做error
        warningsAsErrors true
        // 关闭指定问题检查
        disable 'TypographyFractions','TypographyQuotes'
        // 打开指定问题检查
        enable 'RtlHardcoded','RtlCompat', 'RtlEnabled'
        // 仅检查指定问题
        check 'NewApi', 'InlinedApi'
        // true--error输出文件不包含源码行号
        noLines true
        // true--显示错误的所有发生位置，不截取
        showAll true
        // 回退lint设置(默认规则)
        lintConfig file("default-lint.xml")
        // true--生成txt格式报告(默认false)
        textReport true
        // 重定向输出；可以是文件或'stdout'
        textOutput 'stdout'
        // true--生成XML格式报告
        xmlReport false
        // 指定xml报告文档(默认lint-results.xml)
        xmlOutput file("lint-report.xml")
        // true--生成HTML报告(带问题解释，源码位置，等)
        htmlReport true
        // html报告可选路径(构建器默认是lint-results.html )
        htmlOutput file("lint-report.html")
        //  true--所有正式版构建执行规则生成崩溃的lint检查，如果有崩溃问题将停止构建
        checkReleaseBuilds true
        // 在发布版本编译时检查(即使不包含lint目标)，指定问题的规则生成崩溃
        fatal 'NewApi', 'InlineApi'
        // 指定问题的规则生成错误
        error 'Wakelock', 'TextViewEdits'
        // 指定问题的规则生成警告
        warning 'ResourceAsColor'
        // 忽略指定问题的规则(同关闭检查)
        ignore 'TypographyQuotes'
    }
}
```

# 五、总结

通过`lint`能在编写代码的过程中，实时地发现一些问题，这不仅有利于提高应用的质量，我们还可以通过`lint`提供的提示来了解到怎么样编写高效的代码。

# 六、参考文献

[`1.http://blog.csdn.net/lihenair/article/details/50915441`](https://link.jianshu.com/?t=http://blog.csdn.net/lihenair/article/details/50915441)
[`2.http://hubingforever.blog.163.com/blog/static/17104057920121069261691/`](https://link.jianshu.com/?t=http://hubingforever.blog.163.com/blog/static/17104057920121069261691/)
[`3.http://www.jianshu.com/p/74a50b770816`](https://www.jianshu.com/p/74a50b770816)



作者：泽毛
链接：https://www.jianshu.com/p/4ebe5d502842
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。