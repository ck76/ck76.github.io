[TOC]


## 概述

Gradle是一个基于JVM的构建工具，目前Android Studio中建立的工程都是基于gradle进行构建的，Gradle框架是使用Groovy语言实现的，关于Groovy语言的学习将不再赘述可以参考([精通Groovy](https://www.ibm.com/developerworks/cn/education/java/j-groovy/j-groovy.html)),目前很多技术领域开始使用Gradle的plugin，比如模块化、热修复、SPI的优化等等。

## 两个最重要的概念 Projetc和Tasks

这是Gradle中最重要的两个概念,每次构建至少由一个project构成，一个project由一到多个task构成。`项目结构中的每个build.gradle文件代表一个project`，在这编译脚本文件中可以定义一系列的task；task 本质上又是由一组被顺序执行的`Action`对象构成，Action其实是一段代码块，类似于Java中的方法。

## Gradle 构建生命周期

### 三个阶段

每次构建的执行本质上是执行一系列的task，并且某些task还需要依赖其他task，这些task的依赖关系都是在构建阶段确定的。每次构建分为3个阶段([Build phases 文档 ](https://docs.gradle.org/current/userguide/build_lifecycle.html#sec:build_phases))

- Initialization: 初始化阶段

这是创建Project阶段，构建工具根据每个build.gradle文件创建出一个Project实例。初始化阶段会执行项目根目录下的settings.gradle文件，来分析哪些项目参与构建。

- ## Configuration:配置阶段

这个阶段，通过执行构建脚本来为每个project创建并配置Task。配置阶段会去加载所有参与构建的项目的build.gradle文件，会将每个build.gradle文件实例化为一个Gradle的project对象。然后分析project之间的依赖关系，下载依赖文件，分析project下的task之间的依赖关系。

- Execution:执行阶段

这是Task真正被执行的阶段，Gradle会根据依赖关系决定哪些Task需要被执行，以及执行的先后顺序。task是Gradle中的最小执行单元，我们所有的构建，编译，打包，debug，test等都是执行了某一个task，一个project可以有多个task，task之间可以互相依赖。例如我有两个task，taskA和taskB，指定taskA依赖taskB，然后执行taskA，这时会先去执行taskB，taskB执行完毕后在执行taskA。在AS右侧的Gradle按钮中可以看到这一些列的task

### 监听生命周期

在gradle的构建过程中，gradle为我们提供了钩子，帮助我们针对项目的需求定制构建的逻辑，如下图所示：

![Gradle构建生命周期](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/OOS3.WrwHCnGtq6AbynP8d.7FvhxXNWPHm0SyQTM41s!/r/dDQBAAAAAAAA)

要监听这些生命周期，主要有两种方式：

- 添加监听器
- 使用钩子的配置块

关于可用的钩子可以参考Gradle和Project中的定义，常用的钩子包括：

> Gradle

- beforeProject()/afterProject()
  等同于Project中的beforeEvaluate和afterEvaluate

- settingsEvaluated()
  settings脚本被执行完毕，Settings对象配置完毕

- projectsLoaded()
  所有参与构建的项目都从settings中创建完毕

- projectsEvaluated()
  所有参与构建的项目都已经被评估完

> TaskExecutionGraph

- whenReady（）
  task图生成。所有需要被执行的task已经task之间的依赖关系都已经确立

> Project

- beforeEvaluate（）
- afterEvaluate（）

## 三个重要的gradle文件

Gradle项目有3个重要的文件需要深入理解：

- 项目根目录的 build.gradle

项目根目录的 build.gradle 文件用来配置针对所有模块的一些属性。它默认包含2个代码块：buildscript{…}和allprojects{…}。前者用于配置构建脚本所用到的代码库和依赖关系，后者用于定义所有模块需要用到的一些公共属性

```groovy
buildscript {
    repositories {
        jcenter()
    }
    dependencies {
        classpath 'com.android.tools.build:gradle:2.3.2'
    }
}

allprojects {
    repositories {
        jcenter()
    }
}

task clean(type: Delete) {
    delete rootProject.buildDir
}
```

- settings.gradle

settings.gradle 文件会在构建的 initialization 阶段被执行，它用于告诉构建系统哪些模块需要包含到构建过程中。对于单模块项目， settings.gradle 文件不是必需的。对于多模块项目，如果没有该文件，构建系统就不能知道该用到哪些模块。

- 模块目录的 build.gradle

模块级配置文件 build.gradle 针对每个moudle 的配置，如果这里的定义的选项和顶层 build.gradle定义的相同。它有3个重要的代码块：plugin，android 和 dependencies。



## 链接

- [https://blog.csdn.net/zhaoyanjun6/article/details/77678577#android-gradle-%E7%9A%84-project-%E5%92%8C-tasks](https://blog.csdn.net/zhaoyanjun6/article/details/77678577#android-gradle-的-project-和-tasks)