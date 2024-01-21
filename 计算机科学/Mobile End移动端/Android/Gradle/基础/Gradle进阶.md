[TOC]

## 简介

## Groovy 基础

- [官网](https://gradle.org/)
- [Groovy脚本基础全攻略](https://blog.csdn.net/yanbober/article/details/49047515)

## Gradle DSL 基础

[DSL](https://docs.gradle.org/current/dsl/index.html)

## Android DSL 基础

[ASL](http://google.github.io/android-gradle-dsl/current/index.html)

## 自定义插件开发

### 三种方式

| 类型          | 说明                                                         |
| :------------ | :----------------------------------------------------------- |
| Build script  | 把插件写在 build.gradle 文件中，一般用于简单的逻辑，只在该 build.gradle 文件中可见 |
| buildSrc 项目 | 将插件源代码放在 rootProjectDir/buildSrc/src/main/groovy 中，只对该项目中可见，适用于逻辑较为复杂 |
| 独立项目      | 一个独立的 Groovy 和 Java 项目，可以把这个项目打包成 Jar 文件包，一个 Jar 文件包还可以包含多个插件入口，将文件包发布到托管平台上，供其他人使用。本文将着重介绍此类。 |



## 核心概念

> [NamedDomainObjectContainer](https://docs.gradle.org/current/javadoc/org/gradle/api/DomainObjectCollection.html)

使用NamedDomainObjectContainer 简称NODC 这是一个容纳object的容器,DomainObjectCollection 可以直接访问所有对象，它的特点是它的内部使用SortedSet实现的，内部对象的name是unique的，而且是按name进行排序的。通常创建NDOC的方法就是调用,调用

```groovy
<T> [NamedDomainObjectContainer] <T> container(Class<T> type)
```



这里需要注意的是`type`有一个要求：必须有一个public的构造函数，接受string作为一个参数

比如在buildScript中使用一个对象就需要使用NamedDomainObjectContainer

```groovy
NamedDomainObjectContainer<Book> bookContainer = mProject.container(Book.class,
               new BookFactory(mInstantiator));
```

具体可以参看[构造对象](https://github.com/xsfelvis/GradlePluginStudy/blob/1c39de3500e02db7d2b9a7facd0c88d7f2e0dc8f/complexscriptdsl/src/main/java/com/dev/complexdsl/ComplexDSLPlugin.java)

> 获取 debug还是release

- variant.name.capitalize()这个就是获取的字符串是debug 还是release
- variant.buildType.name.capitalize() 获取buildType

```groovy
def preDexTask = project.tasks.findByName("preDex${variant.name.capitalize()}")
def dexTask = project.tasks.findByName("dex${variant.name.capitalize()}")
def proguardTask = project.tasks.findByName("proguard${variant.name.capitalize()}")
```

这是nuwa热修复的源码他事先定义好了这些任务，这些任务就是把字节码文件打包成dex文件的任务，上面的代码意思就是获取这些任务的名字。（就是apply plugin: ‘com.android.application’里面的任务）。从上面的代码可以看到，我们定义的任务名称分别是（preDex${variant.name.capitalize()}）（dex${variant.name.capitalize()}）（proguard${variant.name.capitalize()}）（$这个符号就是拼接字符串的意思和kotlin一样

## project.beforeEvaluate/project.afterEvaluate

这里在普及一个小知识，项目中gradle执行的时候，会先解析setting.gradle,然后是build.gradle,如果想在解析build.gradle之前做点事，可以使用project.beforeEvaluate如果想在解析build.gradle之后做点事可以project.afterEvaluate。

## 常用操作

### 加载task

```groovy
if (!imgDirectories.empty) {
                project.task(type: ImgOptimizerTask, overwrite: true, Constants.TASK_NAME.
                        concat(project.name.capitalize()).concat(variant.buildType.name.capitalize())) {
                    it.group = "optimize"
                    it.description = "Optimize ${variant.buildType.name} images"
                    it.imgDirs = imgDirectories
                    it.triggerSize = ext.triggerSize
                    it.suffix = ext.suffix
                    it.type = ext.type
                }
            }
```

关于Task的创建参数 参考 [Project](https://docs.gradle.org/current/dsl/org.gradle.api.Project.html#N14C11)

### 读取列表 列表长度不定

比如获取一个person的列表

```groovy
class HelloPlugin implements Plugin<Project> {
    @Override
    void apply(Project project) {
        //创建一个容器 
        NamedDomainObjectContainer<Person> persons = project.container(Person)
        //将容器添加为extension 
        project.extensions.add('team', persons)
        def task = project.task('showTeam') {
            group 'junli' doLast { def team1 = project.extensions.getByName('team') println team1 }
        }
    }
}
```

### 添加子插件

```groovy
// apply the maven publish plugin and dynamic dependency resolve plugin to all the sub projects
project.subprojects {
    it.plugins.apply(MavenPublishPlugin)
    it.plugins.apply(DependencyResolvePlugin)
}
```

### hook资源

```groovy
task hookAssets {
  afterEvaluate {
    tasks.findByName("packageDebug").doFirst { task ->
      copy {
        from "${projectDir.absolutePath}/test.png"
        into "${task.assets.asPath}"
      }
    }
  }
}
```

1. 在 project afterEvaluate 之后找到 packageDebug task
2. 不妨在 app 目录下放入一个 test.png，使用 copy {} 闭包，from 填入的参数为 test.png 的路径，into 填入的参数为输出的路径，也就是 assets 的路径。

### install && launch apk

```groovy
task installAndRun(dependsOn: 'assembleDebug') {
  doFirst {
    exec {
      workingDir "${buildDir}/outputs/apk/debug"
      commandLine 'adb', 'install', '-r', 'app-debug.apk'
    }
    exec {
      def path = "${buildDir}/intermediates/manifests/full/debug/AndroidManifest.xml"
      // xml 解析
      def parser = new XmlParser(false, false).parse(new File(path))
      // application 下的每一个 activity 结点
      parser.application.activity.each { activity ->
        // activity 下的每一个 intent-filter 结点
        activity.'intent-filter'.each { filter ->
          // intent-filter 下的 action 结点中的 @android:name 包含 android.intent.action.MAIN
          if (filter.action.@"android:name".contains("android.intent.action.MAIN")) {
            def targetActivity = activity.@"android:name"
            commandLine 'adb', 'shell', 'am', 'start', '-n',
                "${android.defaultConfig.applicationId}/${targetActivity}"
          }
        }
      }
    }
  }
}
```

### apk变体

根据官方文档可以知道开发者可以通过 android.applicationVariants.all 获取到当前所有的 apk 变体，该变体的类型为 ApplicationVariant，其父类 BaseVariantOutput 中含 name 字段，该字段实际上就是当前变体的名字，那么其实只需要判断该 name 字段是否包含 release 关键字即可。

1. 代码解析

```
功能
```

在 app 目录下创建 pic 文件夹，并添加一个名为 test 的 png 图片，hook apk 打包流程将该图片添加入 apk 的 assets 文件夹。

```groovy
class HookAssetsPlugin implements Plugin<Project> {
  @Override
  void apply(Project project) {
    project.afterEvaluate {
      project.plugins.withId('com.android.application') {
        project.android.applicationVariants.all { ApplicationVariant variant ->
          variant.outputs.each { ApkVariantOutput variantOutput ->
            if (variantOutput.name.equalsIgnoreCase("release")) {
              variantOutput.packageApplication.doFirst { PackageApplication task ->
                project.copy {
                  from "${project.projectDir.absolutePath}/pic/test.png"
                  into "${task.assets.asPath}"
                }
              }
            }
          }
        }
      }
    }
  }
}
```

> 解析点

1. 在 project.afterEvaluate 闭包中才能获取到当前 project 中的所有 task 。
2. 通过 project.plugins.withId(‘com.android.application’) 确保当前 project 是 Android app project 而不是 Android library project，以此来避免无效操作
3. 通过 project.android.applicationVariants.all 获取所有变体信息。
4. 在日常开发中寻找 task 的方式可能更多的是使用 project.tasks.findByName(name)/project.tasks.getByName(name)

### 获取依赖配置

[EasyDenpendency](https://github.com/easilycoder/EasyDependency)

```groovy
// add the extension config
        NamedDomainObjectContainer<DependencyResolveExt> dependencyResolveContainer = targetProject.container(DependencyResolveExt.class)
        targetProject.extensions.add("dynamicDependency", dependencyResolveContainer)

        targetProject.afterEvaluate {
            Map<Project, DependencyResolveExt> resolveExtMap = new HashMap<>()
            targetProject.configurations.all { Configuration configuration ->
                if (configuration.dependencies.size() == 0) {
                    return
                }
                configuration.dependencies.all { dependency ->
                    if (dependency instanceof DefaultProjectDependency) {
                        def projectName = dependency.dependencyProject.name
                        def dependencyResolveExt = dependencyResolveContainer.find {
                            it.name == projectName
                        }
                        if (dependencyResolveExt != null && !dependencyResolveExt.debuggable) {
                            resolveExtMap.put(dependency.dependencyProject, dependencyResolveExt)
                        }
                    }
                }
            }
            targetProject.configurations.all {
                resolutionStrategy {
                    dependencySubstitution {
                        resolveExtMap.each { key, value ->
                            substitute project("${key.path}") with module("${value.groupId}:${getArtifactName(key, value.artifactId)}:${value.version}")
                        }
                    }
                }
            }
```

 