[TOC]

Android Gradle Plugin 插件版本号

```
Android Studio安装目录\gradle\m2repository\com\android\tools\build\gradle
```

> Gradle是什么？Gradle是Android Studio项目的构建系统，是以 Groovy 语言为基础，面向Java应用为主，基于DSL（领域特定语言）语法的自动化构建工具，所以这就出现了Android Studio的项目结构和Eclipse的不一样了，我们具体来看一下,我们在项目的Gradle Scripts文件下可以看到

图1.4.1

> 这些就是我们项目构建之后的一些Gradle配置清单，我们来具体的了解一下这些文件夹是干什么的

- build.gradle 构建文件

- build.gradle 模块构建文件
- gradle-wrapper.properties Gradle相关配置文件
- proguard-rules.pro 混淆文件
- gradle.propertles 配置文件
- setting.gradle 设置配置文件
- local.propertles sdk，ndk配置文件

> 而我们所要了解的也不是说全部，只要了解一下构建文件和模块构建文件就可以，我们会看一下构建文件里面都有些什么

```groovy
    // Top-level build file where you can add configuration options common to all sub-projects/modules.

    buildscript {
        repositories {
            jcenter()
        }
        dependencies {
            classpath 'com.android.tools.build:gradle:2.1.0'

            // NOTE: Do not place your application dependencies here; they belong
            // in the individual module build.gradle files
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

> 这里面，就是一个标准的Gradle构建系统的代码，我们来看

- jcenter() 存储库 可以更换
- classpath 依赖的Gradle版本
- repositories 所有项目的存储库

> 我们再来看一下模块构建系统

```groovy
    apply plugin: 'com.android.application'

    android {
        compileSdkVersion 23
        buildToolsVersion "23.0.3"

        defaultConfig {
            applicationId "com.lgl.helloandroid"
            minSdkVersion 15
            targetSdkVersion 23
            versionCode 1
            versionName "1.0"
        }
        buildTypes {
            release {
                minifyEnabled false
                proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
            }
        }
    }

    dependencies {
        compile fileTree(dir: 'libs', include: ['*.jar'])
        testCompile 'junit:junit:4.12'
        compile 'com.android.support:appcompat-v7:23.3.0'
    }
```

> 这里，我们就要多关注一下了

- apply plugin ：声明一个工程的类型
- compileSdkVersion ： 最高API
- buildToolsVersion : 编译工具版本
- applicationId ： 包名
- minSdkVersion ： 最小API
- targetSdkVersion ： 编译版本
- versionCode ： 版本号
- versionName： 版本号（细分）
- buildTypes： 构建类型
- minifyEnabled ： 是否压缩apk
- proguardFiles getDefaultProguardFile： 混淆配置文件
- dependencies ： 依赖配置
- compile fileTree：依赖二进制文件
- compile ： 远程二进制依赖
