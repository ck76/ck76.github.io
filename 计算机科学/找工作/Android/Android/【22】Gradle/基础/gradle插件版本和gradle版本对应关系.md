[TOC]

### Android Studio gradle插件版本和gradle版本对应关系

| Plugin version | Required Gradle version |
| -------------- | ----------------------- |
| 1.0.0 - 1.1.3  | 2.2.1 - 2.3             |
| 1.2.0 - 1.3.1  | 2.2.1 - 2.9             |
| 1.5.0          | 2.2.1 - 2.13            |
| 2.0.0 - 2.1.2  | 2.10 - 2.13             |
| 2.1.3 - 2.2.3  | 2.14.1+                 |
| 2.3.0+         | 3.3+                    |
| 1.0.0 - 1.1.3  | 2.2.1 - 2.3             |



### jar libs

那么 android 项目 gradle 中依赖 jar libs 又三种方法： 

```shell
#1
 依赖项目相对路径的jar包，当然，你可以换成全路径
compile
files('libs/something_local.jar')

#或者依赖libs目录下的所有jar包
compile
fileTree(dir:
'libs',
include:
['*.jar'])

#2
 依赖maven仓库中的支持包（目前很多好的都在maven进行管理，比如 v4，v7支持包）
compile
'com.android.support:appcompat-v7:20.0.0'

#3
 依赖其他library module
compile
project(':jiechic-library')
```



### 版本号详解

```groovy
android {
    compileSdkVersion 25       //是指本地开发环境编译是时的 Android 版本
    buildToolsVersion "25.0.2"  //你构建工具的版本，其中包括了打包工具aapt、dx等等
    defaultConfig {
        applicationId "com.app" //程序的包名，唯一标识符，相同的applicationId 的应用会覆盖安装
        minSdkVersion 14       //指的是你的应用程序兼容的最低Android系统版本
        targetSdkVersion 25    // 指的是你的应用程序希望运行的Android系统版本
        versionCode 1         // 是你的代码构建编号，一般我们每打一次包就将它增加1
        versionName "1.0"  // 则是你对外发布时，用户看到的应用程序版本号，一般我们都用“点分三个数字”来命名，例如 1.0.0 。
    }
    buildTypes {
        release {
            minifyEnabled false
            proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
        }
    }
}
```

