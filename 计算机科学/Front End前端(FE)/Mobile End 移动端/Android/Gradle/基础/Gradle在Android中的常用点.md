[TOC]

## 设置编译类型

```groovy
android{

  buildTypes {
        release {
            minifyEnabled true  //打开混淆
            proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
        }
        debug {
            minifyEnabled false //关闭混淆
            proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
        }
    }
 }
```

通常我们需要给QA打出一个带调试功能的release包，这时就可以在这个里面加上新的type

## productFlavors 多渠道打包

AndroidManifest.xml 里设置动态渠道变量

```xml
<meta-data
android:name="UMENG_CHANNEL"
android:value="${UMENG_CHANNEL_VALUE}"/>
```

在 build.gradle 设置 productFlavors , 这里假定我们需要打包的渠道为酷安市场、360、小米、百度、豌豆荚。

```groovy
android {  

    productFlavors {
        kuan {
            manifestPlaceholders = [UMENG_CHANNEL_VALUE: "kuan"]
        }
        xiaomi {
            manifestPlaceholders = [UMENG_CHANNEL_VALUE: "xiaomi"]
        }
        qh360 {
            manifestPlaceholders = [UMENG_CHANNEL_VALUE: "qh360"]
        }
        baidu {
            manifestPlaceholders = [UMENG_CHANNEL_VALUE: "baidu"]
        }
        wandoujia {
            manifestPlaceholders = [UMENG_CHANNEL_VALUE: "wandoujia"]
        }
    } 

}
```

或者批量修改

```groovy
android {  

    productFlavors {
        kuan {}
        xiaomi {}
        qh360 {}
        baidu {}
        wandoujia {}
    }  

    productFlavors.all { 
        flavor -> flavor.manifestPlaceholders = [UMENG_CHANNEL_VALUE: name] 
    }
}
```

## 多渠道设置包名

有时候我们需要分渠道设置 applicationId 、友盟的 appkey 、友盟渠道号。

```groovy
productFlavors {
        google {
            applicationId "com.wifi.cool"
            manifestPlaceholders = [                
                    UMENG_APPKEY_VALUE : "456789456789",
                    UMENG_CHANNEL_VALUE: "google",            
            ]
        }

        baidu{
            applicationId 'com.wifi.hacker'
            manifestPlaceholders = [
                    UMENG_APPKEY_VALUE     : "123456789789",
                    UMENG_CHANNEL_VALUE    : "baidu",          
            ]
        }
    }
```

## Signing 签名

在 android 标签下添加 signingConfigs 标签，如下：

```groovy
android {
    signingConfigs {
        config {
            keyAlias 'yiba'
            keyPassword '123456'
            storeFile file('C:/work/Key.jks')
            storePassword '1234567'
        }
    }
 }
```

可以在release 和 debug包中定义签名

```groovy
android {
    signingConfigs {
        config {
            keyAlias 'yiba'
            keyPassword '123456'
            storeFile file('C:/work/Key.jks')
            storePassword '1234567'
        }
    }

    buildTypes {
        release {
            minifyEnabled false
            proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
            signingConfig signingConfigs.config
        }
        debug {
            minifyEnabled false
            proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
            signingConfig signingConfigs.config
        }
    }
}
```

## 依赖管理

### 依赖jar包

- 1、把 jar 包放在 libs 目录下
- 2、在 build.gradle 中添加依赖

```groovy
dependencies {
   compile files('libs/xxx.jar')
}
```

### 依赖aar

- 1、把 aar 包放到 libs 目录下
- 2、在 build.gradle 中添加依赖

```groovy
repositories {
    flatDir {
        dirs 'libs'
    }
}

dependencies {
    compile(name:'YibaAnalytics-release', ext:'aar')
}
```

要是aar需要被多个module依赖时候，我们可以在项目的根目录创建一个目录，比如叫 aar 目录，然后把我们的 aar 包放进去

```groovy
 flatDir {
     dirs '../aar'
}
```

### 排除依赖兼容包

有的时候，我们所依赖的项目/模块会引入多个传递性依赖。而其中部分的传递性依赖我们是不需要的，这时候可以使用exclude排除部分的传递性依赖，如下所示

```groovy
compile('com.google.firebase:firebase-ads:11.0.4', {
      exclude group: 'com.android.support'   //排除v7 , v4 包
})
```

### 强制使用指定的依赖版本

Gradle通过选择依赖关系图中找到的最新版本来解决任何依赖版本冲突。 可是有的时候，某些项目会需要使用一个较老的版本号作为依赖。这时候我们可以强制指定某一个版本。例如：

```groovy
dependencies {
    implementation 'org.apache.httpcomponents:httpclient:4.5.4'
    // 假设commons-codec的最新版本是1.10
    implementation('commons-codec:commons-codec:1.9') {
        force = true
    }
}
```

### 禁止传递性依赖

```groovy
dependencies {
    implementation('com.google.guava:guava:23.0') {
        transitive = false
    }
}
```

### 源码依赖

要是我们自己工程下的module直接依赖进来即可，有时候需要从别的路径下依赖module源码

- 1、将对应的库的源码down下来，记录下放置的路径
- 2、在setting.gradle文件中，进行库的源码引入 (这里以项目外同级目录)

```groovy
//新增依赖
include ':outmodule'
project(':outmodule').projectDir = new File(settingsDir, '../Sdk/Sdk-Android/product/outmodule')
```

Tips：

若是主工程使用了Flavor这里也要使用对应的Flavor

### 全局统一信息配置

有时候多个moudle需要依赖不同的版本，有时候就会出现一些编译异常，而且零散的版本号也是不好管理的，如：compileSdkVersion、buildToolsVersion、androidTestCompile 等。

#### 分类属性配置

- 在项目的根目录创建一个gradle配置文件config.gradle，项目中所有的依赖只要在这个文件中统一配置即可。格式如下(内容根据需要进行修改)：

```groovy
ext {
    
    compileSdkVersion: 24,
    buildToolsVersion: "24.0.2",
    applicationId    : "com.carme.carmerchant",
    minSdkVersion    : 15,
    targetSdkVersion : 22,
    versionCode      : 3,
    versionName      : "1.0.3"
    

    dependencies = [
            "test"                  : "junit:junit:4.12",
            "appcompat-v7"          : "com.android.support:appcompat-v7:25.0.0",
            "support-v4"            : "com.android.support:support-v4:25.0.0",
            "support_design"        : "com.android.support:design:25.0.0",
            "rxjava"                : "io.reactivex:rxjava:1.2.0",
            "rxandroid"             : "io.reactivex:rxandroid:1.2.1",
            "retrofit"              : "com.squareup.retrofit2:retrofit:2.1.0",
            "converter-gson"        : "com.squareup.retrofit2:converter-gson:2.1.0",
            "adapter-rxjava"        : "com.squareup.retrofit2:adapter-rxjava:2.1.0",
            "multidex"              : "com.android.support:multidex:1.0.1"
    ]
}
```

- 其次在根目录的build.gradle文件中添加内容（apply from:”config.gradle”），所有的module都可以从这个（config.gradle）配置文件里读取公共参数。
- 在各个module目录下的build.gradle文件中使用如下

```groovy
android {  
      compileSdkVersion rootProject .ext.android.compileSdkVersion
      buildToolsVersion rootProject .ext.android.buildToolsVersion
      defaultConfig {
          applicationId rootProject .ext.android.applicationId
          minSdkVersion rootProject .ext.android.minSdkVersion
          targetSdkVersion rootProject .ext.android.targetSdkVersion
          versionCode rootProject .ext.android.versionCode
          versionName rootProject .ext.android.versionName
      }
      ...
      }  
      dependencies {
          ...
          compile rootProject .ext.dependencies[ "design"]
          compile rootProject .ext.dependencies[ "appcompat-v7"]
          compile rootProject .ext.dependencies[ "recyclerview-v7"]
          ...
      }
```

## 依赖关系解析

### 使用依赖关系解析规则

依赖关系解析规则提供了一种非常强大的方法来控制依赖关系解析过程，并可用于实现依赖管理中的各种高级模式。比如：

- 统一构件组的版本

很多时候我们依赖一个公司的库会包含多个module，这些module一般都是统一构建、打包和发布的，具备相同的版本号。这个时候我们可以通过控制依赖关系的解析过程做到版本号统一。

```groovy
configurations.all {
    resolutionStrategy.eachDependency { DependencyResolveDetails details ->
    if (details.requested.group == 'org.gradle') {
        details.useVersion '1.4'
        details.because 'API breakage in higher versions'
    }
  }
}
```

- 处理自定义的版本scheme

```groovy
configurations.all {
    resolutionStrategy.eachDependency { DependencyResolveDetails details ->
        if (details.requested.version == 'default') {
            def version = findDefaultVersionInCatalog(details.requested.group, details.requested.name)
            details.useVersion version.version
            details.because version.because
        }
    }
}

def findDefaultVersionInCatalog(String group, String name) {
    //some custom logic that resolves the default version into a specific version
    [version: "1.0", because: 'tested by QA']
}
```

关于更多依赖关系解析规则的使用实例可以参考gradle的API中的 [ResolutionStrategy](https://docs.gradle.org/current/dsl/org.gradle.api.artifacts.ResolutionStrategy.html)

- 使用依赖关系的替代规则

依赖关系的替换规则和上面的依赖关系解析规则有点相似。实际上，依赖关系解析规则的许多功能可以通过依赖关系替换规则来实现。依赖关系的替换规则允许项目依赖（Project Dependency）和模块依赖（Module Dependency）被指定的替换规则透明地替换。

```groovy
// 使用项目依赖替换模块依赖
configurations.all {
    resolutionStrategy.dependencySubstitution {
        substitute module("org.utils:api") with project(":api") because "we work with the unreleased development version"
        substitute module("org.utils:util:2.5") with project(":util")
    }
}
// 使用模块依赖替换项目依赖
configurations.all {
    resolutionStrategy.dependencySubstitution {
        substitute project(":api") with module("org.utils:api:1.3") because "we use a stable version of utils"
    }
}
```

更多可以查看官方的文档[Customizing Dependency Resolution Behavior](https://docs.gradle.org/current/userguide/customizing_dependency_resolution_behavior.html#sec:dependency_substitution_rules)

### SourceSet

SourceSet 可以定义项目结构，也可以修改项目结构。Java插件默认实现了两个SourceSet，main 和 test。每个 SourceSet 都提供了一系列的属性，通过这些属性，可以定义该 SourceSet 所包含的源文件。比如，java.srcDirs，resources.srcDirs 。Java 插件中定义的其他任务，就根据 main 和 test 的这两个 SourceSet 的定义来寻找产品代码和测试代码等。

#### SourceSet 定义源码目录

在 Android 项目中，我们可以在 src/main/java 目录新建 Java 文件，现在我们在src目录下，新建一个test目录，发现不能在该目录下新建java文件，这是由于在 Gradle 中 SourceSet 默认定义的源码文件路径是src/main/java , 其他的文件下下面的源码我们自然无法访问。解决这个问题也很简单，我们需要在 SourceSet 中增加一个源码路径即可。如下所示：

```groovy
android {

    sourceSets {
        main {
            java {
                srcDir 'src/test1' //指定源码目
            }
            // 或者按照如下的方式写也可以
            //java.srcDirs( 'src/test1' , 'src/test2' ,'src/test3' )
        }
    }
}
```

#### SourceSet 定义资源目录

```groovy
android {

    sourceSets {
        main {
            java.srcDirs('src/test1/java')  //定义java 源代码
            res.srcDirs('src/test1/res')    //定义资源目录（layout , drawable,values）
        }
    }
}
```

#### SourceSet 实现 layout 分包

对于一个大项目来说，页面太多，布局文件就很多，有时在众多布局文件中找某个模块的布局文件，很是痛苦，为了解决这个问题，我们可以在创建多个 layout 目录，不同模块的布局文件放在不同的 layout 目录中，这样查找起来，就容易很多。

```groovy
android {

    sourceSets {
        main {
            res.srcDirs 'src/main/res/layouts/login'  //定义登录布局目录
            res.srcDirs 'src/main/res/layouts/register'  //定义注册布局目录
        }
    }
}
```

#### SourceSet 定义 AndroidManifest 文件

在组件化开发中, 我们需要针对 debug 与 release 模式下, 指定不同的 Manifest 文件, 代码如下：

```groovy
android {
    def appDebug = false;

    buildTypes {
        release {
            minifyEnabled false
            proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
            appDebug = false;
        }

        debug {
            minifyEnabled false
            proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
            appDebug = false;
        }
    }

    sourceSets {
        main {
            if (appDebug) {
                manifest.srcFile 'src/test1/AndroidManifest.xml'
            } else {
                manifest.srcFile 'src/main/AndroidManifest.xml'
            }
        }
    }
}
```

### buildConfigField自定义配置

实际开发中服务器可能有正式环境和测试环境，gradle可以通过buildConfigField来配置。

```groovy
sdkA {
            buildConfigField ‘String’, ‘API_HOST’, "\"http:api.test.com\""//API Host
            buildConfigField 'String',   'LANG', '"en-US"'
            buildConfigField 'String',   'MAP', '"google"'
            buildConfigField 'String',   'COORDINATE_SYSTEM', '"WGS_84"'
            buildConfigField 'int',      'COORDINATE_TYPE', '4'
            buildConfigField 'String[]', 'SUPPORTED_LANGS', '{LANG}'
            buildConfigField 'String',   'CONSUMER_HOTLINE', 
            dimension "lang"
        }
```

然后通过BuildConfig可以获取这些属性，从而做些差异化的配置等等

### 打包更改包名

项目的build.gradle

```groovy
android {
 ……
//打包命名
    applicationVariants.all {
        variant ->
            variant.outputs.each {
                output ->
                    if (variant.buildType.name == 'release') {
                        variant.mergedFlavor.versionCode = getVersionCode(false)
                        variant.mergedFlavor.versionName = getVersionName(false)
                        // release
                        def apkName = "${project.getName()}_${variant.flavorName}_${buildType.name}_v${variant.versionCode}.apk";
                        output.outputFile = new File(output.outputFile.parent, apkName);
                    } else {
                        variant.mergedFlavor.versionCode = getVersionCode(true)
                        variant.mergedFlavor.versionName = getVersionName(true)
                        // debug
                        def apkName = "${project.getName()}_${buildType.name}.apk";
                        output.outputFile = new File(output.outputFile.parent, apkName);
                    }
            }
    }

}


// 获取 version code
static def getVersionCode(boolean isDebug) {
    if (isDebug) {
        return Integer.parseInt(new Date().format("yyMMddHHmm"))
    }
    return getRevisionNumber()
}

// 获取 version name
def getVersionName(boolean isDebug) {
    String version = appConfig.appmajor +
            '.' + appConfig.appminor +
            '.' + getRevisionNumber()
    String today = new Date().format('yyMMdd')
    String time = new Date().format('HHmmss')
    if (isDebug) {
        return version + ".$today.$time." + getRevisionDescription()
    }
    return version + ".$today." + getRevisionDescription()
}
```



## 参考

- [https://blog.csdn.net/zhaoyanjun6/article/details/77678577#buildtypes-%E5%AE%9A%E4%B9%89%E4%BA%86%E7%BC%96%E8%AF%91%E7%B1%BB%E5%9E%8B](https://blog.csdn.net/zhaoyanjun6/article/details/77678577#buildtypes-定义了编译类型)
- https://juejin.im/post/5b000522f265da0b7f44d1c7