- **View 的可见性检查**

  ```java
   Rect rect = new Rect();
   view.getGlobalVisibleRect(rect);
  ```

- [ContributesAndroidInjector](https://www.jianshu.com/p/8060a260488d)

- [dagger2](https://www.jianshu.com/p/26d9f99ea3bb)

- **Class.isAssignableFrom()**

  - Assignable(可转让)
  - 判断给定类是否是它本身或其子类

- **instanceof**

  - 给定类的实例是否是某个类的子类

- [**Gradle 多版本打包（ Build Variant）**](https://www.jianshu.com/p/59cd6c7839ea)

  ```groovy
  buildTypes {
      release {
          minifyEnabled true
          proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
      }
      debug {
          minifyEnabled false
      }
  }
  // release版本中设置了开启混淆，并且定义了混淆文件的位置
  ```

  Android plugin允许自定义这两个示例，并且可以创建其他的buildType，如下：

  ```groovy
  buildTypes {
      debug {
          minifyEnabled false
          applicationIdSuffix ".debug"
      }
      custom.initWith(buildTypes.debug)
      custom {
          applicationIdSuffix ".custom"
          versionNameSuffix "-customs"
      }
  }
  ```

  上述配置进行了一下设置：

  - 对默认的debug构建类型进行了修改，关闭了混淆配置，添加applicationId后缀
  - 以debug为基础创建一个叫custom的构建类型（相当于继承了debug版本），在custom的构建类型中修改applicationId后缀，并添加了versionName的后缀。
  
- **Source sets**

  - gradle通过一个叫SourceSets的概念来寻找和关联要编译的源码文件，每当创建一个新的**build type** 的时候，gradle 默认都会创建一个新的**source set**。我们可以建立与**main文件夹同级的**文件夹，根据编译类型的不同我们可以选择对某些源码和资源文件直接进行替换。

  ```groovy
  sourceSets {
          main {
              jniLibs.srcDir 'libs'
              assets.srcDirs 'assets'
          }
  
          i18n {
              jniLibs.srcDir 'src/i18n/libs'
              assets.srcDirs 'src/i18n/assets'
              defaultConfig {
                  minSdkVersion 15
              }
          }
  
          cn {
              jniLibs.srcDir 'src/cn/libs'
              assets.srcDirs 'src/cn/assets'
              defaultConfig {
                  minSdkVersion 14
              }
          }
      }
  ```

  - 对于每一个BuildType，Android plugin都会创建一个对应的sourceSet，默认位置为：src/BuildTypeName
    所以新建BuildType的名字不能是main、androidTest和test这三个已经被用的名字

  - **BuildType的代码/资源会以以下方式进行合并**

    - manifest会被合并到app的manifest文件中
    - res目录下的资源文件会替换main里的资源文件
    - java目录下的文件会被添加到main里的java目录中，所以**不能和main里的类重名**（含包名）

    除此之外，不同编译类型的项目，我们的依赖都可以不同，比如，如果我需要在staging和debug两个版本中使用不同的log框架，我们这样配置：

    ```groovy
    ependencies {
        api LOTTIE_LIB
        cnApi project(':emoji')
        i18nApi project(':emoji-no-op')
    }
    ```

- **Product flavors**

  - Product flavors 默认不引用，我们可以手动添加，同Build type一样，每一个Product flavors 版本也都有属于自己的Source sets，也可以差异化设置构建属性。
  - 当同时设置了BuildType和Product flavors后，二者会组合构建，最多可以生成m×n个版本的apk包。对于二者SourceSets，重复的res文件覆盖原则buildType>Product flavors
  - 在一些情况下，一个应用可能需要基于多个标准来创建多个版本。
     例如，有个 app 需要一个免费版本和一个付费的版本，并且需要在不同的 app 发布平台发布。这个 app 需要 2 个付费版和 2 个特定发布平台，因此就需要生成 4 个APK（不算 Build Types 生成的 Variant 版本）。
  - 然而，这款 app 中，为 2 个发布平台构建的付费版本源代码都是相同，因此创建 4 个 flavor 来实现不是一个好办法。 如果使用两个 flavor 维度，两两组合，构建所有可能的 Variant 组合才是最好的。
  - 这个功能的实现就是使用 Flavor Dimensions 。每一个 Dimensions 代表一个维度，并且 flavor 都被分配到一个指定的 Dimensions 中。

  ```groovy
  android {
  ...
  flavorDimensions 'price', 'store'
  
  productFlavors {
      google {
          dimension 'store'
      }
  
      amazon {
          dimension 'store'
      }
  
      free {
          dimension 'price'
      }
  
      paid {
          dimension 'price'
      }
  }
  }
  /* 注：
  dimension参数在gradle2.0之后替换掉了flavorDimension参数，所以很多文章里依然使用的类似于如下写法：
  google {
          flavorDimension 'store'
      }
  flavorDimension参数现已失效
  */
  ```

  - andorid.flavorDimensions 数组按照先后排序定义了可能使用的 Dimensions 。每一个 Product Flavor 都被分配到一个 Dimensions 中。
- 上面的例子中将 Product Flavor 分为两组（即两个维度），分别为 price 维度 [free, paid] 和 store 维度 [google, amazon] ，再加上默认的 Build Type 有 [debug, release] ，这将会组合生成以下的 Build Variant：
  
  ```java
  free-google-debug
  free-google-release
  free-amazon-debug
  free-amazon-release
  paid-google-debug
  paid-google-release
  paid-amazon-debug
  paid-amazon-release
  ```
  
  - 每一个 Variant 版本的配置由几个 Product Flavor 对象决定：
  
    - 一个来自 price 组中的对象
  
    - 一个来自 store 组中的对象
  
  - android.flavorDimensions 中定义的 Dimensions 排序非常重要（Variant 命名和优先级等）。
  
  - flavorDimensions 中的排序决定了哪一个 flavor 覆盖哪一个，这对于资源来说非常重要，因为一个 flavor 中的值会替换定义在低优先级的 flavor 中的值。
  
  - flavorDimensions 使用最高的优先级定义，因此在上面例子中的优先级为：
  
  > price > store > defaultConfig
  
  
