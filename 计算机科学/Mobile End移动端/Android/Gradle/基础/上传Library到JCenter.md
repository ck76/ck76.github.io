- 个人注册地址 <https://bintray.com/signup/oss>

- 网很慢，注册了好久，账号是chengkun 邮箱是chengkun0804@gmail.com

- 获取`key`和`user`
  - 头像--edit profile
  - 创建`maven`仓库--Add New Repository
  - Name :chengkun
  - Type : Maven
  - Default Licenses (Optional) : Apache2.0

- 根目录下的`build.gradle`中加入上传开源库的依赖：

  ```java
  classpath 'com.novoda:bintray-release:0.9'
  ```

- `library`的`moudel`中加入 `apply`

  ```java
  apply plugin: 'com.novoda.bintray-release'
  注意：bintray-releas的版本 依赖，对应你项目使用 Gradle 版本
  1.bintray-releas version 0.5.0 对应 Gradle 是 version 3.4+ (包括3.4)
  2.bintray-releas version 0.4.0 对应 Gradle 是 version 3.3+ (包括3.3)
  3.bintray-releas version 0.3.4 对应 Gradle 是 version 1.3.0+(包括1.3)
  如果版本不对应 就有可能项目库上传失败 红线内没有版本号信息
  ```

- **执行gradlew命令**

  ```groovy
  ./gradlew clean build bintrayUpload -PbintrayUser=BINTRAY_USERNAME -PbintrayKey=BINTRAY_KEY -PdryRun=false
  其中BINTRAY_USERNAME换成bintray注册的用户名，BINTRAY_KEY换成自己的APIKEY
  ```

- **library的build.gradle**	

  ```groovy
  apply plugin: 'com.android.library'
  apply plugin: 'kotlin-android'
  
  
  android {
      compileSdkVersion 28
  
      defaultConfig {
          minSdkVersion 15
          targetSdkVersion 28
          versionCode 1
          versionName "1.0"
  
          testInstrumentationRunner "android.support.test.runner.AndroidJUnitRunner"
      }
  
      buildTypes {
          release {
              minifyEnabled false
              proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
          }
      }
  
      lintOptions {
          abortOnError false
      }
  }
  
  dependencies {
      implementation fileTree(dir: 'libs', include: ['*.jar'])
  
      implementation 'com.android.support:appcompat-v7:28.0.0'
      testImplementation 'junit:junit:4.12'
      androidTestImplementation 'com.android.support.test:runner:1.0.2'
      androidTestImplementation 'com.android.support.test.espresso:espresso-core:3.0.2'
      implementation "org.jetbrains.kotlin:kotlin-stdlib-jdk7:$kotlin_version"
  }
  
  
  allprojects {
      repositories {
          jcenter()
      }
      //加上这些如果module中有中文注释
      tasks.withType(Javadoc) {
          options{ encoding "UTF-8"
              charSet 'UTF-8'
              links "http://docs.oracle.com/javase/7/docs/api"
          }
      }
  }
  repositories {
      mavenCentral()
  }
  
  //主要配置区
  apply plugin: 'com.novoda.bintray-release'
  publish {
      //repoName="chengkun"//仓库名
      userOrg = 'chengkun'      //bintray注册的用户名
      groupId = 'com.chengkun'         //compile引用时的第1部分groupId
      artifactId = 'binarytest'     //compile引用时的第2部分项目名
      publishVersion = '1.0'    //compile引用时的第3部分版本号
      desc = 'This is a rxbus same of eventbus extend library '//d项目描述
      repoName="chengkun" //你的仓库名称，没有填写默认仓库是maven//这也是很多人上传仓库不对名问题最多情况，
      website = 'https://github.com/ck76/JCenterBinaryTest' //github 托管地址
      dryRun=false
  }
  ```

- **引用三段式**

  ```groovy
  compile  'com.chengkun:binarytest:1.0'
  ```

- **仓库名的重要性**

  ```java
  repoName 如果没有填写，上传的库默认仓库是maven，这个:bintray-release上传也是最多原因.
  
  很多用户的压根没有这个仓库, 然后百度，发现,Bintray需要手动创建仓库是maven。
  
  既然手动创建仓库，为什么仓库名为何自己用定义仓库名， Bintray是可以新建仓库. 
  repoName 这个属性就可以自己项目上传到自己定义仓库下。
  ```

  