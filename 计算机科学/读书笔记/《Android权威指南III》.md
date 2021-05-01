[TOC]

- AAPT - Android Asset Packaging Tool ：Android资源打包工具
- 现代Android编译系统使用Gradle编译工具，要从命令行使用Gradle，请切换到项目目录并执行以下命令:
  `$ ./gradlew tasks`，执行以上命令会显示一系列可用任务。你需要的任务是installDebug，因此，再执行以下命令:`$ ./gradlew installDebug`，以上命令将把应用安装到当前连接的设备上，但不会运行它。要运行应用，需要在设备上手 动启动

- 自定义Toast

  ```java
  Toast toast = new Toast(getApplicationContext());
  toast.setGravity(Gravity.CENTER, 12, 20);//setGravity用来设置Toast显示的位置，相当于xml中的android:gravity或android:layout_gravity
  toast.setDuration(Toast.LENGTH_LONG);//setDuration方法：设置持续时间，以毫秒为单位。该方法是设置补间动画时间长度的主要方法
  toast.setView(view); //添加视图文件
  toast.show();
  ```

- Android应用基于模型视图控制器 (Model-View-Controller，MVC)的架构模式进行设计。MVC设计模式表明，应用的任何对象， 归根结底都属于模型对象、视图对象以及控制器对象中的一种

- 旋转设备会改变设备配置(device configuration)。设备配置实际是一系列特征组合，用来描
  述设备当前状态。这些特征有:屏幕方向、屏幕像素密度、屏幕尺寸、键盘类型、底座模式以及
  语言等。

- Analyze inSpect Code 进行Lint检查

- minSdkVersion、 targetSdkVersion、compileSdkVersion

  - minSdkVersion限制安装application所需要的系统最低版本，低于该版本的系统都不可以安装该application。同时不能使用该level版本SDK所不具备的API，例如在minSdkVersion为8的application中调用【this.getActionBar()】就会出现Call requires API level 11 (current min is 8): android.app.Activity#getActionBar的错误。这是因为ActionBar是在Android 11才出现的新功能（new API）。
  - targetSdkVersion是Android提供向前兼容的主要依据，表明该application已经兼容从minSdkVersion至tartgetSdkVersion之间所有api的变化。在targetSdkVersion更新之前系统不会应用最新的行为变化。
  - compileSdkVersion告诉gradle使用哪个版本Android SDK编译你的应用，使用任何新添加的API就要使用对应level的Android SDK.
  - 他们三者的关系：minSdkVersion <= targetSdkVersion <= compileSdkVersion
  - 理想情况下应该是：minSdkVersion (lowest possible) <=targetSdkVersion == compileSdkVersion (latest SDK)

- Build.VERSION.SDK_INT常量代表了Android设备的版本号

- 开发部分又细分为七大块内容。 

  -   Android培训:初级和中级开发者的培训模块，包括可下载的示例代码。 

  -   API使用指南:基于主题的应用组件、特色功能详述以及它们的最佳实践。 

  -   参考文档:SDK中类、方法、接口、属性常量等可搜索、交叉链接的参考文档。 

  -   示例代码:如何使用各种API的示例代码。 

  -   Android Studio:与Android Studio IDE相关的内容。 

  -   Android NDK:有关Android原生开发工具的介绍和参考链接，该工具允许开发人员使用C 

    或C++开发应用。 

  -   Google服务:Google专属API的相关信息，包括Google地图和Google云消息。 

    Android文档可脱机查看。浏览SDK安装文件所在目录，找到docs目录。该目录包含了全部的 Android开发者文档内容。 

- RecyclerView就是这么做的。它只创建刚好充满屏幕的12个View，而不是100个。用户滑动
  屏幕切换视图时，上一个视图会回收利用。顾名思义，RecyclerView所做的就是回收再利用，
  循环往复。

- ViewHolder -> ItemView -> View

- RecyclerView自身不会创建视图，它创建的是ViewHolder，而ViewHolder引用着itemView

- RecyclerView就能在屏幕上显示crime列表项了。需要注意的是，相对于onBindViewHolder(ViewHolder, int)方法，onCreateViewHolder(ViewGroup, int)方法的调用并不频繁。一旦有了够用的ViewHolder，RecyclerView就会停止调用onCreate-ViewHolder(...)方法。随后，它会回收利用旧的ViewHolder以节约时间和内存