- **设置 JDK 版本**

  默认情况下，用于编译项目的 Java 语言版本基于项目的 [`compileSdkVersion`](http://google.github.io/android-gradle-dsl/current/com.android.build.gradle.BaseExtension.html#com.android.build.gradle.BaseExtension:compileSdkVersion) （因为不同 Android 版本支持不同版本的 Java）。如有必要，您可以通过将以下 [`CompileOptions {}`](http://google.github.io/android-gradle-dsl/current/com.android.build.gradle.internal.CompileOptions.html) 代码块添加到 `build.gradle` 文件来替换此默认 Java 版本：

  ```groovy
  android {
      compileOptions {
          sourceCompatibility JavaVersion.VERSION_1_6
          targetCompatibility JavaVersion.VERSION_1_6
      }
  }
  ```

- **常用快捷键**
  - Command + N 快速生成搜构造函数等
  - Control + O 覆盖方法（重写父类方法）
  - Control + I 实现方法（实现接口中的方法）
  - Option + 方向键上 连续选中代码块
  - Option + 方向键左 / Option + 方向键右 光标跳转到当前单词 / 中文句的左 / 右侧开头位置
  - Option + Enter 显示意向动作和快速修复代码
  - Command + Option + L 格式化代码
  - Control + Option + O 优化import
  - Control + Option + I 自动缩进线
  - Command + X 剪切当前行或选定的块到剪贴板
  - Command + D 复制
  - Command + 加号 / Command + 减号 展开 / 折叠代码块
  - Command + R 文件内替换
  - Command + Shift + R 全局替换（根据路径）
  - Command + O 查找类文件
  - Command + Option + O 前往指定的变量 / 方法
  - Control + Shift +F 全局查找  不光是变量可以查字符串
  - Command + E 显示最近打开的文件记录列表
  - Command + B / Command + 鼠标点击 进入光标所在的方法/变量的接口或是定义处
  - Command + Option + B 跳转到实现处，在某个调用的方法名上使用会跳到具体的实现处，可以跳过接口
  - Command + F12 弹出当前文件结构层，可以在弹出的层上直接输入进行筛选（可用于搜索类中的方法）
  - Control + H 显示当前类的层次结构
  - Command + Shift + H 显示方法层次结构
  - Control + Option + H 显示调用层次结构
  - Control + Options +0 删除无用包

- **使用 SDK Manager 更新您的工具**

  - **Android SDK Build-Tools**

  - **必备。**包含构建 Android 应用的工具。请参阅 [SDK Build Tools 版本说明](https://developer.android.google.cn/studio/releases/build-tools.html)。

    **Android SDK Platform-Tools**

  - **必备。**包含 Android 平台所需的各种工具，包括 [adb](https://developer.android.google.cn/studio/command-line/adb.html) 工具。

    **Android SDK Tools**

  - **必备。**包括 ProGuard 等基本工具。请参阅 [SDK Tools 版本说明](https://developer.android.google.cn/studio/releases/sdk-tools.html)。

    **Android Emulator**