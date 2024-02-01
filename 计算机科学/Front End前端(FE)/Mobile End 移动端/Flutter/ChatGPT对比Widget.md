

- https://roadmap.sh/flutter

在Flutter中，Widgets构成了应用的基础，它们是构建UI的基本单元。以下是不同类型Widgets的对比：

| 特性/Widget类型 | Inherited Widgets                             | Stateless Widgets                  | Stateful Widgets                             | Styled Widgets                         | Material Widgets                       | Cupertino Widgets                       |
| --------------- | --------------------------------------------- | ---------------------------------- | -------------------------------------------- | -------------------------------------- | -------------------------------------- | --------------------------------------- |
| 定义            | 用于将数据从父Widget传递到子Widget            | 不保留状态的Widget                 | 拥有可以在Widget生命周期内改变的状态的Widget | 用于定义特定样式的Widget               | 遵循Google的Material Design的Widget    | 遵循Apple iOS设计的Widget               |
| 状态管理        | 通常用于管理状态和上下文                      | 不管理任何状态                     | 管理Widget状态                               | 依赖于主题或自定义样式来改变Widget外观 | 提供丰富的视觉、结构、平台和交互Widget | 提供iOS风格的控件                       |
| 数据流动        | 自上而下（从父到子）                          | 不适用                             | 自上而下和自下而上                           | 不适用                                 | 不适用                                 | 不适用                                  |
| 生命周期        | 与父Widget相关                                | 创建时定义，不会改变               | 包含可以改变的内部状态                       | 与父Widget相关                         | 与父Widget相关                         | 与父Widget相关                          |
| 常用场景        | 主题、用户登录信息、环境配置等                | 显示固定数据的UI元素，如文本、图标 | 动态应用程序，如计数器、表单等               | 自定义按钮、文本样式等                 | 创建Material风格的应用                 | 创建iOS风格的应用                       |
| 交互性          | 用于Widget树中数据的传递，而不是UI交互        | 无状态交互，如触发函数             | 可以包含复杂的UI交互                         | 可以包装其他Widget以提供样式           | 可以包含交互性控件，如按钮、菜单       | 可以包含交互性控件，如切换、按钮        |
| 性能            | 高效，因为不需要维护状态                      | 高效                               | 状态变化时需要重建                           | 取决于被应用样式的Widget               | 取决于特定Widget                       | 取决于特定Widget                        |
| 自定义性        | 可以定制数据传递的逻辑                        | 可以通过父Widget定制               | 高度可定制                                   | 高度可定制                             | 风格固定，但有一定的可定制性           | 风格固定，但有一定的可定制性            |
| 示例            | `Theme.of(context)`，`MediaQuery.of(context)` | `Text('Hello World')`              | `Checkbox(checked: _isChecked)`              | 使用`TextStyle`或`DecoratedBox`        | `MaterialApp`，`Scaffold`，`AppBar`    | `CupertinoApp`，`CupertinoPageScaffold` |

Flutter的Widgets架构提供了一个高度模块化和可重用的方式来构建用户界面。无论是为了继承数据、管理无状态或有状态的UI、应用风格，还是构建具体的设计系统，Flutter都提供了相应的Widget来满足开发者的需求。





图片上有文字“Responsive Widgets”。在Flutter开发中，“Responsive Widgets”指的是能够根据不同屏幕大小和方向自动调整大小和布局的Widget。

在构建适应不同屏幕尺寸的应用时，开发者会用到如下一些方法和Widgets来实现响应式设计：

1. **MediaQuery**: 可以获取媒体类型（如屏幕）的信息，包括尺寸、设备像素比等。它是实现响应式布局的关键。
   
2. **LayoutBuilder**: 构建一个Widget树，可以依赖于父Widget的尺寸。这对于响应式布局非常有用，因为可以根据不同的父Widget尺寸来决定子Widget的布局。

3. **Flexible & Expanded**: 这些Widget用于弹性布局，能够让子Widget根据剩余空间来占据空间。

4. **FractionallySizedBox**: 允许其子Widget尺寸为可用空间的一定比例。

5. **OrientationBuilder**: 可以根据设备的横竖屏状态构建不同的布局。

通过结合这些Widgets和工具，可以创建能够在多种设备和屏幕尺寸上工作良好的Flutter应用。响应式Widgets是指能够自适应这些不同屏幕尺寸变化的Widgets，确保用户无论在手机、平板还是桌面上都有良好的用户体验。