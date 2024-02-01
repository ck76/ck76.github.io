

- https://roadmap.sh/angular

# Templates

A template is a form of HTML that tells Angular how to render the component.

Visit the following resources to learn more:

- [Introduction to Components and Templates](https://angular.io/guide/architecture-components)

<img src="https://p.ipic.vip/xo6cq3.png" alt="image-20240202000558465" style="zoom:50%;" />

在Angular框架中，Templates提供了一种声明方式，通过它可以定义如何渲染组件的视图。以下是Angular模板中一些常用概念的详细解释：

| 概念                | 描述                                     | 示例或用法                                                   |
| ------------------- | ---------------------------------------- | ------------------------------------------------------------ |
| Interpolation       | 在HTML中插入动态数据值                   | `{{ value }}` 用于在模板中显示组件类中的`value`属性          |
| Property Binding    | 将表达式的值绑定到HTML元素属性           | `[src]="imagePath"` 将组件类中的`imagePath`属性绑定到img元素的src属性 |
| Template Statements | 相应事件发生时执行的表达式               | `(click)="handleClick()"` 在用户点击时调用`handleClick()`方法 |
| Binding             | 数据、属性、事件和样式绑定               | `{{ data }}`, `[attr]="value"`, `(event)="handler()"`, `[style.color]="expr"` |
| Reference Vars      | 在模板中创建局部变量                     | `#refVar` 用于在模板中引用元素，如`<input #inputRef>`        |
| @Input              | 使得属性可以由外部组件传递数据到当前组件 | `@Input() propertyName: Type` 用于在父组件与子组件之间传递数据 |
| @Output             | 允许组件发射事件到外部环境               | `@Output() eventEmitter: EventEmitter<Type>` 用于创建可订阅的事件，以便父组件可以监听到子组件的事件 |

每一种绑定和声明方式都有其特定的用途，允许开发者以不同方式将应用的逻辑和数据与视图连接起来。Angular的模板语法非常强大，可以通过简洁的标记来声明数据绑定、事件监听器、局部变量、以及将应用逻辑传递给HTML元素。