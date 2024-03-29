



在 React 中，自定义组件可以通过几种不同的方式创建，每种方式有其特定的用途和优点。以下是 React 中自定义组件的主要方法及示例：

### 1. 类组件 (Class Component)

类组件通过扩展 `React.Component` 类来创建，并至少实现一个 `render()` 方法，该方法返回 JSX。类组件支持状态 (`state`) 和生命周期方法。

```jsx
import React, { Component } from 'react';

class MyClassComponent extends Component {
  constructor(props) {
    super(props);
    this.state = {
      message: 'Hello, React!'
    };
  }

  render() {
    return <div>{this.state.message}</div>;
  }
}
```

### 2. 函数组件 (Function Component)

函数组件是一个接受 `props` 作为参数并返回 React 元素的纯函数。在引入 Hooks 之前，它们通常用于无状态组件。现在，通过使用 Hooks，函数组件也可以使用状态和其他 React 特性。

```jsx
import React from 'react';

function MyFunctionComponent(props) {
  return <div>Hello, {props.name}!</div>;
}
```

### 3. 箭头函数组件 (Arrow Function Component)

箭头函数组件是函数组件的一种简写形式，使用 JavaScript 的箭头函数。它们通常用于更简洁的组件声明。

```jsx
import React from 'react';

const MyArrowFunctionComponent = props => <div>Hello, {props.name}!</div>;
```

### 4. 使用 Hooks 的函数组件

使用 React 16.8 引入的 Hooks，函数组件可以使用状态 (`useState`) 和其他 React 特性，如生命周期钩子 (`useEffect`) 等。

```jsx
import React, { useState, useEffect } from 'react';

function MyHooksComponent() {
  const [count, setCount] = useState(0);

  useEffect(() => {
    document.title = `You clicked ${count} times`;
  });

  return (
    <div>
      <p>You clicked {count} times</p>
      <button onClick={() => setCount(count + 1)}>Click me</button>
    </div>
  );
}
```

### 5. 高阶组件 (Higher-Order Components - HOC)

高阶组件是一个函数，它接受一个组件并返回一个新的组件。HOC 允许你重用组件逻辑。

```jsx
import React from 'react';

function withLogging(WrappedComponent) {
  return class extends React.Component {
    componentDidMount() {
      console.log('Component did mount');
    }

    render() {
      return <WrappedComponent {...this.props} />;
    }
  };
}

// 使用 HOC
const MyComponentWithLogging = withLogging(MyFunctionComponent);
```

### 6. 渲染属性组件 (Render Prop Component)

渲染属性是一种在 React 组件之间共享代码的技术，通过一个值为函数的 prop 来传递 JSX 配置。

```jsx
import React from 'react';

class MouseTracker extends React.Component {
  render() {
    return (
      <div>
        {this.props.render({
          // Render prop 提供的数据
        })}
      </div>
    );
  }
}

// 使用方式
<MouseTracker render={({ position }) => (
  <div>Mouse position: {position}</div>
)} />
```

这些是在 React 中创建自定义组件的主要方式。随着 React 生态的发展，函数组件配合 Hooks 成为了推荐的方式，因为它们可以提供类似类组件的功能，同时保持代码的简洁和可读性。