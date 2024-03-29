



在 React 中，组件间的数据传输是构建动态和交互式用户界面的基础。以下是几种常见的组件间传输数据的方式：

### 1. Props

父组件向子组件传递数据的基本方式是通过 props。Props 是组件对外的配置接口，父组件通过它将数据传递给子组件。

```jsx
// 父组件
function ParentComponent() {
  return <ChildComponent message="Hello from Parent" />;
}

// 子组件
function ChildComponent(props) {
  return <div>{props.message}</div>;
}
```

### 2. Callback Functions

父组件可以通过 props 传递回调函数给子组件，子组件通过调用这个函数来将数据传回父组件。

```jsx
// 父组件
class ParentComponent extends React.Component {
  handleChildData = (childData) => {
    console.log(childData);
  };

  render() {
    return <ChildComponent onChildAction={this.handleChildData} />;
  }
}

// 子组件
function ChildComponent(props) {
  const data = "Data from Child";
  return <button onClick={() => props.onChildAction(data)}>Send Data to Parent</button>;
}
```

### 3. Context API

当你需要在组件树中的多个层级间传递数据时，Context API 是一个有效的解决方案。它允许你共享特定的数据和函数给所有的组件树，而不必显式地通过每个层级手动传递 props。

```jsx
const MyContext = React.createContext();

// 根组件
function App() {
  return (
    <MyContext.Provider value="Shared Data">
      <ComponentA />
    </MyContext.Provider>
  );
}

// 子组件 A
function ComponentA() {
  return <ComponentB />;
}

// 子组件 B，使用 Context 中的数据
function ComponentB() {
  return (
    <MyContext.Consumer>
      {(value) => <div>{value}</div>}
    </MyContext.Consumer>
  );
}
```

### 4. Lift State Up

当需要在同级的兄弟组件间共享状态时，你可以将状态提升到它们共同的父组件中，然后通过 props 向下传递。

```jsx
// 父组件
class ParentComponent extends React.Component {
  state = {
    sharedData: "Shared Data"
  };

  render() {
    return (
      <div>
        <ChildComponentA data={this.state.sharedData} />
        <ChildComponentB data={this.state.sharedData} />
      </div>
    );
  }
}

// 子组件 A 和 B
function ChildComponentA(props) {
  return <div>{props.data}</div>;
}

function ChildComponentB(props) {
  return <div>{props.data}</div>;
}
```

### 5. 使用高阶组件 (HOC) 和渲染属性模式 (Render Props)

这两种模式都是为了复用组件逻辑。它们可以用来共享数据或逻辑给被包裹的组件。

```jsx
// 使用 Render Props
class DataProvider extends React.Component {
  state = {
    data: "Some data"
  };

  render() {
    return this.props.render(this.state.data);
  }
}

// 在其他组件中使用
function MyComponent() {
  return (
    <DataProvider render={(data) => (
      <div>{data}</div>
    )}/>
  );
}
```

通过以上方法，React 组件可以灵活地进行数据交换和状态管理，满足各种复杂应用的需求。