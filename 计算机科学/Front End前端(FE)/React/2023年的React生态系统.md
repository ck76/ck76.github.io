[TOC]

**文章翻译来自 [The React Ecosystem in 2023](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023)**

随着React在2023年庆祝成立10周年，生态系统继续蓬勃发展，不断进步和创新。作为使用最广泛的JavaScript库之一，React仍然是开发人员构建动态高性能应用程序的最爱。

然而，React生态系统中有大量可用的工具和库，您可能难以抉择如何选择合适的工具库。在本文中，我们将探索开发人员广泛使用和信任的库，并帮助您为自己的React项目使用的正确工具做出明智的决定。

## [React入门](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-getting-started-with-react-b)

对于那些刚接触React的人来说，入门可能是一项艰巨的任务。有几种不同的入门方式可能会令人困惑。

### [CodeSandbox and Stackblitz](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23code-sandbox-and-stackblitz)

如果你是React新手，或者你只是想在不设置项目的情况下玩它，你可以使用在线沙箱，如[CodeSandbox](https://link.juejin.cn?target=https%3A%2F%2Fcodesandbox.io%2F)或[StackBlitz](https://link.juejin.cn?target=https%3A%2F%2Fstackblitz.com%2F)。这些沙箱提供了一个虚拟环境，在那里你可以编写和测试你的React代码，而不必在你的计算机上安装任何东西。

### [Vite](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23vite)

撇开在线sandboxes不谈，一个流行的选择是[Vite](https://link.juejin.cn?target=https%3A%2F%2Fvitejs.dev%2F)，它是一种构建工具，为现代Web项目提供快速简单的开发体验。Vite支持开箱即用的React，这意味着您可以快速设置React项目，而无需手动配置构建过程。运行以下命令并按照提示操作！

```js
js
复制代码npm create vite@latest
```

### [Next.js](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23next-js)

开始使用React的另一个流行选择是[Next. js](https://link.juejin.cn?target=https%3A%2F%2Fnextjs.org%2F)，它是一个基于React构建的框架。Next.js提供了一组强大的功能，包括自动代码拆分、服务端渲染、静态站点生成等等。Next.js非常适合构建需要服务端渲染和SEO优化的复杂应用程序。开始使用Next.js的最简单方法是使用`create-next-app`。它是一个CLI工具，使您能够快速开始构建新的Next.js应用程序，并为您设置好一切。

```js
js
复制代码npx create-next-app@latest
```

### [**精选推荐**](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-top-picks-b)

- **在线沙箱**（如[CodeSandbox](https://link.juejin.cn?target=https%3A%2F%2Fcodesandbox.io%2F)或[StackBlitz](https://link.juejin.cn?target=https%3A%2F%2Fstackblitz.com%2F)）, 用于项目试水，
- **Vite** 适用于中小型项目
- **Next. js**用于需要SSR和SEO优化的项目

## [路由](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-routing-b)

路由是任何现代Web应用程序的重要组成部分，有许多优秀的路由库可用于处理复杂的路由逻辑和创建动态的单页应用程序（SPA）。

### [React Router](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23react-router)

React最受欢迎的路由库之一是[React Router](https://link.juejin.cn?target=https%3A%2F%2Freactrouter.com%2Fen%2Fmain)，提供了一种直接且声明式的方式来处理React应用程序中的路由，因此您可以定义路由并根据当前URL呈现不同的组件。

下面是设置根路由和`/about`路由的示例代码片段，每个路由呈现不同的内容：

```js
js
复制代码const router = createBrowserRouter([
  {
    path: "/",
    element: (
      <div>
        <h1>Hello World</h1>
        <Link to="about">About Us</Link>
      </div>
    ),
  },
  {
    path: "about",
    element: <div>About</div>,
  },
]);
```

### [TanStack Router](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23tan-stack-router)

开发社区新宠儿[TanStack Router](https://link.juejin.cn?target=https%3A%2F%2Ftanstack.com%2Frouter%2Fv1)。功能丰富且轻量级，但与React路由器相比使用相对较少。如果你遇到一个仅在TanStack路由器中存在的特定功能，你可能想试一试。这里有一个功能对比表[comparison table](https://link.juejin.cn?target=https%3A%2F%2Ftanstack.com%2Frouter%2Fv1%2Fdocs%2Fcomparison)可以帮助你做出决定。

### [Next.js](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23next-js)

如果您使用Next. js，则不需要选择路由库，因为[Next.js内置了路由](https://link.juejin.cn?target=https%3A%2F%2Fnextjs.org%2Fdocs%2Fapp%2Fbuilding-your-application%2Frouting)。

### [**精选推荐**](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-top-picks-b)

- **React Router**用于构建SPAs，无需框架路由器
- **Next. js** 适用于完整框架路由器，比如包含SSR/SSG

## [客户端状态管理](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-client-state-management-b)

随着应用程序的增长，更有意识地了解状态的组织方式以及组件之间的数据流动方式会有所帮助。状态管理库可以更轻松地管理复杂的应用程序状态，并使UI与数据保持同步。

### [Redux Toolkit](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23redux-toolkit)

React的一个流行的状态管理库是[Redux Toolkit](https://link.juejin.cn?target=https%3A%2F%2Fredux-toolkit.js.org%2F)。Redux Toolkit是一组用于有效管理状态的工具和最佳实践。它提供了用于定义和更新状态的简化API，以及对不可变更新、可序列化操作类型等功能的内置支持。

Redux Toolkit还包括许多附加功能，例如用于处理异步逻辑的内置Thunk中间件，以及用于调试Redux状态的DevTools扩展。

以下是redux切片的代码片段，它是应用程序中单个功能的redux逻辑和操作的集合：

```js
js
复制代码import { createSlice } from '@reduxjs/toolkit'

const initialState = {
  value: 0,
}

export const counterSlice = createSlice({
  name: 'counter',
  initialState,
  reducers: {
    increment: (state) => {
      state.value += 1
    },
    decrement: (state) => {
      state.value -= 1
    },
    incrementByAmount: (state, action) => {
      state.value += action.payload
    },
  },
})

// Action creators are generated for each case reducer function
export const { increment, decrement, incrementByAmount } = counterSlice.actions

export default counterSlice.reducer
```

### [Zustand](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23zustand)

[Zustand](https://link.juejin.cn?target=https%3A%2F%2Fzustand-demo.pmnd.rs%2F)是React的另一个状态管理库，它为管理应用程序中的状态提供了一个清晰轻量级的解决方案。Zustand提供了一个用于订阅状态更改的内置机制，因此您可以轻松地使您的UI与您的数据保持同步。

对于想要轻量级且易于使用的状态管理解决方案而不需要Redux等更大库的开销的开发人员来说，它是一个不错的选择。

以下是使用Zustand的简单增量计数器的代码片段：

```js
js
复制代码import { create } from 'zustand'

const useStore = create((set) => ({
  count: 1,
  inc: () => set((state) => ({ count: state.count + 1 })),
}))

function Counter() {
  const { count, inc } = useStore()

  return (
    <div>
      <span>{count}</span>
      <button onClick={inc}>one up</button>
    </div>
  )
}
```

### [**精选推荐**](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-top-picks-b)

- Redux Toolkit提供功能更齐全的解决方案，具有更大的API和对附加功能的内置支持。
- Zustand的轻量级和简单的解决方案，易于使用，不需要太多的样板代码。

## [服务器状态管理](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-server-state-management-b)

服务器状态管理是指对存储在服务器上并由客户端应用程序远程访问的数据的管理。这些数据可以包括用户身份验证详细信息、数据库记录和其他后端数据。要在React应用程序中管理服务器状态，有几个可用的库。

### [TanStack Query](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23tan-stack-query)

最流行的是[TanStack Query](https://link.juejin.cn?target=https%3A%2F%2Ftanstack.com%2Fquery%2Flatest)，它提供了一种直观而强大的方式来管理React应用程序中的服务器状态。它提供了一个缓存层，可以自动管理数据的状态，根据需要获取和更新数据。

该库还提供了许多内置功能，例如自动重新获取、轮询和分页，使处理复杂数据集变得容易。

以下是查询API并在函数组件中使用返回的响应的示例代码片段：

```js
js
复制代码function GitHubStats() {
  const { isLoading, error, data, isFetching } = useQuery({
    queryKey: ["repoData"],
    queryFn: () =>
      axios
        .get("https://api.github.com/repos/gopinav/react-query-tutorials")
        .then((res) => res.data),
  });

  if (isLoading) return "Loading...";

  if (error) return "An error has occurred: " + error.message;

  return (
    <div>
      <h1>{data.name}</h1>
      <p>{data.description}</p>
      <strong>👀 {data.subscribers_count}</strong>{" "}
      <strong>✨ {data.stargazers_count}</strong>{" "}
      <strong>🍴 {data.forks_count}</strong>
      <div>{isFetching ? "Updating..." : ""}</div>
    </div>
  );
}
```

### [SWR](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23swr)

[SWR](https://link.juejin.cn?target=https%3A%2F%2Fswr.vercel.app%2F)是另一个流行的库，用于管理React应用程序中的服务器状态。名称“SWR”来自`stale-while-revalidate`，这是一种由[HTTP RFC 5861](https://link.juejin.cn?target=https%3A%2F%2Ftools.ietf.org%2Fhtml%2Frfc5861)推广的缓存失效策略。与TanStack Query相比，SWR确实有一些功能限制。

如果您使用Redux Toolkit进行客户端状态管理，[Redux Toolkit Query](https://link.juejin.cn?target=https%3A%2F%2Fredux-toolkit.js.org%2Frtk-query%2Foverview)是无缝管理服务器状态的绝佳选择。

[ApolloClient](https://link.juejin.cn?target=https%3A%2F%2Fwww.apollographql.com%2Fdocs%2Freact%2F)是另一个用于在React应用程序中管理服务器状态的流行库。它特别适合使用GraphQL API。

### [精选推荐](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-top-picks-b)

- **Tanstack Query**适用于REST API
- **Apollo Client** 适用于 GraphQL

## [表单处理](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-form-handling-b)

处理表单可能是一项乏味且容易出错的任务，但现在有许多优秀的表单处理库可用于React。一些最流行的选项包括[Formik](https://link.juejin.cn?target=https%3A%2F%2Fformik.org%2F)和[React Hook Form](https://link.juejin.cn?target=https%3A%2F%2Freact-hook-form.com%2F)。这些库使得处理表单验证、提交和错误处理变得更加容易。

### [Formik](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23formik)

虽然Formik提供了一个直观的API来管理表单状态、验证输入和提交数据，但该库并未得到积极维护。

### [React Hook Form](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23react-hook-form)

React Hook Form应该是您在2023年处理表单的首选库。它轻量级、快速且易于使用。React Hook Form利用React钩子的强大功能来管理表单状态和验证规则。它还为构建表单提供了灵活的API，并允许您轻松地与[Yup](https://link.juejin.cn?target=https%3A%2F%2Fgithub.com%2Fjquense%2Fyup)和[Zod](https://link.juejin.cn?target=https%3A%2F%2Fzod.dev%2F)等其他库集成以进行验证。

与Formik不同，React Hook Form不需要大量样板代码，并且可以显着减少处理表单数据所需的代码量。此外，React Hook Form具有出色的性能，因为组件不会为字段值的每次更改重新渲染。

以下是接受用户名字和姓氏的反应挂钩表单的示例代码片段：

```js
js
复制代码import { useForm } from "react-hook-form";

export default function App() {
  const { register, handleSubmit, watch, formState: { errors } } = useForm();
  const onSubmit = data => console.log(data);

  return (
    /* "handleSubmit" will validate your inputs before invoking "onSubmit" */
    <form onSubmit={handleSubmit(onSubmit)}>
      {/* register your input into the hook by invoking the "register" function */}
      <input {...register("firstName")} />
      
      {/* include validation with required or other standard HTML validation rules */}
      <input {...register("lastName", { required: true })} />
      {/* errors will return when field validation fails  */}
      {errors.lastName && <span>This field is required</span>}
      
      <button>Submit</button>
    </form>
  );
}
```

### [精选推荐](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-top-picks-b)

- **React Hook Form** 搭配 Yup/Zod 用于高性能、灵活和可扩展的表单，并具易于使用验证。

## [测试](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-testing-b)

测试是构建高质量React应用程序的重要组成部分。在测试React应用程序时，单元测试推荐[Vitest](https://link.juejin.cn?target=https%3A%2F%2Fvitest.dev%2F)和[React Testing Library](https://link.juejin.cn?target=https%3A%2F%2Ftesting-library.com%2Fdocs%2Freact-testing-library%2Fintro%2F)。端到端测试推荐[Playwright](https://link.juejin.cn?target=https%3A%2F%2Fplaywright.dev%2F)或[Cypress](https://link.juejin.cn?target=https%3A%2F%2Fwww.cypress.io%2F)。

### [Vitest](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23vitest)

Vitest是一个由Vite提供支持的极快的单元测试框架。在测试React应用程序的上下文中，它是一个测试运行器，可以查找测试，运行测试，确定测试是通过还是失败，并以人类可读的方式报告。

React测试库是一个javascript测试实用程序，它提供用于测试React组件的虚拟DOM。对于自动化测试，没有实际的DOM可以使用。React测试库提供了一个虚拟DOM，我们可以使用它来与反应组件交互并验证其行为。

### [Playwright and Cypress](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23playwright-and-cypress)

Playwright和Cypress是提供可靠和健壮的方法来从端到端测试React应用程序功能的库。您可以编写测试来模拟真实世界的用户与您的应用程序的交互，包括点击、键盘输入和表单提交。他们还有出色的留档和活跃的社区。

### [精选推荐](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-top-picks-b)

- Vitest + React Testing Library，用于单元测试。
- Playwright或Cypress 用于端到端测试。

## [样式](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-styling-b)

样式是构建现代Web应用程序的一个重要方面。有这么多可用于React的样式库，为您的项目选择合适的样式可能是压倒性的。这里有一些流行的样式库，可以帮助您创建漂亮且响应迅速的用户界面。

### [Tailwind](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23tailwind)

[TailWind CSS](https://link.juejin.cn?target=https%3A%2F%2Ftailwindcss.com%2F)是一个实用程序优先的CSS框架，它提供了一组用于构建UI组件的预定义类。使用TailWind CSS，您可以快速创建复杂的布局和自定义样式，而无需从头开始编写CSS。它具有出色的留档和活跃的社区，使其成为希望创建现代、响应式UI的开发人员的首选。

```js
js
复制代码<button class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded">
  Button
</button>
```

### [Styled Components](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23styled-components)

[Styled Components](https://link.juejin.cn?target=https%3A%2F%2Fstyled-components.com%2F)是一个流行的库，用于使用CSS-in-JS设置React组件的样式。它允许您直接在JavaScript代码中编写CSS，从而可以轻松创建范围为单个组件的动态样式。Styled Components还具有出色的主题支持，允许您在应用程序的不同样式之间快速切换。

```js
js
复制代码import styled from 'styled-components';

const Button = styled.button`
  background-color: #3f51b5;
  color: #fff;
  font-weight: bold;
  padding: 8px 16px;
  border-radius: 4px;
  cursor: pointer;

  &:hover {
    background-color: #303f9f;
  }
`;

export default Button;
```

### [Emotion](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23emotion)

[Etion](https://link.juejin.cn?target=https%3A%2F%2Femotion.sh%2Fdocs%2Fintroduction)是另一个CSS-in-JS库，它为React组件的样式提供了强大的API。它性能很高，允许您使用广泛的语法定义样式，包括CSS、Sass和less。

```js
js
复制代码import { css } from '@emotion/react';

const buttonStyles = css`
  background-color: #3f51b5;
  color: #fff;
  font-weight: bold;
  padding: 8px 16px;
  border-radius: 4px;
  cursor: pointer;

  &:hover {
    background-color: #303f9f;
  }
`;

const Button = () => (
  <button css={buttonStyles}>
    Button
  </button>
);

export default Button;
```

### [CSS Modules](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23css-modules)

[CSS modules](https://link.juejin.cn?target=https%3A%2F%2Fgithub.com%2Fcss-modules%2Fcss-modules)是React中一种流行的样式化方法，它允许您编写范围为单个组件的模块化CSS代码。使用CSS模块，您可以编写仅应用于特定组件的CSS类，防止命名冲突并确保样式被正确封装。

在CSS模块方法中，您需要创建一个单独的CSS文件（例如，`Button.module.css`），其内容如下：

```js
js
复制代码import styles from './Button.module.css';

const Button = () => (
  <button className={styles.button}>
    Button
  </button>
);

export default Button;
css
复制代码.button {
  background-color: #3f51b5;
  color: #fff;
  font-weight: bold;
  padding: 8px 16px;
  border-radius: 4px;
  cursor: pointer;
}

.button:hover {
  background-color: #303f9f;
}
```

### [Vanilla Extract](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23vanilla-extract)

由CSS模块的共同创建者Mark Dalgleish创建的[Vanilla Extract](https://link.juejin.cn?target=https%3A%2F%2Fvanilla-extract.style%2F)是最新的CSS-in-JS库之一。它提供了一种轻量级、零运行时的解决方案，用于为完全支持TypeScript的React组件设置样式。它在构建时提供静态CSS生成，从而提高性能并减少捆绑包大小。如果您更喜欢TypeScript优先的方法和价值性能，Vanilla Extract可以成为为React应用程序设置样式的绝佳选择。

```js
js
复制代码import { style } from '@vanilla-extract/css';

// Define styles
const buttonStyles = style({
  backgroundColor: '#3f51b5',
  color: '#fff',
  fontWeight: 'bold',
  padding: '8px 16px',
  borderRadius: '4px',
  cursor: 'pointer',
  ':hover': {
    backgroundColor: '#303f9f',
  },
});

const Button = () => (
  <button className={buttonStyles}>
    Button
  </button>
);

export default Button;
```

## [UI组件库](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-ui-component-libraries-b)

UI组件库可以为React开发人员节省大量时间，现在有许多优秀的选项可用。一些最流行的选项包括：

- [Material UI](https://link.juejin.cn?target=https%3A%2F%2Fmui.com%2F)
- [Mantine UI](https://link.juejin.cn?target=https%3A%2F%2Fui.mantine.dev%2F)
- [Ant Design](https://link.juejin.cn?target=https%3A%2F%2Fant.design%2F)
- [Chakra UI](https://link.juejin.cn?target=https%3A%2F%2Fchakra-ui.com%2F)

还有TailWind CSS框架，例如：

- [ShadCN](https://link.juejin.cn?target=https%3A%2F%2Fui.shadcn.com%2F)
- [Daisy UI](https://link.juejin.cn?target=https%3A%2F%2Fdaisyui.com%2F)
- [Headless UI](https://link.juejin.cn?target=https%3A%2F%2Fheadlessui.com%2F)

### [精选推荐](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-top-picks-b)

- **Material UI** 适用于大型社区的材料UI和出色的fima套件
- **Mantine UI** 冉冉升起的新星
- **ShadCN** 适用于 Tailwind CSS

## [动画](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-animation-b)

### [React Spring和Framer Motion](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23react-spring-and-framer-motion)

动画可以成为创建引人入胜和交互式用户界面的强大工具，并且有许多优秀的动画库可用于React。一些最流行的选项包括[React Spring](https://link.juejin.cn?target=https%3A%2F%2Fwww.react-spring.dev%2F)和[Framer Motion](https://link.juejin.cn?target=https%3A%2F%2Fwww.framer.com%2Fmotion%2F)。这些库可以轻松地用最少的代码创建流畅和响应迅速的动画。

这是一个使用Framer Motion的示例代码片段。Motion的核心是[运动组件，](https://link.juejin.cn?target=https%3A%2F%2Fwww.framer.com%2Fdocs%2Fcomponent%2F)可以将其视为纯超文本标记语言或SVG元素，具有动画功能。为`motion`组件制作动画就像在[动画道具上](https://link.juejin.cn?target=https%3A%2F%2Fwww.framer.com%2Fdocs%2Fanimation%2F)设置值一样简单。

```js
js
复制代码import { motion } from "framer-motion";

export default function App() {
  return (
    <motion.div
      className="box"
      initial={{ opacity: 0 }}
      animate={{ opacity: 1 }}
    />
  );
}
```

### [精选推荐](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-top-picks-b)

- Framer Motion为创建强大的动画提供了成熟的API。

## [数据可视化](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-data-visualization-b)

数据可视化是许多React应用程序的重要组成部分，尤其是那些依赖复杂数据集的应用程序。一些流行的React数据可视化库包括：

- [Victory](https://link.juejin.cn?target=https%3A%2F%2Fformidable.com%2Fopen-source%2Fvictory%2F)
- [React Chartjs](https://link.juejin.cn?target=https%3A%2F%2Freact-chartjs-2.js.org%2F)
- [Recharts](https://link.juejin.cn?target=https%3A%2F%2Frecharts.org%2Fen-US).

这些库最大限度地减少了创建漂亮的交互式可视化的学习曲线，可以帮助用户理解复杂的数据。

这是一个使用Rechart渲染折线图的示例代码片段：

```js
js
复制代码import { LineChart, Line } from 'recharts';
const data = [{name: 'Page A', uv: 400, pv: 2400, amt: 2400}, ...];

const renderLineChart = (
  <LineChart width={400} height={400} data={data}>
    <Line type="monotone" dataKey="uv" stroke="#8884d8" />
  </LineChart>
);
```

### [精选推荐](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-top-b-picks)

- Rechart是一个很棒的库，用于开始使用大量可能的可视化。

## [表格](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-tables-b)

表格在React中实现可能是一个具有挑战性的组件，但有许多优秀的表库可用。一些流行的选项包括：

- [TanStack Table](https://link.juejin.cn?target=https%3A%2F%2Ftanstack.com%2Ftable%2Fv8)
- [React Data Grid](https://link.juejin.cn?target=https%3A%2F%2Fgithub.com%2Fadazzle%2Freact-data-grid).

这些库可以轻松创建具有排序、过滤和分页等功能的强大且可自定义的表。

### [精选推荐](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-top-picks-b)

- **TanStack Table** 几年来一直是首选。

## [Headless CMS](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23headless-cms)

为了减少仅为对您的站点或应用程序进行内容更新而更新和部署代码的需要，您需要使用headless CMS。

我们建议将[Builder.io](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2F)与React一起使用，因为它扩展了过去的结构化数据编辑，并允许直接使用您的组件进行[可视化编辑](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Fvisual-editing-cms)，这可以大大减少对网站或应用程序内容繁重部分的硬编码需求。

以下是集成的示例：

```js
js
复制代码import { Builder, BuilderComponent, builder } from '@builder.io/react'

// Dynamically render compositions of your components 
// https://www.builder.io/c/docs/quickstart
export default function MyPage({ builderJson }) {
  return <>
    <Header />
    <BuilderComponent model="page" content={builderJson} />
    <Footer />
  </>
}

// Fetch Builder.io content from the content API
// https://www.builder.io/c/docs/content-api
export async function getStaticProps({ params }) {
  // Query content from the CMS https://www.builder.io/c/docs/querying
  const content = await builder.get('page', { url: '/' }).promise()
  return { props: { builderJson: content || null } }
}

// Register your components for user in the visual editor
// // https://www.builder.io/c/docs/custom-components-setup
Builder.registerComponent(MyHeadingComponent, {
  name: 'Heading',
  // Define which props are editable
  inputs: [{ name: 'text', type: 'string', defaultValue: 'Hello world' }]
})
```

以及编辑UI是什么样的：

[cdn.builder.io/o/assets%2F…](https://link.juejin.cn?target=https%3A%2F%2Fcdn.builder.io%2Fo%2Fassets%2FYJIGb4i01jvw0SRdL5Bt%2F77cc5249f71948568195e7314bcc5766%2Fcompressed%3FapiKey%3DYJIGb4i01jvw0SRdL5Bt%26token%3D77cc5249f71948568195e7314bcc5766%26alt%3Dmedia%26optimized%3Dtrue'allowfullscreen)

## [国际化](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-i-18-n-b)

国际化是许多应用程序的一个重要考虑因素，尤其是那些拥有全球受众的应用程序。像[i18next](https://link.juejin.cn?target=https%3A%2F%2Freact.i18next.com%2F)和[React-Intl](https://link.juejin.cn?target=https%3A%2F%2Fformatjs.io%2Fdocs%2Freact-intl%2F)这样的库有助于将您的应用程序翻译成多种语言并处理本地化。

我们的建议包括：

- [i18next](https://link.juejin.cn?target=https%3A%2F%2Freact.i18next.com%2F)
- [React-Intl](https://link.juejin.cn?target=https%3A%2F%2Fformatjs.io%2Fdocs%2Freact-intl%2F)

## [开发工具](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-dev-tools-b)

开发工具可以为React开发人员提供巨大的帮助，并且有许多优秀的选项可用。一些流行的选项包括：

- [React Developer Tools](https://link.juejin.cn?target=https%3A%2F%2Fchrome.google.com%2Fwebstore%2Fdetail%2Freact-developer-tools%2Ffmkadmapgofadopljbjfkapdkoienihi)
- [Redux DevTools](https://link.juejin.cn?target=https%3A%2F%2Fchrome.google.com%2Fwebstore%2Fdetail%2Fredux-devtools%2Flmhkpmbekcpmknklioeibfkpmmfibljd)
- [React Hook Form DevTools](https://link.juejin.cn?target=https%3A%2F%2Freact-hook-form.com%2Fdev-tools)
- [TanStack Query DevTools](https://link.juejin.cn?target=https%3A%2F%2Ftanstack.com%2Fquery%2Fv4%2Fdocs%2Freact%2Fdevtools)

## [类型检查](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-type-checking-b)

类型检查可以帮助捕捉错误并提高React应用程序的可靠性。[TypeScript](https://link.juejin.cn?target=https%3A%2F%2Fwww.typescriptlang.org%2F)是你的选择。

## [组件开发环境](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23component-b-development-environment-b)

创建一个精简高效的开发环境对React开发人员来说很重要。[Storybook](https://link.juejin.cn?target=https%3A%2F%2Fstorybook.js.org%2F)。

## [文档应用](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-documentation-apps-b)

文档是任何软件项目的重要组成部分。对于创建留档应用程序，一个非常好的选择是[Docusaurus](https://link.juejin.cn?target=https%3A%2F%2Fdocusaurus.io%2F)。当然，您也可以将Next. js与[Nextra](https://link.juejin.cn?target=https%3A%2F%2Fnextra.site%2F)这样的库一起使用。

## [原生移动应用](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-native-mobile-apps-b)

[React Native](https://link.juejin.cn?target=https%3A%2F%2Freactnative.dev%2F)已成为使用React构建原生移动应用程序的日益流行的选择，React Native允许开发人员使用React和原生组件创建跨平台移动应用程序。

## [其他优秀组件库](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23b-awesome-libraries-b)

除了上面列出的库和工具之外，还有许多其他很棒的库可供React开发人员使用。一些流行的选项包括：

- [dnd kit](https://link.juejin.cn?target=https%3A%2F%2Fdndkit.com%2F) 用于拖动功能
- [React-slick](https://link.juejin.cn?target=https%3A%2F%2Freact-slick.neostack.com%2F) 用于构建轮播图和滑块
- [react-dropzone](https://link.juejin.cn?target=https%3A%2F%2Freact-dropzone.js.org%2F) 用于文件上传。

这些库可以帮助简化开发并改善React应用程序的用户体验。

## [结论](https://link.juejin.cn?target=https%3A%2F%2Fwww.builder.io%2Fblog%2Freact-js-in-2023%23conclusion)

React生态系统在2023年继续快速发展和增长，有许多优秀的工具和库可用于构建高质量的React应用程序。无论您是刚刚开始使用React还是经验丰富的React开发人员，都有许多选项可以帮助您保持高效并构建出色的用户体验。

作者：我家猫叫佩奇
链接：https://juejin.cn/post/7246266964296417339
来源：稀土掘金
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。