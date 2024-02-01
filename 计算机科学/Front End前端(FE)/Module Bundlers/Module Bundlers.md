

- https://roadmap.sh/frontend

# Module Bundlers

A module bundler is a tool that takes pieces of JavaScript and their  dependencies and bundles them into a single file, usually for use in the browser. You may have used tools such as Browserify, Webpack, Rollup or one of many others.

It usually starts with an entry file, and from there it bundles up all of the code needed for that entry file.

Visit the following resources to learn more:

- [Let’s learn how module bundlers work](https://www.freecodecamp.org/news/lets-learn-how-module-bundlers-work-and-then-write-one-ourselves-b2e3fe6c88ae/)
- [Module Bundlers Explained](https://www.youtube.com/watch?v=5IG4UmULyoA)







下面是一个表格，详细比较了Vite、esbuild、Webpack、Rollup和Parcel这些模块打包工具。

| 特性/工具            | Vite                         | esbuild            | Webpack                | Rollup                         | Parcel           |
| -------------------- | ---------------------------- | ------------------ | ---------------------- | ------------------------------ | ---------------- |
| 类型                 | 前端开发和打包工具           | 极速打包工具       | 模块打包器             | 模块打包器                     | 打包工具         |
| 主要特点             | 快速的冷启动，即时模块热替换 | 极速的打包速度     | 大量插件和加载器       | 高效的ES模块打包               | 零配置           |
| 打包速度             | 非常快（冷启动优化）         | 非常快             | 中等（可能较慢）       | 快（特别是当使用tree-shaking） | 快（零配置缓存） |
| 配置复杂度           | 低（简洁配置）               | 低（无需配置）     | 高（灵活但复杂的配置） | 中（配置相对简单）             | 低（无需配置）   |
| 生态系统             | 正在快速增长                 | 较新，生态正在成长 | 庞大且成熟             | 丰富但小于Webpack              | 正在增长         |
| 使用场景             | 单页面应用，多页面应用       | 任何JavaScript项目 | 复杂的前端项目         | 库和应用程序                   | 应用程序和网站   |
| Tree-shaking         | 支持                         | 支持               | 支持                   | 支持                           | 支持             |
| 模块热替换(HMR)      | 支持                         | 通过插件           | 支持                   | 通过插件                       | 支持             |
| 插件系统             | 支持（基于Rollup）           | 有限支持           | 支持                   | 支持                           | 支持             |
| 代码分割             | 支持                         | 支持               | 支持                   | 支持                           | 支持             |
| 开发服务器           | 内置                         | 需要第三方服务器   | 需要webpack-dev-server | 需要第三方服务器               | 内置             |
| 生产优化             | 内置                         | 基础优化           | 复杂优化               | 高级优化                       | 内置优化         |
| TypeScript支持       | 内置支持                     | 内置支持           | 通过加载器支持         | 通过插件支持                   | 内置支持         |
| 源码映射(Source Map) | 支持                         | 支持               | 支持                   | 支持                           | 支持             |
| 社区支持             | 正在快速增长                 | 正在增长           | 非常活跃               | 活跃                           | 活跃             |
| 文档                 | 良好                         | 良好               | 非常详尽               | 良好                           | 良好             |

模块打包器是现代前端开发的重要组成部分，它们优化了开发流程并提高了性能。选择哪个工具通常取决于项目需求、开发者偏好、必要的配置复杂度、构建速度需求以及对生态系统的要求。随着JavaScript工具链的不断进化，这些工具都在不断更新和改进，以适应开发者的需求。
