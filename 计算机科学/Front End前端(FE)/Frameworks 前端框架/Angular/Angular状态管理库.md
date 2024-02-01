

- https://roadmap.sh/angular



# State Management

Application state management is the process of maintaining knowledge  of an application’s inputs across multiple related data flows that form a complete business transaction — or a session — to understand the  condition of the app at any given moment. In computer science, an input  is information put into the program by the user and state refers to the  condition of an application according to its stored inputs — saved as  variables or constants. State can also be described as the collection of preserved information that forms a complete session.

Visit the following resources to learn more:

- [What is State Management?](https://www.techtarget.com/searchapparchitecture/definition/state-management)
- [ Angular state management made simple with NgRx](https://blog.logrocket.com/angular-state-management-made-simple-with-ngrx/)
- [Angular State Management with NgRx](https://www.syncfusion.com/blogs/post/angular-state-management-with-ngrx.aspx)



在Angular的生态系统中，NGXS和NGRX都是流行的状态管理库，它们提供了不同的方法来管理和维护应用状态。以下是这两个库的对比：

| 特性/库        | NGXS                                                 | NGRX                                                      |
| -------------- | ---------------------------------------------------- | --------------------------------------------------------- |
| 核心理念       | 将状态管理简化，提供一个可操作的、面向对象的状态模型 | 基于Redux模式，强调单一不可变状态树、纯函数reducers和效果 |
| 配置与引导     | 简洁，较少的样板代码                                 | 需要更多的样板代码和配置                                  |
| 状态操作       | 通过简单的类和装饰器进行状态操作                     | 使用actions、reducers和selectors进行状态操作              |
| 异步处理       | 内置支持异步操作                                     | 使用Effects处理副作用和异步操作                           |
| 开发工具支持   | 有NGXS DevTools支持                                  | 有NGRX DevTools支持，兼容Redux DevTools                   |
| 测试           | 较易测试                                             | 测试需要模拟actions和selectors                            |
| 社区和生态系统 | 正在成长                                             | 非常成熟且广泛                                            |
| 学习曲线       | 相对简单                                             | 相对复杂，需要理解Redux及其Angular实现                    |
| 适用场景       | 适合希望快速上手并减少样板代码的项目                 | 适合需要强大、灵活状态管理的大型、复杂应用                |
| 性能           | 高性能，特别是在较小的应用中                         | 性能良好，但在大型应用中可能需要调优                      |
| 调试与维护     | 较容易维护，状态和逻辑通常更集中                     | 状态分散在actions、reducers中，可能更难追踪               |
| 扩展性         | 插件系统提供扩展，如日志、路由插件等                 | 丰富的生态系统提供了多种中间件和附加功能                  |
| 最佳用例       | 适合中小型项目，以及偏好面向对象风格的开发者         | 适合大型、复杂项目，或者有Redux经验的开发者               |

NGXS 和 NGRX 提供了不同的API和抽象层，以满足不同的开发需求和偏好。NGXS主张简化状态管理，而NGRX提供了一个与Redux类似的严格状态管理体验。开发者应根据团队的熟悉度、项目的复杂性以及对状态管理的需求来选择适合的库。