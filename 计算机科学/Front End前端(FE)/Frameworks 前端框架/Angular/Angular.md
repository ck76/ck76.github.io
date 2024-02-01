- https://roadmap.sh/angular

在Angular框架中，路由（Routing）是一个核心功能，它允许用户导航从一个视图到另一个视图。以下是图中提到的路由相关概念的比较：

| 概念           | 描述                                                         | 示例或用法                                                   |
| -------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| Router Outlets | 用作在应用中放置视图的占位符                                 | `<router-outlet></router-outlet>` 在模板中标记放置组件视图的位置 |
| Router Links   | 允许用户通过点击链接来导航到不同的路由                       | `<a [routerLink]="['/user/bob']">Bob</a>` 创建到用户Bob的路由链接 |
| Router Events  | 在路由生命周期中发出的事件，可以用来监听路由的变化           | 订阅`Router.events`来获取如`NavigationStart`, `RoutesRecognized`等事件 |
| Guards         | 路由守卫，用于控制路由的访问权限                             | 使用`CanActivate`来决定是否允许导航到路由                    |
| Configuration  | 路由配置，定义了应用中的路由结构                             | 在`RouterModule.forRoot(routes)`中定义路由配置               |
| Lazy Loading   | 懒加载模块，只有在用户导航到路由时才加载模块，优化初始加载时间 | 使用`loadChildren`来定义特性模块的懒加载 `loadChildren: () => import('./users/users.module').then(m => m.UsersModule)` |

Angular路由系统提供了一种在单页面应用（SPA）中无需重新加载页面即可导航的机制。使用这些工具和配置，开发者可以构建复杂的导航结构，同时保持用户界面的响应性和性能。路由守卫（Guards）提供了对导航的细粒度控制，而懒加载（Lazy Loading）则有助于提高大型应用的性能。