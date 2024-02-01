- https://roadmap.sh/frontend

# Desktop Applications in JavaScript

A while back, developing a desktop app using JavaScript was  impossible. But now JavaScript developers can create desktop  applications using their knowledge for web development. Here is the list of options to create desktop applications in JavaScript.

- [Electron](https://www.electronjs.org/)
- [NodeGUI](https://docs.nodegui.org/)
- [NW.js](https://nwjs.io/)
- [Meteor](https://www.meteor.com/)



以下是Electron, Tauri, 和 Flutter 这三个用于开发桌面应用的技术的详细对比：

| 特性/框架      | Electron                                    | Tauri                                | Flutter                                    |
| -------------- | ------------------------------------------- | ------------------------------------ | ------------------------------------------ |
| 核心语言       | JavaScript, HTML, CSS                       | Rust, HTML, CSS, JavaScript          | Dart                                       |
| UI渲染         | Chromium                                    | Webview (由操作系统提供)             | 自己的渲染引擎                             |
| 性能           | 良好，但通常内存和CPU使用量较大             | 较Electron有更低的资源占用，性能更优 | 高性能，渲染速度快                         |
| 跨平台         | Windows, macOS, Linux                       | Windows, macOS, Linux                | Windows, macOS, Linux (桌面支持尚在开发中) |
| 应用大小       | 较大，因为包括了完整的Chromium和Node.js环境 | 较小，因为使用了系统内置的Webview    | 取决于应用，但Flutter框架相对较大          |
| 安全性         | 依赖于Chromium的沙盒，但需要注意安全问题    | 提供了更多的安全特性，如沙盒化       | 由于不运行在Webview中，通常被认为更安全    |
| 社区和支持     | 非常大的社区和生态系统                      | 正在增长的社区                       | 非常大的社区，特别是移动端                 |
| 开发体验       | 成熟的工具链和大量资源                      | 正在成熟，工具链不断完善             | 良好的开发体验，支持热重载                 |
| 原生模块支持   | 支持Node.js原生模块                         | 支持调用Rust和系统原生代码           | 支持调用C/C++原生代码                      |
| 打包和分发     | 内置的打包工具，如electron-builder          | 内置的打包工具                       | 通过命令行工具或第三方工具打包             |
| 插件和扩展性   | 丰富的第三方Node.js模块                     | 基于Rust生态系统，可扩展性良好       | 丰富的Dart包和插件                         |
| 开箱即用的功能 | Electron API提供了丰富的桌面应用功能        | 更轻量级，但基本的桌面应用功能齐全   | 提供了丰富的UI组件和桌面功能               |
| 文档           | 详尽                                        | 正在完善                             | 详尽                                       |
| 许可和成本     | MIT许可证                                   | MIT许可证                            | BSD许可证                                  |

Electron 是一个成熟且广泛使用的框架，它允许开发者使用Web技术来构建桌面应用。Tauri 是一个较新的框架，提供了更轻量级的方法来开发桌面应用，并专注于安全性和性能。Flutter 是一个由谷歌开发的UI工具包，最初用于移动应用开发，但现在也在向桌面和Web扩展。这三个技术根据项目需求、开发者的技术栈偏好以及对性能和资源占用的考虑，都有各自的适用场景。
