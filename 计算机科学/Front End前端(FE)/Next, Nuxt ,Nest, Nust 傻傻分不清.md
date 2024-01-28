[TOC]

这篇文章适合那些正在寻找新的JavaScript后端框架的人。如果你也想更好地理解Gatsby，Next.js，Nest和Nust框架，那么本文就是为你准备的。本文解释框架之间的相似之处和差异，它们的性能特征，为什么会流行，以及如何选择最适合您的框架。 Next.js、Nuxt.js：它们是分别与特定前端技术 React、Vue 绑定的前端应用开发框架，有一定的相似性，可以放在一起进行调研对比。 Nest.js：是“Angular 的服务端实现”，基于装饰器。可以使用任何兼容的 http 提供程序，如 Express、Fastify 替换底层内核。可用于 http、rpc、graphql 服务，对提供更多样的服务能力有一定参考价值。 Gatsby：是一个基于 React ，用于搭建静态站点的开源框架，用于帮助 开发者构建运行速度极快的网站。可以说它是一个静态站点生成器，Gatsby 主要的应用的技术是 React 和 GraphQL。 Nust：估计“Nust”将是下一个框架的名字。

## 1. Next, Nest, Gatsby, and Nuxt 之争

根据最新的调查，以上这些开源框架都是最受欢迎的后端框架。调查显示，Next.js，Gatsby，Nuxt和Nest位列前五名。它们位于第一名Express之后，而且仍在增长。 Express几乎自Node.js诞生以来就一直存在，是标准的后端JavaScript框架。它相对较接近HTTP协议，提供了极简的界面，这也解释了为什么它经常被用作许多其他框架的基础。排名前五的其他框架遵循不同的策略，为用户提供大量的抽象概念。但是这些抽象概念也需要用户更深入地了解框架本身。本文将介绍使用最多的框架前5名中的其他竞争者：Gatsby，Next.js，Nuxt.js和Nest。 :::tips
 接下来的两节将介绍Web应用的核心，比如LP和LCP，以及渲染策略，比如CSR，SSR和同构应用程序。如果您已经知道这些缩写的含义，您可以直接跳转到**框架**部分。 :::

## 2. Web应用程序的性能度量

在深入研究框架之前，让我们想想一下Web应用程序的要求。几乎所有项目都有一个共同的目标：为用户提供相关的高质量内容。但是我们也知道，即使最好的系统也无法完成任务。它还需要有**合格的性能**（就公共内容而言）也需要高质量内容**被发现的**。 后一个点非常直接的。当您在公共网上发布内容时，您希望搜索引擎索引该页面，然后在搜索中可以发现。 性能方面可能更难定义：哪个性能指标可以告诉您内容是及时显示？个人认为对于这个问题没有完美的答案，但肯定有多种良好的指标可以结合起来进行整体分析： :::tips **1.第一字节时间**(Time to First Byte，TTFB) 第一字节时间（TTFB）是指从浏览器请求页面到从浏览器接收来自服务器发送的信息的第一个字节的时间。这一次包括 DNS 查找和使用（三次）TCP握手和SSL握手建立连接（如果请求是通过https发出的） **2.首次渲染 (First Paint，FP) ** 任何像素第一次对用户可见。这包括TTFB和客户端在呈现作业之前必须做的计算。 **3.首次内容渲染(First Contentful Paint，FCP) ** 第一次内容绘制标记第一次绘制文本或图像的时间。与FP类似，但技术含量较低，并定义了向用户提示页面正在工作的呈现阶段。 **4.最大内容渲染(Largest Contentful Paint，LCP) ** 最大内容渲染标记渲染最大文本或图像的时间。这个度量也可以描述为用户看到预期结果之前的时间。 **5.交互时间(Time To Interactive**，**TTI) ** 交互时间是指页面实现完全交互所需的时间。用户可能期望这是与LCP相同的时间，但这可能是不同的。如果TTI和LCP不一致，用户可能会认为网站坏了。 ::: MDN的解释 :::tips

1. Time to First Byte

第一字节时间（TTFB）是指从浏览器请求页面到从浏览器接收来自服务器发送的信息的第一个字节的时间。这一次包括 DNS 查找和使用（三次）TCP握手和SSL握手建立连接（如果请求是通过https发出的）。 [developer.mozilla.org/zh-CN/docs/…](https://link.juejin.cn?target=https%3A%2F%2Fdeveloper.mozilla.org%2Fzh-CN%2Fdocs%2FGlossary%2Ftime_to_first_byte)

1. First Paint

First Paint，是Paint Timing API的一部分，是页面导航与浏览器将该网页的第一个像素渲染到屏幕上所用的中间时，渲染是任何与输入网页导航前的屏幕上的内容不同的内容。它回答了“发生了什么？”这个问题。 [developer.mozilla.org/zh-CN/docs/…](https://link.juejin.cn?target=https%3A%2F%2Fdeveloper.mozilla.org%2Fzh-CN%2Fdocs%2FGlossary%2FFirst_paint)

1. First Contentful Paint

首次内容绘制(First Contentful Paint, FCP) 是当浏览器从DOM渲染第一块内容，第一次提供页面正在宅如的回馈给使用者。这个问题是“它发生了吗”，首次内容绘制完成时，答案为“是的” [developer.mozilla.org/zh-TW/docs/…](https://link.juejin.cn?target=https%3A%2F%2Fdeveloper.mozilla.org%2Fzh-TW%2Fdocs%2FGlossary%2FFirst_contentful_paint) 4. Largest Contentful Paint，LCP LargestContentfulPaint在用户在网页上输入之前，该界面提供有关最大图像或文本绘制的时间信息。 [developer.mozilla.org/en-US/docs/…](https://link.juejin.cn?target=https%3A%2F%2Fdeveloper.mozilla.org%2Fen-US%2Fdocs%2FWeb%2FAPI%2FLargestContentfulPaint) 5.Time To Interactive，TTI 交互时间(TTI) 是一种非标准化的 Web 性能“进度”指标，定义为最后一个长任务完成后网络和主线程不活动 5 秒的时间点。 [developer.mozilla.org/en-US/docs/…](https://link.juejin.cn?target=https%3A%2F%2Fdeveloper.mozilla.org%2Fen-US%2Fdocs%2FGlossary%2FTime_to_interactive) :::

![image.png](https://p.ipic.vip/go0t87.jpg) :::tips 这些指标是由Chrome团队引入的，通常也被称为Web vital。好消息是，所有基于Blink（谷歌的排版引擎）的浏览器都带有衡量这些指标的开发工具。坏消息是，到目前为止，Safari还不支持这些指标。因此，您可能需要使用其他浏览器进行性能调优，在这些浏览器中使用其他指标，或者坐在屏幕前使用秒表。 不过别担心。对于本文，使用哪种浏览器并不重要，因为我们将在下一节中使用这些指标只是为了突出显示各种呈现选项之间的差异。 :::

## 3. 渲染方式

即使在一个非常高的程度上，所有的网站都是一样的。客户端向包含所需信息的服务器发送初始请求。服务器响应所请求的数据，这可能会或可能不会触发更多请求。最终客户端拥有所有需要的数据，可以呈现一个页面来向用户直观地显示信息。 对于静态内容，即对每个用户看起来都一样并且不需要任何计算的内容，HTML 和 CSS 文件存储在服务器上，并根据请求交付给客户端。客户端，在大多数情况下是浏览器，解析这些文件并根据这些指令构建文档对象模型 ( DOM )。 Web 的很大一部分由动态内容组成，这些内容需要因用户而异，甚至可能取决于用户访问 Web 资源的时间或状态。对于这些场景，计算需要在上面可视化的请求过程中进行。 Java Servlets是一种向网站添加动态内容的流行方法。每当请求到达服务器时，它都会运行预编译的 Java 字节代码，打印请求的 HTML 响应。随着 2010 年AngularJS、Backbone.js和Knockout等框架的兴起，我们看到了向单页应用程序 ( SPA )的转变。 SPA方式主要是在客户端和服务端之间传输JS和JSON文件。在浏览器中执行时，这些文件呈现用户看到的 DOM 元素。Java Servlet 和单页应用程序通常执行代码来生成网站的标记。但是，它们在执行此代码的位置和时间上存在差异。我们也可以说他们使用了不同的渲染策略。 根据您的要求，各种渲染策略各有优缺点。有些可能比其他的与您的项目更相关。以下部分将涵盖我们今天看到的最常见的内容。

- **客户端呈现 (CSR)** 在客户端浏览器中呈现单个页面的应用程序通常也称为单页应用程序。这个概念被称为CSR或Client-side Rendering。服务器实质上提供了在客户端执行的精简 JavaScript 代码，以通过 HTTP 请求获取附加信息并呈现 DOM 元素。对于许多 Web 开发人员来说，这是现在开发 Web 应用程序的标准。常见的 SPA 框架是React、Angular、Vue.js和Svelte。 这种方法的优点是它可以很好地扩展，因为大部分计算都发生在客户端上，这可以快速到达**第一个字节的时间**（TTFB）。另一个优点是您可以使用现代浏览器支持的Web API，例如Geolocation、Clipboard或Push。 不过也有缺点。由于所有渲染都发生在客户端，因此在加载 JavaScript 文件后需要额外的时间，并可能导致用户界面在此期间被阻塞。因此，**首次渲染**、**首次内容渲染**、**最大内容渲染**和**交互时间**等指标需要更多时间。 为了补偿这种等待时间，前端开发人员通常会创建启动画面以减少可感知的加载时间。另一个缺点是大多数搜索引擎不执行 JavaScript，这会在您对 SEO 感兴趣时导致问题——因为爬虫可能很难理解页面内容，这会影响您页面的可发现性。当然，可能有一些项目（例如公司内部应用程序）与 SEO 无关，而且这方面不会影响到你。
- **服务器端渲染 (SSR)** SSR（或服务器端呈现）在响应传入请求之前计算完全呈现的 HTML，交付完整的页面。在许多方面，这种渲染方法与客户端渲染相反。由于搜索引擎爬虫正在解析完整的网页，因此这种方法对SEO 友好，并且仍然显示因用户而异的最新内容。有趣的是，**首次渲染**、**首次内容渲染**、**最大内容渲染**和**交互时间**指标很好，而且通常彼此非常接近，因为网站通常会同时呈现所有内容。 但是，这里也有一个缺点。由于服务器必须在提供标记之前完成计算，**第一字节时间**通常需要更多时间。随着负载每个额外用户的增加而导致计算增加，这种方法相对更难扩展。对于所有用户来说看起来非常相似的网页可能会受到这种方法的严重影响，因为重复计算具有相同结果的计算。
- **静态站点生成 (SSG)** SSG或Static Site Generation在 Web 应用程序的构建期间而不是按需计算页面。这种方法侧重于通过预先计算静态网页来减少重复的服务器端计算。这会导致更长的构建时间，但最终会通过提供这些预先计算的页面来节省计算时间。 所有指标（**第一字节时间、首次渲染**、**首次内容渲染**、**最大内容渲染**和**交互时间**）都受益于这种方法，因为它与静态服务非常相似。这自然也带来了低服务器成本和出色的可扩展性。 显著的缺点是所有页面对每个用户来说都是一样的。这只是某些类型的 Web 应用程序所希望的，甚至是可能的。您能想象一个网上商店，尤其是购物车，对每个用户来说看起来都一样吗？对于一些页面，比如 API 文档或者小型博客，作者可以充分利用这种渲染策略。 但是“静态网上商城”的例子显示了第二个缺点——每次更改产品或文章时，都需要重建站点！您需要重建整个应用程序来更新单个产品描述——否则，商店将显示过时的信息。因此，对于静态站点生成，稍微修改策略并让页面过期或在构建时不呈现它们并等待它们被第一次请求是有意义的。这些修改分别称为[增量静态生成](https://link.juejin.cn?target=https%3A%2F%2Fnextjs.org%2Fdocs%2Fbasic-features%2Fdata-fetching%2Fincremental-static-regeneration)和[延迟静态生成](https://link.juejin.cn?target=https%3A%2F%2Fwww.gatsbyjs.com%2Fdocs%2Fhow-to%2Frendering-options%2Fusing-deferred-static-generation%2F)。
- **混合渲染策略——模块配置、hydrate注水和客户端预加载** 前面提到的渲染方式将所有计算负载分别转移到客户端、服务器或构建服务器。同时列出的优点和缺点，例如，好的SEO支持 与差的 SEO 支持。但不一定要那样做的！为什么不在客户端进行一些计算，而在服务器端进行一些计算呢？ 对于可发现性，尽量减少客户端呈现的页面元素的数量似乎是合理的。这意味着我们只想在浏览器中呈现用户特定的元素。所有其他页面元素——页眉、页脚、侧边栏，甚至整个页面，如“关于我们”——都可以在服务器上呈现和缓存以节省时间。 这种方法在各种框架中使用了几个不同的名称。有些人将这些类型的应用程序描述为[模块配置](https://link.juejin.cn?target=https%3A%2F%2Fnuxtjs.org%2Fdocs%2Fconfiguration-glossary%2Fconfiguration-mode%2F%23the-mode-property)的。其他框架将此过程称为[hydrate注水](https://link.juejin.cn?target=https%3A%2F%2Freact.dev%2Freference%2Freact-dom%2Fhydrate)或[客户端预加载](https://link.juejin.cn?target=https%3A%2F%2Fnextjs.org%2Fdocs%2Fbasic-features%2Fdata-fetching%2Fclient-side)。有时还会遇到术语**预渲染**，但避免使用它，因为它没有明确定义。

```scss
scss
复制代码关于术语预渲染的注释：
这是您在研究渲染方式时经常会遇到的一个术语。起初，这个词似乎很合适，但尽量避免使用它，因为目前还没有找到一个明确的定义。
在博客文章中，一些 Chrome 和 Shopify 工程师提到了“静态渲染和预渲染之间的区别”。Vue.js文档说静态站点生成（SSG）“也称为预呈现”。同样，Next.js 文档说静态生成和服务器端渲染是“预渲染的两种形式”。Jamstack 文档遵循类似的想法，但将任何“可以直接从 CDN 提供服务”的内容称为预渲染。这包括静态站点生成、服务器端渲染和混合方法。
个人也同意这个观点：预渲染是所有不完全在客户端渲染的东西。但不想造成混淆，并尽量避免使用这个术语(至少现在)。
```

独立于名称，此策略具有快速的**第一字节时间**、**首次渲染**和**首次内容渲染**时间，SEO 也友好的。只有**最大内容渲染**和 **交互时间 **通常显示出劣势。但总的来说，这些指标仍然比仅在客户端呈现的单页应用程序表现更好。 这种方法的从Web Vital 指标看并没有缺点，但有一个实用的缺点，它定义何时在何处呈现哪些页面元素会带来额外的复杂性。 **多种渲染策略** 有很多渲染策略可供选择。这些分类比其他任何分类都更具理论性。在现实生活中，一个网站可以针对不同的子页面使用多种策略。以网上商店为例，目录和产品页面必须具有快速加载时间并同时进行 SEO 优化。这听起来像是静态站点生成的工作。但也需要一些特定于用户的页面，其中可发现性不是一个因素，例如购物车。为这些子页面利用混合呈现策略是有意义的。购物车页面的布局对所有用户都是一样的，可以在服务器上呈现，购物车的项目可以通过客户端获取来呈现。 当实现在客户端和服务器上部分呈现的 Web 应用程序时，开发者如何决定哪些代码需要在哪个平台上运行，或者如何将代码缩小并拆分为单独的块？这会很快变得非常复杂。但值得庆幸的是，有一些框架可以帮助开发者完成这项任务。

## 4.框架

### 4.1 Gatsby

Gatsby 最初于 2015 年被概念化为 React Web 应用程序静态站点生成的框架。随着时间的推移，它添加了一些其他渲染方式，例如延迟站点生成（在第一次请求时）、服务器端渲染（应该只在某些情况下使用）和客户端预加载。 您可以从多个系统（例如数据库管理或无头内容管理系统）获取数据，以构建您需要的 React 站点。但是与 Gatsby 关系最密切的技术之一是GraphQL。它包括自己的 GraphQL 服务器，以提供有关服务器和连接源的元数据。 这个框架的一个特别亮点是其开源社区的规模，它构建了一个庞大的插件库，让您站在巨人的肩膀上。 这是 Gatsby404页面的样子：

```javascript
javascript
复制代码import * as React from "react"
import { Link } from "gatsby"
import { StaticImage } from "gatsby-plugin-image";


const pageStyles = {
 color: "#232129",
 padding: "96px",
 fontFamily: "-apple-system, Roboto, sans-serif, serif",
}
const headingStyles = {
 marginTop: 0,
 marginBottom: 64,
 maxWidth: 320,
}

const paragraphStyles = {
 marginBottom: 48,
}
const codeStyles = {
 color: "#8A6534",
 padding: 4,
 backgroundColor: "#FFF4DB",
 fontSize: "1.25rem",
 borderRadius: 4,
}

const NotFoundPage = () => {
 return (
   <main style={pageStyles}>
     <h1 style={headingStyles}>Page not found</h1>
     <StaticImage alt="Clifford, a reddish-brown pitbull"
       src="https://pbs.twimg.com/media/E1oMV3QVgAIr1NT?format=jpg&name=large"
     />

     <p style={paragraphStyles}>
       Sorry 😔, we couldn't find what you were looking for.
       

       {process.env.NODE_ENV === "development" ? (
         <>
           Try creating a page in <code style={codeStyles}>src/pages/</code>.     
         </>
       ) : null}
       <Link to="/">Go home</Link>.
     </p>
   </main>
 )
}

export default NotFoundPage
```

凭借其支持的渲染方式，Gatsby 可以很好地实现[Jamstack Sites](https://link.juejin.cn?target=https%3A%2F%2Fjamstack.org%2F) 网站。这是Jamstack 上的一个很好的资源，它解释了将 Web 体验层与数据和业务逻辑分离的应用程序的好处。使用 Gatsby 构建的站点通常用于提供出色用户体验的“内容消费”，例如公司的网站。 该框架不支持自定义后端逻辑，因此您应该寻找其他框架，例如 Next.js。

### 4.2 Next

Next.js 框架最初于 2016 年发布，在许多方面与 Gatsby 相似，只是增加了一些[边缘计算功能](https://link.juejin.cn?target=https%3A%2F%2Fvercel.com%2Fdocs%2Fconcepts%2Ffunctions%2Fedge-functions)。这些功能包括边缘功能、自定义代码的无服务器执行，以及可用于将某些请求重定向到其他域的[边缘中间件](https://link.juejin.cn?target=https%3A%2F%2Fnextjs.org%2Fdocs%2Fadvanced-features%2Fmiddleware)。这使您可以在全球范围内运行无服务器 Next.js 应用程序，而无需担心服务器管理。Next.js 支持静态站点生成、服务器端渲染、增量静态重新生成和客户端数据获取。 Next.js 中的主页

```javascript
javascript
复制代码import Head from 'next/head'
import Image from 'next/image'
import styles from '../styles/Home.module.css'

export default function Home() {
 return (
   <div className={styles.container}>
     <Head>
       <title>Create Next App</title>
       <meta name="description" content="Generated by create next app" />
       <link rel="icon" href="/favicon.ico" />
     </Head>

     <main className={styles.main}>
       <h1 className={styles.title}>
         Welcome to <a href="https://nextjs.org">Next.js!</a>
       </h1>
       <p className={styles.description}>
         Get started by editing{' '}
         <code className={styles.code}>pages/index.js</code>
       </p>
     </main>
     <footer className={styles.footer}>
       <a
         href="https://vercel.com?utm_source=create-next-app&utm_medium=default-template&utm_campaign=create-next-app"
         target="_blank"
         rel="noopener noreferrer"
       >
         Powered by{' '}
         <span className={styles.logo}>
           <Image src="/vercel.svg" alt="Vercel Logo" width={72} height={16} />
         </span>
       </a>
     </footer>
   </div>
 )
}
```

边缘函数：

```javascript
javascript
复制代码const twilio = require("twilio");

export default function handler(req, res) {
 const twiml = new twilio.twiml.MessagingResponse();
 twiml.message("Hi, I'm a TwiML response");
 res.setHeader("Content-Type", "text/xml");
 res.status(200).send(twiml.toString());
}
```

Next.js 的一个独特之处在于其巨大的知名度、庞大的社区和广泛的使用。该框架允许您将其托管在自己的基础设施上，但您也可以将其部署到多个边缘网络使用外包的服务器管理工作。 另一方面，这种无服务器性质也可能是一个缺点。框架中没有状态的概念；因此，如果不手动添加数据库，就无法轻松地存储状态。

### 4.3 Nuxt

Nuxt 最初于 2016 年发布，在多个方面与之前的框架不同。最明显的区别是不使用 React 进行构建，而是使用另一个流行的 UI 框架：Vue.js。但这并不意味着 Nuxt 是 Next.js 的移植版。Nuxt 在概念上非常不同，它提供所有渲染方式——包括客户端渲染。所以这个框架也可以用于渐进式Web应用。 最重要的是，它带有一个丰富的向导程序，可以针对不同的配置、测试框架、构建框架和组件包提出问题。所以它也可以看作是框架的框架。Nuxt 还带有状态存储功能：Vuex Store。 Nuxt 页面定义

```javascript
javascript
复制代码<template>
 <div>
   <h1>Hello Nuxters! 👋</h1>
   <p>
     This page is rendered on the <strong>{{ rendering }}</strong>
   </p>
   <p v-if="rendering === 'server'">
     First load or hard refresh is done on server side.
   </p>
   <p v-if="rendering === 'client'">Navigation is done on client side.</p>
   <ul>
     <li>Refresh the page for server side rendering.</li>
     <li>Click the links to see client side rendering.</li>
   </ul>

   <NuxtLink to="/about">About Page</NuxtLink>
 </div>
</template>
<script>
export default {
 asyncData() {
   return {
     rendering: process.server ? 'server' : 'client'
   }
 }
}
</script>
```

为避免混淆，值得强调的是 Nuxt 提供了许多用于开发网站的选项。但这并不意味着它是一个用于创建您自己的服务器应用程序的框架，这些应用程序需要处理除 HTTP 之外的对象关系映射器或协议。 :::tips 有一个很好的记忆技巧来区分这些听起来相似的框架：N **e** xt.js 使用 R **e** act 而 N **u** xt 使用 V **u** e.js。 :::

### 4.4 Nest

Nest 是 2018 年受 Angular 启发的框架，是本系列中真正的异类。尽管所有其他框架都实现了各种渲染策略，但 Nest 与前端处于不同的领域。这个服务端应用的框架更类似于大名鼎鼎的Spring框架，可以用来实现微服务。它通过对象关系映射器和许多附加协议（例如WebSockets、GraphQL 和 MQTT）支持几乎所有流行的数据库管理系统。 在底层，Nest 使用 Express，但也提供与广泛的其他库的兼容性，例如Fastify，允许方便地使用第三方插件。但开发者最喜欢的功能是通过模块的简洁代码重用机制。模块允许您将可以在应用程序的多个部分重用或作为库与整个 Nest 社区共享的功能外部化。 Nest 应用程序中的模块用法：

```javascript
javascript
复制代码import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { ConfigModule } from "@nestjs/config";

@Module({
 imports: [ConfigModule.forRoot()],
 controllers: [AppController],
 providers: [AppService],
})
export class AppModule {}
```

重要的是要记住，这个框架在默认情况下与任何前端框架无关。这意味着您需要手动将该**文件夹**与该项目目录中的前端项目**集成（或者更好：在构建脚本中自动执行此操作）。dist**这些类型的应用程序通常需要处理更复杂的计算，因此具有**更高的服务器负载**并且更难扩展。 现实世界的流行度 检查了来自 npm 的下载数量以及来自 GitHub 和 Stack Overflow 的一些社区统计数据。该数据截至 2022 年 12 月是准确的。 ![image.png](https://p.ipic.vip/8f140r.jpg) 注：* 为版本 3 启动了一个新的存储库

## 5.如何选择

既然我们对这些框架有了更多的了解，让我们重新审视这篇文章的开头。哪个框架最适合开发项目？要回答这个问题，您需要问自己以下问题： :::tips

- 您是否要构建全栈应用程序？
- 如果是这样，您是想要更多的自由和灵活性，还是更喜欢开箱即用的模块（如果首先可用），您可以插入这些模块以获得所需的结果？
- 如果应用程序不需要全栈开发，根据您对性能和 SEO 友好性的需求，您更喜欢哪种渲染策略？ ::: 此决策树包括以下问题，将帮助您专注于您可能想要考虑的框架： ![image.png](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/aac11f0b8f434ff5bfb0110dffa6bd1e~tplv-k3u1fbpfcp-zoom-in-crop-mark:1512:0:0:0.awebp) **个人的建议：** 如果您想学习新知识，建议您查看Next.js。如果您想构建一个使用 React 的网站，并且具有信息丰富但没有大量交互的网站，例如公司网站、博客或产品的登录页面，这非常好。如果你想构建这样一个网站但更喜欢 Vue.js，那么Nuxt将适合你。 如果符合您的需要，用React或Vue.js构建一个没有后端框架的SPA也是可以的。另一方面，当你想构建一个服务器应用程序时，有必要评估您是想要使用提供即插即用机制(Nest)的已建立模块构建，还是希望能够使用Express自行构建所有内容。

## 参考：

[segmentfault.com/a/119000004…](https://link.juejin.cn?target=https%3A%2F%2Fsegmentfault.com%2Fa%2F1190000040966032%23item-1) [juejin.cn/post/713230…](https://juejin.cn/post/7132309322544971813) [juejin.cn/post/715529…](https://juejin.cn/post/7155294090077634573) [www.jianshu.com/p/9ec9b1f46…](https://link.juejin.cn?target=https%3A%2F%2Fwww.jianshu.com%2Fp%2F9ec9b1f46a5e)

作者：WhaleFE
链接：https://juejin.cn/post/7218739623245774885
来源：稀土掘金
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。