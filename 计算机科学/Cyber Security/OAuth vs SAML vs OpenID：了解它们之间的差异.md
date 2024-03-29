[TOC]

**简介：** 身份验证允许进入系统，而授权允许访问同一系统内的特定功能。安全断言标记语言 (SAML) 是一种开放标准，它试图弥合身份验证和授权之间的鸿沟。

身份验证允许进入系统，而授权允许访问同一系统内的特定功能。安全断言标记语言 (SAML) 是一种开放标准，它试图弥合身份验证和授权之间的鸿沟。
OAuth 是一个开放的授权标准。OpenID Connect 是在 OAuth 2.0 之上运行的身份验证标准。下面详细介绍这些标准的差异及其在身份验证和授权中的作用。

## 什么是 SAML？

SAML 2.0 是[一个开放标准，用于](https://en.wikipedia.org/wiki/Security_Assertion_Markup_Language)在主体、服务提供者和身份提供者这三个参与者之间传递身份验证和授权信息。

主体是用户，服务提供者是网络资源的所有者，身份提供者执行身份访问管理服务。今天的大多数 SAML 实现仍然支持以前的版本 SAML 1.1，以实现向后兼容性。

SAML 将其留给身份提供者使用合适的身份验证方法，尽管它建议实施传输级安全性和消息级安全性。相反，该标准强制使用基于 XML 的消息在服务提供者和身份提供者之间传递信息。这些消息称为安全断言，可以是以下三种类型中的任何一种，即：

- **身份验证语句：**向服务提供者确认身份提供者已对主体进行身份验证的断言。
- **属性声明：**包含一个或多个属性或名称-值对，在此基础上授予主体访问权限。
- **授权决策声明：**一旦主体有权访问服务提供商提供的 Web 资源，就可以执行一项或多项操作的断言。

SAML 故意限制各方可以使用 XML 断言执行的操作。基于 XML 的可扩展访问控制标记语言 (XACML) 可用于细粒度访问控制。

## SAML 如何工作？

典型的 SAML 交换涉及委托人，通过 Web 浏览器或其他一些所谓的用户代理，尝试访问服务提供商的 Web 资源。

- 服务提供者使用 Web 浏览器向身份提供者发送身份验证请求。
- 身份提供者可以要求用户提供用户名或密码或两者。
- 在验证用户身份后，身份提供者将身份验证声明连同用户凭证一起发送回服务提供者。
- 根据来自身份提供者的消息，服务提供者允许或禁止用户尝试访问其服务。

SAML 简化了[联合身份验证和授权](https://www.hackedu.com/blog/analysis-of-common-federated-identity-protocols-openid-connect-vs-oauth-2.0-vs-saml-2.0)的实施，这涉及使用单个身份提供者跨多个组织和安全域的多个服务提供者。联合标识的一个示例是单点登录 (SSO)。在 SAML 中，SSO 采用允许访问多个应用程序的浏览器会话 cookie 的形式。

## 什么是 OAuth？

OAuth 是一种[开放式授权标准，](https://en.wikipedia.org/wiki/OAuth)通过访问令牌授予对应用程序、设备、应用程序编程接口 (API) 和服务器的安全委托访问权限。OAuth 授权应用程序访问您的数据，而无需授予它访问您的凭据的权限。

在 OAuth 之前，HTTP 基本身份验证是获取系统访问权限的最常用形式，只需要使用用户名和密码。SSO 是在 SAML 流行时出现的，但 SAML 的问题在于它并不特别适合单页应用程序和现代 Web 应用程序，这些应用程序通过异步 JavaScript 和 XML (AJAX) 和 Web 服务对 API 进行后台 HTTP 调用。此外，SAML 也不适用于手机、智能电视和物联网。OAuth 使用 JSON 数据包和 API 调用，是现代 Web 的理想选择。

典型的 OAuth 工作流程涉及三个参与者，即用户、消费者和服务提供者。

- 工作流从用户向服务提供者展示执行涉及消费者的操作的意图开始。
- 消费者从服务提供者那里获得一个请求令牌和一个秘密，将它们交给用户，然后将用户重定向到服务提供者，以便前者可以使用后者显式地授权请求令牌。
- 然后服务提供者授予一个访问令牌，允许消费者代表用户访问受保护的资源。

刷新令牌通常是长期存在的，可用于从服务提供商处获取新的访问令牌。另一方面，访问令牌是短暂的。

OAuth 不向后兼容早期的 OAuth 1.0a。OAuth 2.0 使用更广泛，尽管 OAuth 1.0a 上仍有一些。如果您正在考虑使用 OAuth，建议使用 OAuth 2.0。除了更容易实现之外，OAuth 2.0 的另一个优点是参与者之间传递的令牌在传输过程中被加密。

## 什么是 OpenID Connect？

OpenID Connect基于 OpenID[分散式身份验证协议](https://en.wikipedia.org/wiki/OpenID)，在 OAuth 2.0 之上提供了一个身份验证层。它解决了 OAuth 中缺乏身份验证机制的问题，这在授权敏感交易（如支付）时是一个弱点。

典型的[OpenID Connect](https://en.wikipedia.org/wiki/OpenID_Connect)工作流程涉及三方，即客户端、最终用户和身份提供者。

- 客户端或依赖方将最终用户发送给身份提供者，最终用户在身份提供者处验证身份并授权对客户端的访问。
- 身份提供者向客户端发送一个授权码，然后客户端使用它向身份提供者请求访问权和 ID 令牌。
- 一旦客户端获得令牌，它就可以代表最终用户执行操作。

OpenID Connect 使用签名且可加密验证的 JSON Web 令牌来确保访问和 ID 令牌在各方之间交换信息期间不被篡改。

## OAuth、SAML 和 OpenID Connect 之间有什么区别？

虽然 OAuth、SAML 和 OpenID Connect 的用例多种多样，但从功能上讲，OAuth 用于访问授权，而 SAML 和 OpenID Connect 用于用户认证。因此，OAuth用于与 SAML 和 OpenID Connect[截然不同的情况。](https://www.cloudflare.com/en-gb/learning/access-management/what-is-oauth/)它也可以与 SAML 或 OpenID Connect 一起使用。

#### OAuth

当您让应用程序（例如 Trello）访问您的 Gmail 联系人时，您可能使用了 OAuth。在这种情况下，您是用户，Trello 是消费者，Gmail 是服务提供者。Gmail 提供了允许 Trello 访问您的联系人的令牌。

#### OpenID Connect

对于 OpenID Connect，如果您使用 Facebook 或其他应用程序在另一个应用程序中验证了您的帐户，那么您可能已经使用过它。您登录身份提供商 Facebook 以访问第三方应用程序（例如 Spotify）。您可能已登录 Facebook，但您的凭据安全地存储在 Facebook 中，以防 Spotify 被黑客入侵时免受任何潜在威胁。

#### SAML

SAML 主要用于企业。许多组织使用它来将用户登录到内部网络。登录后，您无需输入凭据即可访问网络中的应用程序。



- https://developer.aliyun.com/article/1080044