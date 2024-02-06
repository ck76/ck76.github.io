[TOC]





<img src="https://p.ipic.vip/hesnif.jpg" alt="1707230495188" style="zoom:63%;" />





## SAML

![1707231684640](https://p.ipic.vip/4oqe7s.jpg)



![continuous_deployment](https://p.ipic.vip/e2p7h1.jpg)



## OIDC

![1707230768871.jpeg](https://p.ipic.vip/9tn6n1.png)





## OAuth

![1707230352129.jpeg](https://p.ipic.vip/8acm9x.png)



![continuous_deployment](https://p.ipic.vip/runbgx.jpg)





# What’s the Difference Between OAuth, OpenID Connect, and SAML?

- https://www.okta.com/identity-101/whats-the-difference-between-oauth-openid-connect-and-saml/

There are as many ways to keep data safe as there are ways to attack it. From multi-factor authentication to single sign-on to on-premises firewalls, the options can be staggering. For developers and IT professionals, the choice of how to keep data and identities secure begins even sooner: choosing the standard that should be deployed to keep [federated identity](https://www.okta.com/identity-101/what-is-federated-identity/) safe.

The decision isn’t always a straightforward one. Many struggle to distinguish between OAuth 2.0, OpenID Connect, and Security Assertion Markup Language (SAML), each of which brings structure to the federation process. This article brings clarity on what these standards mean, how they compare, and the purposes for which enterprises should use them.

We've also got a more focused comparison between [SAML vs OAuth](https://www.okta.com/identity-101/saml-vs-oauth/) in another article if that's what you're looking for.

## The Differences Between Standards

The main differentiator between these three players is that OAuth 2.0 is a framework that controls authorization to a protected resource such as an application or a set of files, while OpenID Connect and SAML are both industry standards for federated authentication. That means that OAuth 2.0 is used in fundamentally different situations than the other two standards (examples of which can be seen below), and can be used simultaneously with either OpenID Connect or SAML.

Using either OpenID Connect or SAML independently, enterprises can achieve user authentication and deploy single sign-on. Though they both deal with logins, they have different strengths and weaknesses.

- OpenID Connect is built on the OAuth 2.0 protocol and uses an additional JSON Web Token (JWT), called an ID token, to standardize areas that OAuth 2.0 leaves up to choice, such as scopes and endpoint discovery. It is specifically focused on user authentication and is widely used to enable user logins on consumer websites and mobile apps.
- SAML is independent of OAuth, relying on an exchange of messages to authenticate in XML SAML format, as opposed to JWT. It is more commonly used to help enterprise users sign in to multiple applications using a single login.

## OAuth 2.0 vs OpenID Connect vs SAML

Remember that it isn’t a question of which structure an organization should use, but rather of when each one should be deployed. A strong identity solution will use these three structures to achieve different ends, depending on the kind of operations an enterprise needs to protect. Their use cases are as follows:

**OAuth 2.0:** If you’ve ever signed up to a new application and agreed to let it automatically source new contacts via Facebook or your phone contacts, then you’ve likely used OAuth 2.0. This standard provides secure delegated access. That means an application can take actions or access resources from a server on behalf of the user, without them having to share their credentials. It does this by allowing the [identity provider (IdP)](https://www.okta.com/identity-101/why-your-company-needs-an-identity-provider/) to issue tokens to third-party applications with the user’s approval.

**OpenID Connect:** If you’ve used your Google to sign in to applications like YouTube, or Facebook to log into an online shopping cart, then you’re familiar with this authentication option. OpenID Connect is an open standard that organizations use to authenticate users. IdPs use this so that users can sign in to the IdP, and then access other websites and apps without having to log in or share their sign-in information. 

**SAML:** You’ve more likely experienced SAML authentication in action in the work environment. For example, it enables you to log into your corporate intranet or IdP and then access numerous additional services, such as Salesforce, Box, or Workday, without having to re-enter your credentials. SAML is an XML-based standard for exchanging authentication and authorization data between IdPs and service providers to verify the user’s identity and permissions, then grant or deny their access to services.

Enterprises rely on web frameworks and protocols like OAuth 2.0, OpenID, and SAML to bring structure and security to federated identity. Knowing when to use each is a key step towards protecting your organization’s data from the ground up.





保护数据安全的方法与攻击数据的方法一样多。从多重身份验证到单点登录再到本地防火墙，选项可能令人惊叹。对于开发人员和 IT 专业人员来说，如何保护数据和身份安全的选择甚至更早开始：选择应部署以保证[联合身份](https://www.okta.com/identity-101/what-is-federated-identity/)安全的标准。

决定并不总是那么简单。许多人很难区分 OAuth 2.0、OpenID Connect 和安全断言标记语言 (SAML)，它们都为联合过程带来了结构。本文阐明了这些标准的含义、它们的比较方式以及企业应使用它们的目的。

如果您正在寻找的话，我们还在另一篇文章中对[SAML 与 OAuth进行了更集中的比较。](https://www.okta.com/identity-101/saml-vs-oauth/)

## 标准之间的差异

这三个参与者之间的主要区别在于，OAuth 2.0 是一个控制对受保护资源（例如应用程序或一组文件）的授权的框架，而 OpenID Connect 和 SAML 都是联合身份验证的行业标准。这意味着 OAuth 2.0 的使用情况与其他两个标准完全不同（示例如下），并且可以与 OpenID Connect 或 SAML 同时使用。

企业可以单独使用OpenID Connect或SAML来实现用户身份验证并部署单点登录。尽管它们都处理登录，但它们有不同的优点和缺点。

- OpenID Connect 基于 OAuth 2.0 协议构建，并使用附加的 JSON Web 令牌 (JWT)（称为 ID 令牌）来标准化 OAuth 2.0 留给选择的区域，例如范围和端点发现。它特别关注用户身份验证，并广泛用于支持用户登录消费者网站和移动应用程序。
- SAML 独立于 OAuth，依赖于消息交换以 XML SAML 格式（而不是 JWT）进行身份验证。它更常用于帮助企业用户使用单次登录来登录多个应用程序。

## OAuth 2.0 与 OpenID Connect 与 SAML

请记住，这不是一个组织应该使用哪种结构的问题，而是何时应该部署每个结构的问题。强大的身份解决方案将使用这三种结构来实现不同的目标，具体取决于企业需要保护的操作类型。它们的用例如下：

**OAuth 2.0：** 如果您曾经注册过新应用程序并同意让它通过 Facebook 或您的手机联系人自动获取新联系人，那么您很可能使用过 OAuth 2.0。该标准提供安全的委派访问。这意味着应用程序可以代表用户执行操作或从服务器访问资源，而无需共享其凭据。它通过允许 [身份提供商 (IdP)](https://www.okta.com/identity-101/why-your-company-needs-an-identity-provider/) 在用户批准的情况下向第三方应用程序颁发令牌来实现此目的。

**OpenID Connect：** 如果您使用 Google 登录过 YouTube 等应用程序，或使用 Facebook 登录过在线购物车，那么您就会熟悉此身份验证选项。 OpenID Connect 是组织用来对用户进行身份验证的开放标准。 IdP 使用此功能，以便用户可以登录 IdP，然后访问其他网站和应用程序，而无需登录或共享其登录信息。 

**SAML：** 您更有可能在工作环境中经历过 SAML 身份验证。例如，它使您能够登录公司 Intranet 或 IdP，然后访问许多附加服务，例如 Salesforce、Box 或 Workday，而无需重新输入您的凭据。 SAML 是一种基于 XML 的标准，用于在 IdP 和服务提供商之间交换身份验证和授权数据，以验证用户的身份和权限，然后授予或拒绝他们对服务的访问权限。

企业依靠 OAuth 2.0、OpenID 和 SAML 等 Web 框架和协议来为联合身份带来结构和安全性。了解何时使用每种方法是从头开始保护组织数据的关键一步。