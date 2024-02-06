[TOC]





- https://www.okta.com/identity-101/saml-vs-oauth/





安全断言标记语言 (SAML) 是一个身份验证过程。早上去上班并登录您的计算机，您可能已经使用过 SAML。

开放授权（OAuth）是一个授权过程。使用它可以从一项服务跳转到另一项服务，而无需输入新的用户名和密码。如果您登录 Google 并使用 Hootsuite 的这些凭据，则您已经使用了 OAuth。

这两个应用程序都可用于 Web [单点登录](https://www.okta.com/blog/2016/10/what-is-single-sign-on/) (SSO)，但 SAML 往往特定于用户，而 OAuth 往往特定于应用程序。两者不可互换，因此我们不会进行直接比较，而是讨论它们如何协同工作。

## SAML 是如何工作的？

[SAML](https://www.okta.com/topic/SAML/) 是一种开放标准，用于验证身份并提供身份验证。在典型的办公环境中，员工必须登录才能访问公司内部功能的任何部分。

完成 SAML 身份验证后，用户可以访问整套工具，包括企业内部网、Microsoft Office 和浏览器。 SAML 允许用户通过一个数字签名访问所有这些资源。

或者在安全性更严格的公司中，SAML 只允许用户打开门或解锁计算机屏幕。用户必须先获得授权才能执行任何其他操作（包括访问文件）。

网络管理员可以使用 SAML [从中央位置管理用户](https://www.sciencedirect.com/science/article/pii/S2212017312002988)。一个密码可以解锁一个人所需的所有服务，同时也可以保护公司的安全。

![SAML图](https://p.ipic.vip/s6vlil.png)

典型的 SAML 工作流程如下所示：

- **请求：** 用户点击“登录”按钮。
- **验证：** SAML 和身份提供者连接以进行身份验证。
- **登录：** 用户看到一个等待用户名和密码数据的屏幕。
- **令牌创建：** 如果用户输入正确的信息，SAML 令牌就会移至服务提供商，从而允许用户登录服务器。

此工作流程允许服务提供商、浏览器和身份提供商无缝地交易信息。用户甚至可能不会注意到延迟，因为此过程通常在几秒钟内处理完毕。

## OAuth 如何工作？

虽然“auth”可以表示身份验证或授权，但对于 [OAuth](https://www.okta.com/blog/2019/04/oauth-when-things-go-wrong/) 协议，我们指的是专门的授权。该协议用于将授权从一项服务传递到另一项服务，同时保护某人的用户名和密码。

[在平均员工每天](https://www.techrepublic.com/article/employees-switch-apps-more-than-1100-times-a-day-decreasing-productivity/)切换工作关键型应用程序高达 1,100 次的环境中，OAuth 可以看作是一个关键的时间节省者 。有时，员工希望有一种方法可以从一个应用程序跳转到另一个应用程序而无需再次登录。 OAuth 使这成为可能。

考虑一名拥有活跃 Google 帐户的员工。该人可以使用相同的凭据来访问在以下位置找到的数据：

- 胡特套件
- 调查猴子
- 热罐
- 微软365
- 销售队伍
- 市场
- 盒子

员工需要所有这些基于网络的程序才能正确完成工作。但同一个人可能会对创建（并记住）五组不同的用户名和密码感到不寒而栗。

复制用户名和密码是一场安全赌博。如果一个站点发生故障，用户的关键数据就会在所有平台上暴露并容易受到攻击。但是使用第一个网站提供的验证登录另一个网站是非常不同的。

一些消费者担心数据挖掘，他们建议使用这样的工具会给 Facebook 这样的公司带来 [太多权力](https://mashable.com/article/stop-syncing-contacts-with-facebook/)。每次用户选择 Facebook 登录其他应用程序和网站时，Facebook 都会获得更多客户洞察。如果 Facebook 的数据遭到泄露，该人的额外登录也可能会失败。

但大多数员工都会感谢能够在繁忙、压力大的时期节省时间。

![OAuth 选项](https://p.ipic.vip/ux9a75.png)

OAuth 工作流程如下所示：

- **请求：** 用户单击网页上的“登录”按钮。
- **选择：** 客户端选择要使用的第三方授权凭证。
- **登录：** 授权服务器创建访问令牌，并将其发送到资源服务器。
- **连接：** 资源服务器验证令牌后，授予访问权限。

在整个过程中，两台服务器来回传递信息。通常，OAuth 使用 JWT 作为令牌，但它也可以使用 JavaScript 对象表示法。

无论它们是如何创建的，令牌总是经过编码，通常是签名，但在从一台服务器传递到另一台服务器时很少进行加密。

## OAuth 与 SAML：异同

OAuth 和 SAML 都是鼓励和标准化互操作性的协议。

人们使用这些工具来避免不断扩大的用户名和密码列表阻止他们访问关键资源。对于应用程序所有者来说，OAuth 和 SAML 可以轻松入门并能够委派用户管理。对于管理员来说，这些工具意味着快速集成以及集中身份验证和授权。

但这两个工具处理的 [功能截然不同](https://medium.com/datadriveninvestor/authentication-vs-authorization-716fea914d55) ，包括：

- **验证。** 这个过程涉及到用户的身份。 SAML 有点像房子钥匙。它允许您访问该设施。
- **授权。** 这个过程涉及到用户的权限。 OAuth 有点像房子的规则，规定了人们进入后可以做什么和不可以做什么。

<iframe allow="autoplay; fullscreen; picture-in-picture; camera; microphone; display-capture; clipboard-write" allowfullscreen="" allowtransparency="true" referrerpolicy="no-referrer-when-downgrade" class="vidyard-iframe-oz2V9Mw353UbaT5TcmdToV" frameborder="0" height="100%" width="100%" scrolling="no" src="https://play.vidyard.com/oz2V9Mw353UbaT5TcmdToV?disable_popouts=1&amp;v=4.3.14&amp;type=inline&amp;cc=en" title="OAuth 2.0 和 OpenID Connect（简单英语）" style="box-sizing: border-box; margin: 0px; padding: 0px; border: 0px; font-style: inherit; font-variant-ligatures: inherit; font-variant-caps: inherit; font-variant-alternates: inherit; font-variant-numeric: inherit; font-variant-east-asian: inherit; font-variant-position: inherit; font-weight: inherit; font-stretch: inherit; line-height: inherit; font-family: inherit; font-optical-sizing: inherit; font-kerning: inherit; font-feature-settings: inherit; font-variation-settings: inherit; font-size: 16px; vertical-align: baseline; opacity: 1; background-color: transparent; top: 0px; left: 0px;"></iframe>

为了进一步分解这个问题，请考虑一名员工的平均工作日。该人在早上使用 SAML 登录一次。该登录授予对整套基于 SAML 的应用程序的访问权限。用户无需再进行从一处单击到另一处的操作。

## 什么时候应该使用 SAML 或 OAuth？

SAML 和 OAuth 都提供了[SSO 机会](https://www.okta.com/identity-101/sso-strategy/)，它们对于高效的员工至关重要。它们并不完全是替代品，更像是可以协同工作的技术。

例如，在[Microsoft 环境](https://docs.microsoft.com/en-us/azure/active-directory/develop/authentication-vs-authorization)中，OAuth 处理授权，SAML 处理身份验证。您可以同时使用这两者来授予访问权限（通过 SAML）并允许访问受保护的资源（通过 OAuth）。

您也可以消除这两种工具。例如，某些网页[不需要](https://www.bu.edu/tech/about/security-resources/bestpractice/auth/)身份验证或授权。

但大多数拥有数字系统的企业都需要某种类型的身份验证和授权系统才能有效运行。必须允许用户在完成日常工作时登录并在公司的系统中移动。

## OpenID Connect (OIDC) 怎么样？

如果您正在为消费者开发辅助工具（例如应用程序或门户），OAuth 可能很重要。您的市场可能会喜欢有机会进入您的工具而无需创建新的用户名和密码。如果您的员工使用非 SAML 工具，OAuth 可能会对他们有所帮助。

但为了与 SAML 进行真正的比较，您需要探索[SAML、OAuth 和 OpenID Connect 之间的区别](https://www.okta.com/identity-101/whats-the-difference-between-oauth-openid-connect-and-saml/)。

## 与 Okta 合作

Okta 以其 SSO 服务而闻名，该服务允许您对日常使用的应用程序进行无缝身份验证。安全单点登录通常使用 SAML 作为选择的协议，但 Okta 还提供了其他几个选项，包括登录小部件、Auth SDK（基于 JavaScript 的库）、社交登录以及适用于任何客户端的身份验证 API。

在此处了解有关[Okta 预构建身份解决方案的](https://www.okta.com/resources/whitepaper-pre-built-identity-solution)更多信息。

## 参考

[单点登录技术调查](https://www.sciencedirect.com/science/article/pii/S2212017312002988)。 （2012）。*普罗塞迪亚科技。* 

[员工每天切换应用程序超过 1,100 次，降低工作效率](https://www.techrepublic.com/article/employees-switch-apps-more-than-1100-times-a-day-decreasing-productivity/)。 （2018 年 12 月）。科技共和国。 

[停止与 Facebook 同步您的联系人](https://mashable.com/article/stop-syncing-contacts-with-facebook/)。 （2019 年 8 月）。可混搭。 

[身份验证与授权](https://medium.com/datadriveninvestor/authentication-vs-authorization-716fea914d55)。 （2018 年 9 月）。中等的。

[身份验证与授权](https://docs.microsoft.com/en-us/azure/active-directory/develop/authentication-vs-authorization)。 （2020 年 5 月）。微软。 

[为什么选择 SAML？ （安全断言标记语言）](https://medium.com/@winma.15/why-saml-security-assertion-markup-language-3d961a333fd7)。 （2018 年 7 月）。中等的。

[了解身份验证、授权和加密](https://www.bu.edu/tech/about/security-resources/bestpractice/auth/)。波士顿大学。





Security assertion markup language (SAML) is an authentication process. Head to work in the morning and log into your computer, and you've likely used SAML.

Open authorization (OAuth) is an authorization process. Use it to jump from one service to another without tapping in a new username and password. If you're logged into Google and used those credentials for Hootsuite, you've used OAuth.

Both applications can be used for web [single sign on](https://www.okta.com/blog/2016/10/what-is-single-sign-on/) (SSO), but SAML tends to be specific to a user, while OAuth tends to be specific to an application. The two are not interchangeable, so instead of an outright comparison, we’ll discuss how they work together.

## How Does SAML Work?

[SAML](https://www.okta.com/topic/SAML/) is an open standard that verifies identity and offers authentication. In a typical office environment, an employee must log on to gain access to any part of the company's inner functions.

With SAML authentication complete, the user may have access to an entire suite of tools, including a corporate intranet, Microsoft Office, and a browser. SAML allows the user to tap into all of these resources under one digital signature.

Or in companies with tighter security, SAML only allows the user to open a door or unlock a computer screen. Authorization is required before the user can do anything else, including accessing files.

Network administrators can use SAML to [manage users from a central location](https://www.sciencedirect.com/science/article/pii/S2212017312002988). One password unlocks all the services a person needs, and it protects the company's security too.

![SAML diagram](https://www.okta.com/sites/default/files/styles/tinypng/public/media/image/2021-03/SAML_diagram_0.png?itok=HMCBOTBA)

A typical SAML workflow looks like this:

- **Request:** A user taps on a "Log in" button.
- **Validation:** The SAML and the identity provider connect for authentication.
- **Login:** The user sees a screen waiting for username and password data.
- **Token creation:** If the user enters the right information, a SAML token moves to the service provider, which allows the user to log into the server.

This workflow allows a service provider, a browser, and an identity provider to trade information seamlessly. The user may not even notice the delay, as this process is typically handled in seconds.

## How Does OAuth Work?

While “auth” can mean Authentication or Authorization, for the [OAuth](https://www.okta.com/blog/2019/04/oauth-when-things-go-wrong/) protocol, we mean specifically authorization. This protocol is used to pass authorization from one service to another, all while protecting someone's username and password.

Think of OAuth as a critical timesaver in an environment where the average employee switches job-critical applications a [whopping 1,100 times per day](https://www.techrepublic.com/article/employees-switch-apps-more-than-1100-times-a-day-decreasing-productivity/). Sometimes, employees want a way to jump from one app to another without logging in again. OAuth makes that possible.

Consider an employee with an active Google account. That person could use the same credentials to tap into data found on:

- Hootsuite
- SurveyMonkey
- HotJar
- Microsoft 365
- Salesforce
- Marketo
- Box

The employee needs all of these web-based programs to do the job right. But that same person may shudder at creating (and remembering) five different sets of usernames and passwords.

Duplicating the usernames and passwords is a security gamble. If one site fails, the user's critical data is exposed and vulnerable on all the platforms. But logging into another site with validation provided by the first is very different.

Some consumers worry about datamining, and they suggest using a tool like this gives companies like Facebook [too much power](https://mashable.com/article/stop-syncing-contacts-with-facebook/). Each time a user selects a Facebook login for other apps and sites, Facebook gains more customer insight. And if Facebook's data is compromised, that person's additional logins could fail too.

But most employees would be thankful for the ability to save time during busy, stressful periods.

![OAuth option](https://www.okta.com/sites/default/files/styles/tinypng/public/media/image/2021-03/Oauth_Option.png?itok=jQd3GXUN)

An OAuth workflow looks like this:

- **Request:** A user clicks on a "Log in" button on a web page.
- **Choice:** The client chooses the third-party authorization credentials to use.
- **Log in:** The authorization server creates an access token, and that’s sent to the resource server.
- **Connection:** After verifying the token, the resource server grants access.

Throughout this process, the two servers are passing information back and forth. Typically, OAuth uses JWT for tokens, but it can also use JavaScript Object Notation instead.

No matter how they are created, tokens are always encoded, usually signed, but rarely encrypted as they pass from one server to another.

## OAuth vs. SAML: Similarities and Differences

Both OAuth and SAML are protocols to encourage and standardize interoperability.

People use these tools to avoid an ever-expanding list of usernames and passwords that block them from accessing critical resources. For app owners, OAuth and SAML allow for easy onboarding and the ability to delegate user management. For admins, these tools mean fast integration and centralized authentication and authorization.

But the two tools handle [very different functions](https://medium.com/datadriveninvestor/authentication-vs-authorization-716fea914d55) involving:

- **Authentication.** This process involves a user's identity. SAML is a bit like a house key. It grants you access to the facility.
- **Authorization.** This process involves a user's privileges. OAuth is a bit like the rules of the house that dictate what the person can and can't do once inside.


To break this down further, consider an employee on an average workday. That person logs in one time in the morning with SAML. That login grants access to the entire suite of SAML-based applications. No more work is required for the user to click from one to the other.

## When Should You Use SAML or OAuth?

Both SAML and OAuth allow for [SSO opportunities](https://www.okta.com/identity-101/sso-strategy/), and they're critical for productive employees. They’re not exactly alternatives, more like technologies that can work together.

In the [Microsoft environment](https://docs.microsoft.com/en-us/azure/active-directory/develop/authentication-vs-authorization), for example, OAuth handles authorization, and SAML handles authentication. You could use the two at the same time to grant access (via SAML) and allow access to a protected resource (via OAuth).

You could also eliminate both of these tools. Some web pages, for example, [don't require either](https://www.bu.edu/tech/about/security-resources/bestpractice/auth/) authentication or authorization.

But most businesses with digital systems need some type of authentication and authorization system to function effectively. Users must be allowed to sign in and move throughout the company's systems as they complete their daily work.

## What About OpenID Connect (OIDC)?

OAuth could be important if you're developing a secondary tool for consumers, such as apps or portals. Your market might appreciate the opportunity to get inside your tools without creating a new username and password. And OAuth could be helpful for your employees if they use non-SAML tools.

But for a true comparison with SAML, you’ll want to explore [the difference between SAML, OAuth, and OpenID Connect](https://www.okta.com/identity-101/whats-the-difference-between-oauth-openid-connect-and-saml/).

## Work With Okta

Okta is best known for its SSO services that allow you to seamlessly authenticate to the applications you use on a daily basis. Secure single sign-on often uses SAML as the protocol of choice, but Okta also provides several other options, including a Sign-in Widget, Auth SDK (a JavaScript-based library), Social Login, and an Authentication API for any client.

Learn more about [Okta’s pre-built identity solutions here](https://www.okta.com/resources/whitepaper-pre-built-identity-solution).

## References

[A Survey on Single Sign-On Techniques](https://www.sciencedirect.com/science/article/pii/S2212017312002988). (2012). *Procedia Technology.* 

[Employees Switch Apps More Than 1,100 Times a Day, Decreasing Productivity](https://www.techrepublic.com/article/employees-switch-apps-more-than-1100-times-a-day-decreasing-productivity/). (December 2018). TechRepublic. 

[Stop Synching Your Contacts with Facebook](https://mashable.com/article/stop-syncing-contacts-with-facebook/). (August 2019). Mashable. 

[Authentication vs. Authorization](https://medium.com/datadriveninvestor/authentication-vs-authorization-716fea914d55). (September 2018). Medium.

[Authentication vs. Authorization](https://docs.microsoft.com/en-us/azure/active-directory/develop/authentication-vs-authorization). (May 2020). Microsoft. 

[Why SAML? (Security Assertion Markup Language)](https://medium.com/@winma.15/why-saml-security-assertion-markup-language-3d961a333fd7). (July 2018). Medium.

[Understanding Authentication, Authorization, and Encryption](https://www.bu.edu/tech/about/security-resources/bestpractice/auth/). Boston University.