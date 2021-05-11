[TOC]

**问题一：**

什么是Spring？

Spring是Enterprise Java的开源开发框架。Spring Framework的核心功能可用于开发任何Java应用程序，但有一些扩展用于在Java EE平台之上构建Web应用程序。Spring框架的目标是通过启用基于POJO的编程模型，使Java EE开发更易于使用并促进良好的编程实践。

**问题二：**

Spring Framework有哪些优点？

轻巧：Spring在尺寸和透明度方面都很轻巧。spring框架的基本版本大约为2MB。

控制反转（IOC）： 使用反转控制技术在Spring中实现松耦合。对象提供依赖关系，而不是创建或查找依赖对象。

面向方面（AOP）： Spring支持面向方面的编程，并将应用程序业务逻辑与系统服务分开。

容器： Spring包含并管理应用程序对象的生命周期和配置。 MVC框架： Spring的Web框架是一个设计良好的Web MVC框架，它为Web框架提供了一个很好的替代方案。

事务管理： Spring提供了一致的事务管理界面，可以缩小到本地事务并扩展到全局事务（JTA）。

异常处理： Spring提供了一个方便的API，用于将特定于技术的异常（由JDBC，Hibernate或JDO抛出）转换为一致的，未经检查的异常。



**问题三：**

核心容器（应用程序上下文）模块是什么？

这是基本的Spring模块，它提供了Spring框架的基本功能。BeanFactory是任何基于spring的应用程序的核心。Spring框架是在这个模块的顶部构建的，它构成了Spring容器。



**问题四：**

AOP模块是什么？

AOP模块用于为我们的Spring应用程序开发方面。AOP联盟提供了大部分支持，以确保Spring与其他AOP框架之间的互操作性。该模块还向Spring引入了元数据编程。



**问题五：**

解释一下JDBC抽象和DAO模块

使用JDBC抽象和DAO模块，我们可以确保我们使数据库代码保持干净和简单，并防止因无法关闭数据库资源而导致的问题。它在几个数据库服务器给出的错误消息之上提供了一层有意义的异常。它还利用Spring的AOP模块为Spring应用程序中的对象提供事务管理服务。



**问题六：**

解释一下对象/关系映射集成模块

通过提供ORM模块，Spring还支持在直接JDBC上使用对象/关系映射（ORM）工具。Spring支持绑定到几个流行的ORM框架，包括Hibernate，JDO和iBATIS SQL Maps。Spring的事务管理支持每个ORM框架以及JDBC。



**问题七：**

web模块的作用是什么？

Spring Web模块构建在应用程序上下文模块上，提供适用于基于Web的应用程序的上下文。此模块还包含对几个面向Web的任务的支持，例如透明地处理文件上载的多部分请求以及将请求参数编程绑定到业务对象。它还包含与Jakarta Struts的集成支持。



**问题八：**

Spring MVC模块的作用是什么？

Spring提供了MVC框架来构建Web应用程序。Spring可以很容易地与其他MVC框架集成，但是Spring的MVC框架是更好的选择，因为它使用IoC来提供控制器逻辑与业务对象的清晰分离。使用Spring MVC，您可以声明性地将请求参数绑定到业务对象。



**问题九：**

什么是Spring配置文件？

Spring配置文件是一个XML文件。此文件包含类信息，并描述了这些类是如何配置和相互引入的。



**问题十：**

什么是Spring IoC容器？

Spring IoC负责创建对象，管理它们（使用依赖注入（DI）），将它们连接在一起，配置它们，以及管理它们的整个生命周期。



**问题十一：**

IOC有什么好处？

IOC或依赖注入最小化应用程序中的代码量。它使测试应用程序变得容易，因为在单元测试中不需要单例或JNDI查找机制。以最小的努力和最少的侵入机制促进松散耦合。IOC容器支持急切的实例化和延迟加载服务。



**问题十二：**

ApplicationContext的常见实现是什么？

该FileSystemXmlApplicationContext来容器从XML文件加载bean的定义。必须将XML bean配置文件的完整路径提供给构造函数。

该ClassPathXmlApplicationContext的容器还加载从XML文件java bean的定义。在这里，您需要正确设置CLASSPATH，因为此容器将在CLASSPATH中查找bean配置XML文件。

该WebXmlApplicationContext：容器从Web应用程序中加载的所有bean类定义的XML文件。



**问题十三：**

Bean Factory和ApplicationContext有什么区别？

ApplicationContex提供了一种解析文本消息的方法，一种加载文件资源（如图像）的通用方法，它们可以将事件发布到注册为侦听器的bean。此外，可以在应用程序上下文中以声明方式处理容器中的容器或容器上的操作，这些操作必须以编程方式与Bean Factory一起处理。ApplicationContex实现MessageSource，一个用于获取本地化消息的接口，实际的实现是可插入的。



**问题十四：**

什么是Spring中的依赖注入？

依赖注入是控制反转（IoC）的一个方面，它是一个通用概念，它可以用许多不同的方式表达。这个概念说你不创建你的对象，而是描述它们应该如何创建。您不能在代码中直接连接组件和服务，而是描述配置文件中哪些组件需要哪些服务。然后，一个容器（IOC容器）负责将其全部挂起。



**问题十五：**

有哪些不同类型的IoC（依赖注入）？

基于构造函数的依赖注入：当容器调用具有许多参数的类构造函数时，完成基于构造函数的DI，每个参数表示对其他类的依赖。 基于Setter的依赖注入：基于Setter的DI是在调用无参数构造函数或无参数静态工厂方法来实例化bean之后，通过容器调用bean上的setter方法来完成的。



**问题十六：**

Spring bean是什么？

Spring Beans是构成Spring应用程序主干的Java对象。它们由Spring IoC容器实例化，组装和管理。这些bean是使用提供给容器的配置元数据创建的，例如，以XML定义的形式。

在spring框架中定义的bean是singleton bean。如果指定为true，则bean标记中有一个名为“singleton”的属性，然后bean变为singleton，如果设置为false，则bean将成为原型bean。默认情况下，它设置为true。因此，spring框架中的所有bean都是默认的单例bean。



**问题十七：**

如何为Spring容器提供配置元数据？

为Spring容器提供配置元数据有三种重要方法：

- 基于XML的配置文件。
- 基于注释的配置。
- 基于Java的配置。



**问题十八：**

如何定义bean的范围？

在Spring中定义一个时，我们也可以为bean声明一个范围。它可以通过bean定义中的scope属性定义。例如，当Spring每次需要生成一个新的bean实例时，bean'sscope属性就是原型。另一方面，当每次需要Spring都必须返回相同的bean实例时，bean scope属性必须设置为singleton。



**问题十九：**

Spring支持的bean范围有哪些？

Spring Framework支持以下五个范围提供了五个范围：

在作用域的单重态中，Spring将bean定义范围限定为每个Spring IoC容器的单个实例。

在原型范围中，单个bean定义具有任意数量的对象实例。

在请求范围中，bean被定义为HTTP请求。此范围仅在Web感知的Spring ApplicationContext中有效。

在会话范围中，bean定义的范围限定为HTTP会话。此范围仅在Web感知的Spring ApplicationContext中有效。

在全局会话范围中，bean定义的范围限定为全局HTTP会话。这也是Web感知Spring ApplicationContext中使用的一种情况。 Spring Bean的默认范围是Singleton。



**问题二十：**

Spring Framework中的Singleton bean线程安全吗？

不，单例bean在Spring框架中不是线程安全的。



**问题二十一：**

解释Spring框架中的Bean生命周期

spring容器从XML文件中查找bean的定义并实例化bean。

Spring填充bean定义（DI）中指定的所有属性。

如果bean实现了StringNameAware接口，则spring将bean的id传递给setBeanName（）

如果Bean implementsBeanFactoryAware接口，spring将beanfactory传递给setBeanFactory（）

如果有任何与bean关联的beanBeanPostProcessors，则Spring调用postProcesserBeforeInitialization（）

如果bean implementsIntializingBean，则调用其afterPropertySet（）方法。

如果bean具有init方法声明，则调用指定的初始化方法。

如果有任何与Bean关联的BeanPostProcessors，则将调用它们的postProcessAfterInitialization（）方法。

如果bean实现了DisposableBean，它将调用destroy（）



**问题二十二：**

哪些是重要的bean生命周期方法？可以覆盖它们吗？

有两个重要的bean生命周期方法。第一个是setup，当bean加载到容器中时调用。第二种方法是拆卸方法，当从容器中卸载bean时调用该方法。

bean标记有两个重要的属性（init-method和destroy-method），您可以使用它们定义自己的自定义初始化和销毁方法。还有相应的注释（@PostConstruct和@PreDestroy）。



**问题二十三：**

Spring的内部beans是什么？

当bean仅用作另一个bean的属性时，可以将其声明为内部bean。Spring的基于XML的配置元数据提供了bean定义中元素的使用，以便定义所谓的内部bean。内部bean总是匿名的，它们总是作为原型。



**问题二十四：**

如何在Spring中注入Java Collection？

Spring提供以下类型的集合配置元素：

在允许重复的情况下，该类型用于注入值列表。 该类型用于连接一组值，但没有任何重复。 该类型用于注入名称 - 值对的集合，其中name和value可以是任何类型。 该类型可用于注入名称 - 值对的集合，其中名称和值都是字符串。



**问题二十五：**

什么是Bean wiring？

当bean在Spring容器中组合在一起时，接线或者bean接线就是这种情况。布线bean时，Spring容器需要知道需要什么bean以及容器应该如何使用依赖注入将它们绑定在一起。



**问题二十六：**

什么是bean auto wiring？

Spring容器能够自动连接协作bean之间的关系。这意味着可以通过检查BeanFactory的内容而不使用和元素来自动让Spring解析bean的协作者（其他bean）。



**问题二十七：**

解释不同的自动接线方式？

自动装配功能有五种模式，可用于指示Spring容器使用自动装配进行依赖注入：

no： 这是默认设置。应使用显式bean引用进行布线。

byName： 当自动装配byName时，Spring容器会查看在XML配置文件中autowireattribute设置为byName的bean的属性。然后，它尝试匹配并将其属性与配置文件中由相同名称定义的bean相连。

byType： 当按数据类型进行自动装配时，Spring容器会在XML配置文件中查看autowireattribute设置为byType的bean的属性。然后，如果属性的类型与配置文件中的一个bean名称匹配，则会尝试匹配并连接属性。如果存在多个这样的bean，则抛出致命异常。

构造函数：此模式类似于byType，但type适用于构造函数参数。如果容器中没有构造函数参数类型的一个bean，则会引发致命错误。

autodetect： Spring首先尝试通过构造函数使用autowire连接，如果它不起作用，Spring会尝试通过byType来自动装配。



**问题二十八：**

autowiring有限制吗？

autowiring的局限性是：

覆盖：您仍然可以使用和设置指定依赖项，这将始终覆盖自动装配。

基元数据类型： 您不能自动装配简单属性，例如基元，字符串和类。

令人困惑的性质：自动装配不如显式布线精确，因此如果可能，请使用明确的布线。



**问题二十九：**

什么是基于Spring Java的配置？给出一些注释示例。

基于Java的配置选项使您可以在没有XML的情况下编写大部分Spring配置，但可以使用少量基于Java的注释。

一个示例是@Configuration注释，它指示Spring IoC容器可以将该类用作bean定义的源。另一个例子是@ Bean注释方法，它将返回一个应该在Spring应用程序上下文中注册为bean的对象。



**问题三十：**

什么是基于注释的容器配置？

基于注释的配置提供了XML设置的替代方案，该配置依赖于字节码元数据来连接组件而不是角括号声明。开发人员不是使用XML来描述bean连接，而是通过在相关的类，方法或字段声明上使用注释将配置移动到组件类本身。



**问题三十一：**

如何打开注释线？

默认情况下，Spring容器中未打开注释接线。为了使用基于注释的布线，我们必须通过配置元素在Spring配置文件中启用它。



**问题三十二：**

如何在Spring框架中更有效地使用JDBC？

使用Spring JDBC框架时，减少了资源管理和错误处理的负担。因此，开发人员只需编写语句和查询即可将数据传入和传出数据库。借助Spring框架提供的模板类（JdbcTemplate），可以更有效地使用JDBC



**问题三十三：**

Spring DAO支持的作用是什么

Spring中的数据访问对象（DAO）支持旨在使您能够以一致的方式轻松使用JDBC，Hibernate或JDO等数据访问技术。这使我们可以非常轻松地在持久性技术和代码之间切换，而无需担心捕获特定于每种技术的异常。



**问题三十四：**

使用Spring访问Hibernate有哪些方法？

使用Spring访问Hibernate有两种方法：

- 使用Hibernate模板和回调进行控制反转。
- ExtendingHibernateDAOSupport并应用AOP拦截器节点。



**问题三十五：**

如何使用HibernateDaoSupport集成Spring和Hibernate？

使用名为LocalSessionFactory的Spring的SessionFactory。整合过程包括3个步骤：

- 配置Hibernate SessionFactory
- 从HibernateDaoSupport扩展DAO实现
- 使用AOP连接事务支持



**问题三十六：**

事务管理类型的Spring支持有哪些？

Spring支持两种类型的事务管理：

程序化事务管理： 这意味着您已经在编程的帮助下管理了事务。这为您提供了极大的灵活性，但很难维护。

声明式事务管理： 这意味着您将事务管理与业务代码分开。您只能使用注释或基于XML的配置来管理事务。



**问题三十七：**

Spring Framework的事务管理有哪些好处？

它在不同的事务API（如JTA，JDBC，Hibernate，JPA和JDO）之间提供了一致的编程模型。 与许多复杂的事务API（如JTA）相比，它为程序化事务管理提供了更简单的API。 它支持声明式事务管理。 它与Spring的各种数据访问抽象集成得非常好。



**问题三十八：**

哪种交易管理类型更可取？

Spring Framework的大多数用户选择声明式事务管理，因为它是对应用程序代码影响最小的选项，因此最符合非侵入式轻量级容器的理想。声明式事务管理优于程序化事务管理，但它不如程序化事务管理灵活，后者允许您通过代码控制事务。



**问题三十九：**

什么是AOP？

面向方面的编程（AOP）是一种编程技术，它允许程序员模块化横切关注点或行为，这些问题或行为跨越典型的责任分工，例如日志记录和事务管理。



**问题四十：**

Spring AOP中的关注点和交叉关注点之间有什么区别？

关注点是我们希望在应用程序模块中拥有的行为。关注点可以定义为我们想要实现的功能。

跨领域的关注点是一个适用于整个应用程序的问题，它会影响整个应用程序。例如，日志记录，安全性和数据传输是应用程序几乎每个模块都需要的问题，因此它们是跨领域的问题。



https://zhuanlan.zhihu.com/p/93594713