[TOC]

## Table of Contents

一个Gradle 插件可以将build逻辑的复用片段打包在一起，我们可以将他们应用在不同的项目之中。Gradle 允许我们实现我们自定义的插件，因此你可可以复用你的build逻辑并且分享给其他人

理论上你可以使用你喜欢的任何语言实现一个Gradle 插件，前提它们可以最后编译为字节码。在我们的例子中，我们使用Groovy作为实现语言。Groovy，Java或者Kotlin都是实现插件语言的好的选择，Gradle API 已经被设计的可以很好地兼容这些语言的。总的来说，一个由Java或者Kotlin实现的插件，将比Groovy实现的更加通俗易懂,

## Packaging a plugin

以下的一些地方可以防止插件的代码

### Build script

你可以直接在build script中直接包含插件源码，这种做法有个优势就是可以自动的编译，并且添加到build script的classpath中。然后，这个插件在外部不可见，因此你不能在该build之外复用这个插件

### buildSrc project

你可以把插件代码放在`rootProjectDir/buildSrc/src/main/groovy`目录下,Gradle将处理好编译和测试插件，并且让它在build script的路径下可用。这个插件在当前项目build内的build script中都是可见的，然而在其他项目中build中还是不可见的，因此你还是不能在本项目之外的复用该插件

你可在参考 [Organizing Gradle Project](https://docs.gradle.org/current/userguide/organizing_gradle_projects.html)了解更多 buildSrc 项目使用的细节

### Standalone project

你可以为你的插件创建一个独立的工程。这个工程可以创建和发布JAR文件，这个jia文件可以在多个项目的build中被使用，总而言之，这个jar文件可以包含一些插件，或者绑定一些相关的task

在我们的例子中,为了使得事情比较简单，我们将在build script使用插件，，然后我们将学习如何创建一个standalone project的插件

## Writing a simple plugin

创建一个Gradle插件，首先需要实现`Plugin`接口，实现`Plugin.apply(T)`方法。其中project对象作为一个参数，这样插件就可以配置它。下面的飞马包含了一个hello world插件，给project添加了一个`hello`task

**Example: A custom plugin**
*build.gradle*

```groovy
class GreetingPlugin implements Plugin<Project> {
    void apply(Project project) {
        project.task('hello') {
            doLast {
                println 'Hello from the GreetingPlugin'
            }
        }
    }
}

// Apply the plugin
apply plugin: GreetingPlugin
```

**Output of gradle -q hello**

```groovy
> gradle -q hello
Hello from the GreetingPlugin
```

我们需要明白的是一个插件是在被应用进的projetc来创建插件的实例，也需要明白插件是一个泛型。在上面例子里，接受一个Project类型作为一个参数。一个插件也可一件接受`Setting`类型的参数，这种插件我们可以把它应用到settin script脚本中;或者接受`Gradle`类型的插件参数,这种插件我们可以应用到script脚本初始化中

## Making the plugin configurable

大多数插件需要从build script中获取一些配置，一种方案就是使用`extension objects`. Gradle `Project`和`ExtensionContainer`对象具有联系，这个对象包含了这个插件应用到project的setting和属性。你可以通过添加扩展对象到这个容器来给你的插件提供配置。一个扩展对象就是一个简单的Java Bean兼容类。Groovy是一个很好的语言选择，是实现来扩展对象，这是由于普通老的Groovy对象包含了所有的setter和getter方法，当然Java和Kotlin也是其他好的选择.

让我们给project添加一个简单的扩展对象，在这里我们给project添加一个`greeting`扩展对象。

**Example: A custom plugin extension**

*build.gradle*

```groovy
class GreetingPluginExtension {
    String message = 'Hello from GreetingPlugin'
}

class GreetingPlugin implements Plugin<Project> {
    void apply(Project project) {
        // Add the 'greeting' extension object
        def extension = project.extensions.create('greeting', GreetingPluginExtension)
        // Add a task that uses configuration from the extension object
        project.task('hello') {
            doLast {
                println extension.message
            }
        }
    }
}

apply plugin: GreetingPlugin

// Configure the extension
greeting.message = 'Hi from Gradle'
```

*Output of gradle -q hello*

```groovy
> gradle -q hello
Hi from Gradle
```

在这个例子中,`GreetingPluginExtension`是一个普通的旧Groovy对象，它带有一个名为message的属性，这个扩展以`greeting`的名字加载到插件列表中。这个对象然后在Project中就变得可用

通常情况下，你需要在一个插件里面配置对个相关属性，Gradle为每个扩展对象添加了闭环配置对象，因而你可以将这些设置分组在一起，下面的例子将要向你展示运作的方式

**Example: A custom plugin with configuration closure**
*build.gradle*

```groovy
class GreetingPluginExtension {
    String message
    String greeter
}

class GreetingPlugin implements Plugin<Project> {
    void apply(Project project) {
        def extension = project.extensions.create('greeting', GreetingPluginExtension)
        project.task('hello') {
            doLast {
                println "${extension.message} from ${extension.greeter}"
            }
        }
    }
}

apply plugin: GreetingPlugin

// Configure the extension using a DSL block
greeting {
    message = 'Hi'
    greeter = 'Gradle'
}
```

*Output of gradle -q hello*

```groovy
> gradle -q hello
Hi from Gradle
```

在这个例子中，一些设置可以通过`greeting`闭包归为一类，在build script中的闭包块name需要和扩展对象名称保持一致。然后当闭包被执行，在扩展对象里面的属性将通过Groovy闭包代理的特性被映射为变量

## Working with files in custom tasks and plugins

当我们开发自定义task和插件的时候，当接受文件位置的输入配置时候需要十分的灵活，要到这个点，您可以利用Project.file（java.lang.Object）方法尽可能晚地解析文件的值。

**Example: Evaluating file properties lazily**

*build.gradle*

```groovy
class GreetingToFileTask extends DefaultTask {

    def destination

    File getDestination() {
        project.file(destination)
    }

    @TaskAction
    def greet() {
        def file = getDestination()
        file.parentFile.mkdirs()
        file.write 'Hello!'
    }
}

task greet(type: GreetingToFileTask) {
    destination = { project.greetingFile }
}

task sayGreeting(dependsOn: greet) {
    doLast {
        println file(greetingFile).text
    }
}

ext.greetingFile = "$buildDir/hello.txt"
```

*Output of gradle -q sayGreeting*

```groovy
> gradle -q sayGreeting
Hello!
```

在这个例子中，我们配置了`greet` Task的`destination`属性作为了闭环，并在最后转换为File对象进行评通过Project.file（java.lang.Object）方法对闭包的返回值估。在这个例子中你可以看到，我们设置`greetingFile`属性，在我们已经为task配置属性之后。这种evaluation的懒加载是文件设置属性可以接受任何值的一个关键的好处,然后在读取属性时解析该值。

## Mapping extension properties to task properties

build script通过扩展获取用户的输入，并且将它映射到输入输出属性是自定义task的最佳实践。最终用户只需要通过扩展定义的暴露出来的DSL进行交互即可。命令逻辑隐藏在插件实现中。

在build script中申明的扩展以及扩展属性和自定义任务属性之间的映射发生在构建生命周期的Gradle配置阶段。为了避免evaluation顺序的问题，必须在excution阶段解决映射属性的实际值。（更多信息请参看 [the section called “Build phases”](https://docs.gradle.org/current/userguide/build_lifecycle.html#sec:build_phases)）.Gradle的API提供了用于表示应该进行延迟evaluate的属性的类型，例如在执行期（参阅[延迟配置](https://docs.gradle.org/current/userguide/lazy_configuration.html)），下面的例子演示了将扩展属性映射到任务属性类型的用法

**Example: Mapping extension properties to task properties**
*build.gradle*

```groovy
class GreetingPlugin implements Plugin<Project> {
    void apply(Project project) {
        def extension = project.extensions.create('greeting', GreetingPluginExtension, project)
        project.tasks.create('hello', Greeting) {
            message = extension.message
            outputFiles = extension.outputFiles
        }
    }
}

class GreetingPluginExtension {
    final Property<String> message
    final ConfigurableFileCollection outputFiles

    GreetingPluginExtension(Project project) {
        message = project.objects.property(String)
        message.set('Hello from GreetingPlugin')
        outputFiles = project.layout.configurableFiles()
    }

    void setOutputFiles(FileCollection outputFiles) {
        this.outputFiles.setFrom(outputFiles)
    }
}

class Greeting extends DefaultTask {
    final Property<String> message = project.objects.property(String)
    final ConfigurableFileCollection outputFiles = project.layout.configurableFiles()

    void setOutputFiles(FileCollection outputFiles) {
        this.outputFiles.setFrom(outputFiles)
    }

    @TaskAction
    void printMessage() {
        outputFiles.each {
            logger.quiet "Writing message 'Hi from Gradle' to file"
            it.text = message.get()
        }
    }
}

apply plugin: GreetingPlugin

greeting {
    message = 'Hi from Gradle'
    outputFiles = layout.files('a.txt', 'b.txt')
}
```

> Note: The code for this example can be found at samples/userguide/tasks/mapExtensionPropertiesToTaskProperties in the ‘-all’ distribution of Gradle.

*Output of gradle -q hello*

```groovy
> gradle -q hello
Writing message 'Hi from Gradle' to file
Writing message 'Hi from Gradle' to file
```

## A standalone project

现在，我们将把插件移到一个独立的project中，这样我们就可以发布并公用这个插件。这个project是个可以生成包含插件class的Jar文件的简单Groovy Project。下面这个project的简单build script，apply了Groovy插件，并且添加了gradle api的依赖

**Example: A build for a custom plugin**

*build.gradle*

```groovy
pply plugin: 'groovy'

dependencies {
    compile gradleApi()
    compile localGroovy()
}
```

> Note: The code for this example can be found at samples/customPlugin/plugin in the ‘-all’ distribution of Gradle.

因此Gradle如何发现实现的插件呢？答案是你需要在`META-INF/gradle-plugins`目录下提供一个匹配你插件id的属性
**Example: Wiring for a custom plugin**

```groovy
- src/main/resources/META-INF/gradle-plugins/org.samples.greeting.properties
  |
  -  implementation-class=org.gradle.GreetingPlugin
```



需要注意的是，属性文件名与插件标识匹配，并放置在资源文件夹中，而实现类属性标识插件实现类。

### Creating a plugin id

插件ID以类似于Java软件包的方式被完全限定（即反向域名）。这有助于避免冲突，并提供了一种将类似所有权的插件分组的方法。

您的插件ID应该是反映名称空间的组件的组合（指向您或您的组织的合理指针）以及它提供的插件的名称。例如，如果您有一个名为“foo”的Github帐户，并且您的插件名为“bar”，则合适的插件ID可能是com.github.foo.bar。同样，如果插件是在baz组织开发的，插件ID可能是org.baz.bar。

插件id应该按照以下规则：

- 可以包含任何字母数字字符，’.’和’ - ‘。
- 必须包含至少一个’.’将名称空间与插件名称分开的字符。
- 通常对名称空间使用小写反向域名约定。
- 通常在名称中只使用小写字符
- 不要使用 org.gradle和com.gradleware命名空间
- 无法以’.’开始或结束
- 不能包含连续的’.’字符（即’..’）

尽管插件ID和包名称之间存在传统的相似性，但包名通常比插件ID所需的更详细。例如，将“gradle”添加为插件ID的组件似乎是合理的，但由于插件ID仅用于Gradle插件，这将是多余的。一般而言，标识所有权和名称的名称空间都是良好的插件标识所需的

### Publishing your plugin

如果您在公司内部发布您的插件以供您的组织使用，可以像发布其他任何代码工件一样发布它。关于发布工件，请参阅[lvy](https://docs.gradle.org/current/userguide/publishing_ivy.html)和[maven](https://docs.gradle.org/current/userguide/publishing_maven.html)章节。

如果您有兴趣发布您的插件以供更广泛的Gradle社区使用，您可以将其发布到[Gradle plugin portal. ](https://plugins.gradle.org/docs/submit?_ga=2.115105858.438770069.1528507228-204910210.1513922382)。该网站提供搜索和收集Gradle社区贡献插件的信息的能力。请参阅[此处](https://plugins.gradle.org/?_ga=2.115105858.438770069.1528507228-204910210.1513922382)的说明，了解如何使您的插件在本网站上可用。

### Using your plugin in another project

要在构建脚本中使用插件，需要将插件类添加到构建脚本的类路径中。为此，请使用“buildscript {}”块，如[“Applying plugins with the buildscript block”](https://docs.gradle.org/current/userguide/plugins.html#sec:applying_plugins_buildscript). 一节中所述。以下示例显示了当包含该插件的JAR已发布到本地存储库时，您需要这样做：

**Example: Using a custom plugin in another project**
*build.gradle*

```groovy
buildscript {
    repositories {
        maven {
            url uri('../repo')
        }
    }
    dependencies {
        classpath group: 'org.gradle', name: 'customPlugin',
                  version: '1.0-SNAPSHOT'
    }
}
apply plugin: 'org.samples.greeting'
```

或者，如果您的插件已发布到插件门户，则可以使用孵化插件DSL（请参阅“[使用插件DSL应用插件](https://docs.gradle.org/current/userguide/plugins.html#sec:plugins_block)”一节）来应用该插件：

**Example: Applying a community plugin with the plugins DSL**
*build.gradle*

```groovy
plugins {
    id 'com.jfrog.bintray' version '0.4.1'
}
```

## Providing a configuration DSL for the plugin

正如我们上面看到的，您可以使用扩展对象为您的插件提供配置。使用扩展对象还扩展了Gradle DSL以为该插件添加项目属性和DSL块。扩展对象只是一个常规对象，因此您可以通过向扩展对象添加属性和方法来提供嵌套在此块中的DSL元素。Gradle提供了一些便利来帮助为插件创建一个行为良好的DSL。

### Nested DSL elements

当Gradle创建任务或扩展对象时，Gradle修饰实现类以混合DSL支持。要创建一个嵌套的DSL元素，可以使用ObjectFactory类型来创建具有类似修饰的对象。然后，可以通过插件扩展的属性和方法使这些修饰对象可见。

**Example: Nested DSL elements**

*build.gradle*

```groovy
class Person {
    String name
}

class GreetingPluginExtension {
    String message
    final Person greeter

    @javax.inject.Inject
    GreetingPluginExtension(ObjectFactory objectFactory) {
        // Create a Person instance
        greeter = objectFactory.newInstance(Person)
    }

    void greeter(Action<? super Person> action) {
        action.execute(greeter)
    }
}

class GreetingPlugin implements Plugin<Project> {
    void apply(Project project) {
        // Create the extension, passing in an ObjectFactory for it to use
        def extension = project.extensions.create('greeting', GreetingPluginExtension, project.objects)
        project.task('hello') {
            doLast {
                println "${extension.message} from ${extension.greeter.name}"
            }
        }
    }
}

apply plugin: GreetingPlugin

greeting {
    message = 'Hi'
    greeter {
        name = 'Gradle'
    }
}
Output of gradle -q hello

> gradle -q hello
Hi from Gradle
```

在这个例子中，插件通过其构造函数将项目的ObjectFactory传递给扩展对象。构造函数使用它创建一个嵌套对象，并通过greeter属性使该对象可供DSL使用。