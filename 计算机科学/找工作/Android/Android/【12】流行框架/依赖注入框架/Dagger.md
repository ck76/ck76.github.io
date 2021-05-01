# 当定义Dagger2 Scope时，你在定义什么？

## 前言

我在简书上写得第一篇文章是关于Dagger2在Android平台上新的使用方式的，见[Dagger2在Android平台上的新姿势](https://www.jianshu.com/p/ad777c73b528)。当时觉得“真香”。

之后写了一篇介绍了这种新的使用方式背后是如何实现的，见[Dagger2在Android平台上的新魔法](https://www.jianshu.com/p/c01fdda42434)。也是这篇文章让我重新思考dagger.android是否真的给开发带了便捷，从那之后我便放弃使用dagger.android。真讽刺，因为不了解觉得“真香”，因为了解而放弃。
这里面的核心问题有两个：

1. 什么是Scope，当你定义Scope时，你在定义什么？
2. 你是否真的需要SubComponent/Dependent Component？

## 1. 什么是Component？

要想解释什么是Scope，必须先明白什么是Component。Component说白了其实就是一个对象图（object graph），它包含了很多对象，以及对象之间的依赖关系，所以我们才能通过Component直接注入我们需要的对象，而不需要考虑依赖关系的问题。

![img](https://upload-images.jianshu.io/upload_images/4803763-7ad11d044827df32.png)

Component就是对象图

如上图所示，Component的对象图包含有5个对象：A、B、C、D、E。这些对象之间有依赖关系，通过Component可以直接注入（也可以理解为获取）这5个对象中的任意一个/几个，不需要考虑这些对象之间的依赖关系，因为Dagger已经帮我们构建出了这么一个对象图。例如，需要注入A对象，需要先构建C和D两个对象，我们不需要关心这些，直接注入A对象即可。
Component是一个对象图，然而，Component本身也是一个对象（“面向对象”中那个对象），不同Component之间也有关联，类比“面向对象”中的说法，Component也有“组合”和“继承”。

### 1.1 Component间的依赖关系

所谓Component的“组合”是指Component之间的“依赖”关系，对应于Dagger中的Component dependencies。一个Component可以依赖另外一个/几个Component，前面已经说过，所谓Component也就是对象图而已，一个Component依赖另外的Component，换一种说法也就是，这个Component除了自己的对象图外还包含有别的Component的“部分”对象图。这里的“部分”指的是那些已经在别的Component中被声明/暴露出来的对象。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gl4prfek8aj30g009cmx6.jpg)

Component间的依赖

如上图所示，右侧的Component依赖于左侧的Component，也就是说右侧Component的对象图不止包含有对象D、E，还包含有左侧Component声明/暴露出来的C对象，所以D对象可以依赖于C对象，但是不能依赖于A或者B对象。

### 1.2 Component间的父子关系

所谓Component的“继承”是指Component之间的“父子”关系，对应于Dagger中的SubComponent。不同于上面提到的Component间的依赖关系，SubComponent继承了父Component中的所有对象图，不需要父Component进行声明/暴露。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gl4pree4bmj30g009cjrf.jpg)

Component间的继承

如上图所示，右侧SubComponent的对象图包含了左侧父Component的所有对象，不管这些对象是否在父Component中被声明/暴露，所以对象D可以依赖于对象C和对象B。需要注意的是，左侧父Component也可以是SubComponent，所以说，右侧的SubComponent可能不仅包含了父Component的对象图，还包含了爷Component的对象图，等等。但是这并不重要，还是可以简单的说SubComponent继承了父Component的对象图，至于父Component的对象图是完全自己定义的，还是继承自别的什么Component，这对于当前的SubComponent而言并不重要。

## 2. 什么是Scope？

上面介绍了什么是Component，这和Scope有什么关系呢？答案很简单，Scope就是Component的名字而已。
众所周知，`@Singleton`也是一个Scope（如果你还不知道，就假装自己已经知道了）。这就奇怪了，既然`@Singleton`也是一个Scope，这和我们自己定义的Scope有什么区别，为什么使用`@Singleton`就可以实现全局单例，而我们自定义一个`@ActivityScope`或者`@PerActivity`就只能实现一个在每个Activity中的单例，同理也适用于`@FragmentScope`或者`@PerFragment`？这是Dagger2最微妙的部分，也是让很多初学者困惑的地方，我当初学习Dagger2的时候就有这样的困惑。先直接抛出答案，其实，Dagger2没有这样的魔法，自定义的`@ActivityScope`，`@FragmentScope`和`@Singleton`没有什么区别，单例的作用域取决于`Component`自身的存活范围。

### 2.1 Scope就是Component的名字

前面介绍了Component之间有依赖关系和父子关系，建立这些关系的前提是每个Component都得有个“名字”。而这个名字就是Scope。

> 没有“名字”（不定义Scope）的Component之间也可以建立这两种关联，但是实际使用当中没有太大作用，所以你可以简单地认为Component必须定义自己的Scope。

那么为什么必须给Component起个名字呢？最核心的原因在于要给单例提供作用域。依赖注入绕不开问题就是如何提供单例对象，以及这个单例对象的使用范围（作用域）。Dagger2解决这两个问题的关键就在于Scope。
之前介绍Component的对象图时都忽略了单例的情况，下面仔细看看Component中的单例是如何存在的。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gl4pr7qotej30g009cjrg.jpg)

Singleton

如上图所示，D对象是个单例，而C对象不是，每次需要C对象是都会新建一个，而D对象在该Component中始终只有一个，这是因为，单例对象D被存储在了该Component中，只要能获得该Component，就能获得单例对象D。
并且，Dagger还规定了，Component中使用的Module只能提供unscope的对象（普通对象，每次需要时都新建），或者跟该Component Scope一致的对象。例如，上图中，Component定义的Scope是`@Singleton`，那么所有Module就只能提供unscope的对象C，或者`@Singleton`的对象D。之所以这么规定，是因为每个Component定义了自己的Scope，并管理着自己的对象图，与之关联的Module提供这个Scope内的单例或者unscope就可以了，不能通过这个Component向别的Component的作用域提供单例，因为那么别的Component的事情，与你无关。
为什么我说Scope就是Component的名字而已？因为这个Component的Scope可以是任意Scope，可以是默认的`@Singleton`，也可以是自定义的`@ActivityScope`，或者任意别的自定义Scope都可以。这个Scope并不决定单例的范围，它只是一个标识，并且强制要求该Component使用到的Module都遵守这个标识，不能越界。通过这个标识标注的单例都被存储到了该Component中，只要能获得该Component，你就能获得这些单例，单例的范围/作用域的关键在于该Component的存活范围。在Application中保存了该Component，那么就是全局单例的；在Activity中保存了该Component，那么就是在Activity范围内的单例，等等。跟定义的Scope叫什么没有一毛钱关系。

> 虽说Scope的名字可以任意取，但是也不能瞎取。在程序中起名字最重要的就是表意。所以我们经常去`@ActivityScope`,`@FragmentScope`这样的名字，但是单例的作用域跟这些名字之间并没有任何关系，并不是因为你定义的Scope叫`@ActivityScope`，所以单例的作用域就被限定在了Activity的范围内。就好像，你养了一只狗，你非得给他取名叫“喵咪”，也不能说你错，但是大家肯定觉得你有毛病。反过来，即使他叫“喵咪”，他也仍然是一只狗。

## 3. 是否需要SubComponent/Dependent Component

假设你平时做Android开发时会为每个Activity和Fragment定义相应的Component（例如使用dagger.android）。当你明白了什么是Component，什么是Scope之后，你有没有想过这么一个问题，这些定义的Component是否有必要。
如前所述，定义Scope就是给Component起名字，而给Component起名字主要是为了构建多个Component之间的关联。试问，构建这些关联的目的是什么？当然是为了分层管理，在不同作用域内共享一些单例。如果你定义了一个SubComponent/Dependent Component，与之关联的所有Module却并没有向它提供单例（所有Module全部提供的是unscope对象），那么这个SubComponent/Dependent Component是没必要的，你完全可以使用上一层的Component（父Component/被依赖的Component）去完成相同的任务，除非你确实需要为这个Component提供这些Module，以对对象进行分层管理。甚至，更极端的情况，这个SubComponent/Dependent Component完全不需要Module，在这种情况下，使用这个SubComponent/Dependent Component完全是多余的。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gl4pr9rra1j30g009c74h.jpg)

多余的Component

### 3.1 为什么放弃dagger.android

上面这张图就是我放弃dagger.android的原因是，因为我发现自己之前使用dagger.android时就是这样的，完全没有为SubComponent提供新的Module，因此我现在已经不再使用dagger.andoird。
dagger.android在使用上有其限制，主要表现在不能向Module提供参数（注意这里说的是dagger.android）。这使得dagger.android的使用陷入到这样的尴尬：

1. 多数情况下我们不需要在Activity、Service、Fragment的范围内提供单例。
2. 假设我们需要在某个Activity范围内提供某个单例，以实现在它的Fragment中共享该单例，但是却发现不能为Module提供参数，因为多数情况下都是需要通过参数来创建Activity范围内的单例。

第一种情形下，dagger.android显得多余，徒增麻烦。第二种情形下，dagger.android根本无法使用。

### 3.2 重新审视你的Component

在我看来，在Android平台上很多SubComponent/Dependent Component是没有必要的，多数情况下，我们需要的单例是全局的，把这个全局的Component保存在Application中即可。并不经常需要在Activity、Fragment范围内共享什么单例。如果你使用MVP模式，那么有时确实需要在Activity范围内去共享某个Presenter，但是也没必要为每个Activity、Fragment创建Component。这种误用往往是因为不了解Dagger，把这种使用方式当成一种范式，仿佛只有这样Dagger才能正常运行，其实完全不是这样的。

## 4. 总结

1. Component就是个对象图，Scope是给Component起得名字，目的是为了构建Component之间的依赖、继承关系，这些关系体现了Dagger对于依赖注入的分层管理。
2. Module只能提供unscope或者和Component相同Scope的对象，这是因为各个Component管理各自的对象图，别的Component无法插手。
3. 单例对象被保存在对应的Component中，这个Component的存活范围就是单例的作用域，跟Scope叫什么名字没有关系。
4. 当你新建SubComponent/Dependent Component时，多问问自己有没有必要。

## 参考

[用Dagger2实现依赖注入](https://github.com/xitu/gold-miner/blob/master/TODO/Dependency-Injection-with-Dagger-2.md)

[什么决定了Component的生命周期](https://stackoverflow.com/questions/28411352/what-determines-the-lifecycle-of-a-component-object-graph-in-dagger-2)