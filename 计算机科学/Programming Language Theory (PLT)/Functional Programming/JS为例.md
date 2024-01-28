# 函数式编程、纯函数、高阶函数、闭包、柯里化函数，偏应用，组合和管道，函子，Generator

[![img](https://upload.jianshu.io/users/upload_avatars/2706477/510c66fffaff.jpg?imageMogr2/auto-orient/strip|imageView2/1/w/80/h/80/format/webp)royluck](https://www.jianshu.com/u/f0b30c80bcef)关注赞赏支持



# 函数式编程、纯函数、高阶函数、闭包、柯里化函数，偏应用，组合和管道，函子，Generator

![img](https://upload.jianshu.io/users/upload_avatars/2706477/510c66fffaff.jpg?imageMogr2/auto-orient/strip|imageView2/1/w/96/h/96/format/webp)

[royluck](https://www.jianshu.com/u/f0b30c80bcef)关注

2019.10.25 16:51:20字数 2,030阅读 85

##### >>函数式编程：

**原则：小**

> f(x) = y[数学函数]
> 1、函数必须总是接受一个参数；
> 2、函数必须总时返回一个值；
> 3、函数应该依据接收到的参数（例如x），而不是外部环境运行；
> 4、对于一个给定的x，只会输出唯一一个y。

函数式编程是一种范式，我们能够以此创建仅依赖输入就可以完成自身逻辑的函数。这保证了当函数被多次调用时任然
返回相同的结果。函数不会改变任何外部环境的变量，这将产生可缓存的，可测试的代码库。

- *引用透明性*：所有的函数对于相同的输入都将返回相同值
- *替换模型*：直接替换函数的结果（主要因为函数的逻辑不依赖其他全局的变量），这使并发代码和缓存成为可能。

函数式编程主张**声明式编程**和**编写抽象的代码**。
（区分声明式编程和命令式编程：for 和 forEach）

**“如何”做的部分**将被抽象到普通函数中（高阶函数）

函数式编程主张以抽象的方式创建函数，这些函数能够在代码的其他部分被重用。

**抽象：**把复杂的东西抽出来，变成简单的东西。

------

##### >>纯函数：

纯函数是数学函数

纯函数是对给定的输入返回相同的输出的函数
->**产生可测试的代码**（纯函数不应改变任何外部环境的变量）
->**合理的代码**：必须具有一个有意义的名称（通过函数名能够轻易地推理出该函数的作用）
=>包含纯函数的代码易于阅读、理解和测试。

js不是一种纯函数语言（因为可以不用参数传入）

js支持函数作为参数，以及将函数传值给另一函数等特性（js将函数视为一等公民）

------

##### >>高阶函数：

高阶函数：接受另一个函数作为其参数的函数称为高阶函数。

```jsx
// JS七种基本类型
{
  Number
  String
  Boolean
  Object
  Null
  Undefined
  Symbol
}
```

*[在《JavaScript权威指南》中把function被看做是object基本数据类型的一种特殊对象，另外《悟透JavaScript》和《JavaScript高级程序设计》也把函数视为对象，而不是一种基本数据类型。](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.cnblogs.com%2Fleezhxing%2Fp%2F4103893.html)

高阶函数是接受函数作为参数并且/或者返回函数作为输出的函数。

大多数高阶函数与闭包一起使用。

------

##### >>闭包

闭包与作用域相关联：

```kotlin
outer(){
     inner(){
    ...
  }
}
```

inner是闭包，inner能访问自身作用域的变量，也能访问全局变量，也能访问外部函数父级（outer）作用域的变量。

- 闭包能访问外部函数的变量，该属性使闭包变量非常强大！
- 闭包可以记住它的上下文。

------

##### >>柯里化函数（curry）：

**(高阶函数和闭包构成柯里化函数)**

把一个多参数函数转换为一个嵌套的一元函数的过程；
注：curry函数应用参数列表的顺序是从左到最右

- 两元函数转柯里化

```jsx
let curry = (fn) => {
            return function (x) {
                return function (y) {
                    return fn(x,y)
                }
            }
        }

let add = function (x,y) {
            return x+y
        }

curry(add)(2)(3) // => 5
```

- 多参数函数转化一元函数的curry函数

```jsx
let curryN = (fn) => {
            if( typeof fn !== 'function' ){
                throw Error('no function provided');
            }
            return function curriedFn (...args) {
                if(args.length < fn.length){
                    return function () {
                        return curriedFn.apply(null,args.concat([].slice.call(arguments)));
                    }
                }
                return fn.apply(null,args);
            }
        }

let add = function (x,y,z,o,p) {
            return x+y+z+o+p
        }

curry(add)(2)(3)(4)(5)(6) // => 20
```

应用场景：

```jsx
// 封装调试console
const loggerHelper = (mode,initialMsg,errorMsg,lineNo) => {
  if(mode === 'DeBug'){
    console.debug(initialMsg,errorMsg+"at line："+lineNo)
  }else if(mode === 'Log'){
    console.log(initialMsg,errorMsg+"at line："+lineNo)
  }else {
    throw Error("Wrong mode")
  }
}
//  利用柯里化转化
let debugLogger = curryN(loggerHelper)('Log')("DeBug at stats.js")
debugLogger('debug message',123) // => DeBug at stats.js debug messageat line：123
// 在数组内容中查找数字
let match = curryN(function (expr,str) {
  return str.match(expr)

})

let hasNumber = match(/[0-9]+/)
        
let filter = curryN(function (fn,arr) {
  return arr.filter(fn)
})

let findNumbersInArray = filter(hasNumber)

findNumbersInArray(['is','number2']) // =>['number2']
// 求数组的平方
let map = function (x) {
  return x*x
}

let squareAll = curryN(function (fn,arr) {
  return arr.map(fn)
})

console.log(squareAll(map)([2,3,4]))
```

------

##### >>偏函数（partial）：

产生背景：curry函数应用参数列表的顺序是从左到最右，但有时候我们需要颠倒某两个参数的顺序

```jsx
const partial = function (fn,...partialArgs) {
            let args = partialArgs;
            return function (...fullArguments) {
                let arg = 0;
                for (let i=0; i<args.length && arg<fullArguments.length; i++){
                    if(args[i] === undefined){
                        args[i] = fullArguments[arg++]
                    }
                }
                return fn.apply(null,args);
            }
        }
```

应用场景：

```jsx
// 定时执行
let delayTenMsg = partial(setTimeout,undefined,10)
delayTenMsg(()=>{
  console.log('执行结束')
})
```

partial应用于任何含有多个参数的函数：

```jsx
let obj = {
  foo: 'bar',
  bar: 'foo'
}
JSON.stringify(obj,null,2)

// 偏函数改造：
let prettyPrintJSON = partial(JSON.stringify,undefined,null,2)
prettyPrintJSON(obj) 
```

**注：**这偏函数实现中有一个小bug，只能是一次性的，因为我们是用undefined替换partialArgs，而数组传递的是引用！因而当你第二次传参`let obj2 = { foo: 'bar2', bar: 'foo2' }`打印出来的值还是上一次的obj结果。

> **总结：**什么时候用curry，什么时候用partial，归结于使用场景，curry的传参必须从左到右依次执行，partial用于解决这种不能从左到右依次执行的情况。

> 柯里化和偏应用是函数式编程的两种重要技术，作为一名js程序员，应该在代码库中选择柯里化或偏应用直一。

------

##### >>组合和管道：

函数式组合在函数式中被称为组合。

**Unix的理念：**

- 每个程序只做好一件事。为了完成一项新的任务，重新构建要好于在复杂的旧程序中添加新属性；
- 每个程序的输出应该是另一个尚未知的程序输入
- 管道（ | ）在两个命令之间扮演了桥梁的角色
- 基础函数需要遵循如下规则：每一个基础函数都需要接收一个参数并返回数据！

> 组合函数的真正优势在于：**无须创建新的函数就可以通过基础函数解决眼前的问题。**

**compose函数定义：**

- 两元组合函数：

```jsx
const compose = (a,b)=> c =>a(b(c))
```

应用：

```tsx
let number = compose(Math.round,parseFloat)
number('6.63') // =>7
let splitIntoSpace = (str) => str.split(" ")
let count = arr => arr.length
let countWords = compose(count,splitIntoSpace)
countWords("hello i am your dad") // =>5
```

- 多元组合函数：

```jsx
const composeN = (...fns) => value => {
  let _fns = fns.reverse()
  return _fns.reduce((acc,fn)=>fn(acc),value)
}
```

应用：

```jsx
let splitIntoSpace = (str) => str.split(" ")
let count = arr => arr.length
let addOrEven = num => num%2 == 1 ? 'even' : 'add' // 判断奇数还是偶数
let countWords = composeN(addOrEven,count,splitIntoSpace)
composeN(addOrEven,count,splitIntoSpace) // => "even"
```

**pipe函数定义：**

与compose函数所作的事请相同，只不过交换了数据流的方向；
pipe和compose在现实开发中应该二选一，否则会让开发者感到困扰。

```jsx
const pipe = (...fns) => value => {
  return fns.reduce((acc,fn)=>fn(acc),value)
}
```

> *组合满足结合律：*
> composeN(f, composeN(g,h)) = composeN(composeN(f, g),h)

**identity函数定义：**

用于定位组合函数的数据流中哪个出错

```jsx
const identity = (it) =>{
            console.log(it)
            return it
        }
let countWords2 = composeN(addOrEven,count,identity,splitIntoSpace) 
// => ["hello", "i", "am", "your", "dad"]
// => "even"
```

------

##### >>函子：

（错误处理）
函子，它将用一种纯函数式的方式帮助我们处理错误；
**函子是一个普通对象（在其他语言中，可能是一个类），它实现了map函数，在遍历每对象值得时候生成一个新对象。**

```jsx
const Container = function (value) {
  this.value = value
}

Container.of = function(val){
  return new Container(val)
}

// 函子实现 map方法：
Container.prototype.map = function (fn) {
  return Container.of(fn(this.value))
}


Container.of({a:3}) // =>Container {value: {a:3}}
Container.of([1,2,3]) // =>Container {value: [1,2,3]}
Container.of(3).map(e=>e+1) // =>Container {value: 4}
```

支持链式操作：

```jsx
Container.of(3).map(e=>e+1).map(e=>e*e) // =>16
```

> 换句话讲：函子是一个实现了map契约的对象！

**MayBe函子：**
利用函数式编程技术处理错误或异常的问题；
任何层级的链式map都会被调用（管你传了null还undefined）该过程将连接到链条中的最后一个map函数被调用。

```jsx
const MayBe = function (value) {
  this.value = value
}

MayBe.of = function(val){
  return new MayBe(val)
}

MayBe.prototype.isNothing = function () {
  return (this.value === null || this.value === undefined)
}

// 函子实现map方法：
MayBe.prototype.map = function (fn) {
  return this.isNothing() ? MayBe.of(null) : MayBe.of(fn(this.value))
}

// 应用
MayBe.of('Roy is a man').map(e=>e.toUpperCase()).map(e=> "Mr. " + e) // MayBe {value: "Mr. ROY IS A MAN"}
```

> 拓展：ES5的类为什么不可以用箭头函数，因为其不具有[[constructor]]和prototype属性

**Either函子：**
Either的出现是为了解决MayBe不能返回undefined或null值执行失败的状态

```jsx
const Nothing = function (value) {
  this.value = value
}

Nothing.of = function(val){
  return new Nothing(val)
}

Nothing.prototype.map = function (fn) {
  return this // 返回对象本身
}

const Some = function (value) {
  this.value = value
}

Some.of = function(val){
  return new Some(val)
}

Some.prototype.map = function (fn) {
  return Some.of(fn(this.value))
}

// Either定义
const Either = {
  Some: Some,
  Nothing: Nothing
}
```

*（案例代码见代码清单8-14）*

**Pointed函子：**
函子只是一个实现了map契约的接口
Pointed函子是一个函子的子集，它具有实现了of契约的接口

ES6增加了Array.of，**这使数组成为一个Pointed函子！**

```jsx
Array.of("you are a pointed functor,too?") // =>["you are a pointed functor,too?"]
```

**Monad函子：**
含有chain的Pointed函子被称为Monad函子。

- **join解决问题**

通过join解决问题，解决map深层次嵌套问题

为MayBe函子添加一个join方法：**将嵌套的结构展开为一个单一层级**

```csharp
MayBe.prototype.join = function () {
  return this.isNothing() ? MayBe.of(null) : this.value
}
// join方法虽很简单，但却能帮助我们打开嵌套的MayBe
let joinExample = MayBe.of(MayBe.of(5)) // => MayBe { value : MayBe { value : 5} }
joinExample.join() //  => MayBe { value : 5}
```

- **实现chain**
  我们总是要在map后调用join，下面把逻辑封装在一个名为chain的方法中。

```jsx
MayBe.prototype.chain = function (fn) {
  return this.map(fn).join
}
```

> 总结：重复的map调用会导致嵌套问题，chain能帮助扁平化MayBe数据

------

##### >>Generator

作用：用generator的next调用替换回调

```jsx
// 这里的业务场景：先通过接口1获取picturesJson数据，然后通过picturesJson数据的url字段，再次请求接口2，最终获得第一张图的数据
function request(url) {
  httpGetAsync(url,function (reponse) {
    generator.next(reponse)
  })
}

function *main() {
  let picturesJson = yield request('https://www.xxx.com/pics/.json')
  let firstPictureData = yield  request(picturesJson.data.children[0].data.url + '.json')
  console.log(firstPictureData) // 最后成功打印结果
}

// 执行
let generator = main()
genera
```