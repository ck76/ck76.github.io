---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (02)：Functional Programming 基本觀念
date: 2016-12-18T23:08:47.000Z
description: Functional Programming 是 Rx 最重要的觀念之一，基本上只要學會 FP 要上手 Rx 就不難了！Functional Programming 可以說是近年來的顯學，各種新的函式編程語言推出之外，其他舊有的語言也都在新版中加強對 FP 的支援！
image: null
tags:
  - JavaScript
  - RxJS
  - Functional Programming
  - RxJS 30 Days
previous: ./thirty-days-RxJS-01.md
next: ./thirty-days-RxJS-03.md
---

這是【30天精通 RxJS】的 02 篇，如果還沒看過 01 篇可以往這邊走：
[30 天精通 RxJS (01)： 認識 RxJS](/series/rxjs/thirty-days-RxJS-01)

什麼是 Functional Programming ?
------

![functional programming icon](https://res.cloudinary.com/dohtkyi84/image/upload/v1481362001/cover/%E8%9E%A2%E5%B9%95%E5%BF%AB%E7%85%A7_2016-12-10_%E4%B8%8B%E5%8D%885.26.11_mgc7al.png)

Functional Programming 是一種編程範式(programming paradigm)，就像 Object-oriented Programming(OOP)一樣，就是一種寫程式的方法論，這些方法論告訴我們如何思考及解決問題。

簡單說 Functional Programming 核心思想就是做運算處理，並用 function 來思考問題，例如像以下的算數運算式：

```javascript
(5 + 6) - 1 * 3
```

我們可以寫成

```javascript
const add = (a, b) => a + b
const mul = (a, b) => a * b
const sub = (a, b) => a - b

sub(add(5, 6), mul(1, 3))
```

我們把每個運算包成一個個不同的 function，並用這些 function 組合出我們要的結果，這就是最簡單的 Functional Programming。

Functional Programming 基本要件
------

跟 OOP 一樣不是所有的語言都支持 FP，要能夠支持 FP 的語言至少需要符合**函式為一等公民**的特性。

### 函式為一等公民 (First Class)

一等公民就是指跟其他資料型別具有同等地位，也就是說函式能夠被賦值給變數，函式也能夠被當作參數傳入另一個函式，也可當作一個函式的回傳值

**函式能夠被賦值給變數**

```javascript
var hello = function() {}
```

**函式能被當作參數傳入**

```javascript
fetch('www.google.com')
.then(function(response) {}) // 匿名 function 被傳入 then()
```

**函式能被當作回傳值**

```javascript
var a = function(a) {
	return function(b) {
	  return a + b;
	}; 
	// 可以回傳一個 function
}
```

Functional Programming 重要特性
------


### Expression, no Statement

Functional Programming 都是 表達式 (Expression) 不會是 陳述式(Statement)。
基本區分表達式與陳述式：

**表達式** 是一個運算過程，一定會有返回值，例如執行一個 function

```javascript
add(1,2)
```

- 陳述式 則是表現某個行為，例如一個 賦值給一個變數

```javascript
a = 1;
```


> 有時候表達式也可能同時是合法的陳述式，這裡只講基本的判斷方法。如果想更深入了解其中的差異，可以看這篇文章 [Expressions versus statements in JavaScript](http://www.2ality.com/2012/09/expressions-vs-statements.html)

由於 Functional Programming 最早就是為了做運算處理不管 I/O，而 Statement 通常都屬於對系統 I/O 的操作，所以 FP 很自然的不會是 Statement。

> 當然在實務中不可能完全沒有 I/O 的操作，Functional Programming 只要求對 I/O 操作限制到最小，不要有不必要的 I/O 行為，盡量保持運算過程的單純。

### Pure Function

**Pure function 是指 一個 function 給予相同的參數，永遠會回傳相同的返回值，並且沒有任何顯著的副作用(Side Effect)**

舉個例子：

```javascript
var arr = [1, 2, 3, 4, 5];

arr.slice(0, 3); // [1, 2, 3]

arr.slice(0, 3); // [1, 2, 3]

arr.slice(0, 3); // [1, 2, 3]
```

這裡可以看到 slice 不管執行幾次，返回值都是相同的，並且除了返回一個值(value)之外並沒有做任何事，所以 `slice` 就是一個 pure function。

```javascript
var arr = [1, 2, 3, 4, 5];

arr.splice(0, 3); // [1, 2, 3]

arr.splice(0, 3); // [4, 5]

arr.slice(0, 3); // []
```

這裡我們換成用 `splice`，因為 `splice` 每執行一次就會影響 `arr` 的值，導致每次結果都不同，這就很明顯不是一個 pure function。

**Side Effect**

Side Effect 是指一個 function 做了跟本身運算返回值沒有關係的事，比如說修改某個全域變數，或是修改傳入參數的值，甚至是執行 `console.log` 都算是 Side Effect。

Functional Programming 強調沒有 Side Effect，也就是 function 要保持純粹，只做運算並返回一個值，沒有其他額外的行為。

這裡列舉幾個前端常見的 Side Effect，但不是全部

- 發送 http request
- 在畫面印出值或是 log
- 獲得使用者 input
- Query DOM 物件

**Referential transparency**

前面提到的 pure function 不管外部環境如何，只要參數相同，函式執行的返回結果必定相同。這種不依賴任何外部狀態，只依賴於傳入的參數的特性也稱為 引用透明(Referential transparency)


### 利用參數保存狀態

由於最近很紅的 Redux 使我能很好的舉例，讓大家了解什麼是用參數保存狀態。了解 Redux 的開發者應該會知 Redux 的狀態是由各個 reducer 所組成的，而每個 reducer 的狀態就是保存在參數中！

```javascript
function countReducer(state = 0, action) {
// ...
}
```

如果你跟 Redux 不熟可以看下面遞回的例子

```javascript
function findIndex(arr, predicate, start = 0) {
    if (0 <= start && start < arr.length) {
        if (predicate(arr[start])) {
            return start;
        }
        return findIndex(arr, predicate, start+1);
    }
}
findIndex(['a', 'b'], x => x === 'b'); // 找陣列中 'b' 的 index
```

這裡我們寫了一個 findIndex 用來找陣列中的元素位置，我們在 `findIndex` 中故意多塞了一個參數用來保存當前找到第幾個 index 的**狀態**，這就是利用參數保存狀態！

>  這邊用到了遞回，遞回會不斷的呼叫自己，製造多層 stack frame，會導致運算速度較慢，而這通常需要靠編譯器做優化！

> 那 JS 有沒有做遞回優化呢？ 恭喜大家，ES6 提供了 [尾呼優化(tail call optimization)](http://www.2ality.com/2015/06/tail-call-optimization.html)，讓我們有一些手法可以讓遞回更有效率！

Functional Programming 優勢
------


### 可讀性高

當我們透過一系列的函式封裝資料的操作過程，程式碼能變得非常的簡潔且可讀性極高，例如下面的例子

```javascript
[9, 4].concat([8, 7]) // 合併陣列
      .sort()  // 排序
      .filter(x => x > 5) // 過濾出大於 5 的
```

### 可維護性高

因為 Pure function 等特性，執行結果不依賴外部狀態，且不會對外部環境有任何操作，使 Functional Programming 能更好的除錯及撰寫單元測試。

### 易於併行/平行處理

Functional Programming 易於做併行/平行(Concurrency/Parallel)處理，因為我們基本上只做運算不碰 I/O，再加上沒有 Side Effect 的特性，所以較不用擔心 deadlock 等問題。


今日小結
------

今天講了 Functional Programming 的基本特性，及其優勢。現在愈來愈多的 Library 用到了 FP 的觀念，JS 也越來越多 Functional 的函式庫，例如：Lodash, Underscore, lazy, Ramda。了解 FP 的基本觀念有助於我們在學習其他 Library 更容易上手，也能使我們撰寫出更好的程式碼，希望各位讀者有所收穫，若有任何疑問歡迎在下方留言給我！

