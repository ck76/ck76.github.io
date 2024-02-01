---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (20)：Observable Operators - window, windowToggle, groupBy
date: 2017-01-05T23:12:48.000Z
description: 前幾天我們講完了能把 Higher Order Observable 轉成一般的 Observable 的 operators，今天我們要講能夠把一般的 Observable 轉成 Higher Order Observable 的 operators。其實前端不太有機會用到這類型的 Operators，都是在比較特殊的需求下才會看到，但還是會有遇到的時候。
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Operator
  - RxJS 30 Days
previous: ./thirty-days-RxJS-19.md
next: ./thirty-days-RxJS-21.md
---

## Operators

### window

window 是一整個家族，總共有五個相關的 operators

- window
- windowCount
- windowTime
- windowToggle
- windowWhen

這裡我們只介紹 window 跟 windowToggle 這兩個方法，其他三個的用法相對都簡單很多，大家如果有需要可以再自行到官網查看。

window 很類似 buffer 可以把一段時間內送出的元素拆出來，只是 buffer 是把元素拆分到陣列中變成

```bash
Observable<T> => Observable<Array<T>>
```

而 window 則是會把元素拆分出來放到新的 observable 變成

```bash
Observable<T> => Observable<Observable<T>>
```

buffer 是把拆分出來的元素放到陣列並送出陣列；window 是把拆分出來的元素放到 observable 並送出 observable，讓我們來看一個例子
 
```javascript
var click = Rx.Observable.fromEvent(document, 'click');
var source = Rx.Observable.interval(1000);
var example = source.window(click);

example
  .switch()
  .subscribe(console.log);
// 0
// 1
// 2
// 3
// 4
// 5 ...
``` 
 
首先 window 要傳入一個 observable，每當這個 observable 送出元素時，就會把正在處理的 observable 所送出的元素放到新的 observable 中並送出，這裡看 Marble Diagram 會比較好解釋

```bash
click  : -----------c----------c------------c--
source : ----0----1----2----3----4----5----6---..
                    window(click)
example: o----------o----------o------------o--
         \          \          \
          ---0----1-|--2----3--|-4----5----6|
                    switch()
       : ----0----1----2----3----4----5----6---... 
```

這裡可以看到 example 變成發送 observable 會在每次 click 事件發送出來後結束，並繼續下一個 observable，這裡我們用 switch 才把它攤平。

當然這個範例只是想單存的表達 window 的作用，沒什麼太大的意義，實務上 window 會搭配其他的 operators 使用，例如我們想計算一秒鐘內觸發了幾次 click 事件

```javascript
var click = Rx.Observable.fromEvent(document, 'click');
var source = Rx.Observable.interval(1000);
var example = click.window(source)

example
  .map(innerObservable => innerObservable.count())
  .switch()
  .subscribe(console.log);
```
[JSBin](https://jsbin.com/fudocigewi/4/edit?html,js,output) | [JSFiddle](https://jsfiddle.net/sy1fybre/3/)

注意這裡我們把 source 跟 click 對調了，並用到了 observable 的一個方法 `count()`，可以用來取得 observable 總共送出了幾個元素，用 Marble Diagram 表示如下

```bash
source : ---------0---------1---------2--...
click  : --cc---cc----c-c----------------...
                    window(source)
example: o--------o---------o---------o--..
         \        \         \         \
          -cc---cc|---c-c---|---------|--..
                    count()
       : o--------o---------o---------o--
         \        \         \         \
          -------4|--------2|--------0|--..
                    switch()
       : ---------4---------2---------0--... 
```

從 Marble Diagram 中可以看出來，我們把部分元素放到新的 observable 中，就可以利用 Observable 的方法做更靈活的操作

windowToggle
------

windowToggle 不像 window 只能控制內部 observable 的結束，windowToggle 可以傳入兩個參數，第一個是開始的 observable，第二個是一個 callback 可以回傳一個結束的 observable，讓我們來看範例

```javascript
var source = Rx.Observable.interval(1000);
var mouseDown = Rx.Observable.fromEvent(document, 'mousedown');
var mouseUp = Rx.Observable.fromEvent(document, 'mouseup');

var example = source
  .windowToggle(mouseDown, () => mouseUp)
  .switch();
  
example.subscribe(console.log);
```
[JSBin](https://jsbin.com/fudocigewi/3/edit?html,js,output) | [JSFiddle](https://jsfiddle.net/sy1fybre/2/)

一樣用 Marble Diagram 會比較好解釋

```bash
source   : ----0----1----2----3----4----5--...

mouseDown: -------D------------------------...
mouseUp  : ---------------------------U----...

        windowToggle(mouseDown, () => mouseUp)

         : -------o-------------------------...
                  \
                   -1----2----3----4--|
                   switch()
example  : ---------1----2----3----4---------...                                     
```

從 Marble Diagram 可以看得出來，我們用 windowToggle 拆分出來內部的 observable 始於 mouseDown 終於 mouseUp。

### groupBy

最後我們來講一個實務上比較常用的 operators - groupBy，它可以幫我們把相同條件的元素拆分成一個 Observable，其實就跟平常在下 SQL 是一樣個概念，我們先來看個簡單的例子

```javascript
var source = Rx.Observable.interval(300).take(5);

var example = source
              .groupBy(x => x % 2);
              
example.subscribe(console.log);

// GroupObservable { key: 0, ...}
// GroupObservable { key: 1, ...}
```
[JSBin](https://jsbin.com/fudocigewi/1/edit?html,js,console) | [JSFiddle](https://jsfiddle.net/sy1fybre/1/)

上面的例子，我們傳入了一個 callback function 並回傳 groupBy 的條件，就能區分每個元素到不同的 Observable 中，用 Marble Diagram 表示如下

```bash
source : ---0---1---2---3---4|
             groupBy(x => x % 2)
example: ---o---o------------|
            \   \
            \   1-------3----|
            0-------2-------4|
```

在實務上，我們可以拿 groupBy 做完元素的區分後，再對 inner Observable 操作，例如下面這個例子我們將每個人的分數作加總再送出

```javascript
var people = [
    {name: 'Anna', score: 100, subject: 'English'},
    {name: 'Anna', score: 90, subject: 'Math'},
    {name: 'Anna', score: 96, subject: 'Chinese' }, 
    {name: 'Jerry', score: 80, subject: 'English'},
    {name: 'Jerry', score: 100, subject: 'Math'},
    {name: 'Jerry', score: 90, subject: 'Chinese' }, 
];
var source = Rx.Observable.from(people)
						   .zip(
						     Rx.Observable.interval(300), 
						     (x, y) => x);

var example = source
  .groupBy(person => person.name)
  .map(group => group.reduce((acc, curr) => ({ 
	    name: curr.name,
	    score: curr.score + acc.score 
	})))
	.mergeAll();
	
example.subscribe(console.log);
// { name: "Anna", score: 286 }
// { name: 'Jerry', score: 270 }
```
[JSBin](https://jsbin.com/fudocigewi/2/edit?html,js,console) | [JSFiddle](https://jsfiddle.net/sy1fybre/)

這裡我們範例是想把 Jerry 跟 Anna 的分數個別作加總，畫成 Marble Diagram 如下

```bash
source : --o--o--o--o--o--o|
  
  groupBy(person => person.name)
     
       : --i--------i------|
           \        \
           \         o--o--o|
            o--o--o--|
            
	   map(group => group.reduce(...))
	     
       : --i---------i------|
           \         \
           o|        o|
        
             mergeAll()
example: --o---------o------|           
```


今日小結
------

今天講了兩個可以把元素拆分到新的 observable 的 operators，這兩個 operators 在前端比較少用到，但在後端或是比較複雜了前端應用才比較有機會用到。不知道讀者有沒有收穫呢？ 如果有任何問題歡迎留言給我，謝謝。