---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (17)：Observable Operators - switch, mergeAll, concatAll
date: 2017-01-02T23:47:35.000Z
description: 今天我們要講三個 operators，這三個 operators 都是用來處理 Higher Order Observable。
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Operator
  - RxJS 30 Days
previous: ./thirty-days-RxJS-16.md
next: ./thirty-days-RxJS-18.md
---

所謂的 Higher Order Observable 就是指一個 Observable 送出的元素還是一個 Observable，就像是二維陣列一樣，一個陣列中的每個元素都是陣列。如果用泛型來表達就像是

```bash
Observable<Observable<T>>
```

通常我們需要的是第二層 Observable 送出的元素，所以我們希望可以把二維的 Observable 改成一維的，像是下面這樣

```bash
Observable<Observable<T>> => Observable<T>
```

其實想要做到這件事有三個方法 switch、mergeAll 和 concatAll，其中 concatAll 我們在之前的文章已經稍微講過了，今天這篇文章會講解這三個 operators 各自的效果跟差異。

Operators
------

### concatAll

我們在講簡易拖拉的範例時就有講過這個 operator，concatAll 最重要的重點就是他會處理完前一個 observable 才會在處理下一個 observable，讓我們來看一個範例

```javascript
var click = Rx.Observable.fromEvent(document.body, 'click');
var source = click.map(e => Rx.Observable.interval(1000));

var example = source.concatAll();
example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// (點擊後)
// 0
// 1
// 2
// 3
// 4
// 5 ...
```
[JSBin](https://jsbin.com/numuji/2/edit?js,console,output)

上面這段程式碼，當我們點擊畫面時就會開始送出數值，如果用 Marble Diagram 表示如下

```bash
click  : ---------c-c------------------c--.. 
        map(e => Rx.Observable.interval(1000))
source : ---------o-o------------------o--..
                   \ \
                    \ ----0----1----2----3----4--...
                     ----0----1----2----3----4--...
                     concatAll()
example: ----------------0----1----2----3----4--..
```

從 Marble Diagram 可以看得出來，當我們點擊一下 click 事件會被轉成一個 observable 而這個 observable 會每一秒送出一個遞增的數值，當我們用 concatAll 之後會把二維的 observable 攤平成一維的 observable，但 **concatAll 會一個一個處理，一定是等前一個 observable 完成(complete)才會處理下一個 observable**，因為現在送出 observable 是無限的永遠不會完成(complete)，就導致他永遠不會處理第二個送出的 observable!

我們再看一個例子

```javascript
var click = Rx.Observable.fromEvent(document.body, 'click');
var source = click.map(e => Rx.Observable.interval(1000).take(3));

var example = source.concatAll();
example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```

現在我們把送出的 observable 限制只取前三個元素，用 Marble Diagram 表示如下

```bash
click  : ---------c-c------------------c--.. 
        map(e => Rx.Observable.interval(1000))
source : ---------o-o------------------o--..
                   \ \                  \
                    \ ----0----1----2|   ----0----1----2|
                     ----0----1----2|
                     concatAll()
example: ----------------0----1----2----0----1----2--..
```

這裡我們把送出的 observable 變成有限的，只會送出三個元素，這時就能看得出來 concatAll 不管兩個 observable 送出的時間多麽相近，一定會先處理前一個 observable 再處理下一個。

### switch

switch 同樣能把二維的 observable 攤平成一維的，但他們在行為上有很大的不同，我們來看下面這個範例

```javascript
var click = Rx.Observable.fromEvent(document.body, 'click');
var source = click.map(e => Rx.Observable.interval(1000));

var example = source.switch();
example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```
[JSBin](https://jsbin.com/numuji/edit?js,console,output)

用 Marble Diagram 表示如下

```bash
click  : ---------c-c------------------c--.. 
        map(e => Rx.Observable.interval(1000))
source : ---------o-o------------------o--..
                   \ \                  \----0----1--...
                    \ ----0----1----2----3----4--...
                     ----0----1----2----3----4--...
                     switch()
example: -----------------0----1----2--------0----1--...
```

switch 最重要的就是他會**在新的 observable 送出後直接處理新的 observable 不管前一個 observable 是否完成，每當有新的 observable 送出就會直接把舊的 observable 退訂(unsubscribe)，永遠只處理最新的 observable!**

所以在這上面的 Marble Diagram 可以看得出來第一次送出的 observable 跟第二次送出的 observable 時間點太相近，導致第一個 observable 還來不及送出元素就直接被退訂了，當下一次送出 observable 就又會把前一次的 observable 退訂。

### mergeAll

我們之前講過 merge 他可以讓多個 observable 同時送出元素，mergeAll 也是同樣的道理，它會把二維的 observable 轉成一維的，並且能夠同時處理所有的 observable，讓我們來看這個範例

```javascript
var click = Rx.Observable.fromEvent(document.body, 'click');
var source = click.map(e => Rx.Observable.interval(1000));

var example = source.mergeAll();
example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```

上面這段程式碼用 Marble Diagram 表示如下

```bash
click  : ---------c-c------------------c--.. 
        map(e => Rx.Observable.interval(1000))
source : ---------o-o------------------o--..
                   \ \                  \----0----1--...
                    \ ----0----1----2----3----4--...
                     ----0----1----2----3----4--...
                     switch()
example: ----------------00---11---22---33---(04)4--...
```

從 Marble Diagram 可以看出來，所有的 observable 是並行(Parallel)處理的，也就是說 mergeAll 不會像 switch 一樣退訂(unsubscribe)原先的 observable 而是並行處理多個 observable。以我們的範例來說，當我們點擊越多下，最後送出的頻率就會越快。

另外 mergeAll 可以傳入一個數值，這個數值代表他可以同時處理的 observable 數量，我們來看一個例子

```javascript
var click = Rx.Observable.fromEvent(document.body, 'click');
var source = click.map(e => Rx.Observable.interval(1000).take(3));

var example = source.mergeAll(2);
example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```

這裡我們送出的 observable 改成取前三個，並且讓 mergeAll 最多只能同時處理 2 個 observable，用 Marble Diagram 表示如下


```bash
click  : ---------c-c----------o----------.. 
        map(e => Rx.Observable.interval(1000))
source : ---------o-o----------c----------..
                   \ \          \----0----1----2|     
                    \ ----0----1----2|  
                     ----0----1----2|
                     mergeAll(2)
example: ----------------00---11---22---0----1----2--..
```

當 mergeAll 傳入參數後，就會等處理中的其中一個 observable 完成，再去處理下一個。以我們的例子來說，前面兩個 observabel 可以被並行處理，但第三個 observable 必須等到第一個 observable 結束後，才會開始。

我們可以利用這個參數來決定要同時處理幾個 observable，如果我們傳入 `1` 其行為就會跟 `concatAll` 是一模一樣的，這點在原始碼可以看到他們是完全相同的。

今日小結
------

今天介紹了三個可以處理 High Order Observable 的方法，並講解了三個方法的差異，不知道讀者有沒有收穫呢？ 如果有任何問題歡迎在下方留言給我，感謝！