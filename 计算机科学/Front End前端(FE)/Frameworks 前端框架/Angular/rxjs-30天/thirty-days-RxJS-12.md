---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (12)：Observable Operator - scan, buffer
date: 2016-12-28T23:11:24.000Z
description: 今天要繼續講兩個簡單的 transformation operators 並帶一些小範例，這兩個 operators 都是實務上很常會用到的方法。
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Operator
  - RxJS 30 Days
previous: ./thirty-days-RxJS-11.md
next: ./thirty-days-RxJS-13.md
---

Operators
------

### scan

scan 其實就是 Observable 版本的 reduce 只是命名不同。如果熟悉陣列操作的話，應該會知道原生的 JS Array 就有 reduce 的方法，使用方式如下

```javascript
var arr = [1, 2, 3, 4];
var result = arr.reduce((origin, next) => { 
    console.log(origin)
    return origin + next
}, 0);

console.log(result)
// 0
// 1
// 3
// 6
// 10
```
[JSBin](https://jsbin.com/guyaki/1/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/brkztLLw/)

reduce 方法需要傳兩個參數，第一個是 callback 第二個則是起始狀態，這個 callback 執行時，會傳入兩個參數一個是原本的狀態，第二個是修改原本狀態的參數，最後回傳一個新的狀態，再繼續執行。

所以這段程式碼是這樣執行的

- 第一次執行 callback 起始狀態是 0 所以 origin 傳入 0，next 為 arr 的第一個元素 1，相加之後變成 1 回傳並當作下一次的狀態。
- 第二次執行 callback，這時原本的狀態(origin)就變成了 1，next 為 arr 的第二個元素 2，相加之後變成 3 回傳並當作下一次的狀態。
-  第三次執行 callback，這時原本的狀態(origin)就變成了 3，next 為 arr 的第三個元素 3，相加之後變成 6 回傳並當作下一次的狀態。
-  第三次執行 callback，這時原本的狀態(origin)就變成了 6，next 為 arr 的第四個元素 4，相加之後變成 10 回傳並當作下一次的狀態。
-  這時 arr 的元素都已經遍歷過了，所以不會直接把 10 回傳。

scan 整體的運作方式都跟 reduce 一樣，範例如下

```javascript
var source = Rx.Observable.from('hello')
             .zip(Rx.Observable.interval(600), (x, y) => x);

var example = source.scan((origin, next) => origin + next, '');

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// h
// he
// hel
// hell
// hello
// complete
```
[JSBin](https://jsbin.com/guyaki/8/edit?html,js,console,output) |[JSFiddle](https://jsfiddle.net/s6323859/brkztLLw/1/)

畫成 Marble Diagram

```bash
source : ----h----e----l----l----o|
    scan((origin, next) => origin + next, '')
example: ----h----(he)----(hel)----(hell)----(hello)|
```

這裡可以看到第一次傳入 `'h'` 跟 `''` 相加，返回 `'h'` 當作下一次的初始狀態，一直重複下去。

> scan 跟 reduce 最大的差別就在 scan 一定會回傳一個 observable 實例，而 reduce 最後回傳的值有可能是任何資料型別，必須看使用者傳入的 callback 才能決定 reduce 最後的返回值。

> Jafar Husain 就曾說：「JavaScript 的 reduce 是錯了，它最後應該永遠回傳陣列才對！」

> 如果大家之前有到[這裡](http://reactivex.io/learnrx/)練習的話，會發現 reduce 被設計成一定回傳陣列，而這個網頁就是 Jafar 做的。

scan 很常用在狀態的計算處理，最簡單的就是對一個數字的加減，我們可以綁定一個 button 的 click 事件，並用 map 把 click event 轉成 1，之後送處 scan 計算值再做顯示。

這裡筆者寫了一個小範例，來示範如何做最簡單的加減

```javascript
const addButton = document.getElementById('addButton');
const minusButton = document.getElementById('minusButton');
const state = document.getElementById('state');

const addClick = Rx.Observable.fromEvent(addButton, 'click').mapTo(1);
const minusClick = Rx.Observable.fromEvent(minusButton, 'click').mapTo(-1);

const numberState = Rx.Observable.empty()
  .startWith(0)
  .merge(addClick, minusClick)
  .scan((origin, next) => origin + next, 0)
  
numberState
  .subscribe({
    next: (value) => { state.innerHTML = value;},
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
  });
```
[JSBin](https://jsbin.com/guyaki/4/edit?js,output) | [JSFiddle](https://jsfiddle.net/s6323859/yf02gt9j/1/)

這裡我們用了兩個 button，一個是 add 按鈕，一個是 minus 按鈕。

我把這兩個按鈕的點擊事件各建立了 `addClcik`, `minusClick` 兩個 observable，這兩個 observable 直接 mapTo(1) 跟 mapTo(-1)，代表被點擊後會各自送出的數字！

接著我們用了 `empty()` 建立一個空的 observable 代表畫面上數字的狀態，搭配 `startWith(0)` 來設定初始值，接著用 `merge` 把兩個 observable 合併透過 scan 處理之後的邏輯，最後在 subscribe 來更改畫面的顯示。

這個小範例用到了我們這幾天講的 operators，包含 mapTo, empty, startWith, merge 還有現在講的 scan，建議讀者一定要花時間稍微練習一下。

### buffer

buffer 是一整個家族，總共有五個相關的 operators

- buffer
- bufferCount
- bufferTime
- bufferToggle
- bufferWhen

這裡比較常用到的是 buffer, bufferCount 跟 bufferTime 這三個，我們直接來看範例。

```javascript
var source = Rx.Observable.interval(300);
var source2 = Rx.Observable.interval(1000);
var example = source.buffer(source2);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// [0,1,2]
// [3,4,5]
// [6,7,8]...
```
[JSBin](https://jsbin.com/guyaki/9/edit?html,js,console,output) | [JSFiddle](https://jsfiddle.net/s6323859/brkztLLw/2/)

畫成 Marble Diagram 則像是

```bash
source : --0--1--2--3--4--5--6--7..
source2: ---------0---------1--------...
            buffer(source2)
example: ---------([0,1,2])---------([3,4,5])    
```

buffer 要傳入一個 observable(source2)，它會把原本的 observable (source)送出的元素緩存在陣列中，等到傳入的 observable(source2) 送出元素時，就會觸發把緩存的元素送出。

這裡的範例 source2 是每一秒就會送出一個元素，我們可以改用 bufferTime 簡潔的表達，如下

```javascript
var source = Rx.Observable.interval(300);
var example = source.bufferTime(1000);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// [0,1,2]
// [3,4,5]
// [6,7,8]...
```
[JSBin](https://jsbin.com/guyaki/5/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/brkztLLw/3/)

除了用時間來作緩存外，我們更常用數量來做緩存，範例如下

```javascript
var source = Rx.Observable.interval(300);
var example = source.bufferCount(3);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// [0,1,2]
// [3,4,5]
// [6,7,8]...
```
[JSBin](https://jsbin.com/guyaki/10/edit?html,js,console,output) | [JSFiddle](https://jsfiddle.net/s6323859/brkztLLw/4/)
在實務上，我們可以用 buffer 來做某個事件的過濾，例如像是滑鼠連點才能真的執行，這裡我們一樣寫了一個小範例

```javascript
const button = document.getElementById('demo');
const click = Rx.Observable.fromEvent(button, 'click')
const example = click
                .bufferTime(500)
                .filter(arr => arr.length >= 2);

example.subscribe({
    next: (value) => { console.log('success'); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```
[JSBin](https://jsbin.com/sudepabiji/1/edit?html,js,console,output) | [JSFiddle](https://jsfiddle.net/s6323859/brkztLLw/5/)

這裡我們只有在 500 毫秒內連點兩下，才能成功印出 `'success'`，這個功能在某些特殊的需求中非常的好用，也能用在批次處理來降低 request 傳送的次數！

今日小結
------

今天我們介紹了兩個 operators 分別是 scan, buffer，也做了兩個小範例，不知道讀者有沒有收穫呢？


