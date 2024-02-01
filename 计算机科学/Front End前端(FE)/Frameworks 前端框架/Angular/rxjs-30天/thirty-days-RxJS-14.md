---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (14)：Observable Operator - throttle, debounce
date: 2016-12-30T23:46:30.000Z
description: 昨天講到了在 UI 操作上很常用的 delay，今天我們接著要來講另外兩個也非常實用 operators，尤其在做效能優化時更是不可或缺的好工具！
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Operator
  - RxJS 30 Days
previous: ./thirty-days-RxJS-13.md
next: ./thirty-days-RxJS-15.md
---

Operators
------

### debounce

跟 buffer、bufferTime 一樣，Rx 有 debounce 跟 debounceTime 一個是傳入 observable 另一個則是傳入毫秒，比較常用到的是 debounceTime，這裡我們直接來看一個範例

```javascript
var source = Rx.Observable.interval(300).take(5);
var example = source.debounceTime(1000);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// 4
// complete
```
[JSBin](https://jsbin.com/nemepo/5/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/kqwk0yvp/1/)

這裡只印出 `4` 然後就結束了，因為 **debounce 運作的方式是每次收到元素，他會先把元素 cache 住並等待一段時間，如果這段時間內已經沒有收到任何元素，則把元素送出；如果這段時間內又收到新的元素，則會把原本 cache 住的元素釋放掉並重新計時，不斷反覆。**

以現在這個範例來講，我們每 300 毫秒就會送出一個數值，但我們的 debounceTime 是 1000 毫秒，也就是說每次 debounce 收到元素還等不到 1000 毫秒，就會收到下一個新元素，然後重新等待 1000 毫秒，如此重複直到第五個元素送出時，observable 結束(complete)了，debounce 就直接送出元素。

以 Marble Diagram 表示如下

```bash
source : --0--1--2--3--4|
        debounceTime(1000)
example: --------------4|        
```

debounce 會在收到元素後等待一段時間，這很適合用來處理**間歇行為**，間歇行為就是指這個行為是一段一段的，例如要做 Auto Complete 時，我們要打字搜尋不會一直不斷的打字，可以等我們停了一小段時間後再送出，才不會每打一個字就送一次 request！

這裡舉一個簡單的例子，假設我們想要自動傳送使用者打的字到後端

```javascript
const searchInput = document.getElementById('searchInput');
const theRequestValue = document.getElementById('theRequestValue');

Rx.Observable.fromEvent(searchInput, 'input')
  .map(e => e.target.value)
  .subscribe((value) => {
    theRequestValue.textContent = value;
    // 在這裡發 request
  })
```

如果用上面這段程式碼，就會每打一個字就送一次 request，當很多人在使用時就會對 server 造成很大的負擔，實際上我們只需要使用者最後打出來的文字就好了，不用每次都送，這時就能用 debounceTime 做優化。

```javascript
const searchInput = document.getElementById('searchInput');
const theRequestValue = document.getElementById('theRequestValue');

Rx.Observable.fromEvent(searchInput, 'input')
  .debounceTime(300)
  .map(e => e.target.value)
  .subscribe((value) => {
    theRequestValue.textContent = value;
    // 在這裡發 request
  })
```
[JSBin](https://jsbin.com/nemepo/2/edit?js,output) | [JSFiddle](https://jsfiddle.net/s6323859/kqwk0yvp/2/)

這裡建議大家到 JSBin 親手試試，可以把 `debounceTime(300)` 註解掉，看看前後的差異。


### throttle

基本上每次看到 debounce 就會看到 throttle，他們兩個的作用都是要降低事件的觸發頻率，但行為上有很大的不同。

跟 debounce 一樣 RxJS 有 throttle 跟 throttleTime 兩個方法，一個是傳入 observable 另一個是傳入毫秒，比較常用到的也是 throttleTime，讓我們直接來看範例

```javascript
var source = Rx.Observable.interval(300).take(5);
var example = source.throttleTime(1000);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// 0
// 4
// complete
```
[JSBin](https://jsbin.com/nemepo/6/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/kqwk0yvp/)

跟 debounce 的不同是 throttle 會先開放送出元素，等到有元素被送出就會沈默一段時間，等到時間過了又會開放發送元素。

throttle 比較像是控制行為的最高頻率，也就是說如果我們設定 **1000 毫秒**，那該事件頻率的最大值就是**每秒觸發一次**不會再更快，debounce 則比較像是必須等待的時間，要等到一定的時間過了才會收到元素。

throttle 更適合用在**連續性行為**，比如說 UI 動畫的運算過程，因為 UI 動畫是連續的，像我們之前在做拖拉時，就可以加上 `throttleTime(12)` 讓 mousemove event 不要發送的太快，避免畫面更新的速度跟不上樣式的切換速度。

> 瀏覽器有一個 [requestAnimationFrame](https://developer.mozilla.org/zh-TW/docs/Web/API/Window.requestAnimationFrame) API 是專門用來優化 UI 運算的，通常用這個的效果會比 throttle 好，但並不是絕對還是要看最終效果。

> RxJS 也能用 requestAnimationFrame 做優化，而且使用方法很簡單，這個部份會在 Scheduler 提到。

今日小結
------

今天介紹了兩個非常實用的方法，可以幫我們做程式的效能優化，而且使用方式非常的簡單，不知道讀者有沒有收穫？ 如果有任何問題，歡迎在下方留言給我，謝謝。

