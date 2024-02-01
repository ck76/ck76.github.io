---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (13)：Observable Operator - delay, delayWhen
date: 2016-12-29T23:47:00.000Z
description: 在所有非同步中行為中，最麻煩的大概就是 UI 操作了，因為 UI 是直接影響使用者的感受，如果處理的不好對使用體驗會大大的扣分！
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Operator
  - RxJS 30 Days
previous: ./thirty-days-RxJS-12.md
next: ./thirty-days-RxJS-14.md
---

UI 大概是所有非同步行為中最不好處理的，不只是因為它直接影響了用戶體驗，更大的問題是 UI 互動常常是高頻率觸發的事件，而且多個元件間的時間序需要不一致，要做到這樣的 UI 互動就不太可能用 Promise 或 async/await，但是用 RxJS 仍然能輕易地處理！


今天我們要介紹的兩個 Operators，delay 跟 delayWhen 都是跟 UI 互動比較相關的。當我們的網頁越來越像應用程式時，UI 互動就變得越重要，讓我們來試試如何用 RxJS 完成基本的 UI 互動！

Operators
------

### delay

delay 可以延遲 observable 一開始發送元素的時間點，範例如下

```javascript
var source = Rx.Observable.interval(300).take(5);

var example = source.delay(500);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// 0
// 1
// 2
// 3
// 4
```
[JSBin](https://jsbin.com/qodegan/1/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/pnjw51o5/)

當然直接從 log 出的訊息看，是完全看不出差異的

讓我們直接看 Marble Diagram

```bash
source : --0--1--2--3--4|
        delay(500)
example: -------0--1--2--3--4|
```

從 Marble Diagram 可以看得出來，第一次送出元素的時間變慢了，雖然在這裡看起來沒什麼用，但是在 UI 操作上是非常有用的，這個部分我們最後示範。

delay 除了可以傳入毫秒以外，也可以傳入 Date 型別的資料，如下使用方式

```javascript
var source = Rx.Observable.interval(300).take(5);

var example = source.delay(new Date(new Date().getTime() + 1000));

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```
[JSBin](https://jsbin.com/qodegan/2/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/pnjw51o5/1/)

~~這好像也能用在預定某個日期，讓程式掛掉~~

delayWhen
------

delayWhen 的作用跟 delay 很像，最大的差別是 delayWhen 可以影響每個元素，而且需要傳一個 callback 並回傳一個 observable，範例如下

```javascript
var source = Rx.Observable.interval(300).take(5);

var example = source
              .delayWhen(
                  x => Rx.Observable.empty().delay(100 * x * x)
              );

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```
[JSBin](https://jsbin.com/qodegan/3/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/pnjw51o5/2/)

這時我們的 Marble Diagram 如下

```bash
source : --0--1--2--3--4|
    .delayWhen(x => Rx.Observable.empty().delay(100 * x * x));
example: --0---1----2-----3-----4|
```

這裡傳進來的 x 就是 source 送出的每個元素，這樣我們就能對每一個做延遲。

這裡我們用 delay 來做一個小功能，這個功能很簡單就是讓多張照片跟著滑鼠跑，但每張照片不能跑一樣快！

首先我們準備六張大頭照，並且寫進 HTML

```html
<img src="https://res.cloudinary.com/dohtkyi84/image/upload/c_scale,w_50/v1483019072/head-cover6.jpg" alt="">
<img src="https://res.cloudinary.com/dohtkyi84/image/upload/c_scale,w_50/v1483019072/head-cover5.jpg" alt="">
<img src="https://res.cloudinary.com/dohtkyi84/image/upload/c_scale,w_50/v1483019072/head-cover4.jpg" alt="">
<img src="https://res.cloudinary.com/dohtkyi84/image/upload/c_scale,w_50/v1483019072/head-cover3.jpg" alt="">
<img src="https://res.cloudinary.com/dohtkyi84/image/upload/c_scale,w_50/v1483019072/head-cover2.jpg" alt="">
<img src="https://res.cloudinary.com/dohtkyi84/image/upload/c_scale,w_50/v1483019072/head-cover1.jpg" alt="">
```

用 CSS 把 img 改成圓形，並加上邊筐以及絕對位置

```css
img{
  position: absolute;
  border-radius: 50%;
  border: 3px white solid;
  transform: translate3d(0,0,0);
}
```

再來寫 JS，一樣第一步先抓 DOM

```javascript
var imgList = document.getElementsByTagName('img');
```

第二步建立 observable

```javascript
var movePos = Rx.Observable.fromEvent(document, 'mousemove')
.map(e => ({ x: e.clientX, y: e.clientY }))
```

第三步撰寫邏輯

```javascript
function followMouse(DOMArr) {
  const delayTime = 600;
  DOMArr.forEach((item, index) => {
    movePos
      .delay(delayTime * (Math.pow(0.65, index) + Math.cos(index / 4)) / 2)
      .subscribe(function (pos){
        item.style.transform = 'translate3d(' + pos.x + 'px, ' + pos.y + 'px, 0)';
      });
  });
}

followMouse(Array.from(imgList))
```

這裡我們把 imgList 從 Collection 轉成 Array 後傳入 `followMouse()`，並用 forEach 把每個 omg 取出並利用 index 來達到不同的 delay 時間，這個 delay 時間的邏輯大家可以自己想，不用跟我一樣，最後 subscribe 就完成啦！

最後完整的範例在[這裡](https://jsbin.com/hayixa/2/edit?html,css,js,output)


今日小結
------

今天我們介紹了兩個 operators 並帶了一個小範例，這兩個 operators 在 UI 操作上都非常的實用，我們明天會接著講幾個 operators 可以用來做高頻率觸發的事件優化！

