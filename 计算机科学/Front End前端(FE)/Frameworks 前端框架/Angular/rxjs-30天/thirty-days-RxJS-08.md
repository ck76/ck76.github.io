---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (08)：簡易拖拉實作 - take, first, takeUntil, concatAll
date: 2016-12-24T23:23:27.000Z
description: 如果是你會如何實作拖拉的功能？
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Drag&Drop
  - RxJS 30 Days
previous: ./thirty-days-RxJS-07.md
next: ./thirty-days-RxJS-09.md
---

這是【30天精通 RxJS】的 08 篇，如果還沒看過 07 篇可以往這邊走：
[30 天精通 RxJS (07)： Observable Operators & Marble Diagrams](/series/rxjs/thirty-days-RxJS-07)

今天建議大家直接看影片
[![Yes](https://img.youtube.com/vi/bgi3Uaab1ok/0.jpg)](https://www.youtube.com/watch?v=bgi3Uaab1ok)

我們今天要接著講 take, first, takeUntil, concatAll 這四個 operators，並且實作一個簡易的拖拉功能。

Operators
------

### take

take 是一個很簡單的 operator，顧名思義就是取前幾個元素後就結束，範例如下

```javascript
var source = Rx.Observable.interval(1000);
var example = source.take(3);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// 0
// 1
// 2
// complete
```
[JSBin](https://jsbin.com/jogesut/3/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/ckyjuuva/1/)

這裡可以看到我們的 `source` 原本是會發出無限個元素的，但這裡我們用 `take(3)` 就會只取前 3 個元素，取完後就直接結束(complete)。

用 Marble diagram 表示如下

```bash
source : -----0-----1-----2-----3--..
                take(3)
example: -----0-----1-----2|
``` 

### first

first 會取 observable 送出的第 1 個元素之後就直接結束，行為跟 take(1) 一致。

```javascript
var source = Rx.Observable.interval(1000);
var example = source.first();

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});

// 0
// complete
```
[JSBin](https://jsbin.com/jogesut/5/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/ckyjuuva/3/)

用 Marble diagram 表示如下

```bash
source : -----0-----1-----2-----3--..
                first()
example: -----0|
```

### takeUntil

在實務上 takeUntil 很常使用到，他可以在某件事情發生時，讓一個 observable 直送出 完成(complete)訊息，範例如下

```javascript
var source = Rx.Observable.interval(1000);
var click = Rx.Observable.fromEvent(document.body, 'click');
var example = source.takeUntil(click);     
   
example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// 0
// 1
// 2
// 3
// complete (點擊body了
```
[JSBin](https://jsbin.com/jogesut/2/edit?js,console,output) | [JSFiddle](https://jsfiddle.net/s6323859/ckyjuuva/)

這裡我們一開始先用 `interval` 建立一個 observable，這個 observable 每隔 1 秒會送出一個從 0 開始遞增的數值，接著我們用 `takeUntil`，傳入另一個 observable。

當 `takeUntil` 傳入的 observable 發送值時，原本的 observable 就會直接進入完成(complete)的狀態，並且發送完成訊息。也就是說上面這段程式碼的行為，會先每 1 秒印出一個數字(從 0 遞增)直到我們點擊 body 為止，他才會送出 complete 訊息。

如果畫成 Marble Diagram 則會像下面這樣

```bash
source : -----0-----1-----2------3--
click  : ----------------------c----
                takeUntil(click)
example: -----0-----1-----2----|
```

當 click 一發送元素的時候，observable 就會直接完成(complete)。

### concatAll

有時我們的 Observable 送出的元素又是一個 observable，就像是二維陣列，陣列裡面的元素是陣列，這時我們就可以用 `concatAll` 把它攤平成一維陣列，大家也可以直接把 concatAll 想成把所有元素 concat 起來。

```javascript
var click = Rx.Observable.fromEvent(document.body, 'click');
var source = click.map(e => Rx.Observable.of(1,2,3));

var example = source.concatAll();
example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```
[JSBin](https://jsbin.com/jogesut/6/edit?js,console,output) | [JSFiddle](https://jsfiddle.net/s6323859/ckyjuuva/4/)

這個範例我們每點擊一次 body 就會立刻送出 1,2,3，如果用 Marble diagram 表示則如下

```bash
click  : ------c------------c--------

        map(e => Rx.Observable.of(1,2,3))

source : ------o------------o--------
                \            \
                 (123)|       (123)|

                   concatAll()

example: ------(123)--------(123)------------
```

這裡可以看到 `source` observable 內部每次發送的值也是 observable，這時我們用 concatAll 就可以把 source 攤平成 example。

這裡需要注意的是 `concatAll` 會處理 source 先發出來的 observable，必須等到這個 observable 結束，才會再處理下一個 source 發出來的 observable，讓我們用下面這個範例說明。

```javascript
var obs1 = Rx.Observable.interval(1000).take(5);
var obs2 = Rx.Observable.interval(500).take(2);
var obs3 = Rx.Observable.interval(2000).take(1);

var source = Rx.Observable.of(obs1, obs2, obs3);

var example = source.concatAll();

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
// 0
// 1
// 0
// complete
```
[JSBin](https://jsbin.com/jogesut/4/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/ckyjuuva/2/)

這裡可以看到 `source` 會送出 3 個 observable，但是 `concatAll` 後的行為永遠都是先處理第一個 observable，**等到當前處理的結束後才會再處理下一個**。

用 Marble diagram 表示如下

```bash
source : (o1                 o2      o3)|
           \                  \       \
            --0--1--2--3--4|   -0-1|   ----0|
                
                concatAll()        

example: --0--1--2--3--4-0-1----0|
```

簡易拖拉
------

當學完前面幾個 operator 後，我們就很輕鬆地做出拖拉的功能，先讓我們來看一下需求

1. 首先畫面上有一個元件(#drag)
2. 當滑鼠在元件(#drag)上按下左鍵(mousedown)時，開始監聽滑鼠移動(mousemove)的位置
3. 當滑鼠左鍵放掉(mouseup)時，結束監聽滑鼠移動
4. 當滑鼠移動(mousemove)被監聽時，跟著修改元件的樣式屬性

第一步我已經完成了，大家可以直接到以下兩個連結做練習

- [JSBin](https://jsbin.com/yopawop/1/edit?js,output)
- [JSFiddle](https://jsfiddle.net/s6323859/dc0se480/)

第二步我們要先取得各個 DOM 物件，元件(#drag) 跟 body。

```javascript
const dragDOM = document.getElementById('drag');
const body = document.body;
```

要取得 body 的原因是因為滑鼠移動(mousemove)跟滑鼠左鍵放掉(mouseup)都應該是在整個 body 監聽。

第三步我們寫出各個會用到的監聽事件，並用 `fromEvent` 來取得各個 observable。

- 對 #drag 監聽 mousedown
- 對 body 監聽 mouseup
- 對 body 監聽 mousemove

```javascript
const mouseDown = Rx.Observable.fromEvent(dragDOM, 'mousedown');
const mouseUp = Rx.Observable.fromEvent(body, 'mouseup');
const mouseMove = Rx.Observable.fromEvent(body, 'mousemove');
```

> 記得還沒 `subscribe` 之前都不會開始監聽，一定會等到 subscribe 之後 observable 才會開始送值。

第四步開始寫邏輯

**當 mouseDown 時，轉成 mouseMove 的事件**

```javascript
const source = mouseDown.map(event => mouseMove)
```

**mouseMove 要在 mouseUp 後結束**

加上 `takeUntil(mouseUp)`

```javascript
const source = mouseDown
               .map(event => mouseMove.takeUntil(mouseUp))
```

這時 source 大概長像這樣

```
source: -------e--------------e-----
                \              \
                  --m-m-m-m|     -m--m-m--m-m|
```

> m 代表 mousemove event

用 `concatAll()` 攤平 source 成一維。

```javascript
const source = mouseDown
               .map(event => mouseMove.takeUntil(mouseUp))
               .concatAll();                 
```

用 map 把 mousemove event 轉成 x,y 的位置，並且訂閱。

```javascript
source
.map(m => {
    return {
        x: m.clientX,
        y: m.clientY
    }
})
.subscribe(pos => {
  	dragDOM.style.left = pos.x + 'px';
    dragDOM.style.top = pos.y + 'px';
})              
```

到這裡我們就已經完成了簡易的拖拉功能了!完整的程式碼如下

```javascript
const dragDOM = document.getElementById('drag');
const body = document.body;

const mouseDown = Rx.Observable.fromEvent(dragDOM, 'mousedown');
const mouseUp = Rx.Observable.fromEvent(body, 'mouseup');
const mouseMove = Rx.Observable.fromEvent(body, 'mousemove');

mouseDown
  .map(event => mouseMove.takeUntil(mouseUp))
  .concatAll()
  .map(event => ({ x: event.clientX, y: event.clientY }))
  .subscribe(pos => {
  	dragDOM.style.left = pos.x + 'px';
    dragDOM.style.top = pos.y + 'px';
  })
```

不知道讀者有沒有感受到，我們整個程式碼不到 15 行，而且只要能夠看懂各個 operators，我們程式可讀性是非常的高。

雖然這只是一個簡單的拖拉實現，但已經展示出 RxJS 帶來的威力，它讓我們的程式碼更加的簡潔，也更好的維護！

> [這裡](https://jsfiddle.net/s6323859/1ahzh7a7/2/)有完整的成果可以參考。

今日小結
------

我們今天介紹了四個 operators 分別是 take, first, takeUntil, concatAll，並且完成了一個簡易的拖拉功能，我們之後會把這個拖拉功能做得更完整，並且整合其他功能！

不知道讀者今天有沒有收穫？如果有任何問題，歡迎在下方留言給我！
如果你喜歡這篇文章，請至標題旁幫我按個 星星＋like，謝謝。