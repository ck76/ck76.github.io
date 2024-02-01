---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (11)：實務範例 - 完整拖拉應用
date: 2016-12-27T23:11:24.000Z
description: 有次不小心進到了優酷，發現優酷有個不錯的功能，能大大的提升用戶體驗，就讓我們一起來實作這個效果吧！
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Drag&Drop
  - RxJS 30 Days
previous: ./thirty-days-RxJS-10.md
next: ./thirty-days-RxJS-12.md
---

一樣建議大家可以直接看影片
[![Yes](https://img.youtube.com/vi/ANtuoQPkDDM/0.jpg)](https://www.youtube.com/watch?v=ANtuoQPkDDM)


在第 08 篇的時候，我們已經成功做出簡易的拖拉效果，今天要來做一個完整的應用，而且是實務上有機會遇到但不好處理的需求，那就是優酷的影片效果！


> 如果還沒有用過優酷的讀者可以先前往[這裡](http://v.youku.com/v_show/id_XMTQ5NTg2MDk3Ng==.html?&f=26824174&from=y1.2-3.4.2&spm=a2h0j.8191423.item_XMTQ5NTg2MDk3Ng==.A)試用。

當我們在優酷看影片時往下滾動畫面，影片會變成一個小視窗在右下角，這個視窗還能夠拖拉移動位置。這個功能可以讓使用者一邊看留言同時又能看影片，且不影響其他的資訊顯示，真的是很不錯的 feature。

![優酷影片拖拉功能](https://res.cloudinary.com/dohtkyi84/image/upload/v1482841073/30days/youku_drag.png)

就讓我們一起來實作這個功能，同時補完拖拉所需要注意的細節吧！

需求分析
------

首先我們會有一個影片在最上方，原本是位置是靜態(static)的，捲軸滾動到低於影片高度後，影片改為相對於視窗的絕對位置(fixed)，往回滾會再變回原本的狀態。當影片為 fixed 時，滑鼠移至影片上方(hover)會有遮罩(masker)與鼠標變化(cursor)，可以拖拉移動(drag)，且移動範圍不超過可視區間！

上面可以拆分成以下幾個步驟

- 準備 static 樣式與 fixed 樣式
- HTML 要有一個固定位置的錨點(anchor)
- 當滾動超過錨點，則影片變成 fixed
- 當往回滾動過錨點上方，則影片變回 static
- 影片 fixed 時，要能夠拖拉
- 拖拉範圍限制在當前可視區間

基本的 HTML 跟 CSS 筆者已經幫大家完成，大家可以直接到下面的連結接著實作：

- [JSBin](https://jsbin.com/pevozex/1/edit?html,css,js,output)
- [JSFiddle](https://jsfiddle.net/s6323859/ochbtpk5/1/)

先讓我們看一下 HTML，首先在 HTML 裡有一個 div(#anchor)，這個 div(#anchor) 就是待會要做錨點用的，它內部有一個 div(#video)，則是滾動後要改變成 fixed 的元件。

CSS 的部分我們只需要知道滾動到下方後，要把 div(#video) 加上 `video-fixed` 這個 class。

接著我們就開始實作滾動的效果切換 class 的效果吧！

### 第一步，取得會用到的 DOM

因為先做滾動切換 class，所以這裡用到的 DOM 只有 #video, #anchor。

```javascript
const video = document.getElementById('video');
const anchor = document.getElementById('anchor');
```

### 第二步，建立會用到的 observable

這裡做滾動效果，所以只需要監聽滾動事件。

```javascript
const scroll = Rx.Observable.fromEvent(document, 'scroll');
```

### 第三步，撰寫程式邏輯

這裡我們要取得了 scroll 事件的 observable，當滾過 #anchor 最底部時，就改變 #video 的 class。  
  
首先我們會需要滾動事件發生時，去判斷是否**滾過 #anchor 最底部**，所以把原本的滾動事件變成是否滾過最底部的 true or false。

```javascript
scroll.map(e => anchor.getBoundingClientRect().bottom < 0)
```

這裡我們用到了 `getBoundingClientRect` 這個瀏覽器原生的 API，他可以取得 DOM 物件的寬高以及上下左右離螢幕可視區間上(左)的距離，如下圖

![](https://res.cloudinary.com/dohtkyi84/image/upload/v1482844440/30days/getBoundingClientRect.png)

當我們可視範圍區間滾過 #anchor 底部時， `anchor.getBoundingClientRect().bottom` 就會變成負值，此時我們就改變 #video 的 class。

```javascript
scroll
.map(e => anchor.getBoundingClientRect().bottom < 0)
.subscribe(bool => {
    if(bool) {
        video.classList.add('video-fixed');
    } else {
        video.classList.remove('video-fixed');
    }
})
```

到這裡我們就已經完成滾動變更樣式的效果了！

全部的 JS 程式碼，如下

```javascript
const video = document.getElementById('video');
const anchor = document.getElementById('anchor');

const scroll = Rx.Observable.fromEvent(document, 'scroll');

scroll
.map(e => anchor.getBoundingClientRect().bottom < 0)
.subscribe(bool => {
    if(bool) {
        video.classList.add('video-fixed');
    } else {
        video.classList.remove('video-fixed');
    }
})
```

> 當然這段還能在用 debounce/throttle 或 requestAnimationFrame 做優化，這個部分我們日後的文章會在提及。

接下來我們就可以接著做**拖拉的行為**了。

### 第一步，取得會用到的 DOM

這裡我們會用到的 DOM 跟前面是一樣的(#video)，所以不用多做什麼。

### 第二步，建立會用到的 observable

這裡跟上次一樣，我們會用到 mousedown, mouseup, mousemove 三個事件。

```javascript
const mouseDown = Rx.Observable.fromEvent(video, 'mousedown')
const mouseUp = Rx.Observable.fromEvent(document, 'mouseup')
const mouseMove = Rx.Observable.fromEvent(document, 'mousemove')
```

### 第三步，撰寫程式邏輯

跟上次是差不多的，首先我們會點擊 #video 元件，點擊(mousedown)後要變成移動事件(mousemove)，而移動事件會在滑鼠放開(mouseup)時結束(takeUntil)

```javascript
mouseDown
.map(e => mouseMove.takeUntil(mouseUp))
.concatAll()
```

因為把 mouseDown observable 發送出來的**事件**換成了 mouseMove observable，所以變成了 observable(mouseDown) 送出 observable(mouseMove)。因此最後用 concatAll 把後面送出的元素變成 mouse move 的事件。

> 這段如果不清楚的可以回去看一下 08 篇的講解

但這裡會有一個問題，就是我們的這段拖拉事件其實只能做用到 video-fixed 的時候，所以我們要加上 `filter`

```javascript
mouseDown
.filter(e => video.classList.contains('video-fixed'))
.map(e => mouseMove.takeUntil(mouseUp))
.concatAll()
```

這裡我們用 filter 如果當下 #video 沒有 `video-dragable` class 的話，事件就不會送出。

再來我們就能跟上次一樣，把 mousemove 事件變成 { x, y } 的物件，並訂閱來改變 #video 元件

```javascript
mouseDown
    .filter(e => video.classList.contains('video-fixed'))
    .map(e => mouseMove.takeUntil(mouseUp))
    .concatAll()
    .map(m => {
        return {
            x: m.clientX,
            y: m.clientY
        }
    })
    .subscribe(pos => {
        video.style.top = pos.y + 'px';
        video.style.left = pos.x + 'px';
    })
```

到這裡我們基本上已經完成了所有功能，其步驟跟 08 篇的方法是一樣的，如果不熟悉的人可以回頭看一下！

但這裡有兩個大問題我們還沒有解決

1. 第一次拉動的時候會閃一下，不像優酷那麼順
2. 拖拉會跑出當前可視區間，跑上出去後就抓不回來了

讓我們一個一個解決，首先第一個問題是因為我們的拖拉直接給元件滑鼠的位置(clientX, clientY)，而非給滑鼠相對移動的距離！

所以要解決這個問題很簡單，我們只要把點擊目標的左上角當作 (0,0)，並以此改變元件的樣式，就不會有閃動的問題。

這個要怎麼做呢？ 很簡單，我們在昨天講了一個 operator 叫做 withLatestFrom，我們可以用它來把 mousedown 與 mousemove 兩個 Event 的值同時傳入 callback。

```javascript
mouseDown
    .filter(e => video.classList.contains('video-fixed'))
    .map(e => mouseMove.takeUntil(mouseUp))
    .concatAll()
    .withLatestFrom(mouseDown, (move, down) => {
        return {
            x: move.clientX - down.offsetX,
            y: move.clientY - down.offsetY
        }
    })
    .subscribe(pos => {
        video.style.top = pos.y + 'px';
        video.style.left = pos.x + 'px';
    })
```

當我們能夠同時得到 mousemove 跟 mousedown 的事件，接著就只要把 滑鼠相對可視區間的距離(client) 減掉點按下去時 滑鼠相對元件邊界的距離(offset) 就行了。這時拖拉就不會先閃動一下囉！

> 大家只要想一下，其實 client - offset 就是元件相對於可視區間的距離，也就是他一開始沒動的位置！

![offset&client](https://res.cloudinary.com/dohtkyi84/image/upload/v1482854816/30days/offset.png)

接著讓我們解決第二個問題，拖拉會超出可視範圍。這個問題其實只要給最大最小值就行了，因為需求的關係，這裡我們的元件是相對可視居間的絕對位置(fixed)，也就是說   
  
- top 最小是 0
- left 最小是 0
- top 最大是**可視高度**扣掉**元件本身高度**
- left 最大是**可視寬度**扣掉**元件本身寬度**

這裡我們先宣告一個 function 來處理這件事

```javascript
const validValue = (value, max, min) => {
    return Math.min(Math.max(value, min), max)
}
```

第一個參數給原本要給的位置值，後面給最大跟最小，如果今天大於最大值我們就取最大值，如果今天小於最小值則取最小值。

再來我們就可以直接把這個問題解掉了

```javascript
mouseDown
    .filter(e => video.classList.contains('video-fixed'))
    .map(e => mouseMove.takeUntil(mouseUp))
    .concatAll()
    .withLatestFrom(mouseDown, (move, down) => {
        return {
            x: validValue(move.clientX - down.offsetX, window.innerWidth - 320, 0),
            y: validValue(move.clientY - down.offsetY, window.innerHeight - 180, 0)
        }
    })
    .subscribe(pos => {
        video.style.top = pos.y + 'px';
        video.style.left = pos.x + 'px';
    })
```

這裡我偷懶了一下，直接寫死元件的寬高(320, 180)，實際上應該用 `getBoundingClientRect` 計算是比較好的。

現在我們就完成整個應用囉！

> [這裡](https://jsfiddle.net/s6323859/ochbtpk5/3/)有最後完成的結果。

今日結語
----

我們簡單地用了不到 35 行的程式碼，完成了一個還算複雜的功能。更重要的是我們還保持了整支程式的可讀性，讓我們之後維護更加的輕鬆。

今天的練習就到這邊結束了，不知道讀者有沒有收穫呢？ 如果有任何問題歡迎在下方留言給我！

如果你喜歡本篇文章請幫我按個 like 跟 星星。
