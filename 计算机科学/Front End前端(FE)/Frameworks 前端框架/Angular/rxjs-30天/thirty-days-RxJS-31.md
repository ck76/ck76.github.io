---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS(31)：如何 Debug？
date: 2017-03-19T15:04:10.000Z
description: Debug 一直是 RxJS 的難題，原因是當我們使用 RxJS 後，程式碼就會變得高度 **抽象化**；實際上抽象並不是什麼壞事，抽象會讓程式碼顯得簡潔、乾淨，但同時也帶來了除錯上的困難。
image: null
tags:
  - Front End
  - RxJS
  - Debug
  - RxJS 30 Days
previous: ./thirty-days-RxJS-30.md
next: null
---

在撰寫程式時，我們都會希望程式碼是簡潔且可讀的。但當我們用 **簡潔** 的程式碼來處理 **複雜** 的問題，就表示我們的程式碼會變得 **高度抽象**！其實人類在思考複雜的問題都會偏好用抽象的方式來處理，例如說在下圍棋時，常常說的 **棋形** 或是黑白哪一邊的 **勢** 比較好，這都是在抽象化處理問題。  

## RxJS 如何除錯？

### do

在 RxJS 的世界中，有一個 Operator 叫作 `do`，它不會對元素產生任何影響，在實務上很常用來做錯誤的追蹤，如下

```javascript
const source = Rx.Observable.interval(1000).take(3);

const example = source
                .do(x => console.log('do log: ' + x))
                .map(x => x + 1);

example.subscribe((x) => {
    console.log('subscription log: ' + x)
})

// do log: 0
// subscription log: 1
// do log: 1
// subscription log: 2
// do log: 2
// subscription log: 3
```
[JSBin](https://jsbin.com/temagoqehe/2/edit?js,console) | [JSFiddle](https://jsfiddle.net/dre5ur0e/)

從上面的例子可以看出來，我們可以傳入一個 callback function 給 `do`，我們可以在 do 的內部對元素作任何操作（像是 log），但不會對元素產生影響。這很適合用在檢測每一步送出的元素是否符合我們的預期。

> `do(...)` 的行為跟 `map(x => { ... return x; })` 本質上是一樣的

### Observable 間的關聯圖

當程式有點複雜時，我們最好是能先畫出 Observable 與 Observable 之間的關聯，在釐清各個 Observable 間的關係後，我們就能更輕易地找出問題在哪。範例如下


```javascript
const addButton = document.getElementById('addButton');
const minusButton = document.getElementById('minusButton');
const state = document.getElementById('state');

const addClick = Rx.Observable.fromEvent(addButton, 'click');
const minusClick = Rx.Observable.fromEvent(minusButton, 'click');
const initialState = Rx.Observable.of(0);

const numberState = initialState
    .merge(
        addClick.mapTo(1), 
        minusClick.mapTo(-1)
    )
    .scan((origin, next) => origin + next)
  
numberState
  .subscribe({
    next: (value) => { state.innerHTML = value;},
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
  });
```
[JSBin](https://jsbin.com/womiduceno/5/edit?js,output) | [JSFiddle](https://jsfiddle.net/97021g7p/)

上面這段程式碼，我們可以把關聯圖畫成以下的樣子

```bash
--------------        --------------        --------------
'            '        '            '        '            ' 
'initialState'        '  addClcik  '        ' minusClick '
'            '        '            '        '            '
--------------        --------------        --------------
      |                     |                      |
      |                     |  mapTo(1)            | mapTo(-1)
merge | ____________________|                      |
      | \__________________________________________|
      |                      
     \|/
      |
      | scan((origin, next) => origin + next)
      |
     \|/
-------------
'           '
'numberState'  
'           '
-------------
```

把每個一 observable 物件都框起來，並畫出之間的關聯，以及中間使用到的 Operators，這樣一來我們就能夠很清楚的了解這段程式碼在做什麼，以及如何運作。最後我們只要在每一個環節去確認送出的元素就能找出錯誤出現在哪裡。


### Marble Diagram

在釐清每個 observable 之間的關係並找出問題出現在哪個環節後，我們只要畫出該環節的 Marble Diagram 前後變化就能清楚地知道問題是如何發生。接續上面的例子，如果今天問題出在 `merge()` 之後，那我們就把 `merge()` 前後的 Marble Diagram 畫出來


```javascript
initialState: 0|
addClick    : ----------1---------1--1-------
minusClick  : -----(-1)---(-1)---------------

                       merge(...)

            : 0----(-1)-1-(-1)----1--1-------
           
           scan((origin, next) => origin +next)

numberState : 0----(-1)-0-(-1)----0--1-------            
```

到這裡我們應該就能清楚地知道問題出在哪，最後就只要想如何解決問題就行了。

> 如果還是不知道問題在哪，很有可能是 Marble Diagram 畫錯，可以再利用 `do` 進行檢查 

只要照著以上三個步驟做除錯，基本上就不用擔心會有解決不了的錯誤，但是這三個步驟仍然顯得太過繁瑣，或許我們應該做一個工具來簡化這整個流程！

## RxJS Devtools

RxJS Devtools 是我跟我的好友 [Jerry Lin](https://www.facebook.com/jiazhi.lin?hc_ref=NEWSFEED) 共同開發的 Chrome Extension，目前還在 preview 階段，很多 feature 還沒有實作但基本的功能已經能動了，使用方式很簡單如下

```javascript
Observable.prototype.debug = window.rxDevTool(Observable);
```

首先我們的 extension 會在 window 底下塞入一個方法叫 `rxDevTool`，所以開發者只要傳入 Observable 並把這個 `rxDevTool` 的回傳值塞到 `Observable.prototype.debug` 就能使用 `debug` 了。

```javascript
Observable.interval(1000).take(5)
    .debug('source1')
    .map(x => x + 1)
    .debug('source2')
    .subscribe(function() {
        //...
    })
```

這個 `debug()` 跟 `do()` 一樣，不會對元素造成任何影響，但不同的是 `debug()` 要傳入的參數是 開發者自訂的名稱，代表當前的 observable，這時在 Chrome 的開發者工具中切到 RxJS 的 tab 頁就能看到自動畫出 Marble Diagram，如下圖

![RxJS Devtools Demo1](https://i.giphy.com/l0Heb67CJnRLoaR0s.gif)

送出元素是物件也行喔！

![RxJS Devtools Demo2](/img/xTiN0JCbQuHqsGeWCQ.gif)

目前 RxJS Devtools 已經能夠自動畫出 Marble Diagram，也能做到類似 do 的功能(放在第二個參數)，之後會希望能夠自動畫出 observable 之間的關聯圖，這樣一來我們在做 RxJS 的除錯時就會方便非常多！

> 等到 RxJS Devtools 正式 release 後，會在專門寫一篇文章介紹如何使用。

## 結語

這篇文章主要在講述我們使用 RxJS 後要如何進行除錯，基本上只要照著以下三個步驟就能找出問題

- 善用 `do()` 檢查送出的元素
- 畫出 observable 之間的關聯圖
- 畫出關鍵環節前後的 Marble Diagram

最後簡單的介紹了 RxJS Devtools 的使用方式與功能，也請期待 RxJS Devtools 的正式釋出。