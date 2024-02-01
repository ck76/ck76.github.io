---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (18)：Observable Operators - switchMap, mergeMap, concatMap
date: 2017-01-03T23:47:35.000Z
description: 今天我們要講三個非常重要的 operators，這三個 operators 在很多的 RxJS 相關的 library 的使用範例上都會看到。很多初學者在使用這些 library 時，看到這三個 operators 很可能就放棄了，但其實如果有把這個系列的文章完整看過的話，現在應該就能很好接受跟理解。
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Operator
  - RxJS 30 Days
previous: ./thirty-days-RxJS-17.md
next: ./thirty-days-RxJS-19.md
---

Operators
------

### concatMap

concatMap 其實就是 map 加上 concatAll 的簡化寫法，我們直接來看一個範例

```javascript
var source = Rx.Observable.fromEvent(document.body, 'click');

var example = source
                .map(e => Rx.Observable.interval(1000).take(3))
                .concatAll();
                
example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```

上面這個範例就可以簡化成

```javascript
var source = Rx.Observable.fromEvent(document.body, 'click');

var example = source
                .concatMap(
                    e => Rx.Observable.interval(100).take(3)
                );
                
example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```

前後兩個行為是一致的，記得 concatMap 也會先處理前一個送出的 observable 在處理下一個 observable，畫成 Marble Diagram 如下

```bash
source : -----------c--c------------------...
        concatMap(c => Rx.Observable.interval(100).take(3))
example: -------------0-1-2-0-1-2---------...
```

這樣的行為也很常被用在發送 request 如下

```javascript
function getPostData() {
    return fetch('https://jsonplaceholder.typicode.com/posts/1')
    .then(res => res.json())
}
var source = Rx.Observable.fromEvent(document.body, 'click');

var example = source.concatMap(
                    e => Rx.Observable.from(getPostData()));

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```
[JSBin](https://jsbin.com/nuhita/5/edit?js,console,output) | [JSFiddle](https://jsfiddle.net/t2zxtuh0/2/)

這裡我們每點擊一下畫面就會送出一個 HTTP request，如果我們快速的連續點擊，大家可以在開發者工具的 network 看到每個 request 是等到前一個 request 完成才會送出下一個 request，如下圖

![](https://res.cloudinary.com/dohtkyi84/image/upload/v1483454601/30days/concatMap_request.png)

> 這裡建議把網速模擬調到最慢

![](/img/throttle_network.png)

從 network 的圖形可以看得出來，第二個 request 的發送時間是接在第一個 request 之後的，我們可以確保每一個 request 會等前一個 request 完成才做處理。

concatMap 還有第二個參數是一個 selector callback，這個 callback 會傳入四個參數，分別是 

1. 外部 observable 送出的元素
2. 內部 observable 送出的元素
3. 外部 observable 送出元素的 index
4. 內部 observable 送出元素的 index

回傳值我們想要的值，範例如下

```javascript
function getPostData() {
    return fetch('https://jsonplaceholder.typicode.com/posts/1')
    .then(res => res.json())
}
var source = Rx.Observable.fromEvent(document.body, 'click');

var example = source.concatMap(
                e => Rx.Observable.from(getPostData()), 
                (e, res, eIndex, resIndex) => res.title);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```
[JSBin](https://jsbin.com/nuhita/7/edit?js,console,output) | [JSFiddle](https://jsfiddle.net/t2zxtuh0/3/)

這個範例的外部 observable 送出的元素就是 click event 物件，內部 observable 送出的元素就是 response 物件，這裡我們回傳 response 物件的 title 屬性，這樣一來我們就可以直接收到 title，這個方法很適合用在 response 要選取的值跟前一個事件或順位(index)相關時。

### switchMap

switchMap 其實就是 map 加上 switch 簡化的寫法，如下

```javascript
var source = Rx.Observable.fromEvent(document.body, 'click');

var example = source
                .map(e => Rx.Observable.interval(1000).take(3))
                .switch();
                
example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```

上面的程式碼可以簡化成

```javascript
var source = Rx.Observable.fromEvent(document.body, 'click');

var example = source
                .switchMap(
                    e => Rx.Observable.interval(100).take(3)
                );
                
example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```

畫成 Marble Diagram 表示如下

```bash
source : -----------c--c-----------------...
        concatMap(c => Rx.Observable.interval(100).take(3))
example: -------------0--0-1-2-----------...
```

只要注意一個重點 switchMap 會在下一個 observable 被送出後直接退訂前一個未處理完的 observable，這個部份的細節請看上一篇文章 switch 的部分。

另外我們也可以把 switchMap 用在發送 HTTP request

```javascript
function getPostData() {
    return fetch('https://jsonplaceholder.typicode.com/posts/1')
    .then(res => res.json())
}
var source = Rx.Observable.fromEvent(document.body, 'click');

var example = source.switchMap(
                    e => Rx.Observable.from(getPostData()));

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```
[JSBin](https://jsbin.com/nuhita/1/edit?js,console,output) |  [JSFiddle](https://jsfiddle.net/t2zxtuh0/4/)

如果我們快速的連續點擊五下，可以在開發者工具的 network 看到每個 request 會在點擊時發送，如下圖

![](https://res.cloudinary.com/dohtkyi84/image/upload/v1483456745/30days/switchMap_request.png)

> 灰色是瀏覽器原生地停頓行為，實際上灰色的一開始就是 fetch 執行送出 request，只是卡在瀏覽器等待發送。

從上圖可以看到，雖然我們發送了多個 request 但最後真正印出來的 log 只會有一個，代表前面發送的 request 已經不會造成任何的 side-effect 了，這個很適合用在只看最後一次 request 的情境，比如說 自動完成(auto complete)，我們只需要顯示使用者最後一次打在畫面上的文字，來做建議選項而不用每一次的。

switchMap 跟 concatMap 一樣有第二個參數 selector callback 可用來回傳我們要的值，這部分的行為跟 concatMap 是一樣的，這裡就不再贅述。

### mergeMap

mergeMap 其實就是 map 加上 mergeAll 簡化的寫法，如下

```javascript
var source = Rx.Observable.fromEvent(document.body, 'click');

var example = source
                .map(e => Rx.Observable.interval(1000).take(3))
                .mergeAll();
                
example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```

上面的程式碼可以簡化成

```javascript
var source = Rx.Observable.fromEvent(document.body, 'click');

var example = source
                .mergeMap(
                    e => Rx.Observable.interval(100).take(3)
                );
                
example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```

畫成 Marble Diagram 表示

```bash
source : -----------c-c------------------...
        concatMap(c => Rx.Observable.interval(100).take(3))
example: -------------0-(10)-(21)-2----------...
```

記得 mergeMap 可以並行處理多個 observable，以這個例子來說當我們快速點按兩下，元素發送的時間點是有機會重疊的，這個部份的細節大家可以看上一篇文章 merge 的部分。

另外我們也可以把 mergeMap 用在發送 HTTP request

```javascript
function getPostData() {
    return fetch('https://jsonplaceholder.typicode.com/posts/1')
    .then(res => res.json())
}
var source = Rx.Observable.fromEvent(document.body, 'click');

var example = source.mergeMap(
                    e => Rx.Observable.from(getPostData()));

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```
[JSBin](https://jsbin.com/nuhita/3/edit?js,console,output) | [JSFiddle](https://jsfiddle.net/t2zxtuh0/5/)

如果我們快速的連續點擊五下，大家可以在開發者工具的 network 看到每個 request 會在點擊時發送並且會 log 出五個物件，如下圖

![](https://res.cloudinary.com/dohtkyi84/image/upload/v1483457934/30days/mergeMap_request.png)

mergeMap 也能傳入第二個參數 selector callback，這個 selector callback 跟 concatMap 第二個參數也是完全一樣的，但 mergeMap 的重點是我們可以傳入第三個參數，來限制並行處理的數量

```javascript
function getPostData() {
    return fetch('https://jsonplaceholder.typicode.com/posts/1')
    .then(res => res.json())
}
var source = Rx.Observable.fromEvent(document.body, 'click');

var example = source.mergeMap(
                e => Rx.Observable.from(getPostData()), 
                (e, res, eIndex, resIndex) => res.title, 3);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```
[JSBin](https://jsbin.com/nuhita/4/edit?js,console,output) | [JSFiddle](https://jsfiddle.net/t2zxtuh0/5/)

這裡我們傳入 3 就能限制，HTTP request 最多只能同時送出 3 個，並且要等其中一個完成在處理下一個，如下圖

![](https://res.cloudinary.com/dohtkyi84/image/upload/v1483458530/30days/mergeMap_3_request.png)

大家可以注意看上面這張圖，我連續點按了五下，但第四個 request 是在第一個完成後才送出的，這個很適合用在特殊的需求下，可以限制同時發送的 request 數量。

> RxJS 5 還保留了 mergeMap 的別名叫 flatMap，雖然官方文件上沒有，但這兩個方法是完全一樣的。請參考[這裡](https://github.com/ReactiveX/RxJS/issues/333)

### switchMap, mergeMap, concatMap

這三個 operators 還有一個共同的特性，那就是這三個 operators 可以把第一個參數所回傳的 promise 物件直接轉成 observable，這樣我們就不用再用 `Rx.Observable.from` 轉一次，如下

```javascript
function getPersonData() {
    return fetch('https://jsonplaceholder.typicode.com/posts/1')
    .then(res => res.json())
}
var source = Rx.Observable.fromEvent(document.body, 'click');

var example = source.concatMap(e => getPersonData());
                                    //直接回傳 promise 物件

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```

至於在使用上要如何選擇這三個 operators？ 其實都還是看使用情境而定，這裡筆者簡單列一下大部分的使用情境

- concatMap 用在可以確定**內部的 observable 結束時間比外部 observable 發送時間來快的情境**，並且不希望有任何並行處理行為，適合少數要一次一次完成到底的的 UI 動畫或特別的 HTTP request 行為。
- switchMap 用在只要最後一次行為的結果，適合絕大多數的使用情境。
- mergeMap 用在並行處理多個 observable，適合需要並行處理的行為，像是多個 I/O 的並行處理。

> 建議初學者不確定選哪一個時，使用 switchMap

> 在使用 concatAll 或 concatMap 時，請注意內部的 observable 一定要能夠的結束，且外部的 observable 發送元素的速度不能比內部的 observable 結束時間快太多，不然會有 memory issues

今日小結
------

今天的文章內容主要講了三個 operators，如果有看完上一篇文章的讀者應該不難吸收，主要還是使用情境上需要思考以及注意一些細節。

不知道今天讀者有沒有收穫呢？ 如果有任何問題，歡迎留言給我，謝謝
