---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (16)：Observable Operators - catch, retry, retryWhen, repeat
date: 2017-01-01T23:46:55.000Z
description: "我們已經快把所有基本的轉換(Transformation)、過濾(Filter)和合并(Combination)的 operators 講完了。今天要講錯誤處理(Error Handling)的 operators，錯誤處理是非同步行為中的一大難題，尤其有多個交錯的非同步行為時，更容易凸顯錯誤處理的困難。就讓我們一起來看看在 RxJS 中能如何處理錯誤吧！"
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Operator
  - RxJS 30 Days
previous: ./thirty-days-RxJS-15.md
next: ./thirty-days-RxJS-17.md
---

Operators
------

### catch

catch 是很常見的非同步錯誤處理方法，在 RxJS 中也能夠直接用 catch 來處理錯誤，在 RxJS 中的 catch 可以回傳一個 observable 來送出新的值，讓我們直接來看範例：

```javascript
var source = Rx.Observable.from(['a','b','c','d',2])
            .zip(Rx.Observable.interval(500), (x,y) => x);

var example = source
                .map(x => x.toUpperCase())
                .catch(error => Rx.Observable.of('h'));

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});                    
```
[JSBin](https://jsbin.com/nafusoq/16/edit?js,console) | [JSFiddle](https://jsfiddle.net/aruku1xr/10/)

這個範例我們每隔 500 毫秒會送出一個字串(String)，並用字串的方法 `toUpperCase()` 來把字串的英文字母改成大寫，過程中可能未知的原因送出了一個數值(Number) `2` 導致發生例外(數值沒有 toUpperCase 的方法)，這時我們在後面接的 catch 就能抓到錯誤。

catch 可以回傳一個新的 Observable、Promise、Array 或任何 Iterable 的物件，來傳送之後的元素。

以我們的例子來說最後就會在送出 `X` 就結束，畫成 Marble Diagram 如下

```bash
source : ----a----b----c----d----2|
        map(x => x.toUpperCase())
         ----a----b----c----d----X|
        catch(error => Rx.Observable.of('h'))
example: ----a----b----c----d----h|         
```

這裡可以看到，當錯誤發生後就會進到 catch 並重新處理一個新的 observable，我們可以利用這個新的 observable 來送出我們想送的值。

也可以在遇到錯誤後，讓 observable 結束，如下

```javascript
var source = Rx.Observable.from(['a','b','c','d',2])
            .zip(Rx.Observable.interval(500), (x,y) => x);

var example = source
                .map(x => x.toUpperCase())
                .catch(error => Rx.Observable.empty());

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});                    
```
[JSBin](https://jsbin.com/nafusoq/15/edit?js,console) | [JSFiddle](https://jsfiddle.net/aruku1xr/9/)

回傳一個 empty 的 observable 來直接結束(complete)。

另外 catch 的 callback 能接收第二個參數，這個參數會接收當前的 observalbe，我們可以回傳當前的 observable 來做到重新執行，範例如下

```javascript
var source = Rx.Observable.from(['a','b','c','d',2])
            .zip(Rx.Observable.interval(500), (x,y) => x);

var example = source
                .map(x => x.toUpperCase())
                .catch((error, obs) => obs);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});                    
```
[JSBin](https://jsbin.com/nafusoq/14/edit?js,console) | [JSFiddle](https://jsfiddle.net/aruku1xr/8/)

這裡可以看到我們直接回傳了當前的 obserable(其實就是 example)來重新執行，畫成 Marble Diagram 如下

```bash
source : ----a----b----c----d----2|
        map(x => x.toUpperCase())
         ----a----b----c----d----X|
        catch((error, obs) => obs)
example: ----a----b----c----d--------a----b----c----d--..
```

因為是我們只是簡單的示範，所以這裡會一直無限循環，實務上通常會用在斷線重連的情境。

另上面的處理方式有一個簡化的寫法，叫做 `retry()`。

### retry

如果我們想要一個 observable 發生錯誤時，重新嘗試就可以用 retry 這個方法，跟我們前一個講範例的行為是一致

```javascript
var source = Rx.Observable.from(['a','b','c','d',2])
            .zip(Rx.Observable.interval(500), (x,y) => x);

var example = source
                .map(x => x.toUpperCase())
                .retry();

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
}); 
```
[JSBin](https://jsbin.com/nafusoq/13/edit?js,console) | [JSFiddle](https://jsfiddle.net/aruku1xr/7/)

通常這種無限的 `retry` 會放在即時同步的重新連接，讓我們在連線斷掉後，不斷的嘗試。另外我們也可以設定只嘗試幾次，如下

```javascript
var source = Rx.Observable.from(['a','b','c','d',2])
            .zip(Rx.Observable.interval(500), (x,y) => x);

var example = source
                .map(x => x.toUpperCase())
                .retry(1);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
}); 
// a
// b
// c
// d
// a
// b
// c
// d
// Error: TypeError: x.toUpperCase is not a function
```
[JSBin](https://jsbin.com/nafusoq/12/edit?js,console) | [JSFiddle](https://jsfiddle.net/aruku1xr/6/)

這裡我們對 retry 傳入一個數值 `1`，能夠讓我們只重複嘗試 1 次後送出錯誤，畫成 Marble Diagram 如下

```bash
source : ----a----b----c----d----2|
        map(x => x.toUpperCase())
         ----a----b----c----d----X|
                retry(1)
example: ----a----b----c----d--------a----b----c----d----X|
```

這種處理方式很適合用在 HTTP request 失敗的場景中，我們可以設定重新發送幾次後，再秀出錯誤訊息。

### retryWhen

RxJS 還提供了另一種方法 `retryWhen`，他可以把例外發生的元素放到一個 observable 中，讓我們可以直接操作這個 observable，並等到這個 observable 操作完後再重新訂閱一次原本的 observable。

這裡我們直接來看程式碼

```javascript
var source = Rx.Observable.from(['a','b','c','d',2])
            .zip(Rx.Observable.interval(500), (x,y) => x);

var example = source
                .map(x => x.toUpperCase())
                .retryWhen(errorObs => errorObs.delay(1000));

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
}); 
```
[JSBin](https://jsbin.com/nafusoq/11/edit?js,console) | [JSFiddle](https://jsfiddle.net/aruku1xr/5/)


這裡 retryWhen 我們傳入一個 callback，這個 callback 有一個參數會傳入一個 observable，這個 observable 不是原本的 observable(example) 而是例外事件送出的錯誤所組成的一個 observable，我們可以對這個由錯誤所組成的 observable 做操作，等到這次的處理完成後就會重新訂閱我們原本的 observable。

這個範例我們是把錯誤的 observable 送出錯誤延遲 1 秒，這會使後面重新訂閱的動作延遲 1 秒才執行，畫成 Marble Diagram 如下

```bash
source : ----a----b----c----d----2|
        map(x => x.toUpperCase())
         ----a----b----c----d----X|
        retryWhen(errorObs => errorObs.delay(1000))
example: ----a----b----c----d-------------------a----b----c----d----...
```

從上圖可以看到後續重新訂閱的行為就被延後了，但實務上我們不太會用 retryWhen 來做重新訂閱的延遲，通常是直接用 catch 做到這件事。這裡只是為了示範 retryWhen 的行為，實務上我們通常會把 retryWhen 拿來做錯誤通知或是例外收集，如下

```javascript
var source = Rx.Observable.from(['a','b','c','d',2])
            .zip(Rx.Observable.interval(500), (x,y) => x);

var example = source
                .map(x => x.toUpperCase())
                .retryWhen(
                errorObs => errorObs.map(err => fetch('...')));

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
}); 
```

這裡的 `errorObs.map(err => fetch('...'))` 可以把 errorObs 裡的每個錯誤變成 API 的發送，通常這裡個 API 會像是送訊息到公司的通訊頻道(Slack 等等)，這樣可以讓工程師馬上知道可能哪個 API 掛了，這樣我們就能即時地處理。

> retryWhen 實際上是在背地裡建立一個 Subject 並把錯誤放入，會在對這個 Subject 進行內部的訂閱，因為我們還沒有講到 Subject 的觀念，大家可以先把它當作 Observable 就好了，另外記得這個 observalbe 預設是無限的，如果我們把它結束，原本的 observable 也會跟著結束。

### repeat

我們有時候可能會想要 retry 一直重複訂閱的效果，但沒有錯誤發生，這時就可以用 repeat 來做到這件事，範例如下

```javascript
var source = Rx.Observable.from(['a','b','c'])
            .zip(Rx.Observable.interval(500), (x,y) => x);

var example = source.repeat(2);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
 
// a
// b
// c
// a
// b
// c
// complete
```
[JSBin](https://jsbin.com/nafusoq/10/edit?js,console) | [JSFiddle](https://jsfiddle.net/aruku1xr/3/)

這裡 repeat 的行為跟 retry 基本一致，只是 retry 只有在例外發生時才觸發，畫成 Marble Diagram 如下

```bash
source : ----a----b----c|
            repeat(2)
example: ----a----b----c----a----b----c|
```

同樣的我們可以不給參數讓他無限循環，如下

```javascript
var source = Rx.Observable.from(['a','b','c'])
            .zip(Rx.Observable.interval(500), (x,y) => x);

var example = source.repeat();

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```
[JSBin](https://jsbin.com/nafusoq/9/edit?js,console) | [JSFiddle](https://jsfiddle.net/aruku1xr/1/)

這樣我們就可以做動不斷重複的行為，這個可以在建立輪詢時使用，讓我們不斷地發 request 來更新畫面。

最後我們來看一個錯誤處理在實務應用中的小範例

```javascript
const title = document.getElementById('title');

var source = Rx.Observable.from(['a','b','c','d',2])
            .zip(Rx.Observable.interval(500), (x,y) => x)
            .map(x => x.toUpperCase()); 
            // 通常 source 會是建立即時同步的連線，像是 web socket

var example = source.catch(
                (error, obs) => Rx.Observable.empty()
                               .startWith('連線發生錯誤： 5秒後重連')
                               .concat(obs.delay(5000))
                 );

example.subscribe({
    next: (value) => { title.innerText = value },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
}); 
```
[JSBin](https://jsbin.com/nafusoq/6/edit?js,output) | [JSFiddle](https://jsfiddle.net/aruku1xr/) 

這個範例其實就是模仿在即時同步斷線時，利用 catch 返回一個新的 observable，這個 observable 會先送出錯誤訊息並且把原本的 observable 延遲 5 秒再做合併，雖然這只是一個模仿，但它清楚的展示了 RxJS 在做錯誤處理時的靈活性。

### 今日小結

今天我們講了三個錯誤處理的方法還有一個 repeat operator，這幾個方法都很有機會在實務上用到，不知道今天大家有沒有收穫呢？ 如果有任何問題，歡迎在下方留言給我，謝謝！