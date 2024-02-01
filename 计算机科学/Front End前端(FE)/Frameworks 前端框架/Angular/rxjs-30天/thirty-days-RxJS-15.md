---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (15)：Observable Operators - distinct, distinctUntilChanged
date: 2016-12-31T23:39:48.000Z
description: 新的一年馬上就要到了，各位讀者都去哪裡跨年呢？ 筆者很可憐的只能一邊寫文章一邊跨年，今天就簡單看幾個 operators 讓大家好好跨年吧！
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Operator
  - RxJS 30 Days
previous: ./thirty-days-RxJS-14.md
next: ./thirty-days-RxJS-16.md
---

昨天我們講到了 throttle 跟 debounce 兩個方法來做效能優化，其實還有另一個方法可以做效能的優化處理，那就是 distinct。

Operators
------

### distinct

如果會下 SQL 指令的應該都對 distinct 不陌生，它能幫我們把相同值的資料濾掉只留一筆，RxJS 裡的 distinct 也是相同的作用，讓我們直接來看範例

```javascript
var source = Rx.Observable.from(['a', 'b', 'c', 'a', 'b'])
            .zip(Rx.Observable.interval(300), (x, y) => x);
var example = source.distinct()

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// a
// b
// c
// complete
```
[JSBin](https://jsbin.com/dipabe/2/edit?js,console) | [JSFiddle](https://jsfiddle.net/3pfs88g8/)

如果用 Marble Diagram 表示如下

```bash
source : --a--b--c--a--b|
            distinct()
example: --a--b--c------|
```

從上面的範例可以看得出來，當我們用 distinct 後，只要有重複出現的值就會被過濾掉。

另外我們可以傳入一個 selector callback function，這個 callback function 會傳入一個接收到的元素，並回傳我們真正希望比對的值，舉例如下

```javascript
var source = Rx.Observable.from([{ value: 'a'}, { value: 'b' }, { value: 'c' }, { value: 'a' }, { value: 'c' }])
            .zip(Rx.Observable.interval(300), (x, y) => x);
var example = source.distinct((x) => {
    return x.value
});

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// {value: "a"}
// {value: "b"}
// {value: "c"}
// complete
```
[JSBin](https://jsbin.com/dipabe/3/edit?js,console) | [JSFiddle](https://jsfiddle.net/3pfs88g8/2/)


這裡可以看到，因為 source 送出的都是物件，而 js 物件的比對是比對記憶體位置，所以在這個例子中這些物件永遠不會相等，但實際上我們想比對的是物件中的 value，這時我們就可以傳入 selector callback，來選擇我們要比對的值。

> distinct 傳入的 callback 在 RxJS 5 幾個 bate 版本中有過很多改變，現在網路上很多文章跟教學都是過時的，請讀者務必小心！

實際上 `distinct()` 會在背地裡建立一個 Set，當接收到元素時會先去判斷 Set 內是否有相同的值，如果有就不送出，如果沒有則存到 Set 並送出。所以記得盡量不要直接把 distinct 用在一個無限的 observable 裡，這樣很可能會讓 Set 越來越大，建議大家可以放第二個參數 flushes，或用 distinctUntilChanged

> 這裡指的 Set 其實是 RxJS 自己實作的，跟 ES6 原生的 Set 行為也都一致，只是因為 ES6 的 Set 支援程度還並不理想，所以這裡是直接用 JS 實作。

distinct 可以傳入第二個參數 flushes observable 用來清除暫存的資料，範例如下

```javascript
var source = Rx.Observable.from(['a', 'b', 'c', 'a', 'c'])
            .zip(Rx.Observable.interval(300), (x, y) => x);
var flushes = Rx.Observable.interval(1300);
var example = source.distinct(null, flushes);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// a
// b
// c
// c
// complete
```
[JSBin](https://jsbin.com/dipabe/4/edit?js,console) | [JSFiddle](https://jsfiddle.net/3pfs88g8/3/)

這裡我們用 Marble Diagram 比較好表示

```bash
source : --a--b--c--a--c|
flushes: ------------0---...
        distinct(null, flushes);
example: --a--b--c-----c|
```

其實 flushes observable 就是在送出元素時，會把 distinct 的暫存清空，所以之後的暫存就會從頭來過，這樣就不用擔心暫存的 Set 越來愈大的問題，但其實我們平常不太會用這樣的方式來處理，通常會用另一個方法 distinctUntilChanged。

### distinctUntilChanged

distinctUntilChanged 跟 distinct 一樣會把相同的元素過濾掉，但 distinctUntilChanged 只會跟最後一次送出的元素比較，不會每個都比，舉例如下

```javascript
var source = Rx.Observable.from(['a', 'b', 'c', 'c', 'b'])
            .zip(Rx.Observable.interval(300), (x, y) => x);
var example = source.distinctUntilChanged()

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// a
// b
// c
// b
// complete
```
[JSBin](https://jsbin.com/dipabe/6/edit?js,console) | [JSFiddle](https://jsfiddle.net/3pfs88g8/4/)

這裡 distinctUntilChanged 只會暫存一個元素，並在收到元素時跟暫存的元素比對，如果一樣就不送出，如果不一樣就把暫存的元素換成剛接收到的新元素並送出。

```bash
source : --a--b--c--c--b|
            distinctUntilChanged()
example: --a--b--c-----b|
```

從 Marble Diagram 中可以看到，第二個 c 送出時剛好上一個就是 c 所以就被濾掉了，但最後一個 b 則跟上一個不同所以沒被濾掉。

distinctUntilChanged 是比較常在實務上使用的，最常見的狀況是我們在做多方同步時。當我們有多個 Client，且每個 Client 有著各自的狀態，Server 會再一個 Client 需要變動時通知所有 Client 更新，但可能某些 Client 接收到新的狀態其實跟上一次收到的是相同的，這時我們就可用 distinctUntilChanged 方法只處理跟最後一次不相同的訊息，像是多方通話、多裝置的資訊同步都會有類似的情境。


今日小結
------

今天講了兩個 distinct 方法，這兩個方法平常可能用不太到，但在需求複雜的應用裡是不可或缺的好方法，尤其要處理非常多人即時同步的情境下，這會是非常好用的方法，不知道讀者們今天有沒有收穫呢？ 如果有任何問題，歡迎在下方留言給我，感謝！

