---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (10)：Observable Operator - combineLatest, withLatestFrom, zip
date: 2016-12-26T23:11:24.000Z
description: "昨天我們最後講到了 `merge` 的用法，它的邏輯就像是 OR(||)一樣，可以把多個 observable 合併且同時處理，當其中任合一個 observable 送出元素時，我們都做相同的處理。

今天我們要講的三個 operators 則像是 AND(&&) 邏輯，它們都是在多個元素送進來時，只輸出一個新元素，但各自的行為上仍有差異，需要讀者花點時間思考，建議在頭腦清醒時閱讀本篇文章。"
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Operator
  - RxJS 30 Days
previous: ./thirty-days-RxJS-09.md
next: ./thirty-days-RxJS-11.md
---

Operators
------

### combineLatest

首先我們要介紹的是 combineLatest，它會取得各個 observable 最後送出的值，再輸出成一個值，我們直接看範例會比較好解釋。

```javascript
var source = Rx.Observable.interval(500).take(3);
var newest = Rx.Observable.interval(300).take(6);

var example = source.combineLatest(newest, (x, y) => x + y);

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
// 5
// 6
// 7
// complete
```
[JSBin](https://jsbin.com/posutey/1/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/55edtqpk/)

大家第一次看到這個 output 應該都會很困惑，我們直接來看 Marble Diagram 吧！

```bash
source : ----0----1----2|
newest : --0--1--2--3--4--5|

    combineLatest(newest, (x, y) => x + y);

example: ----01--23-4--(56)--7|
```

首先 `combineLatest` 可以接收多個 observable，最後一個參數是 callback function，這個 callback function 接收的參數數量跟合併的 observable 數量相同，依照範例來說，因為我們這裡合併了兩個 observable 所以後面的 callback function 就接收 x, y 兩個參數，x 會接收從 source 發送出來的值，y 會接收從 newest 發送出來的值。

最後一個重點就是一定會等兩個 observable 都**曾有送值**出來才會呼叫我們傳入的 callback，所以這段程式是這樣運行的

- newest 送出了 `0`，但此時 source 並沒有送出過任何值，所以不會執行 callback
- source 送出了 `0`，此時 newest 最後一次送出的值為 `0`，把這兩個數傳入 callback 得到 `0`。
- newest 送出了 `1`，此時 source 最後一次送出的值為 `0`，把這兩個數傳入 callback 得到 `1`。
- newest 送出了 `2`，此時 source 最後一次送出的值為 `0`，把這兩個數傳入 callback 得到 `2`。
- source 送出了 `1`，此時 newest 最後一次送出的值為 `2`，把這兩個數傳入 callback 得到 `3`。
- newest 送出了 `3`，此時 source 最後一次送出的值為 `1`，把這兩個數傳入 callback 得到 `4`。
- source 送出了 `2`，此時 newest 最後一次送出的值為 `3`，把這兩個數傳入 callback 得到 `5`。
- source 結束，但 newest 還沒結束，所以 example 還不會結束。
- newest 送出了 `4`，此時 source 最後一次送出的值為 `2`，把這兩個數傳入 callback 得到 `6`。
- newest 送出了 `5`，此時 source 最後一次送出的值為 `2`，把這兩個數傳入 callback 得到 `7`。
- newest 結束，因為 source 也結束了，所以 example 結束。

不管是 source 還是 newest 送出值來，只要另一方曾有送出過值(有最後的值)，就會執行 callback 並送出新的值，這就是 combineLatest。

combineLatest 很常用在運算多個因子的結果，例如最常見的 BMI 計算，我們身高變動時就拿上一次的體重計算新的 BMI，當體重變動時則拿上一次的身高計算 BMI，這就很適合用 combineLatest 來處理！

### zip

在講 withLatestFrom 之前，先讓我們先來看一下 zip 是怎麼運作的，zip 會取每個 observable 相同順位的元素並傳入 callback，也就是說每個 observable 的第 n 個元素會一起被傳入 callback，這裡我們同樣直接用範例講解會比較清楚

```javascript
var source = Rx.Observable.interval(500).take(3);
var newest = Rx.Observable.interval(300).take(6);

var example = source.zip(newest, (x, y) => x + y);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// 0
// 2
// 4
// complete
```
[JSBin](https://jsbin.com/posutey/2/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/55edtqpk/1/)

Marble Diagram 長這樣

```bash
source : ----0----1----2|
newest : --0--1--2--3--4--5|
    zip(newest, (x, y) => x + y)
example: ----0----2----4|
```

以我們的範例來說，zip 會等到 source 跟 newest **都送出了第一個元素**，再傳入 callback，下次則等到 source 跟 newest **都送出了第二個元素**再一起傳入 callback，所以運行的步驟如下：

- newest 送出了**第一個**值 `0`，但此時 source 並沒有送出**第一個**值，所以不會執行 callback。
- source 送出了**第一個**值 `0`，newest 之前送出的**第一個**值為 `0`，把這兩個數傳入 callback 得到 `0`。
- newest 送出了**第二個**值 `1`，但此時 source 並沒有送出**第二個**值，所以不會執行 callback。
- newest 送出了**第三個**值 `2`，但此時 source 並沒有送出**第三個**值，所以不會執行 callback。
- source 送出了**第二個**值 `1`，newest 之前送出的**第二個**值為 `1`，把這兩個數傳入 callback 得到 `2`。
- newest 送出了**第四個**值 `3`，但此時 source 並沒有送出**第四個**值，所以不會執行 callback。
- source 送出了**第三個**值 `2`，newest 之前送出的**第三個**值為 `2`，把這兩個數傳入 callback 得到 `4`。
- source 結束 example 就直接結束，因為 source 跟 newest 不會再有對應順位的值

zip 會把各個 observable 相同順位送出的值傳入 callback，這很常拿來做 demo 使用，比如我們想要間隔 100ms 送出 'h', 'e', 'l', 'l', 'o'，就可以這麼做

```javascript
var source = Rx.Observable.from('hello');
var source2 = Rx.Observable.interval(100);

var example = source.zip(source2, (x, y) => x);
```

這裡的 Marble Diagram 就很簡單

```bash
source : (hello)|
source2: -0-1-2-3-4-...
        zip(source2, (x, y) => x)
example: -h-e-l-l-o|
```

這裡我們利用 zip 來達到原本只能同步送出的資料變成了非同步的，很適合用在建立示範用的資料。

> 建議大家平常沒事不要亂用 zip，除非真的需要。因為 zip 必須 cache 住還沒處理的元素，當我們兩個 observable 一個很快一個很慢時，就會 cache 非常多的元素，等待比較慢的那個 observable。這很有可能造成記憶體相關的問題！

### withLatestFrom

withLatestFrom 運作方式跟 combineLatest 有點像，只是他有主從的關係，只有在主要的 observable 送出新的值時，才會執行 callback，附隨的 observable 只是在背景下運作。讓我們看一個例子

```javascript
var main = Rx.Observable.from('hello').zip(Rx.Observable.interval(500), (x, y) => x);
var some = Rx.Observable.from([0,1,0,0,0,1]).zip(Rx.Observable.interval(300), (x, y) => x);

var example = main.withLatestFrom(some, (x, y) => {
    return y === 1 ? x.toUpperCase() : x;
});

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```
[JSBin](https://jsbin.com/posutey/6/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/55edtqpk/2/)

先看一下 Marble Diagram

```bash
main   : ----h----e----l----l----o|
some   : --0--1--0--0--0--1|

withLatestFrom(some, (x, y) =>  y === 1 ? x.toUpperCase() : x);

example: ----h----e----l----L----O|
```

withLatestFrom 會在 main 送出值的時候執行 callback，但請注意如果 main 送出值時 some 之前沒有送出過任何值 callback 仍然不會執行！

這裡我們在 main 送出值時，去判斷 some 最後一次送的值是不是 1 來決定是否要切換大小寫，執行步驟如下

- main 送出了 `h`，此時 some 上一次送出的值為 `0`，把這兩個參數傳入 callback 得到 `h`。 
- main 送出了 `e`，此時 some 上一次送出的值為 `0`，把這兩個參數傳入 callback 得到 `e`。
- main 送出了 `l`，此時 some 上一次送出的值為 `0`，把這兩個參數傳入 callback 得到 `l`。
- main 送出了 `l`，此時 some 上一次送出的值為 `1`，把這兩個參數傳入 callback 得到 `L`。
- main 送出了 `o`，此時 some 上一次送出的值為 `1`，把這兩個參數傳入 callback 得到 `O`。

withLatestFrom 很常用在一些 checkbox 型的功能，例如說一個編輯器，我們開啟粗體後，打出來的字就都要變粗體，粗體就像是 some observable，而我們打字就是 main observable。

今日小結
------

今天介紹了三個合併用的 operators，這三個 operators 的 callback 都會依照合併的 observable 數量來傳入參數，如果我們合併了三個 observable，callback 就會有三個參數，而不管合併幾個 observable 都會只會回傳一個值。

這幾個 operators 需要花比較多的時間思考，讀者們不用硬記他的運作行為，只要稍微記得有這些 operators 可以用就可以了。等到真的要用時，再重新回來看他們的運作方式做選擇。

不知道讀者們今天有沒有收穫呢？ 如果有任何問題，歡迎在下方留言給我，謝謝！
