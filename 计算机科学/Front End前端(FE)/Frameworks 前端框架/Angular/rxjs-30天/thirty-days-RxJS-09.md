---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (09)：Observable Operator - skip, takeLast, last, concat, startWith, merge
date: 2016-12-25T23:16:13.000Z
description: "今天是美好的聖誕節，先祝讀者們聖誕快樂！
為了讓大家在聖誕節好好的陪家人，所以今天的文章內容就輕鬆點，讓我們簡單介紹幾個的 operators 就好了。"
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Operator
  - RxJS 30 Days
previous: ./thirty-days-RxJS-08.md
next: ./thirty-days-RxJS-10.md
---

這是【30天精通 RxJS】的 09 篇，如果還沒看過 08 篇可以往這邊走：
[30 天精通 RxJS (08)：簡易拖拉實作 - take, first, takeUntil, concatAll](/series/rxjs/thirty-days-RxJS-08)

Operators
------

### skip

我們昨天介紹了 `take` 可以取前幾個送出的元素，今天介紹可以略過前幾個送出元素的 operator: `skip`，範例如下：

```javascript
var source = Rx.Observable.interval(1000);
var example = source.skip(3);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// 3
// 4
// 5...
```
[JSBin](https://jsbin.com/rucopex/1/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/tucvcrmh/)


原本從 0 開始的就會變成從 3 開始，但是記得原本元素的等待時間仍然存在，也就是說此範例第一個取得的元素需要等 4 秒，用 Marble Diagram 表示如下。

```bash
source : ----0----1----2----3----4----5--....
                    skip(3)
example: -------------------3----4----5--...
```

### takeLast

除了可以用 take 取前幾個之外，我們也可以倒過來取最後幾個，範例如下：

```javascript
var source = Rx.Observable.interval(1000).take(6);
var example = source.takeLast(2);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// 4
// 5
// complete
```

這裡我們先取了前 6 個元素，再取最後兩個。所以最後會送出 4, 5, complete，這裡有一個重點，就是 takeLast 必須等到整個 observable 完成(complete)，才能知道最後的元素有哪些，並且**同步送出**，如果用 Marble Diagram 表示如下

```bash
source : ----0----1----2----3----4----5|
                takeLast(2)
example: ------------------------------(45)|
```

這裡可以看到 takeLast 後，比須等到原本的 observable 完成後，才立即同步送出 4, 5, complete。

### last

跟 `take(1)` 相同，我們有一個 `takeLast(1)` 的簡化寫法，那就是 `last()` 用來取得最後一個元素。

```javascript
var source = Rx.Observable.interval(1000).take(6);
var example = source.last();

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// 5
// complete
```

用 Marble Diagram 表示如下

```bash
source : ----0----1----2----3----4----5|
                    last()
example: ------------------------------(5)|
```

### concat

`concat` 可以把多個 observable 實例合併成一個，範例如下

```javascript
var source = Rx.Observable.interval(1000).take(3);
var source2 = Rx.Observable.of(3)
var source3 = Rx.Observable.of(4,5,6)
var example = source.concat(source2, source3);

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
// complete
```
[JSBin](https://jsbin.com/rucopex/2/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/tucvcrmh/1/)

跟 `concatAll` 一樣，必須先等前一個 observable 完成(complete)，才會繼續下一個，用 Marble Diagram 表示如下。

```bash
source : ----0----1----2|
source2: (3)|
source3: (456)|
            concat()
example: ----0----1----2(3456)|
```

另外 concat 還可以當作靜態方法使用

```javascript
var source = Rx.Observable.interval(1000).take(3);
var source2 = Rx.Observable.of(3);
var source3 = Rx.Observable.of(4,5,6);
var example = Rx.Observable.concat(source, source2, source3);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```
[JSBin](https://jsbin.com/rucopex/3/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/tucvcrmh/2/)

### startWith

`startWith` 可以在 observable 的一開始塞要發送的元素，有點像 `concat` 但參數不是 observable 而是要發送的元素，使用範例如下

```javascript
var source = Rx.Observable.interval(1000);
var example = source.startWith(0);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// 0
// 0
// 1
// 2
// 3...
```

這裡可以看到我們在 source 的一開始塞了一個 `0`，讓 example 會在一開始就立即送出 `0`，用 Marble Diagram 表示如下

```bash
source : ----0----1----2----3--...
                startWith(0)
example: (0)----0----1----2----3--...
```

記得 startWith 的值是一開始就同步發出的，這個 operator 很常被用來保存程式的起始狀態！

### merge

`merge` 跟 `concat` 一樣都是用來合併 observable，但他們在行為上有非常大的不同！

讓我們直接來看例子吧

```javascript
var source = Rx.Observable.interval(500).take(3);
var source2 = Rx.Observable.interval(300).take(6);
var example = source.merge(source2);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
// 0
// 0
// 1
// 2
// 1
// 3
// 2
// 4
// 5
// complete
```
[JSBin](https://jsbin.com/rucopex/6/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/tucvcrmh/3/)

上面可以看得出來，`merge` 把多個 observable 同時處理，這跟 `concat` 一次處理一個 observable 是完全不一樣的，由於是同時處理行為會變得較為複雜，這裡我們用 Marble Diagram 會比較好解釋。

```bash
source : ----0----1----2|
source2: --0--1--2--3--4--5|
            merge()
example: --0-01--21-3--(24)--5|
```

這裡可以看到 `merge` 之後的 example 在時間序上同時在跑 source 與 source2，當兩件事情同時發生時，會同步送出資料(被 merge 的在後面)，當兩個 observable 都結束時才會真的結束。

merge 同樣可以當作靜態方法用

```javascript
var source = Rx.Observable.interval(500).take(3);
var source2 = Rx.Observable.interval(300).take(6);
var example = Rx.Observable.merge(source, source2);

example.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
```

merge 的邏輯有點像是 OR(||)，就是當兩個 observable 其中一個被觸發時都可以被處理，這很常用在一個以上的按鈕具有部分相同的行為。

例如一個影片播放器有兩個按鈕，一個是暫停(II)，另一個是結束播放(口)。這兩個按鈕都具有相同的行為就是影片會被停止，只是結束播放會讓影片回到 00 秒，這時我們就可以把這兩個按鈕的事件 merge 起來處理影片暫停這件事。

```javascript
var stopVideo = Rx.Observable.merge(stopButton, endButton);

stopVideo.subscribe(() => {
    // 暫停播放影片
})
```


今日小結
------

今天介紹的六個 operators 都是平時很容易用到的，我們之後的範例也有機會再遇到。希望讀者們能自己試試這些方法，之後使用時會比較有印象！

不知道讀者今天有沒有收穫呢？ 如果有任何問題，歡迎在下方留言給我，謝謝。