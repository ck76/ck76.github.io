---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (28)：Scheduler 基本觀念
date: 2017-01-13T23:48:28.000Z
description: 不曉得讀者們還記不記得，我們在前面的文章中有提到 Scheduler 是為了解決 RxJS 衍生的最後一個問題，而我們現在就在揭曉這個謎底。
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Scheduler
  - RxJS 30 Days
previous: ./thirty-days-RxJS-27.md
next: ./thirty-days-RxJS-29.md
--- 

其實 RxJS 用久了之後就會發現 Observable 有一個優勢是可以同時處理同步和非同步行為，但這個優勢也帶來了一個問題，就是我們常常會搞不清處現在的 observable 執行方式是同步的還是非同步的。換句話說，我們很容易搞不清楚 observable 到底什麼時候開始發送元素！

舉例來說，我們可能很清楚 `interval` 是非同步送出元素的，但 `range` 呢？ `from` 呢？他們可能有時候是非同步有時候是同步，這就會變得有點困擾，尤其在除錯時執行順序就非常重要。

而 Scheduler 基本上就是拿來處理這個問題的！

## 什麼是 Scheduler？

Scheduler 控制一個 observable 的訂閱什麼時候開始，以及發送元素什麼時候送達，主要由以下三個元素所組成

- Scheduler 是一個資料結構。 它知道如何根據優先級或其他標準來儲存並佇列任務。
- Scheduler 是一個執行環境。 它意味著任務何時何地被執行，比如像是 立即執行、在回呼(callback)中執行、setTimeout 中執行、animation frame 中執行
- Scheduler 是一個虛擬時鐘。 它透過 `now()` 這個方法提供了時間的概念，我們可以讓任務在特定的時間點被執行。

簡言之 Scheduler 會影響 Observable 開始執行及元素送達的時機，比如下面這個例子

```javascript
var observable = Rx.Observable.create(function (observer) {
    observer.next(1);
    observer.next(2);
    observer.next(3);
    observer.complete();
});

console.log('before subscribe');
observable.observeOn(Rx.Scheduler.async) // 設為 async
.subscribe({
    next: (value) => { console.log(value); },
    error: (err) => { console.log('Error: ' + err); },
    complete: () => { console.log('complete'); }
});
console.log('after subscribe');

// "before subscribe"
// "after subscribe"
// 1
// 2
// 3
// "complete"
```
[JSBin](https://jsbin.com/sunekab/2/edit?js,console)

上面這段程式碼原本是同步執行的，但我們用了 `observable.observeOn(Rx.Scheduler.async)` 原本是同步執行的就變成了非同步執行了。

## 有哪些 Scheduler 可以用

目前 RxJS 5 Scheduler 跟 RxJS 4.x 以前的版本完全不同，在 RxJS 5 當中有提供四個 scheduler，預設為 undefined 會直接以遞回的方式執行

- queue
- asap
- async
- animationFrame

這四個 scheduler 我們會在下面搭配程式碼一一講解

> RxJS 5 跟 RxJS 4.x 預設的 Scheduler 不同，所以在某些使用情境下會出現不同的結果，例如[這個](https://github.com/ReactiveX/rxjs/issues/1994) issue，請特別注意。

## 使用 Scheduler

其實我們在使用各種不同的 operator 時，這些 operator 就會各自預設不同的 scheduler，例如一個無限的 observable 就會預設為 `queue` scheduler，而 timer 相關的 operator 則預設為 `async` scheduler。

要使用 Scheduler 除了前面用到的 `observeOn()` 方法外，以下這幾個 creation operators 最後一個參數都能接收 Scheduler

- bindCallback
- bindNodeCallback
- combineLatest
- concat
- empty
- from
- fromPromise
- interval
- merge
- of
- range
- throw
- timer

例如下面這個例子

```javascript
var observable = Rx.Observable.from([1,2,3,4,5], Rx.Scheduler.async);
```

另外還有多個 operators 最後一個參數可以傳入 Scheduler 這邊就不一一列出，這已參考官方的[文件](http://reactivex.io/rxjs/class/es6/Observable.js~Observable.html)，最通用的方式還是 `observeOn()` 只要是 observable 就可以用這個方法。

### queue

queue 的運作方式跟預設的立即執行很像，但是當我們使用到遞回的方法時，他會佇列這些行為而非直接執行，一個遞回的 operator 就是他會執行另一個 operator，最好的例子就是 `repeat()`，如果我們不給他參數的話，他會執行無限多次，像下面這個例子

```javascript
Rx.Observable.of(10).repeat().take(1)
.subscribe(console.log);
```

這個例子在 RxJS 4.x 的版本中執行會使瀏覽器掛掉，因為 `take(1)` 永遠不會被執行到 `repeat` 會一直重複要元素，而在 RxJS 5 中他預設了無限的 observable 為 queue 所以他會把 repeat 的 next 行為先佇列起來，因為前一個 complete 還在執行中，而這時 repeat 就會回傳一個可退訂的物件給 `take(1)` 等到 repeat 的 next 被第一次執行時就會結束，因為 `take(1)` 會直接收到值。

**使用情境：**

queue 很適合用在會有遞回的 operator 且具有大量資料時使用，在這個情況下 queue 能避免不必要的效能損耗。

### asap

asap 的行為很好理解，它是非同步的執行，在瀏覽器其實就是 setTimeout 設為 0 秒 (在 NodeJS 中是用 process.nextTick)，因為行為很好理解這裡就不寫例子了。

**使用情境：**

asap 因為都是在 setTimeout 中執行，所以不會有 block event loop 的問題，很適合用在永遠不會退訂的 observable，例如在背景下持續監聽 server 送來的通知。

### async

這個是在 RxJS 5 中新出現的 Scheduler，它跟 asap 很像但是使用 setInterval 來運作，通常是跟時間相關的 operator 才會用到。

### animationFrame

這個相信大家應該都知道，他是利用 `Window.requestAnimationFrame` 這個 API 去實作的，所以執行週期就跟 `Window.requestAnimationFrame` 一模一樣。

**使用情境：**

在做複雜運算，且高頻率觸發的 UI 動畫時，就很適合使用 animationFrame，以可以搭配 throttle operator 使用。

## 今日小結

這篇文章簡單的介紹了 RxJS 的 Scheduler，因為篇幅的關係沒有辦法很細的去講，但實務上 Scheduler 的使用非常簡單，只要在 operator 的最後一個參數加上去或是用 observeOn 就可以了。平常其實不太需要用到 Scheduler，尤其在 RxJS 5 中已經有針對各種情況給不同的預設，筆者最常用到的還是 animationFrame！

不知道今天讀者們有沒有收穫呢？ 如果有任何疑問，歡迎在下方留言給我，謝謝。
