---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (23)：Subject, BehaviorSubject, ReplaySubject, AsyncSubject
date: 2017-01-08T23:01:49.000Z
description: 昨天我們介紹了 Subject 是什麼，今天要講 Subject 一些應用方式，以及 Subject 的另外三種變形。
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Subject
  - RxJS 30 Days
previous: ./thirty-days-RxJS-22.md
next: ./thirty-days-RxJS-24.md
---

Subject
------

昨天我們講到了 Subject 實際上就是 Observer Pattern 的實作，他會在內部管理一份 observer 的清單，並在接收到值時遍歷這份清單並送出值，所以我們可以這樣用 Subject

```javascript
var subject = new Rx.Subject();

var observerA = {
    next: value => console.log('A next: ' + value),
    error: error => console.log('A error: ' + error),
    complete: () => console.log('A complete!')
}

var observerB = {
    next: value => console.log('B next: ' + value),
    error: error => console.log('B error: ' + error),
    complete: () => console.log('B complete!')
}

subject.subscribe(observerA);
subject.subscribe(observerB);

subject.next(1);
// "A next: 1"
// "B next: 1"
subject.next(2);
// "A next: 2"
// "B next: 2"
```
[JSBin](https://jsbin.com/nazekem/1/edit?js,console)

這裡我們可以直接用 subject 的 next 方法傳送值，所有訂閱的 observer 就會接收到，又因為 Subject 本身是 Observable，所以這樣的使用方式很適合用在某些無法直接使用 Observable 的前端框架中，例如在 React 想對 DOM 的事件做監聽

```javascript
class MyButton extends React.Component {
    constructor(props) {
        super(props);
        this.state = { count: 0 };
        this.subject = new Rx.Subject();
        
        this.subject
            .mapTo(1)
            .scan((origin, next) => origin + next)
            .subscribe(x => {
                this.setState({ count: x })
            })
    }
    render() {
        return <button onClick={event => this.subject.next(event)}>{this.state.count}</button>
    }
}
```
[JSBin](https://jsbin.com/nazekem/6/edit?js,output) | [JSFiddle](https://jsfiddle.net/jLh8ham3/3/)

從上面的程式碼可以看出來，因為 React 本身 API 的關係，如果我們想要用 React 自訂的事件，我們沒辦法直接使用 Observable 的 creation operator 建立 observable，這時就可以靠 Subject 來做到這件事。

Subject 因為同時是 observer 和 observable，所以應用面很廣除了前面所提的之外，還有上一篇文章講的組播(multicase)特性也會在接下來的文章做更多應用的介紹，這裡先讓我們來看看 Subject 的三個變形。

## BehaviorSubject

很多時候我們會希望 Subject 能代表當下的狀態，而不是單存的事件發送，也就是說如果今天有一個新的訂閱，我們希望 Subject 能立即給出最新的值，而不是沒有回應，例如下面這個例子

```javascript
var subject = new Rx.Subject();

var observerA = {
    next: value => console.log('A next: ' + value),
    error: error => console.log('A error: ' + error),
    complete: () => console.log('A complete!')
}

var observerB = {
    next: value => console.log('B next: ' + value),
    error: error => console.log('B error: ' + error),
    complete: () => console.log('B complete!')
}

subject.subscribe(observerA);

subject.next(1);
// "A next: 1"
subject.next(2);
// "A next: 2"
subject.next(3);
// "A next: 3"

setTimeout(() => {
    subject.subscribe(observerB); // 3 秒後才訂閱，observerB 不會收到任何值。
},3000)
```

以上面這個例子來說，observerB 訂閱的之後，是不會有任何元素送給 observerB 的，因為在這之後沒有執行任何 `subject.next()`，但很多時候我們會希望 subject 能夠表達當前的狀態，在一訂閱時就能收到最新的狀態是什麼，而不是訂閱後要等到有變動才能接收到新的狀態，以這個例子來說，我們希望 observerB 訂閱時就能立即收到 `3`，希望做到這樣的效果就可以用 BehaviorSubject。

BehaviorSubject 跟 Subject 最大的不同就是 BehaviorSubject 是用來呈現當前的值，而不是單純的發送事件。BehaviorSubject 會記住最新一次發送的元素，並把該元素當作目前的值，在使用上 BehaviorSubject 建構式需要傳入一個參數來代表起始的狀態，範例如下

```javascript
var subject = new Rx.BehaviorSubject(0); // 0 為起始值
var observerA = {
    next: value => console.log('A next: ' + value),
    error: error => console.log('A error: ' + error),
    complete: () => console.log('A complete!')
}

var observerB = {
    next: value => console.log('B next: ' + value),
    error: error => console.log('B error: ' + error),
    complete: () => console.log('B complete!')
}

subject.subscribe(observerA);
// "A next: 0"
subject.next(1);
// "A next: 1"
subject.next(2);
// "A next: 2"
subject.next(3);
// "A next: 3"

setTimeout(() => {
    subject.subscribe(observerB); 
    // "B next: 3"
},3000)
```
[JSBin](https://jsbin.com/nazekem/7/edit?js,console) | JSFiddle

從上面這個範例可以看得出來 BehaviorSubject 在建立時就需要給定一個狀態，並在之後任何一次訂閱，就會先送出最新的狀態。其實這種行為就是一種狀態的表達而非單存的事件，就像是年齡跟生日一樣，年齡是一種狀態而生日就是事件；所以當我們想要用一個 stream 來表達年齡時，就應該用 BehaviorSubject。

## ReplaySubject

在某些時候我們會希望 Subject 代表事件，但又能在新訂閱時重新發送最後的幾個元素，這時我們就可以用 ReplaySubject，範例如下

```javascript
var subject = new Rx.ReplaySubject(2); // 重複發送最後 2 個元素
var observerA = {
    next: value => console.log('A next: ' + value),
    error: error => console.log('A error: ' + error),
    complete: () => console.log('A complete!')
}

var observerB = {
    next: value => console.log('B next: ' + value),
    error: error => console.log('B error: ' + error),
    complete: () => console.log('B complete!')
}

subject.subscribe(observerA);
subject.next(1);
// "A next: 1"
subject.next(2);
// "A next: 2"
subject.next(3);
// "A next: 3"

setTimeout(() => {
    subject.subscribe(observerB);
    // "B next: 2"
    // "B next: 3"
},3000)
```
[JSBin](https://jsbin.com/nazekem/10/edit?js,console) | 

可能會有人以為 `ReplaySubject(1)` 是不是就等同於 BehaviorSubject，其實是不一樣的，BehaviorSubject 在建立時就會有起始值，比如 `BehaviorSubject(0)` 起始值就是 `0`，BehaviorSubject 是代表著狀態而 ReplaySubject 只是事件的重放而已。

## AsyncSubject

AsyncSubject 是最怪的一個變形，他有點像是 operator `last`，會在 subject 結束後送出最後一個值，範例如下

```javascript
var subject = new Rx.AsyncSubject();
var observerA = {
    next: value => console.log('A next: ' + value),
    error: error => console.log('A error: ' + error),
    complete: () => console.log('A complete!')
}

var observerB = {
    next: value => console.log('B next: ' + value),
    error: error => console.log('B error: ' + error),
    complete: () => console.log('B complete!')
}

subject.subscribe(observerA);
subject.next(1);
subject.next(2);
subject.next(3);
subject.complete();
// "A next: 3"
// "A complete!"

setTimeout(() => {
    subject.subscribe(observerB);
    // "B next: 3"
    // "B complete!"
},3000)
```
[JSBin](https://jsbin.com/nazekem/edit?js,console) |

從上面的程式碼可以看出來，AsyncSubject 會在 subject 結束後才送出最後一個值，其實這個行為跟 Promise 很像，都是等到事情結束後送出一個值，但實務上我們非常非常少用到 AsyncSubject，絕大部分的時候都是使用 BehaviorSubject 跟 ReplaySubject 或 Subject。

~~我們把 AsyncSubject 放在大腦的深處就好~~

## 今日小結

今天介紹了 Subject 的一些應用方式，以及 BehaviorSubject, ReplaySubject, AsyncSubject 三個變形各自的特性介紹，不知道讀者麼是否有收穫呢？ 如果有任何問題，歡迎在下方留言給我！