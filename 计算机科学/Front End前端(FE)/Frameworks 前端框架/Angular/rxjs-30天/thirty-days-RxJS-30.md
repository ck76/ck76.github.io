---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS(30)：Cold & Hot Observable
date: 2017-03-17T15:04:10.000Z
description: Hot Observable 跟 Cold Observable 的差別，其實就是 **資料源(Data Source)** 在 Observable 內部建立還是外部建立。
image: null
tags:
  - Front End
  - RxJS
  - Observable
  - RxJS 30 Days
previous: ./thirty-days-RxJS-29.md
next: ./thirty-days-RxJS-31.md
---

在 RxJS 中很常會看到 Cold Observable 跟 Hot Observable 這兩個名詞，其實他們是在區分不同行為的 Observable，所謂的 Cold Observable 就是指每次訂閱都是**獨立的執行**，而 Hot Observable 則是**共用的訂閱**。

## Cold Observable

Cold Observable 代表 Observable 的每個訂閱都是獨立的，他們不會互相影響，如下

```javascript
const source = Rx.Observable.interval(1000).take(5);

source.subscribe(value => console.log('sub1: ' + value))

setTimeout(() => {
    source.subscribe(value => console.log('sub2: ' + value))    
}, 3500);

// sub1: 0
// sub1: 1
// sub1: 2
// sub1: 3
// sub2: 0
// sub1: 4
// sub2: 1
// sub2: 2
// sub2: 3
// sub2: 4
```
[JSBin](https://jsbin.com/sapuvilipa/4/edit?js,console) | [JSFiddle](https://jsfiddle.net/mk5y5hhu/)

從上面的程式碼可以看出來每次訂閱 `source` 都是獨立運行的，這種每次訂閱都是 **獨立執行** 的 Observable 就稱為 Cold Observable。

如果從 Observable 內部來看，代表 **資料源(Data Source)** 是在 Observable **內部**建立的的，大概會長像下面

```javascript
const source = Rx.Observable.create(function(observer) {
    // 訂閱時，才建立新的資料源
    const someDataSource = getSomeDataSource();
    someDataSource.addEventListener('message', (data) => {
        observer.next(data)
    })
})
```

因為每次訂閱都建立一個新的資料源，就會使資料從頭開始傳送。


# Hot Observable

Hot Observable 代表 Observable 的每個訂閱是共用的，所謂的共用訂閱就是指 一個 Observable 在多次訂閱時，不會每次都從新開始發送元素，例如

```javascript
var source = Rx.Observable.interval(1000)
            .take(5)
            .share(); // 共用

source.subscribe(value => console.log('sub1: ' + value))

setTimeout(() => {
    source.subscribe(value => console.log('sub2: ' + value))    
}, 3500);

// sub1: 0
// sub1: 1
// sub1: 2
// sub1: 3
// sub2: 3
// sub1: 4
// sub2: 4
```
[JSBin](https://jsbin.com/sapuvilipa/3/edit?js,console) | [JSFiddle](https://jsfiddle.net/mk5y5hhu/1/)

從上面的程式碼可以看出，當我們對 source 第二次做訂閱時，接收到的元素是接續第一個訂閱往下發送的，而不是從新(0)開始，這種 **共用訂閱** 的 Observable 就稱為 Hot Observable。

如果從 Observable 內部來看，就是資料源是在 Observable **外部**建立的，程式碼大概就會像下面這樣

```javascript
// 只有一個資料源，每次訂閱都是用同一個
const someDataSource = getSomeDataSource();
const source = Rx.Observable.create(function(observer) {
    someDataSource.addEventListener('message', (data) => {
        observer.next(data)
    })
});
```

## Cold 與 Hot 

一般的情況下 Observable 都是 Cold 的，這樣不同的訂閱才不會有 Side Effect 互相影響。但在需要多次訂閱的情境下，我們就很有可能需要 Hot Observable，而讓 RxJS 提供了很多讓 Cold Observable 變成 Hot Observable 的方法，這個部分可以參考以下文章：

- 30 天精通 RxJS(22): Subject 基本觀念
- 30 天精通 RxJS(24): Observable operators - multicast, refCount, publish, share

## 小結

Hot Observable 跟 Cold Observable 的差異就是多次訂閱時，是否共用訂閱或是獨立執行。 而這一切的差異就是來自於 資料源 是在 Observable 內部建立還是外部建立。