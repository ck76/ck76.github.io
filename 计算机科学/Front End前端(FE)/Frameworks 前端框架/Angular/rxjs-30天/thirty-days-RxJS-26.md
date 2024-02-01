---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (26)：簡易實作 Observable(一)
date: 2017-01-11T23:39:24.000Z
description: 因為實在太多讀者在問要如何實作 Observable，所以特別調整了本系列文章最後幾篇的內容，空出一天的位置來寫如何簡易實作 Observable。
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Observer
  - RxJS 30 Days
previous: ./thirty-days-RxJS-25.md
next: ./thirty-days-RxJS-27.md
--- 

為什麼是簡易實作而不完整實作呢？ 當然這個系列的文章是希望讀者能學會如何使用 RxJS，而 實作 Observable 其實只是幫助我們理解 Observable 的運作方式，所以這篇文章會盡可能地簡單，一來讓讀者容易理解及吸收，二來有興趣的讀者可以再沿著這篇文章的內容去完整的實作。

## 重點觀念

Observable 跟 Observer Pattern 是不同的，Observable 內部並沒有管理一份訂閱清單，**訂閱 Observable 就像是執行一個 function 一樣**！

所以實作過程的重點

- 訂閱就是執行一個 funciton
- 訂閱接收的物件具備 next, error, complete 三個方法
- 訂閱會返回一個可退訂(unsubscribe)的物件

## 基本 observable 實作

先用最簡單的 function 來建立 observable 物件

```javascript
function create(subscriber) {
    var observable = {
        subscribe: function(observer) {
            subscriber(observer)
        }       
    };
    return observable;
}
```

上面這段程式碼就可以做最簡單的訂閱，像下面這樣

```javascript
function create(subscriber) {
    var observable = {
        subscribe: function(observer) {
            subscriber(observer)
        }       
    };
    return observable;
}

var observable = create(function(observer) {
  observer.next(1);
  observer.next(2);
  observer.next(3);
})

var observer = {
  next: function(value) {
    console.log(value)
  }
}

observable.subscribe(observer)
// 1
// 2
// 3
```
[JSBin](https://jsbin.com/tububez/1/edit?js,console) 

這時我們已經有最簡單的功能了，但這裡有一個大問題，就是 observable 在結束(complete)就不應該再發送元素

```javascript
var observable = create(function(observer) {
  observer.next(1);
  observer.next(2);
  observer.next(3);
  observer.complete();
  observer.next('still work');
})

var observer = {
  next: function(value) {
    console.log(value)
  },
  complete: function() {
    console.log('complete!')
  }
}

observable.subscribe(observer)
// 1
// 2
// 3
// "complete!"
// "still work"
```
[JSBin](https://jsbin.com/tububez/2/edit?js,console)

從上面的程式碼可以看到 complete 之後還是能送元素出來，另外還有一個問題就是 observer，如果是不完整的就會出錯，這也不是我們希望看到的。

```javascript
var observable = create(function(observer) {
  observer.next(1);
  observer.next(2);
  observer.next(3);
  observer.complete(); // error: complete is not a function 
})

var observer = {
  next: function(value) {
    console.log(value)
  }
}

observable.subscribe(observer)
// 1
// 2
// 3
// "complete!"
// "still work"
```
[JSBin](https://jsbin.com/tububez/3/edit?js,console)

上面這段程式碼可以看出來，當使用者 observer 物件沒有 complete 方法時，就會報錯。
我們應該修正這兩個問題！

## 實作簡易 Observer

要修正這兩個問題其實並不難，我們只要實作一個 Observer 的類別，每次使用者傳入的 observer 都會利用這個類別轉乘我們想要 Observer 物件。

首先訂閱時有可能傳入一個 observer 物件，或是一到三個 function(next, error, complete)，所以我們要建立一個類別可以接受各種可能的參數

```javascript
class Observer {
  constructor(destinationOrNext, error, complete) {
    switch (arguments.length) {
      case 0:
        // 空的 observer
      case 1:
        if (!destinationOrNext) {
          // 空的 observer
        }
        if (typeof destinationOrNext === 'object') {
          // 傳入了 observer 物件
        }
      default:
        // 如果上面都不是，代表應該是傳入了一到三個 function
        break;
    }
  }
}
```

寫一個方法(safeObserver)來回傳正常的 observer

```javascript
class Observer {
  constructor(destinationOrNext, error, complete) {
    // ... 一些程式碼
  }
  safeObserver(observerOrNext, error, complete) {
    let next;

    if (typeof (observerOrNext) === 'function') {
      // observerOrNext 是 next function
      next = observerOrNext;
    } else if (observerOrNext) {
      // observerOrNext 是 observer 物件
      next = observerOrNext.next || () => {};
      error = observerOrNext.error || function(err) { 
        throw err 
      };
      complete = observerOrNext.complete || () => {};
    }
    // 最後回傳我們預期的 observer 物件
    return {
      next: next,
      error: error,
      complete: complete
    };
  }
}
```

再把 constructor 完成

```javascript
// 預設空的 observer 
const emptyObserver = {
  next: () => {},
  error: (err) => { throw err; },
  complete: () => {}
}

class Observer {
  constructor(destinationOrNext, error, complete) {
    switch (arguments.length) {
      case 0:
        // 空的 observer
        this.destination = this.safeObserver(emptyObserver);
        break;
      case 1:
        if (!destinationOrNext) {
          // 空的 observer
          this.destination = this.safeObserver(emptyObserver);
          break;
        }
        if (typeof destinationOrNext === 'object') {
          // 傳入了 observer 物件
          this.destination = this.safeObserver(destinationOrNext);
          break;
        }
      default:
        // 如果上面都不是，代表應該是傳入了一到三個 function
        this.destination = this.safeObserver(destinationOrNext, error, complete);
        break;
    }
  }
  safeObserver(observerOrNext, error, complete) {
    // ... 一些程式碼
  }
}
```
[JSBin](https://jsbin.com/tububez/5/edit?js,console)

這裡我們把真正的 observer 塞到 `this.destination`，接著完成 observer 的方法。

Observer 的三個主要的方法(next, error, complete)都應該結束或退訂後不能再被執行，所以我們在物件內部偷塞一個 boolean 值來作為是否曾經結束的依據。

```javascript
class Observer {
  constructor(destinationOrNext, error, complete) {
    // ... 一些程式碼
  }
  safeObserver(observerOrNext, error, complete) {
    // ... 一些程式碼
  }
  unsubscribe() {
    this.isStopped = true; // 偷塞一個屬性 isStopped
  }
}
```

接著要實作三個主要的方法就很簡單了，只要先判斷 `isStopped` 在使用 `this.destination` 物件來傳送值就可以了

```javascript
class Observer {
  constructor(destinationOrNext, error, complete) {
    // ... 一些程式碼
  }
  safeObserver(observerOrNext, error, complete) {
    // ... 一些程式碼
  }
  
  next(value) {
    if (!this.isStopped && this.next) {
      // 先判斷是否停止過
      try {
        this.destination.next(value); // 傳送值
      } catch (err) {
        this.unsubscribe();
        throw err;
      }
    }
  }
  
  error(err) {
    if (!this.isStopped && this.error) {
      // 先判斷是否停止過
      try {
        this.destination.error(err); // 傳送錯誤
      } catch (anotherError) {
        this.unsubscribe();
        throw anotherError;
      }
      this.unsubscribe();
    }
  }

  complete() {
    if (!this.isStopped && this.complete) {
      // 先判斷是否停止過
      try {
        this.destination.complete(); // 發送停止訊息
      } catch (err) {
        this.unsubscribe();
        throw err;
      }
      this.unsubscribe(); // 發送停止訊息後退訂
    }
  }
  
  unsubscribe() {
    this.isStopped = true;
  }
}
```
[JSBin](https://jsbin.com/tububez/6/edit?js,console)

到這裡我們就完成基本的 Observer 實作了，接著讓我們拿到基本版的 observable 中使用吧。

```javascript
function create(subscriber) {
    const observable = {
        subscribe: function(observerOrNext, error, complete) {
            const realObserver = new Observer(observerOrNext, error, complete)
            subscriber(realObserver);
            return realObserver;
        }       
    };
    return observable;
}

var observable = create(function(observer) {
  observer.next(1);
  observer.next(2);
  observer.next(3);
  observer.complete();
  observer.next('not work');
})

var observer = {
  next: function(value) {
    console.log(value)
  },
  complete: function() {
      console.log('complete!')
  }
}

observable.subscribe(observer);
// 1
// 2
// 3
// complete!
```
[JSBin](https://jsbin.com/tububez/7/edit?js,console)

到這裡我們就完成最基本的 observable 了，至少基本的行為都跟我們期望的一致，我知道讀者們仍然不會放過我，你們會希望做出一個 Observable 型別以及至少一個 operator 對吧？ 不用擔心，我們下一篇就會講解如何建立一個 Observable 型別和 operator 的方法！

## 今日小結

今天我們複習了 Observable 的重要概念，並用這些重要的概念實作出了基本的 observable 以及 Observer 的類別。

不知道今天讀者們有沒有收穫呢？ 如果有任何問題，歡迎在下方留言給我，謝謝！