---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (27)：簡易實作 Observable(二)
date: 2017-01-12T23:43:17.000Z
description: 前一篇文章我們已經完成了基本的 observable 以及 Observer 的簡易實作，這篇文章我們會接續上一篇的內容實作簡易的 Observable 類別，以及一個 creation operator 和一個  transform operator。
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Observer
  - RxJS 30 Days
previous: ./thirty-days-RxJS-26.md
next: ./thirty-days-RxJS-28.md
--- 

## 建立簡易 Observable 類別

這是我們上一篇文章寫的建立 observable 物件的函式

```javascript
function create(subscribe) {
    const observable = {
        subscribe: function() {
            const realObserver = new Observer(...arguments);
            subscribe(realObserver);
            return realObserver;
        }       
    };
    return observable;
}
```
[JSBin](https://jsbin.com/paxevam/2/edit?js,console)

從這個函式可以看出來，回傳的 observable 物件至少會有 subscribe 方法，所以最簡單的 Observable 類別大概會長像下面這樣

```javascript
class Observable {
  subscribe() {
    // ...做某些事
  }
}
```

另外 create 的函式在執行時會傳入一個 subscribe 的 function，這個 function 會決定 observable 的行為

```javascript
var observable = create(function(observer) {
  observer.next(1);
  observer.next(2);
  observer.next(3);
  observer.complete();
  observer.next('not work');
})
```

把上面這一段改成下面這樣

```javascript
var observable = new Observable(function(observer) {
  observer.next(1);
  observer.next(2);
  observer.next(3);
  observer.complete();
  observer.next('not work');
})
```

所以我們的 Observable 的建構式應該會接收一個 subscribe function

```javascript
class Observable {
  constructor(subscribe) {
    if(subscribe) {
      this._subscribe = subscribe; // 把 subscribe 存到屬性中
    }
  }
  subscribe() {
    // ...做某些事
  }
}
```

接著我們就能完成 subscribe 要做的事情了

```javascript
class Observable {
  constructor(subscribe) {
    if(subscribe) {
      this._subscribe = subscribe; // 把 subscribe 存到 _subscribe 屬性中
    }
  }
  subscribe() {
    const observer = new Observer(...arguments);
    this._subscribe(observer); // 就是執行一個 function 對吧
    return observer;
  }
}
```

到這裡我們就成功的把 create 的函式改成 Observable 的類別了，我們可以直接來使用看看

```javascript
class Observable {
  constructor(subscribe) {
    if(subscribe) {
      this._subscribe = subscribe; // 把 subscribe 存到屬性中
    }
  }
  subscribe() {
    const observer = new Observer(...arguments);
    this._subscribe(observer);
    return observer;
  }
}

var observable = new Observable(function(observer) {
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
```
[JSBin](https://jsbin.com/paxevam/3/edit?js,console)

當然我們可以仿 RxJS 在靜態方法中加入 create，如下

```javascript
class Observable {
  constructor(subscribe) {
    if(subscribe) {
      this._subscribe = subscribe; // 把 subscribe 存到屬性中
    }
  }
  subscribe() {
    const observer = new Observer(...arguments);
    this._subscribe(observer);
    return observer;
  }
}

Observable.create = function(subscribe) {
    return new Observable(subscribe);
}
```

這樣一來我們就可以用 `Observable.create` 建立 observable 物件實例。

```javascript
var observable = Observable.create(function(observer) {
  observer.next(1);
  observer.next(2);
  observer.next(3);
  observer.complete();
  observer.next('not work');
});
```
[JSBin](https://jsbin.com/paxevam/4/edit?js,console)

## 建立 creation operator - fromArray

當我們有 Observable 類別後要建立 creation operator 就不難了，這裡我們建立一個 fromArray 的方法，可以接收 array 來建立 observable，算是 Rx 的 Observable.from 的簡化版本，記得 creation operators 都屬於 static 方法

```javascript
class Observable {
  constructor(subscribe) {
    if(subscribe) {
      this._subscribe = subscribe; // 把 subscribe 存到屬性中
    }
  }
  subscribe() {
    const observer = new Observer(...arguments);
    this._subscribe(observer);
    return observer;
  }
}

// 建立靜態方法 
Observable.fromArray = function(array) {
    if(!Array.isArray(array)) {
        // 如果傳入的參數不是陣列，則拋出例外
        throw new Error('params need to be an array');
    }
    return new Observable(function(observer) {
        try{
            // 遍歷每個元素並送出
            array.forEach(value => observer.next(value))
            observer.complete()
        } catch(err) {
            observer.error(err)
        }
    });
}

var observable = Observable.fromArray([1,2,3,4,5]);
```
[JSBin](https://jsbin.com/paxevam/6/edit?js,console)

上面的程式碼我們只是簡單的用 new Observable 就可以輕鬆地實現我們要的功能，之後就可以用 fromArray 來建立 observable 物件。

相信讀者到這之前應該都不會有太大的問題，接下來這個部份就困難的多，請讀者們一定要搞懂前面的各個實作再接著往下看。

## 建立 transform operator - map

相信很多人在實作 Observable 都是卡在這個階段，因為 operators 都是回傳一個新的 observable 這中間有很多細節需要注意，並且有些小技巧才能比較好的實現，在開始實作之前先讓我們釐清幾個重點

- operators(transform, filter, conditional...) 都是回傳一個新個 observable 
- 大部分的 operator 其實就是在原本 observer 外包裹一層物件，讓執行 next 方法前先把元素做一次處理
- operator 回傳的 observable 訂閱時，還是需要執行原本的 observable(資料源)，也就說我們要想辦法保留原本的 observable

讓我們一步一步來，首先 operators 執行完會回傳一個新的 observable，這個 observable 在訂閱時會先去執行 operator 的行為再發送元素，所以 observable 的訂閱方法就不能像現在這樣直接把 observer 傳給 subscribe 執行

```javascript
class Observable {
  constructor(subscribe) {
    if(subscribe) {
      this._subscribe = subscribe; // 把 subscribe 存到屬性中
    }
  }
  subscribe() {
    const observer = new Observer(...arguments);
    // 先做某個判斷是否當前的 observable 是具有 operator 的
    if(??) {
      // 用 operator 的操作
    } else {
      // 如果沒有 operator 再直接把 observer 丟給 _subscribe
      this._subscribe(observer);
    }
    return observer;
  }
}
```

> 以我們的 Observable 實作為例，這裡最重要的就是 this._subscribe 執行，每當執行時就是開始發送元素。

這裡我們可以想像一下當一個 map 產生的 observable 訂閱時，應該先判斷出有 map 這個 operator 並且傳入原本的資料源以及當前的 observer。也就是說我們的 map 至少有以下這幾件事要做

- 建立新的 observable
- 保存原本的 observable(資料源)，之後訂閱時才有辦法執行
- 建立並保存 operator 本身的行為，等到訂閱時執行

```javascript
class Observable {
  constructor(subscribe) {
    // 一些程式碼...
  }
  subscribe() {
    // 一些程式碼...
  }
  map(callback) {
    const observable = new Observable(); // 建立新的 observable
    
    observable.source = this; // 保存當前的 observable(資料源)
    
    observable.operator = {
        call: (observer, source) => { // 執行這個 operator 的行為 }
    }; // 儲存當前 operator 行為，並作為是否有 operator 的依據，
    
    return observable; // 返回這個新的 observable
  }
}
```

上面這三個步驟都是必要的，特別是用到了 `observable.source = this` 這個小技巧，來保存原本的 observable。但這裡我們還有一個地方沒完成就是 operator 要做的事，這個部分我們等一下再補，先把 subscribe 寫完

```javascript
class Observable {
  constructor(subscribe) {
    // 一些程式碼...
  }
  subscribe() {
    const observer = new Observer(...arguments);
    // 先用 this.operator 判斷當前的 observable 是否具有 operator 
    if(this.operator) {
      this.operator.call(observer, this.source)
    } else {
      // 如果沒有 operator 再直接把 observer 丟給 _subscribe
      this._subscribe(observer);
    }
    return observer;
  }
  map(callback) {
    const observable = new Observable(); // 建立新的 observable
    
    observable.source = this; // 保存當前的 observable(資料源)
    
    observable.operator = {
        call: (observer, source) => { // 執行這個 operator 的行為 }
    }; // 儲存當前 operator 行為，並作為是否有 operator 的依據，
    
    return observable; // 返回這個新的 observable
  }
}
```

記得這裡補的 subscribe 行為，已經是 map 回傳新 observable 的行為，不是原本的 observable 了。

到這裡我們就幾乎要完成了，接著只要實作 map 這個 operator 的行為就可以囉！記得我們在前面講的 operator 其實就是在原本的 observer 做一層包裹，讓 next 執行前先對元素做處理，所以我們改寫一下 Observer 並建立一個 MapObserver 來做這件事

```javascript
class Observer {
  constructor(destinationOrNext, error, complete) {
    switch (arguments.length) {
      case 0:
        this.destination = this.safeObserver(emptyObserver);
        break;
      case 1:
        if (!destinationOrNext) {
          this.destination = this.safeObserver(emptyObserver);
          break;
        }
        // 多一個判斷，是否傳入的 destinationOrNext 原本就是 Observer 的實例，如果是就不用在用執行 `this.safeObserver`
        if(destinationOrNext instanceof Observer){
          this.destination = destinationOrNext;
          break;
        }
        if (typeof destinationOrNext === 'object') {
          this.destination = this.safeObserver(destinationOrNext);
          break;
        }
      default:
        this.destination = this.safeObserver(destinationOrNext, error, complete);
        break;
    }
  }
  
  // ...下面都一樣
}

class MapObserver extends Observer {
  constructor(observer, callback) {
    // 這裡會傳入原本的 observer 跟 map 的 callback
    super(observer); // 因為有繼承所以要先執行一次父層的建構式
    this.callback = callback; // 保存 callback
    this.next = this.next.bind(this); // 確保 next 的 this
  }
  next(value) {
    try {
      this.destination.next(this.callback(value)); 
      // this.destination 是父層 Observer 保存的 observer 物件
      // 這裡 this.callback(value) 就是 map 的操作
    } catch (err) {
      this.destination.error(err);
      return;
    }
  }
}
```

上面這段程式碼就可以讓我們包裹 observer 物件，利用物件的繼承覆寫原本的 next 方法。

最後我們就只要補完 map 方法就可以了

```javascript
class Observable {
  constructor(subscribe) {
    // 一些程式碼...
  }
  subscribe() {
    // 一些程式碼...
  }
  map(callback) {
    const observable = new Observable(); 
    observable.source = this;
    observable.operator = {
      call: (observer, source) => { 
        // 執行這個 operator 的行為
        const newObserver = new MapObserver(observer, callback);
        // 建立包裹後的 observer
        // 訂閱原本的資料源，並回傳
        return source.subscribe(newObserver);
      }
    };    
    return observable; 
  }
}
```

這裡做的事情就簡單很多，我們只要建立包裹過的 observer，並用這個包裹後的 observer 訂閱原本的 source。(記得這個 function 是在 subscribe 時執行的)

[這裡有](https://jsbin.com/paxevam/7/edit?js,console)完整的程式碼，可以讓大家參考。

另外這裡有抽出 lift 方法的實作，其實跟我們現在的版本很接近了，只是把建立新的 observable 封裝到 lift 而已。

## 今日小結

今天這篇文章介紹了要如何簡易的實作 Observable，雖然說是簡易版本但實際上已經非常非常接近 RxJS 官方的實作了，希望讀者花點耐心一步一步跟著程式碼做，做出來後再慢慢吸收。

不知道今天讀者們有沒有收穫呢？ 如果有任何問題，歡迎在下方留言給我，謝謝！