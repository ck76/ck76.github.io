---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (05)： 建立 Observable(一)
date: 2016-12-21T21:28:25.000Z
description: Observable 是 RxJS 的核心，今天讓我們從如何建立 Observable 開始！
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Observer
  - RxJS 30 Days
previous: ./thirty-days-RxJS-04.md
next: ./thirty-days-RxJS-06.md
---

這是【30天精通 RxJS】的 05 篇，如果還沒看過 04 篇可以往這邊走：
[30 天精通 RxJS (04)： 什麼是 Observable ?](/series/rxjs/thirty-days-RxJS-04)

[![Yes](https://img.youtube.com/vi/tTMMLNNJySc/0.jpg)](https://www.youtube.com/watch?v=tTMMLNNJySc)
不想看文章的人，可以直接看影片喔！

> 今天大家看文章一定要分清楚 **Observable** 跟 **Observer**，不要搞混。

前幾天我們把所有重要的觀念及前置的知識都講完了，今天要正式進入 RxJS 的應用，整個 RxJS 說白了就是**一個核心三個重點**。

一個核心是 Observable 再加上相關的 Operators(map, filter...)，這個部份是最重要的，其他三個重點本質上也是圍繞著這個核心在轉，所以我們會花將近 20 天的篇數講這個部份的觀念及使用案例。

另外三個重點分別是 

- Observer
- Subject
- Schedulers

Observer 是這三個當中一定會用到卻是最簡單的，所以我們今天就會把它介紹完。Subject 一般應用到的頻率就相對低很多，但如果想要看懂 RxJS 相關的 Library 或 Framework，Subject 就是一定要會的重點，所以這個部份我們大概會花 3-5 天的時間講解。至於 Schedulers 則是要解決 RxJS 衍伸出的最後一道問題，這個部份會視情況加入或是在 30 天後補完。

![redux-observable logo](https://redux-observable.js.org/logo/logo-small.gif)
> [redux-observable](https://github.com/redux-observable/redux-observable) 就是用了 Subject 實作的

> 讓我賣個關子，先不說 RxJS 最後一道問題是什麼。

說了這麼多，我們趕快進入到今天的主題 Observable 吧！

建立 Observable: `create`
------

建立 Observable 的方法有非常多種，其中 `create` 是最基本的方法。`create` 方法在 `Rx.Observable` 物件中，要傳入一個 callback function ，這個 callback function 會接收一個 observer 參數，如下

```javascript
var observable = Rx.Observable
	.create(function(observer) {
		observer.next('Jerry'); // RxJS 4.x 以前的版本用 onNext
		observer.next('Anna');
	})
```

這個 callback function 會定義 observable 將會如何發送值。

> 雖然 Observable 可以被 `create`，但實務上我們通常都使用 **creation operator** 像是 from, of, fromEvent, fromPromise 等。這裡只是為了從基本的開始講解所以才用 `create`

我們可以訂閱這個 observable，來接收他送出的值，程式碼如下

```javascript
var observable = Rx.Observable
	.create(function(observer) {
		observer.next('Jerry'); // RxJS 4.x 以前的版本用 onNext
		observer.next('Anna');
	})
	
// 訂閱這個 observable	
observable.subscribe(function(value) {
	console.log(value);
})
```
[JSBin](https://jsbin.com/vetoti/1/edit?js,console,output) | [JSFiddle](https://jsfiddle.net/s6323859/yL8n4v53/)

當我們訂閱這個 observable，他就會依序送出 `'Jerry'` `'Anna'` 兩個字串。

> 訂閱 Observable 跟 addEventListener 在實作上其實有非常大的不同。雖然在行為上很像，但實際上 Observable 根本沒有管理一個訂閱的清單，這個部份的細節我們留到最後說明！

這裡有一個重點，很多人認為 RxJS 是在做非同步處理，所以所有行為都是非同步的。但其實這個觀念是錯的，RxJS 確實主要在處理非同步行為沒錯，但也同時能處理同步行為，像是上面的程式碼就是同步執行的。

證明如下

```javascript
var observable = Rx.Observable
	.create(function(observer) {
		observer.next('Jerry'); // RxJS 4.x 以前的版本用 onNext
		observer.next('Anna');
	})
	
console.log('start');
observable.subscribe(function(value) {
	console.log(value);
});
console.log('end');
```
[JSBin](https://jsbin.com/vetoti/2/edit?js,console,output) | [JSFiddle](https://jsfiddle.net/s6323859/yL8n4v53/1/)


上面這段程式碼會印出

```javascript
start
Jerry
Anna
end
```

而不是 

```javascript
start
end
Jerry
Anna
```

所以很明顯的這段程式碼是同步執行的，當然我們可以拿它來處理非同步的行為！

```javascript
var observable = Rx.Observable
	.create(function(observer) {
		observer.next('Jerry'); // RxJS 4.x 以前的版本用 onNext
		observer.next('Anna');
		
		setTimeout(() => {
			observer.next('RxJS 30 Days!');
		}, 30)
	})
	
console.log('start');
observable.subscribe(function(value) {
	console.log(value);
});
console.log('end');
```
[JSBin](https://jsbin.com/vetoti/4/edit?js,console,output) | [JSFiddle](https://jsfiddle.net/s6323859/yL8n4v53/2/)

這時就會印出

```javascript
start
Jerry
Anna
end
RxJS 30 Days!
```

從上述的程式碼能看得出來

**Observable 同時可以處理同步與非同步的行為！**

觀察者 Observer
------

Observable 可以被訂閱(subscribe)，或說可以被觀察，而訂閱 Observable 的物件又稱為 **觀察者(Observer)**。觀察者是一個具有三個方法(method)的物件，每當 Observable 發生事件時，便會呼叫觀察者相對應的方法。

> 注意這裡的觀察者(Observer)跟上一篇講的觀察者模式(Observer Pattern)無關，觀察者模式是一種設計模式，是思考問題的解決過程，而這裡講的觀察者是一個被定義的物件。

觀察者的三個方法(method)：

- next：每當 Observable 發送出新的值，next 方法就會被呼叫。

- complete：在 Observable 沒有其他的資料可以取得時，complete 方法就會被呼叫，在 complete 被呼叫之後，next 方法就不會再起作用。

- error：每當 Observable 內發生錯誤時，error 方法就會被呼叫。

說了這麼多，我們還是直接來建立一個觀察者吧！

```javascript
var observable = Rx.Observable
	.create(function(observer) {
			observer.next('Jerry');
			observer.next('Anna');
			observer.complete();
			observer.next('not work');
	})
	
// 宣告一個觀察者，具備 next, error, complete 三個方法
var observer = {
	next: function(value) {
		console.log(value);
	},
	error: function(error) {
		console.log(error)
	},
	complete: function() {
		console.log('complete')
	}
}

// 用我們定義好的觀察者，來訂閱這個 observable	
observable.subscribe(observer)
```
[JSBin](https://jsbin.com/vetoti/3/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/yL8n4v53/3/)

上面這段程式碼會印出

```javascript
Jerry
Anna
complete
```

上面的範例可以看得出來在 complete 執行後，next 就會自動失效，所以沒有印出 `not work`。

下面則是送出錯誤的範例

```javascript
var observable = Rx.Observable
  .create(function(observer) {
    try {
      observer.next('Jerry');
      observer.next('Anna');
      throw 'some exception';
    } catch(e) {
      observer.error(e)
    }
  });
	
// 宣告一個觀察者，具備 next, error, complete 三個方法
var observer = {
	next: function(value) {
		console.log(value);
	},
	error: function(error) {
		console.log('Error: ', error)
	},
	complete: function() {
		console.log('complete')
	}
}

// 用我們定義好的觀察者，來訂閱這個 observable	
observable.subscribe(observer)
```
[JSBin](https://jsbin.com/poyefom/1/edit?js,console) | [JSFiddle](https://jsfiddle.net/s6323859/kf6dphqp/)

這裡就會執行 error 的 function 印出 `Error: some exception`。

另外觀察者可以是不完整的，他可以只具有一個 next 方法，如下

```javascript
var observer = {
	next: function(value) {
		//...
	}
}
```

> 有時候 Observable 會是一個無限的序列，例如 click 事件，這時 `complete` 方法就有可能永遠不會被呼叫！

我們也可以直接把 next, error, complete 三個 function 依序傳入 `observable.subscribe`，如下：

```javascript
observable.subscribe(
    value => { console.log(value); },
    error => { console.log('Error: ', error); },
    () => { console.log('complete') }
)
```

`observable.subscribe` 會在內部自動組成 observer 物件來操作。

實作細節
------

我們前面提到了，其實 Observable 的訂閱跟 addEventListener 在實作上有蠻大的差異，雖然他們的行為很像！

addEventListener 本質上就是 Observer Pattern 的實作，在內部會有一份訂閱清單，像是我們昨天實作的 Producer

```javascript
class Producer {
	constructor() {
		this.listeners = [];
	}
	addListener(listener) {
		if(typeof listener === 'function') {
			this.listeners.push(listener)
		} else {
			throw new Error('listener 必須是 function')
		}
	}
	removeListener(listener) {
		this.listeners.splice(this.listeners.indexOf(listener), 1)
	}
	notify(message) {
		this.listeners.forEach(listener => {
			listener(message);
		})
	}
}
```

我們在內部儲存了一份所有的監聽者清單(`this.listeners`)，在要發佈通知時會對逐一的呼叫這份清單的監聽者。

但在 Observable 不是這樣實作的，在其內部並沒有一份訂閱者的清單。訂閱 Observable 的行為比較像是執行一個物件的方法，並把資料傳進這個方法中。 

我們以下面的程式碼做說明

```javascript
var observable = Rx.Observable
	.create(function (observer) {
			observer.next('Jerry');
			observer.next('Anna');
	})
	
observable.subscribe({
	next: function(value) {
		console.log(value);
	},
	error: function(error) {
		console.log(error)
	},
	complete: function() {
		console.log('complete')
	}
})
```

像上面這段程式，他的行為比較像這樣

```javascript

function subscribe(observer) {
		observer.next('Jerry');
		observer.next('Anna');
}

subscribe({
	next: function(value) {
		console.log(value);
	},
	error: function(error) {
		console.log(error)
	},
	complete: function() {
		console.log('complete')
	}
});
```

這裡可以看到 subscribe 是一個 function，這個 function 執行時會傳入觀察者，而我們在這個 function 內部去執行觀察者的方法。

**訂閱一個 Observable 就像是執行一個 function**

今日小結
------

今天在講關於建立 Observable 的實例，用到了 `create` 的方法，但大部分的內容還是在講 Observable 幾個重要的觀念，如下

- Observable 可以同時處理**同步**跟**非同步**行為
- Observer 是一個物件，這個物件具有三個方法，分別是 **next**, **error**, **complete**
- 訂閱一個 Observable 就像在執行一個 function

不知道讀者是否有所收穫，如果有任何問題或建議，歡迎在下方留言給我，謝謝。


