---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (04)：什麼是 Observable ?
date: 2016-12-20T22:13:19.000Z
description: 整個 RxJS 的基礎就是 Observable，只要弄懂 Observable 就算是學會一半的 RxJS 了，剩下的就只是一些方法的練習跟熟悉；但到底什麼是 Observable 呢？
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Iterator
  - Observer
  - RxJS 30 Days
previous: ./thirty-days-RxJS-03.md
next: ./thirty-days-RxJS-05.md
---

這是【30天精通 RxJS】的 04 篇，如果還沒看過 03 篇可以往這邊走：
[30 天精通 RxJS (03)： Functional Programming 通用函式](/series/rxjs/thirty-days-RxJS-03)

要理解 Observable 之前，我們必須先談談兩個設計模式(Design Pattern)， Iterator Pattern 跟 Observer Pattern。今天這篇文章會帶大家快速的了解這兩個設計模式，並解釋這兩個 Pattern 跟 Observable 之間的關係！

Observer Pattern
------

Observer Pattern 其實很常遇到，在許多 API 的設計上都用了 Observer Pattern 實作，最簡單的例子就是 DOM 物件的事件監聽，程式碼如下

```javascript
function clickHandler(event) {
	console.log('user click!');
}

document.body.addEventListener('click', clickHandler)
```

在上面的程式碼，我們先宣告了一個 `clickHandler` 函式，再用 DOM 物件 (範例是 body) 的 `addEventListener` 來監聽**點擊**(click)事件，每次使用者在 body 點擊滑鼠就會執行一次 `clickHandler`，並把相關的資訊(event)帶進來！這就是觀察者模式，我們可以對某件事註冊監聽，並在事件發生時，自動執行我們註冊的監聽者(listener)。

Observer 的觀念其實就這麼的簡單，但筆者希望能透過程式碼帶大家了解，如何實作這樣的 Pattern！

首先我們需要一個建構式，這個建構式 new 出來的實例可以被監聽。

> 這裡我們先用 ES5 的寫法，會再附上 ES6 的寫法

```javascript
function Producer() {
	
	// 這個 if 只是避免使用者不小心把 Producer 當作函式來調用
	if(!(this instanceof Producer)) {
	  throw new Error('請用 new Producer()!');
	  // 仿 ES6 行為可用： throw new Error('Class constructor Producer cannot be invoked without 'new'')
	}
	
	this.listeners = [];
}

// 加入監聽的方法
Producer.prototype.addListener = function(listener) {
	if(typeof listener === 'function') {
		this.listeners.push(listener)
	} else {
		throw new Error('listener 必須是 function')
	}
}

// 移除監聽的方法
Producer.prototype.removeListener = function(listener) {
	this.listeners.splice(this.listeners.indexOf(listener), 1)
}

// 發送通知的方法
Producer.prototype.notify = function(message) {
	this.listeners.forEach(listener => {
		listener(message);
	})
}
```

> 這裡用到了 this, prototype 等觀念，大家不了解可以去看我的一支[影片](https://youtu.be/BlT6pCG2M1I)專門講解這幾個觀念！

附上 ES6 版本的程式碼，跟上面程式碼的行為基本上是一樣的

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

有了上面的程式碼後，我們就可以來建立物件實例了

```javascript
var egghead = new Producer(); 
// new 出一個 Producer 實例叫 egghead

function listener1(message) {
	console.log(message + 'from listener1');
}

function listener2(message) {
	console.log(message + 'from listener2');
}

egghead.addListener(listener1); // 註冊監聽
egghead.addListener(listener2);

egghead.notify('A new course!!') // 當某件事情方法時，執行
```

當我們執行到這裡時，會印出：

```bash
a new course!! from listener1
a new course!! from listener2
```

每當 `egghead.notify` 執行一次，`listener1` 跟 `listener2` 就會被通知，而這些 listener 可以額外被添加，也可以被移除！

雖然我們的實作很簡單，但它很好的說明了 Observer Pattern 如何在**事件**(event)跟**監聽者**(listener)的互動中做到去耦合(decoupling)。

Iterator Pattern
------

Iterator 是一個物件，它的就像是一個指針(pointer)，指向一個資料結構並產生一個序列(sequence)，這個序列會有資料結構中的所有元素(element)。

先讓我們來看看原生的 JS 要怎麼建立 iterator

```javascript
var arr = [1, 2, 3];

var iterator = arr[Symbol.iterator]();

iterator.next();
// { value: 1, done: false }
iterator.next();
// { value: 2, done: false }
iterator.next();
// { value: 3, done: false }
iterator.next();
// { value: undefined, done: true }
```

> JavaScript 到了 ES6 才有原生的 Iterator

> 在 ECMAScript 中 Iterator 最早其實是要採用類似 Python 的 Iterator 規範，就是 Iterator 在沒有元素之後，執行 `next` 會直接拋出錯誤；但後來經過一段時間討論後，決定採更 functional 的做法，改成在取得最後一個元素之後執行 `next` 永遠都回傳 `{ done: true, value: undefined }`

JavaScript 的 Iterator 只有一個 next 方法，這個 next 方法只會回傳這兩種結果：

1. 在最後一個元素前： `{ done: false, value: elem }`
2. 在最後一個元素之後： `{ done: true, value: undefined }`

當然我們可以自己實作簡單的 Iterator Pattern

```javascript
function IteratorFromArray(arr) {
	if(!(this instanceof IteratorFromArray)) {
		throw new Error('請用 new IteratorFromArray()!');
	}
	this._array = arr;
	this._cursor = 0;	
}

IteratorFromArray.prototype.next = function() {
	return this._cursor < this._array.length ?
		{ value: this._array[this._cursor++], done: false } :
		{ done: true };
}
```

附上 ES6 版本的程式碼，行為同上

```javascript
class IteratorFromArray {
	constructor(arr) {
		this._array = arr;
		this._cursor = 0;
	}
  
	next() {
		return this._cursor < this._array.length ?
		{ value: this._array[this._cursor++], done: false } :
		{ done: true };
	}
}
```

Iterator Pattern 雖然很單純，但同時帶來了兩個優勢，第一它漸進式取得資料的特性可以拿來做延遲運算(Lazy evaluation)，讓我們能用它來處理大資料結構。第二因為 iterator 本身是序列，所以可以實作所有陣列的運算方法像 map, filter... 等！

這裡我們利用最後一段程式碼實作 map 試試

```javascript
class IteratorFromArray {
	constructor(arr) {
		this._array = arr;
		this._cursor = 0;
	}
  
	next() {
		return this._cursor < this._array.length ?
		{ value: this._array[this._cursor++], done: false } :
		{ done: true };
	}
	
	map(callback) {
		const iterator = new IteratorFromArray(this._array);
		return {
			next: () => {
				const { done, value } = iterator.next();
				return {
					done: done,
					value: done ? undefined : callback(value)
				}
			}
		}
	}
}

var iterator = new IteratorFromArray([1,2,3]);
var newIterator = iterator.map(value => value + 3);

newIterator.next();
// { value: 4, done: false }
newIterator.next();
// { value: 5, done: false }
newIterator.next();
// { value: 6, done: false }
```

### 補充: 延遲運算(Lazy evaluation)

延遲運算，或說 call-by-need，是一種運算策略(evaluation strategy)，簡單來說我們延遲一個表達式的運算時機直到真正需要它的值在做運算。

以下我們用 generator 實作 iterator 來舉一個例子

```javascript
	function* getNumbers(words) {
		for (let word of words) {
			if (/^[0-9]+$/.test(word)) {
			    yield parseInt(word, 10);
			}
		}
	}
	
	const iterator = getNumbers('30 天精通 RxJS (04)');
	
	iterator.next();
	// { value: 3, done: false }
	iterator.next();
	// { value: 0, done: false }
	iterator.next();
	// { value: 0, done: false }
	iterator.next();
	// { value: 4, done: false }
	iterator.next();
	// { value: undefined, done: true }
```

這裡我們寫了一個函式用來抓取字串中的數字，在這個函式中我們用 for...of 的方式來取得每個字元並用正則表示式來判斷是不是數值，如果為真就轉成數值並回傳。當我們把一個字串丟進 `getNumbers` 函式時，並沒有馬上運算出字串中的所有數字，必須等到我們執行 `next()` 時，才會真的做運算，這就是所謂的延遲運算(evaluation strategy)

Observable
------

在了解 Observer 跟 Iterator 後，不知道大家有沒有發現其實 Observer 跟 Iterator 有個共通的特性，就是他們都是 **漸進式**(progressive) 的取得資料，差別只在於 Observer 是生產者(Producer)推送資料(push)，而 Iterator 是消費者(Consumer)要求資料(pull)!

![push & pull](https://res.cloudinary.com/dohtkyi84/image/upload/v1482240798/push_pull.png)

Observable 其實就是這兩個 Pattern 思想的結合，Observable 具備**生產者推送資料**的特性，同時能像序列，擁有**序列處理資料的方法**(map, filter...)！

更簡單的來說，**Observable 就像是一個序列，裡面的元素會隨著時間推送**。

> 注意這裡講的是 **思想的結合**，Observable 跟 Observer 在實作上還是有差異，這我們在下一篇文章中講到。

今日小結
------

今天講了 Iterator 跟 Observer 兩個 Pattern，這兩個 Pattern 都是漸進式的取得元素，差異在於 Observer 是靠生產者推送資料，Iterator 則是消費者去要求資料，而 Observable 就是這兩個思想的結合！

今天的觀念需要比較多的思考，希望讀者能多花點耐心想一想，如果有任何問題請在下方留言給我。
