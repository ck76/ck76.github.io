---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS(21)：深入 Observable
date: 2017-01-06T21:07:32.000Z
description: 我們已經把絕大部分的 operators 都介紹完了，但一直沒有機會好好的解釋 Observable 的 operators 運作方式。
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Operator
  - RxJS 30 Days
previous: ./thirty-days-RxJS-20.md
next: ./thirty-days-RxJS-22.md
---

在系列文章的一開頭是以陣列(Array)的 operators(map, filter, concatAll) 作為切入點，讓讀者們在學習 observable 時會更容易接受跟理解，但實際上 observable 的 operators 跟陣列的有很大的不同，主要差異有兩點

1. 延遲運算
2. 漸進式取值

## 延遲運算

延遲運算很好理解，所有 Observable 一定會等到訂閱後才開始對元素做運算，如果沒有訂閱就不會有運算的行為

```javascript
var source = Rx.Observable.from([1,2,3,4,5]);
var example = source.map(x => x + 1);
```

上面這段程式碼因為 Observable 還沒有被訂閱，所以不會真的對元素做運算，這跟陣列的操作不一樣，如下

```javascript
var source = [1,2,3,4,5];
var example = source.map(x => x + 1); 
```

上面這段程式碼執行完，example 就已經取得所有元素的返回值了。

延遲運算是 Observable 跟陣列最明顯的不同，延遲運算所帶來的優勢在之前的文章也已經提過這裡就不再贅述，因為我們還有一個更重要的差異要講，那就是**漸進式取值**

## 漸進式取值

陣列的 operators 都必須完整的運算出每個元素的返回值並組成一個陣列，再做下一個 operator 的運算，我們看下面這段程式碼

```javascript
var source = [1,2,3];
var example = source
              .filter(x => x % 2 === 0) // 這裡會運算並返回一個完整的陣列
              .map(x => x + 1) // 這裡也會運算並返回一個完整的陣列
```

上面這段程式碼，相信讀者們都很熟悉了，大家應該都有注意到 `source.filter(...)` 就會返回一整個新陣列，再接下一個 operator 又會再返回一個新的陣列，這一點其實在我們實作 map 跟 filter 時就能觀察到

```javascript
Array.prototype.map = function(callback) {
    var result = []; // 建立新陣列
    this.forEach(function(item, index, array) {
        result.push(callback(item, index, array))
    });
    return result; // 返回新陣列
}
```

每一次的 operator 的運算都會建立一個新的陣列，並在每個元素都運算完後返回這個新陣列，我們可以用下面這張動態圖表示運算過程

![](https://media.giphy.com/media/l0HlPZeB9OvFu7QwE/giphy.gif)

Observable operator 的運算方式跟陣列的是完全的不同，雖然 Observable 的 operator 也都會回傳一個新的 observable，但因為元素是漸進式取得的關係，所以每次的運算是一個元素運算到底，而不是運算完全部的元素再返回。

```javascript
var source = Rx.Observable.from([1,2,3]);
var example = source
              .filter(x => x % 2 === 0)
              .map(x => x + 1)

example.subscribe(console.log);
```

上面這段程式碼運行的方式是這樣的

1. 送出 `1` 到 filter 被過濾掉
2. 送出 `2` 到 filter 在被送到 map 轉成 `3`，送到 observer `console.log` 印出
3. 送出 `3` 到 filter 被過濾掉

每個元素送出後就是運算到底，在這個過程中不會等待其他的元素運算。這就是漸進式取值的特性，不知道讀者們還記不記得我們在講 Iterator 跟 Observer 時，就特別強調這兩個 Pattern 的共同特性是漸進式取值，而我們在實作 Iterator 的過程中其實就能看出這個特性的運作方式

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

var myIterator = new IteratorFromArray([1,2,3]);
var newIterator = myIterator.map(x => x + 1);
newIterator.next(); // { done: false, value: 2 }
```

雖然上面這段程式碼是一個非常簡單的示範，但可以看得出來每一次 map 雖然都會返回一個新的 oterator，但實際上在做元素運算時，因為漸進式的特性會使一個元素運算到底，Observable 也是相同的概念，我們可以用下面這張動態圖表示運算過程

![](https://media.giphy.com/media/3o6ZtqrBfUyHvMDQ2c/giphy.gif)

漸進式取值的觀念在 Observable 中其實非常的重要，這個特性也使得 Observable 相較於 Array 的 operator 在做運算時來的高效很多，尤其是在處理大量資料的時候會非常明顯！

## 今日小結

今天我們講解了 Observable 跟陣列各自 operators 運作上的差異，這些細微的差異實際上對程式的運行效率有著很大的影響。

從我們一開始從陣列作為 obsevable 的切入點，中間介紹了各種常用的 operator，到今天我們釐清了陣列跟 Observable 運作上的差異，在 Observable 這塊我們幾乎已經完成了，剩下的是一些衍生出來的東西，像是 multicast, publish... 等，這些我們會在介紹完 Subject 後在做說明！