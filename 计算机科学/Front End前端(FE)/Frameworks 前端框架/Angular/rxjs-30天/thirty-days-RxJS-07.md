---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (07)：Observable Operators & Marble Diagrams
date: 2016-12-23T23:14:14.000Z
description: Observable 的 Operators 是實務應用上最重要的部份，我們需要了解各種 Operators 的使用方式，才能輕鬆實作各種需求！
image: null
tags:
  - JavaScript
  - RxJS
  - Operator
  - Observable
  - Marble Diagram
  - RxJS 30 Days
previous: ./thirty-days-RxJS-06.md
next: ./thirty-days-RxJS-08.md
---

這是【30天精通 RxJS】的 07 篇，如果還沒看過 06 篇可以往這邊走：
[30 天精通 RxJS (06)： 建立 Observable(二)](/series/rxjs/thirty-days-RxJS-06)

昨天我們把所有建立 Observable 實例的 operators 講完了，接下來我們要講關於轉換(Transformation)、過濾(Filter)、合併(Combination)等操作方法。先來讓我們看看什麼是 Operator

什麼是 Operator？
------

Operators 就是一個個被附加到 Observable 型別的函式，例如像是 map, filter, contactAll... 等等，所有這些函式都會拿到原本的 observable 並回傳一個新的 observable，就像有點像下面這個樣子

```javascript
var people = Rx.Observable.of('Jerry', 'Anna');

function map(source, callback) {
    return Rx.Observable.create((observer) => {
        return source.subscribe(
            (value) => { 
                try{
                    observer.next(callback(value));
                } catch(e) {
                    observer.error(e);
                }
            },
            (err) => { observer.error(err); },
            () => { observer.complete() }
        )
    })
}

var helloPeople = map(people, (item) => item + ' Hello~');

helloPeople.subscribe(console.log);
// Jerry Hello~
// Anna Hello~
```
[JSBin](https://jsbin.com/roginet/1/edit?js,console,output) | [JSFiddle](https://jsfiddle.net/s6323859/Lruuusf0/)

這裡可以看到我們寫了一個 map 的函式，它接收了兩個參數，第一個是原本的 observable，第二個是 map 的 callback function。map 內部第一件事就是用 `create` 建立一個新的 observable 並回傳，並且在內部訂閱原本的 observable。

當然我們也可以直接把 map 塞到 `Observable.prototype`

```javascript
function map(callback) {
    return Rx.Observable.create((observer) => {
        return this.subscribe(
            (value) => { 
                try{
                    observer.next(callback(value));
                } catch(e) {
                    observer.error(e);
                }
            },
            (err) => { observer.error(err); },
            () => { observer.complete() }
        )
    })
}
Rx.Observable.prototype.map = map;
var people = Rx.Observable.of('Jerry', 'Anna');
var helloPeople = people.map((item) => item + ' Hello~');

helloPeople.subscribe(console.log);
// Jerry Hello~
// Anna Hello~
```

這裡有兩個重點是我們一定要知道的，每個 operator 都會回傳一個新的 observable，而我們可以透過 `create` 的方法建立各種 operator。

> 在 RxJS 5 的實作中，其實每個 operator 是透過原來 observable 的 lift 方法來建立新的 observable，這個方法會在新回傳的 observable 物件內偷塞兩個屬性，分別是 source 與 operator，記錄原本的資料源跟當前使用的 operator。

> 其實 lift 方法還是用 new Observable(跟 create 一樣)。至於為什麼要獨立出這個方法，除了更好的封裝以外，主要的原因是為了讓 RxJS 5 的使用者能更好的 debug。關於 RxJS 5 的除錯方式，我們會專門寫一篇來講解！

> 這裡我們只是簡單的實作 operator。如果之後實務上，想要不影響原本的 Observable 又能夠自訂 operator 可以參考官方的這份[文件](https://github.com/ReactiveX/rxjs/blob/master/doc/operator-creation.md)。(現在先不用看)

其實 RxJS 提供的各種 operators 已經非常夠用了，不太需要我們自己創造 operator，這裡只是想讓大家先對 operator 的建立有個基本的觀念，之後在學習的過程中會比較輕鬆。

在我們開始介紹 RxJS 的 operators 前，為了能讓我們更好地理解各種 operators，我們需要先訂定一個簡單的方式來表達 observable！

Marble diagrams
------

我們在傳達事物時，文字其實是最糟的手段，雖然文字是我們平時溝通的基礎，但常常千言萬語也比不過一張清楚的圖。如果我們能訂定 observable 的圖示，就能讓我們更方便的溝通及理解 observable 的各種 operators！

我們把描繪 observable 的圖示稱為 Marble diagrams，在網路上 RxJS 有非常多的 Marble diagrams，規則大致上都是相同的，這裡為了方便撰寫以及跟讀者的留言互動，所以採用類似 ASCII 的繪畫方式。

我們用 `-` 來表達一小段時間，這些 `-` 串起就代表一個 observable。

```bash
----------------
```

`X` (大寫 X)則代表有錯誤發生

```bash
---------------X
```

`|` 則代表 observable 結束

```bash
----------------|
```

在這個時間序當中，我們可能會發送出值(value)，如果值是數字則直接用阿拉伯數字取代，其他的資料型別則用相近的英文符號代表，這裡我們用 `interval` 舉例

```javascript
var source = Rx.Observable.interval(1000);
```

`source` 的圖形就會長像這樣

```bash
-----0-----1-----2-----3--...
```

當 observable 是同步送值的時候，例如

```javascript
var source = Rx.Observable.of(1,2,3,4);
```

`source` 的圖形就會長像這樣

```bash
(1234)|
```

小括號代表著同步發生。

另外的 Marble diagrams 也能夠表達 operator 的前後轉換，例如

```javascript
var source = Rx.Observable.interval(1000);
var newest = source.map(x => x + 1); 
```

這時 Marble diagrams 就會長像這樣

```bash
source: -----0-----1-----2-----3--...
            map(x => x + 1)
newest: -----1-----2-----3-----4--...
```

最上面是原本的 observable，中間是 operator，下面則是新的 observable。

以上就是 Marble diagrams 如何表示 operator 對 observable 的操作，這能讓我們更好的理解各個 operator。

> Marble Diagrams 相關資源：http://rxmarbles.com/

最後讓我們來看幾個簡單的 Operators！

Operators
------

### map

Observable 的 map 方法使用上跟陣列的 map 是一樣的，我們傳入一個 callback function，這個 callback function 會帶入每次發送出來的元素，然後我們回傳新的元素，如下

```javascript
var source = Rx.Observable.interval(1000);
var newest = source.map(x => x + 2); 

newest.subscribe(console.log);
// 2
// 3
// 4
// 5..
```

用 Marble diagrams 表達就是

```bash
source: -----0-----1-----2-----3--...
            map(x => x + 1)
newest: -----1-----2-----3-----4--...
```

我們有另外一個方法跟 map 很像，叫 mapTo

### mapTo

mapTo 可以把傳進來的值改成一個固定的值，如下

```javascript
var source = Rx.Observable.interval(1000);
var newest = source.mapTo(2); 

newest.subscribe(console.log);
// 2
// 2
// 2
// 2..
```

mapTo 用 Marble diagrams 表達

```bash
source: -----0-----1-----2-----3--...
                mapTo(2)
newest: -----2-----2-----2-----2--...
```


### filter 

filter 在使用上也跟陣列的相同，我們要傳入一個 callback function，這個 function 會傳入每個被送出的元素，並且回傳一個 boolean 值，如果為 true 的話就會保留，如果為 false 就會被濾掉，如下

```javascript
var source = Rx.Observable.interval(1000);
var newest = source.filter(x => x % 2 === 0); 

newest.subscribe(console.log);
// 0
// 2
// 4
// 6..
```

filter 用 Marble diagrams 表達

```bash
source: -----0-----1-----2-----3-----4-...
            filter(x => x % 2 === 0)
newest: -----0-----------2-----------4-...
```

> 讀者應該有發現 map, filter 這些方法其實都跟陣列的相同，因為這些都是 functional programming 的通用函式，就算換個語言也有機會看到相同的命名及相同的用法。

> 實際上 Observable 跟 Array 的 operators(map, filter)，在行為上還是有極大的差異。當我們的資料量很大時，Observable 的效能會好上非常多。我們會有一天專門講這個部份！

今日小結
------

今天我們講了 Observable Operators 的相關知識，有以下幾個重點

- 什麼是 Operators
    - 如何建立 operator
- Marble diagrams
- Operators
    - map
    - mapTo
    - filter

不知道今天讀者有沒有收穫呢？歡迎在下方留言給我，這是精通 RxJS 的第 07 篇！