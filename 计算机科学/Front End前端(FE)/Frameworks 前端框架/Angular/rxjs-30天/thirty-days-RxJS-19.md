---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (19)：實務範例 - 簡易 Auto Complete 實作
date: 2017-01-04T23:51:58.000Z
description: "今天我們要做一個 RxJS 的經典範例 - 自動完成 (Auto Complete)，自動完成在實務上的應用非常廣泛，幾乎隨處可見這樣的功能，只要是跟表單、搜尋相關的都會看到。
雖然是個很常見的功能，但多數的工程師都只是直接套套件來完成，很少有人會自己從頭到尾把完整的邏輯寫一次。"
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Operator
  - RxJS 30 Days
previous: ./thirty-days-RxJS-18.md
next: ./thirty-days-RxJS-20.md
---

如果有自己實作過 Auto Complete 功能的工程師，應該就會知道這個功能在實作的過程中很多細節會讓程式碼變的非常複雜，像是要如何取消上一次發送出去的 request、要如何優化請求次數... 等等，這些小細節都會讓程式碼變的非常複雜且很難維護。

就讓我們一起來用 RxJS 來實作這個功能吧！

需求分析
------

首先我們會有一個搜尋框(input#search)，當我們在上面打字並停頓超過 100 毫秒就發送 HTTP Request 來取得建議選項並顯示在收尋框下方(ul#suggest-list)，如果使用者在前一次發送的請求還沒有回來就打了下一個字，此時前一個發送的請求就要捨棄掉，當建議選項顯示之後可以用滑鼠點擊取建議選項代搜尋框的文字。

![](https://res.cloudinary.com/dohtkyi84/image/upload/v1483543558/30days/autocomplete.png)

上面的敘述可以拆分成以下幾個步驟

- 準備 input#search 以及 ul#suggest-list 的 HTML 與 CSS
- 在 input#search 輸入文字時，等待 100 毫秒再無輸入，就發送 HTTP Request 
- 當 Response 還沒回來時，使用者又輸入了下一個文字就捨棄前一次的並再發送一次新的 Request
- 接受到 Response 之後顯示建議選項
- 滑鼠點擊後取代 input#search 的文字


基本的 HTML 跟 CSS 筆者已經幫大家完成，大家可以直接到下面的連結接著實作：

- [JSBin](https://jsbin.com/yaxupi/3/edit?js,output)

先讓我們看一下 HTML，首先在 HTML 裡有一個 input(#search)，這個 input(#search) 就是要用來輸入的欄位，它下方有一個 ul(#suggest-list)，則是放建議選項的地方

CSS 的部分可以不用看，JS 的部分已經寫好了要發送 API 的 url 跟方法`getSuggestList`，接著就開始實作自動完成的效果吧！



### 第一步，取得需要的 DOM 物件

這裡我們會用到 #search 以及 #suggest-list 這兩個 DOM

```javascript
const searchInput = document.getElementById('search');
const suggestList = document.getElementById('suggest-list');
```

### 第二步，建立所需的 Observable

這裡我們要監聽 收尋欄位的 input 事件，以及建議選項的點擊事件

```javascript
const keyword = Rx.Observable.fromEvent(searchInput, 'input');
const selectItem = Rx.Observable.fromEvent(suggestList, 'click');
```

### 第三步，撰寫程式邏輯

每當使用者輸入文字就要發送 HTTP request，並且有新的值被輸入後就捨棄前一次發送的，所以這裡用 switchMap

```javascript
keyword.switchMap(e => getSuggestList(e.target.value))
```

這裡我們先試著訂閱，看一下 API 會回傳什麼樣的資料

```javascript
keyword
    .switchMap(e => getSuggestList(e.target.value))
    .subscribe(console.log)
```

在 search 欄位亂打幾個字

![](https://res.cloudinary.com/dohtkyi84/image/upload/v1483545742/30days/wikires.png)

大家可以在 console 看到資料長相這樣，他會回傳一個陣列帶有四個元素，其中第一個元素是我們輸入的值，第二個元素才是我們要的建議選項清單。

所以我們要取的是 response 陣列的第二的元素，用 switchMap 的第二個參數來選取我們要的

```javascript
keyword
    .switchMap(
        e => getSuggestList(e.target.value),
        (e, res) => res[1]
    )
    .subscribe(console.log)
```

這時再輸入文字就可以看到確實是我們要的返回值

![](https://res.cloudinary.com/dohtkyi84/image/upload/v1483546009/30days/wikirealres.png)

寫一個 render 方法，把陣列轉成 li 並寫入 suggestList

```javascript
const render = (suggestArr = []) => {
    suggestList.innerHTML = suggestArr
                            .map(item => '<li>'+ item +'</li>')
                            .join('');  
}
```

這時我們就可用 render 方法把取得的陣列傳入

```javascript
const render = (suggestArr = []) => {
    suggestList.innerHTML = suggestArr
                            .map(item => '<li>'+ item +'</li>')
                            .join('');  
}

keyword
  .switchMap(
    e => getSuggestList(e.target.value),
    (e, res) => res[1]
  )
  .subscribe(list => render(list))
```

如此一來我們打字就能看到結果出現在 input 下方了

![](https://res.cloudinary.com/dohtkyi84/image/upload/v1483543558/30days/autocomplete.png)

只是目前還不能點選，先讓我們來做點選的功能，這裡點選的功能我們需要用到 delegation event 的小技巧，利用 ul 的 click 事件，來塞選是否點到了 li，如下

```javascript
selectItem
  .filter(e => e.target.matches('li'))
```

上面我們利用 DOM 物件的 matches 方法(裡面的字串放 css 的 selector)來過濾出有點擊到 li 的事件，再用 map 轉出我們要的值並寫入 input。

```javascript
selectItem
  .filter(e => e.target.matches('li'))
  .map(e => e.target.innerText)
  .subscribe(text => searchInput.value = text)
```

現在我們就能點擊建議清單了，但是點擊後清單沒有消失，這裡我們要在點擊後重新 redner，所以把上面的程式碼改一下

```javascript
selectItem
  .filter(e => e.target.matches('li'))
  .map(e => e.target.innerText)
  .subscribe(text => { 
      searchInput.value = text;
      render();
  })
```

這樣一來我們就完成最基本的功能了，大家可以到[這裡](https://jsbin.com/yaxupi/6/edit?js,output)看初步的完成品。

還記得我們前面說每次打完字要等待 100 毫秒在發送 request 嗎？ 這樣能避免過多的 request 發送，可以降低 server 的負載也會有比較好的使用者體驗，要做到這件事很簡單只要加上 `debounceTime(100)` 就完成了

```javascript
keyword
  .debounceTime(100)
  .switchMap(
    e => getSuggestList(e.target.value),
    (e, res) => res[1]
  )
  .subscribe(list => render(list))
```

當然這個數值可以依照需求或是請 UX 針對這個細節作調整。

這樣我們就完成所有功能了，大家可以到[這裡](https://jsbin.com/yaxupi/7/edit?js,output)查看結果。

今日小結
------

我們用了不到 30 行的程式碼就完成了 auto complete 的基本功能，當我們能夠自己從頭到尾的完成這樣的功能，在面對各種不同的需求，我們就能很方便的針對需求作調整，而不會受到套件的牽制！比如說我們希望使用者打了 2 個字以上在發送 request，這時我們只要加上一行 filter 就可以了

```javascript
keyword
  .filter(e => e.target.value.length > 2)
  .debounceTime(100)
  .switchMap(
    e => getSuggestList(e.target.value),
    (e, res) => res[1]
  )
  .subscribe(list => render(list))
```

又或者網站的使用量很大，可能 API 在量大的時候會回傳失敗，主管希望可以在 API 失敗的時候重新嘗試 3 次，我們只要加個 `retry(3)` 就完成了

```javascript
keyword
  .filter(e => e.target.value.length > 2)
  .debounceTime(100)
  .switchMap(
    e => Rx.Observable.from(getSuggestList(e.target.value))
                      .retry(3),
    (e, res) => res[1]
  )
  .subscribe(list => render(list))
```

大家會發現我們的靈活度變的非常高，又同時兼顧了程式碼的可讀性，短短的幾行程式碼就完成了一個複雜的需求，這就是 RxJS 的魅力啊～