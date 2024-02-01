---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (25)：Subject 總結
date: 2017-01-10T23:23:02.000Z
description: Subject 其實在 RxJS 中最常被誤解的一部份，因為 Subject 可以讓你用命令式的方式雖送值到一個 observable 的串流中。
image: null
tags:
  - JavaScript
  - RxJS
  - Observable
  - Subject
  - Operator
  - RxJS 30 Days
previous: ./thirty-days-RxJS-24.md
next: ./thirty-days-RxJS-26.md
---

很多人會直接把 Subject 拿來用在 **不知道如何建立 Observable 的狀況**，比如我們在 [30 天精通 RxJS(23)](/series/rxjs/thirty-days-RxJS-23) 中提到的可以用在 ReactJS 的 Event 中，來建立 event 的 observable 

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

因為在 React API 的關係，如果我們想要把 React Event 轉乘 observable 就可以用 Subject 幫我們做到這件事；但絕大多數的情況我們是可以透過 `Observable.create` 來做到這件事，像下面這樣

```javascript
const example = Rx.Observable.creator(observer => {
    const source = getSomeSource(); // 某個資料源
    source.addListener('some', (some) => {
        observer.next(some)
    })
});
```

大概就會像上面這樣，如果沒有合適的 creation operators 我們還是可以利用 `Observable.create` 來建立 observable，除非真的因為框架限制才會直接用 Subject。

## Subject 與 Observable 的差異

永遠記得 Subject 其實是 Observer Design Pattern 的實作，所以當 observer 訂閱到 subject 時，subject 會把訂閱者塞到一份訂閱者清單，在元素發送時就是在遍歷這份清單，並把元素一一送出，這跟 Observable 像是一個 function 執行是完全不同的(請參考 05 篇)。

Subject 之所以具有 Observable 的所有方法，是因為 Subject 繼承了 Observable 的型別，其實 Subject 型別中**主要**實做的方法只有 next、error、 complete、subscribe 及 unsubscribe 這五個方法，而這五個方法就是依照 Observer Pattern 下去實作的。

總而言之，Subject 是 Observable 的子類別，這個子類別當中用上述的五個方法實作了 Observer Pattern，所以他同時具有 Observable 與 Observer 的特性，而跟 Observable 最大的差異就是 Subject 是具有狀態的，也就是儲存的那份清單！

## 當前版本會遇到的問題

因為 Subject 在訂閱時，是把 observer 放到一份清單當中，並在元素要送出(next)的時候遍歷這份清單，大概就像下面這樣 

```javascript
//...
next() {
    // observers 是一個陣列存有所有的 observer 
    for (let i = 0; i < observers.length; i++) {
        observers[i].next(value);
    }
}
//...
```

這會衍伸一個大問題，就是在某個 observer 發生錯誤卻沒有做錯誤處理時，就會影響到別的訂閱，看下面這個例子

```javascript
const source = Rx.Observable.interval(1000);
const subject = new Rx.Subject();

const example = subject.map(x => {
    if (x === 1) {
        throw new Error('oops');
    }
    return x;
});
subject.subscribe(x => console.log('A', x));
example.subscribe(x => console.log('B', x));
subject.subscribe(x => console.log('C', x));

source.subscribe(subject);
```
[JSBin](https://jsbin.com/hukalo/1/edit?html,js,console)

上面這個例子，大家可能會預期 B 會在送出 1 的時候掛掉，另外 A 跟 C 則會持續發送元素，確實正常應該像這樣運席；但目前 RxJS 的版本中會在 B 報錯之後，A 跟 C 也同時停止運行。原因就像我前面所提的，在遍歷所有 observer 時發生了例外會導致之後的行為停止。

> 這個應該會在之後的版本中改掉的，前陣子才在 [TC39 Observable proposal](https://github.com/tc39/proposal-observable/issues/119#issuecomment-269429238) 中討論完。

那要如何解決這個問題呢？ 目前最簡單的方式當然是盡可能地把所有 observer 的錯誤處理加進去，這樣一來就不會有例外發生

```javascript
const source = Rx.Observable.interval(1000);
const subject = new Rx.Subject();

const example = subject.map(x => {
    if (x === 1) {
        throw new Error('oops');
    }
    return x;
});
subject.subscribe(
    x => console.log('A', x),
    error => console.log('A Error:' + error));
example.subscribe(x => console.log('B', x),
    error => console.log('B Error:' + error));
subject.subscribe(x => console.log('C', x),
    error => console.log('C Error:' + error));

source.subscribe(subject);
```
[JSBin](https://jsbin.com/hukalo/2/edit?html,js,console)

像上面這段程式碼，當 B 發生錯誤時就只有 B 會停止，而不會影響到 A 跟 C。

當然還有另一種解法是用 Scheduler，但因為我們這系列的文章還沒有講到 Scheduler 所以這個解法大家看看就好

```javascript
const source = Rx.Observable.interval(1000);
const subject = new Rx.Subject().observeOn(Rx.Scheduler.asap);

const example = subject.map(x => {
    if (x === 1) {
        throw new Error('oops');
    }
    return x;
});
subject.subscribe(x => console.log('A', x));
example.subscribe(x => console.log('B', x));
subject.subscribe(x => console.log('C', x));

source.subscribe(subject);
```


## 一定需要使用 Subject 的時機？

Subject 必要的使用時機除了本篇文章一開始所提的之外，正常應該是當我們一個 observable 的操作過程中發生了 side-effect 而我們不希望這個 side-effect 因為多個 subscribe 而被觸發多次，比如說下面這段程式碼

```javascript
var result = Rx.Observable.interval(1000).take(6)
             .map(x => Math.random()); // side-effect，平常有可能是呼叫 API 或其他 side effect

var subA = result.subscribe(x => console.log('A: ' + x));
var subB = result.subscribe(x => console.log('B: ' + x));
```
[JSBin](https://jsbin.com/bogiful/2/edit?html,js,console)

這段程式碼 A 跟 B 印出來的亂數就不一樣，代表 random(side-effect) 被執行了兩次，這種情況就一定會用到 subject(或其相關的 operators)

```javascript
var result = Rx.Observable.interval(1000).take(6)
             .map(x => Math.random()) // side-effect
             .multicast(new Rx.Subject())
             .refCount();

var subA = result.subscribe(x => console.log('A: ' + x));
var subB = result.subscribe(x => console.log('B: ' + x));
```
[JSBin](https://jsbin.com/bogiful/1/edit?html,js,console)

改成這樣後我們就可以讓 side-effect 不會因為訂閱數而多執行，這種情狀就是一定要用 subject 的。

## 今日小結

今天總結了 Subject 的使用情境，以及釐清跟 Observable 的關係，並且指出在使用時要避免犯發生的錯誤。

這幾點都非常的重要，不知道今天讀者有沒有收穫呢？ 如果有任何問題，歡迎在下方留言給我，謝謝！