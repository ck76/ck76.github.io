---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (03)：Functional Programming 通用函式
date: 2016-12-19T23:04:41.000Z
description: 了解 Functional Programming 的通用函式，能讓我們寫出更簡潔的程式碼，也能幫助我們學習 RxJS。
image: null
tags:
  - JavaScript
  - RxJS
  - Functional Programming
  - RxJS 30 Days
previous: ./thirty-days-RxJS-02.md
next: ./thirty-days-RxJS-04.md
---

這是【30天精通 RxJS】的 03 篇，如果還沒看過 02 篇可以往這邊走：
[30 天精通 RxJS (02)： Functional Programming 基本觀念](/series/rxjs/thirty-days-RxJS-02)

讀者可能會很好奇，我們的主題是 RxJS 為什麼要特別講 Functional Programming 的通用函式呢？ 實際上，RxJS 核心的 Observable 操作觀念跟 FP 的陣列操作是極為相近的，只學會以下幾個基本的方法跟觀念後，會讓我們之後上手 Observable 簡單很多！

今天的程式碼比較多，大家可以直接看影片！

[![Yes](https://img.youtube.com/vi/BmXgCagaJy0/0.jpg)](https://www.youtube.com/watch?v=BmXgCagaJy0)

## ForEach

> forEach 是 JavaScript 在 ES5 後，原生就有支援的方法。

原本我們可能要透過 for loop 取出陣列中的每一個元素

```javascript
var arr = ['Jerry', 'Anna'];

for(var i = 0; i < arr.length; i++) {
	console.log(arr[i]);
}
```

現在可以直接透過陣列的 forEach 取出每一個元素。

```javascript
var arr = ['Jerry', 'Anna'];

arr.forEach(item => console.log(item));
```

forEach 是 FP 操作陣列的基本方法，我們可以用這個方法來實作下面三個我們今天要講的重點分別為 map, filter, concatAll。

## Map

試著把 newCourseList 每個元素的 { id, title } 塞到新的陣列 idAndTitlePairs

```javascript
var newCourseList = [
	{
		"id": 511021,
		"title": "React for Beginners",
		"coverPng": "https://res.cloudinary.com/dohtkyi84/image/upload/v1481226146/react-cover.png",
		"rating": 5
	},
	{
		"id": 511022,
		"title": "Vue2 for Beginners",
		"coverPng": "https://res.cloudinary.com/dohtkyi84/image/upload/v1481226146/react-cover.png",
		"rating": 5
	},
	{
		"id": 511023,
		"title": "Angular2 for Beginners",
		"coverPng": "https://res.cloudinary.com/dohtkyi84/image/upload/v1481226146/react-cover.png",
		"rating": 5
	},
	{
		"id": 511024,
		"title": "Webpack for Beginners",
		"coverPng": "https://res.cloudinary.com/dohtkyi84/image/upload/v1481226146/react-cover.png",
		"rating": 4
	}
], idAndTitle = [];

newCourseList.forEach((course) => {
	idAndTitle.push({ id: course.id, title: course.title });
});
```

雖然我們成功的把 newCourseList 轉成 idAndTitlePairs，但這樣的寫法還是顯得有點太複雜了，我們可以用更抽象化的方式來完成。

上面我們練習到 newCourseList 轉換成一個新的陣列 idAndTitlePairs，這個轉換的過程其實就是兩件事

- 遍歷 newCourseList 所有的元素
- 把每個元素的預期值給到新的陣列

把這個過程抽象化成一個方法 map，以下是簡化的基本思路：

1. 我們會讓每個 陣列 都有一個 map 方法
2. 這個方法會讓使用者自訂傳入一個 callback function
3. 這個 callback function 會回傳使用者預期的元素

> 雖然 ES5 之後原生的 JavaScript 陣列有 map 方法了，但希望讀者自我實做一次，能幫助理解。

```javascript
// 我們希望每一個陣列都有 map 這個方法，所以我們在 Array.prototype 擴充 map function
Array.prototype.map = function(callback) {
  var result = []; // map 最後一定會返回一個新陣列，所以我們先宣告一個新陣列
  
  this.forEach(function(element, index) {
	  // this 就是呼叫 map 的陣列
	  result.push(callback(element, index));
	  // 執行使用者定義的 callback， callback 會回傳使用者預期的元素，所以我們把它 push 進新陣列
  })
  
  return result;
}
```

> 這裡用到了 JavaScript 的 prototype chain 以及 this 等觀念，可以看此[影片](https://www.youtube.com/watch?v=BlT6pCG2M1I)了解！

到這裡我們就實作完成 map 的方法了，讓我們來試試這個方法吧！

```javascript
var idAndTitle = newCourseList
                 .map((course) => {
                     return { id: course.id, title: course.title };
                 });
```

可以看到我們的程式碼更加的簡潔！

## Filter

如果我們希望過濾一個陣列，留下陣列中我們想要的元素，並產生一個新的陣列，要怎麼做呢？ 
先讓我們用 forEach 完成！

讓我們過濾出 rating 值是 5 的元素

```javascript
var ratingIsFive = [];

newCourseList.forEach((course) => {
	if(course.rating === 5) {
		ratingIsFive.push(course);
	}
});
```

同樣的我們試著來簡化這個過程，首先在這個轉換的過程中，我們做了兩件事：  

1. 遍歷 newCourseList 中的所有元素
2. 判斷元素是否符合條件，符合則加到新的陣列中

```javascript
Array.prototype.filter = function(callback) {
	var result = [];
	this.forEach((item, index) => {
		if(callback(item, index))
			result.push(item);
	});
	return result;
}
```

試試這個方法

```javascript
var ratingIsFive = newCourseList
                   .filter((course) => course.rating === 5);
```

會發現我們的程式碼又變簡單了，接著我們試著把 filter, map 串起來。

如果我想要取出所有 rating 是 5 的所有 course title

```javascript
var ratingIsFive = newCourseList
                   .filter((course) => course.rating === 5)
                   .map(course => course.title);

```

## ConcatAll

有時候我們會遇到組出一個二維陣列，但我們希望陣列是一維的，問題如下：

假如我們要取出 courseLists 中所有 rating 為 5 的課程，這時可能就會用到兩個 forEach

```javascript
var user = {
  id: 888,
  name: 'JerryHong',
  courseLists: [{
    "name": "My Courses",
    "courses": [{
      "id": 511019,
      "title": "React for Beginners",
      "coverPng": "https://res.cloudinary.com/dohtkyi84/image/upload/v1481226146/react-cover.png",
      "tags": [{ id: 1, name: "JavaScript" }],
      "rating": 5
    }, {
      "id": 511020,
      "title": "Front-End automat workflow",
      "coverPng": "https://res.cloudinary.com/dohtkyi84/image/upload/v1481226146/react-cover.png",
      "tags": [{ "id": 2, "name": "gulp" }, { "id": 3, "name": "webpack" }],
      "rating": 4
    }]
  }, {
    "name": "New Release",
    "courses": [{
      "id": 511022,
      "title": "Vue2 for Beginners",
      "coverPng": "https://res.cloudinary.com/dohtkyi84/image/upload/v1481226146/react-cover.png",
      "tags": [{ id: 1, name: "JavaScript" }],
      "rating": 5
    }, {
      "id": 511023,
      "title": "Angular2 for Beginners",
      "coverPng": "https://res.cloudinary.com/dohtkyi84/image/upload/v1481226146/react-cover.png",
      "tags": [{ id: 1, name: "JavaScript" }],
      "rating": 4
    }]
  }]
};

var allCourseIds = [];

user.courseLists.forEach(list => {
  list.courses
    .filter(item => item.rating === 5)
    .forEach(item => {
      allCourseIds.push(item)
    })
})
```

可以看到上面的程式碼，我們用了較為低階的操作來解決這個問題，我們剛剛已經試著用抽象化的方式實作了 map 跟 filter，那我們同樣也能夠定義一個方法用來 攤平二維陣列。

讓我們來加入一個 concatAll 方法來簡化這段程式碼吧！
concatAll 要做的事情很簡單，就是把一個二維陣列轉成一維。

```javascript
Array.prototype.concatAll = function() {
  var result = [];
  
  // 用 apply 完成
  this.forEach((array) => {
    result.push.apply(result, array);
  });

  // 用兩個 forEach 完成
  // this.forEach((array) => {
  //   array.forEach(item => {
  //     result.push(item)
  //   })
  // });
  
  // 用 ES6 spread 完成
  // this.forEach((array) => {
  //   result.push(...array);
  // })
  
  return result;
};
```

同樣的我們用前面定要好的 courseLists 來試試 concatAll 吧！

```javascript
var allCourseIds = user.courseLists.map(list => {
	return list.courses.filter(course => course.rating === 5)
}).concatAll()
```

這邊出一個比較難的題目，大家可以想想看要怎麼解

```javascript
var courseLists = [{
  "name": "My Courses",
  "courses": [{
    "id": 511019,
    "title": "React for Beginners",
    "covers": [{
      width: 150,
      height: 200,
      url: "http://placeimg.com/150/200/tech"
    }, {
      width: 200,
      height: 200,
      url: "http://placeimg.com/200/200/tech"
    }, {
      width: 300,
      height: 200,
      url: "http://placeimg.com/300/200/tech"
    }],
    "tags": [{
      id: 1,
      name: "JavaScript"
    }],
    "rating": 5
  }, {
    "id": 511020,
    "title": "Front-End automat workflow",
    "covers": [{
      width: 150,
      height: 200,
      url: "http://placeimg.com/150/200/arch"
    }, {
      width: 200,
      height: 200,
      url: "http://placeimg.com/200/200/arch"
    }, {
      width: 300,
      height: 200,
      url: "http://placeimg.com/300/200/arch"
    }],
    "tags": [{
      "id": 2,
      "name": "gulp"
    }, {
      "id": 3,
      "name": "webpack"
    }],
    "rating": 5
  }]
}, {
  "name": "New Release",
  "courses": [{
    "id": 511022,
    "title": "Vue2 for Beginners",
    "covers": [{
      width: 150,
      height: 200,
      url: "http://placeimg.com/150/200/nature"
    }, {
      width: 200,
      height: 200,
      url: "http://placeimg.com/200/200/nature"
    }, {
      width: 300,
      height: 200,
      url: "http://placeimg.com/300/200/nature"
    }],
    "tags": [{
      id: 1,
      name: "JavaScript"
    }],
    "rating": 5
  }, {
    "id": 511023,
    "title": "Angular2 for Beginners",
    "covers": [{
      width: 150,
      height: 200,
      url: "http://placeimg.com/150/200/people"
    }, {
      width: 200,
      height: 200,
      url: "http://placeimg.com/200/200/people"
    }, {
      width: 300,
      height: 200,
      url: "http://placeimg.com/300/200/people"
    }],
    "tags": [{
      id: 1,
      name: "JavaScript"
    }],
    "rating": 5
  }]
}];

/* 
var result = courseList
不得直接使用索引 covers[0]，請用 concatAll, map, filter, forEach 完成
result 結果為 [
    {
      id: 511019,
      title: "React for Beginners",
      cover: "http://placeimg.com/150/200/tech"
    }, {
      id: 511020,
      title: "Front-End automat workflow",
      cover: "http://placeimg.com/150/200/arch"
    }, {
      id: 511022,
      title: "Vue2 for Beginners",
      cover: "http://placeimg.com/150/200/nature"
    }, {
      id: 511023,
      title: "Angular2 for Beginners",
      cover: "http://placeimg.com/150/200/people"
    },
 ]
*/
```
練習連結： [JSBin](https://jsbin.com/wifulas/6/edit?js,output) | [JSFiddle](https://jsfiddle.net/s6323859/5wcgnf89/1/)

這題有點難，大家可以想想看，我把答案寫在[這裡](https://jsbin.com/rahezacane/edit?js,console)了！

如果大家還想做更多的練習可以到這個連結：http://reactivex.io/learnrx/

> 這個連結是 Jafar 大神為他的 RxJS workshop 所做的練習網站！

## 今日小結

今天講了 FP 操作陣列的三個通用函式 forEach, map, filter，以及我們自己定義的一個方法叫 concatAll。這幾天我們把學習 RxJS 的前置觀念跟知識基本上都講完了，明天我們就開始進入 RxJS 的重點核心 Observable 囉！
