---
templateKey: series
series: 30 天精通 RxJS
title: 30 天精通 RxJS (29)：30 天感言
date: 2017-01-14T23:49:21.000Z
description: 30 天悄悄的就過了，這 30 篇的文章基本上已經把 RxJS 一個核心三個重點(Observable + Observer + Subject + Scheduler)以及各個 operators 幾乎也都有寫到。最開始寫這個系列的文章是希望能讓 RxJS 的學習曲線降低，所以文章的前後順序及內容都是特別規劃過的，不知道我到底是不是真的做到了。
image: null
tags:
  - RxJS
  - RxJS 30 Days
previous: ./thirty-days-RxJS-28.md
next: ./thirty-days-RxJS-30.md
--- 

這 30 天真的是每天都像是在打仗一樣，要強迫自己從腦袋中組織文字真的是一件很痛苦的事，好險最後還是撐過來了，雖然中間生了一個禮拜的病，有些文章是隔了幾天才補上的，但最後仍及時的完成了全部文章真的很開心！

## 感謝

也很感謝以下幾位朋友在這 30 天中幫助我完善文章的內容，如果沒有你們我很可能沒辦法寫出這麼高質量的文章。

- Anna Su
- 林彥宇
- 莊育銘
- 徐如林
- 李佳怡
- 何振志
- 嚴偉安

也感謝幾位 IT 幫友，一直給我鼓勵讓我有動力堅持把文章完成，也在過程中不斷的給我反饋，真的很謝謝你們！


## 遺珠

RxJS 的東西真的太多了，尤其是 operators 很難在 30 天的文章中全部涵蓋，但沒有講到的如果不是不常用就是使用方式很直覺，不會太困難。

~~另外下面兩篇文章是因為在讀者的反饋，調整了內容所以被移掉的，這兩篇之後會再補上~~

以下兩篇已經補完囉




## 這 30 篇文章的目錄

0. [關於本系列文章](/series/rxjs/thirty-days-RxJS-00)
1. [Rx.js 簡介](/series/rxjs/thirty-days-RxJS-01)
2. [Functional Programming 基本觀念](/series/rxjs/thirty-days-RxJS-02)
3. [Functional Programming 通用函式](/series/rxjs/thirty-days-RxJS-03)
4. [什麼是 Observable?](/series/rxjs/thirty-days-RxJS-04)
5. [建立 Observable(一)](/series/rxjs/thirty-days-RxJS-05)
6. [建立 Observable(二)](/series/rxjs/thirty-days-RxJS-06)
7. [Observable Operators - Marble diagrams, map, mapTo, filter](/series/rxjs/thirty-days-RxJS-07)
8. [簡易拖拉實作 - take, first, concatAll, takeUntil](/series/rxjs/thirty-days-RxJS-08)
9. [Observable Operators - skip, takeLast, last, concat, startWith, merge](/series/rxjs/thirty-days-RxJS-09)
10. [Observable Operators - combineLast, withLatestFrom, zip](/series/rxjs/thirty-days-RxJS-10)
11. [實務應用 - Scroll + 完整拖拉](/series/rxjs/thirty-days-RxJS-11)
12. [Observable Operators - scan, buffer](/series/rxjs/thirty-days-RxJS-12)
13. [Observable Operators - delay, delayWhen](/series/rxjs/thirty-days-RxJS-13)
14. [Observable Operators - debounce, throttle](/series/rxjs/thirty-days-RxJS-14)
15. [Observable Operators - distinct, distinctUntilChanged](/series/rxjs/thirty-days-RxJS-15)
16. [Observable Operators - catch, retry, retryWhen, repeat](/series/rxjs/thirty-days-RxJS-16)
17. [Observable Operators - switch, mergeAll, concatAll](/series/rxjs/thirty-days-RxJS-17)
18. [Observable Operators - switchMap, mergeMap, concatMap](/series/rxjs/thirty-days-RxJS-18)
19. [實務範例 - 簡易 Auto Complete 實作](/series/rxjs/thirty-days-RxJS-19) 
20. [Observable Operators - window, windowToggle, groupBy](/series/rxjs/thirty-days-RxJS-20)
21. [深入 Observable](/series/rxjs/thirty-days-RxJS-21)
22. [什麼是 Subject？](/series/rxjs/thirty-days-RxJS-22)
23. [BehaviorSubject & ReplaySubject & AsyncSubject](/series/rxjs/thirty-days-RxJS-23)
24. [Observable operators - multicast, refCount, publish, share](/series/rxjs/thirty-days-RxJS-24)
25. [Subject 總結](/series/rxjs/thirty-days-RxJS-25)
26. [簡易實作 Observable（一）](/series/rxjs/thirty-days-RxJS-26)
27. [簡易實作 Observable（二）](/series/rxjs/thirty-days-RxJS-27)
28. [Scheduler 基本觀念](/series/rxjs/thirty-days-RxJS-28)
29. 30 天感言
30. [如何 Debug](/series/rxjs/thirty-days-RxJS-30)
31 [Cold & Hot Observable](/series/rxjs/thirty-days-RxJS-31)
