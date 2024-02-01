[TOC]

ANR(Application Not responding)。Android中，主线程(UI线程)如果在规定时内没有处理完相应工作，就会出现ANR。具体来说，ANR会在以下几种情况中出现:
(1) 输入事件(按键和触摸事件)5s内没被处理
(2) BroadcastReceiver的事件(onRecieve方法)在规定时间内没处理完(前台广播为10s，后台广播为60s)
(3) service 前台20s后台200s未完成启动
(4) ContentProvider的publish在10s内没进行完

1.耗时的网络访问
2.大量的数据读写
3.数据库操作
4.硬件操作（比如camera)
5.调用thread的join()方法、sleep()方法、wait()方法或者等待线程锁的时候
6.service binder的数量达到上限
7.system server中发生WatchDog ANR
8.service忙导致超时无响应
9.其他线程持有锁，导致主线程等待超时
10.其它线程终止或崩溃导致主线程一直等待