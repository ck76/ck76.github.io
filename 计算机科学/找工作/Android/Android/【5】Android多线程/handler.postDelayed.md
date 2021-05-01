```c
这是一种可以创建多线程消息的函数
使用方法：
1，首先创建一个Handler对象
Handler handler=new Handler();
2，然后创建一个Runnable对象
Runnable runnable=new Runnable(){
   @Override
   public void run() {
    // TODO Auto-generated method stub
    //要做的事情，这里再次调用此Runnable对象，以实现每两秒实现一次的定时器操作
    handler.postDelayed(this, 2000);
   } 
};
3，使用PostDelayed方法，两秒后调用此Runnable对象
handler.postDelayed(runnable, 2000);
实际上也就实现了一个2s的一个定时器
4，如果想要关闭此定时器，可以这样操作
handler.removeCallbacks(runnable);

当然，你也可以做一个闹钟提醒延时的函数试试，比如，先用MediaPlayer播放闹钟声音，
如果不想起，被停止播放之后，下次就5分钟后再播放，再被停止的话，下次就4分钟后播放，
………………
只要更改延时的时间就可以实现了，用一个static对象的话会比较容易操作。
```

---

阅读之前先问大家一个问题：Handler.postDelayed()是先delay一定的时间，然后再放入messageQueue中，还是先直接放入MessageQueue中，然后在里面wait delay的时间？为什么？如果你不答不上来的话，那么此文值得你看看。

原文：

使用handler发送消息时有两种方式，`post(Runnable r)`和`post(Runnable r, long delayMillis)`都是将指定Runnable（包装成PostMessage）加入到MessageQueue中，然后Looper不断从MessageQueue中读取Message进行处理。

然而我在使用的时候就一直有一个疑问，类似Looper这种「轮询」的工作方式，如果在每次读取时判断时间，是无论如何都会有误差的。但是在测试中发现Delay的误差并没有大于我使用`System.out.println(System.currentTimeMillis())`所产生的误差，几乎可以忽略不计，那么Android是怎么做到的呢？

### Handler.postDelayed()的调用路径

一步一步跟一下`Handler.postDelayed()`的调用路径：

1. Handler.postDelayed(Runnable r, long delayMillis)
2. Handler.sendMessageDelayed(getPostMessage(r), delayMillis)
3. Handler.sendMessageAtTime(msg, SystemClock.uptimeMillis() + delayMillis)
4. Handler.enqueueMessage(queue, msg, uptimeMillis)
5. MessageQueue.enqueueMessage(msg, uptimeMillis)

最后发现Handler没有自己处理Delay，而是交给了MessageQueue处理，我们继续跟进去看看MessageQueue又做了什么：

```
msg.markInUse();
msg.when = when;
Message p = mMessages;
boolean needWake;
if (p == null || when == 0 || when < p.when) {
    // New head, wake up the event queue if blocked.
    msg.next = p;
    mMessages = msg;
    needWake = mBlocked;
} else {
    ...
}
```

MessageQueue中组织Message的结构就是一个简单的单向链表，只保存了链表头部的引用（果然只是个Queue啊）。在`enqueueMessage()`的时候把应该执行的时间（上面Hanlder调用路径的第三步延迟已经加上了现有时间，所以叫when）设置到msg里面，并没有进行处理……WTF？

继续跟进去看看Looper是怎么读取MessageQueue的，在`loop()`方法内：

```
for (;;) {
    Message msg = queue.next(); // might block
    if (msg == null) {
        // No message indicates that the message queue is quitting.
        return;
    }
    ...
}
```

原来调用的是`MessageQueue.next()`，还贴心地注释了这个方法可能会阻塞，点进去看看：

```
for (;;) {
    if (nextPollTimeoutMillis != 0) {
        Binder.flushPendingCommands();
    }
 
    nativePollOnce(ptr, nextPollTimeoutMillis);
 
    synchronized (this) {
        // Try to retrieve the next message.  Return if found.
        final long now = SystemClock.uptimeMillis();
        Message prevMsg = null;
        Message msg = mMessages;
        if (msg != null && msg.target == null) {
            // Stalled by a barrier.  Find the next asynchronous message in the queue.
            do {
                prevMsg = msg;
                msg = msg.next;
            } while (msg != null && !msg.isAsynchronous());
        }
        if (msg != null) {
            if (now < msg.when) {
                // Next message is not ready.  Set a timeout to wake up when it is ready.
                nextPollTimeoutMillis = (int) Math.min(msg.when - now, Integer.MAX_VALUE);
            } else {
                // Got a message.
                mBlocked = false;
                if (prevMsg != null) {
                    prevMsg.next = msg.next;
                } else {
                    mMessages = msg.next;
                }
                msg.next = null;
                if (DEBUG) Log.v(TAG, "Returning message: " + msg);
                msg.markInUse();
                return msg;
            }
        } else {
            // No more messages.
            nextPollTimeoutMillis = -1;
        }
        ...
    }
}
```

可以看到，在这个方法内，==如果头部的这个Message是有延迟而且延迟时间没到的（now < msg.when），会计算一下时间（保存为变量nextPollTimeoutMillis），然后在循环开始的时候判断如果这个Message有延迟，就调用`nativePollOnce(ptr, nextPollTimeoutMillis)`进行阻塞==。`nativePollOnce()`的作用类似与`object.wait()`，只不过是使用了Native的方法对这个线程精确时间的唤醒。

精确延时的问题到这里就算是基本解决了，不过我又产生了一个新的疑问：如果Message会阻塞MessageQueue的话，那么先postDelay10秒一个Runnable A，消息队列会一直阻塞，然后我再post一个Runnable B，B岂不是会等A执行完了再执行？正常使用时显然不是这样的，那么问题出在哪呢？

再来一步一步顺一下Looper、Handler、MessageQueue的调用执行逻辑，重新看到`MessageQueue.enqueueMessage()`的时候发现，似乎刚才遗漏了什么东西：

```
msg.markInUse();
msg.when = when;
Message p = mMessages;
boolean needWake;
if (p == null || when == 0 || when < p.when) {
    // New head, wake up the event queue if blocked.
    msg.next = p;
    mMessages = msg;
    needWake = mBlocked;
} else {
    ...
}
...
// We can assume mPtr != 0 because mQuitting is false.
if (needWake) {
    nativeWake(mPtr);
}
```

这个needWake变量和`nativeWake()`方法似乎是唤醒线程啊？继续看看mBlocked是什么：

```
Message next() {
    for (;;) {
        ...
        if (msg != null) {
            ...
        } else {
            // Got a message.
            mBlocked = false;
            ...
        }
        ...
    }
    ...
    if (pendingIdleHandlerCount <= 0) {
        // No idle handlers to run.  Loop and wait some more.
        mBlocked = true;
        continue;
    }
    ...
}
```

就是这里了，在`next()`方法内部，如果有阻塞（没有消息了或者只有Delay的消息），会把mBlocked这个变量标记为true，在下一个Message进队时会判断这个message的位置，如果在队首就会调用`nativeWake()`方法唤醒线程！

现在整个调用流程就比较清晰了，以刚刚的问题为例：

1. `postDelay()`一个10秒钟的Runnable A、消息进队，MessageQueue调用`nativePollOnce()`阻塞，Looper阻塞；
2. 紧接着`post()`一个Runnable B、消息进队，判断现在A时间还没到、正在阻塞，把B插入消息队列的头部（A的前面），然后调用`nativeWake()`方法唤醒线程；
3. `MessageQueue.next()`方法被唤醒后，重新开始读取消息链表，第一个消息B无延时，直接返回给Looper；
4. Looper处理完这个消息再次调用`next()`方法，MessageQueue继续读取消息链表，第二个消息A还没到时间，计算一下剩余时间（假如还剩9秒）继续调用`nativePollOnce()`阻塞；
5. 直到阻塞时间到或者下一次有Message进队；

这样，基本上就能保证`Handler.postDelayed()`发布的消息能在相对精确的时间被传递给Looper进行处理而又不会阻塞队列了。



==比较时间然后插入 nativePol（） nativeWake（）唤醒l==

另外，这里在阅读原文的基础上添加一点思考内容：

MessageQueue会根据post delay的时间排序放入到链表中，链表头的时间小，尾部时间最大。因此能保证时间Delay最长的不会block住时间短的。当每次post message的时候会进入到MessageQueue的next()方法，会根据其delay时间和链表头的比较，如果更短则，放入链表头，并且看时间是否有delay，如果有，则block，等待时间到来唤醒执行，否则将唤醒立即执行。

所以handler.postDelay并不是先等待一定的时间再放入到MessageQueue中，而是直接进入MessageQueue，以MessageQueue的时间顺序排列和唤醒的方式结合实现的。使用后者的方式，我认为是集中式的统一管理了所有message，而如果像前者的话，有多少个delay message，则需要起多少个定时器。前者由于有了排序，而且保存的每个message的执行时间，因此只需一个定时器按顺序next即可。