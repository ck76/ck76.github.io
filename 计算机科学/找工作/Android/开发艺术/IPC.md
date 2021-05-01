```java
package cn.ck.xjbl;

import android.os.Parcel;
import android.os.Parcelable;

/**
 * @author chengkun
 * @since 2019/2/20 23:52
 */
public class Book implements Parcelable {
    private int bookId;
    private String bookName;

    public Book(int bookId, String bookName) {
        this.bookId = bookId;
        this.bookName = bookName;
    }

    public int getBookId() {
        return bookId;
    }

    public void setBookId(int bookId) {
        this.bookId = bookId;
    }

    public String getBookName() {
        return bookName;
    }

    public void setBookName(String bookName) {
        this.bookName = bookName;
    }

    protected Book(Parcel in) {
        bookId = in.readInt();
        bookName = in.readString();
    }

    public static final Creator<Book> CREATOR = new Creator<Book>() {
        @Override
        public Book createFromParcel(Parcel in) {
            return new Book(in);
        }

        @Override
        public Book[] newArray(int size) {
            return new Book[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeInt(bookId);
        dest.writeString(bookName);
    }
}
```



```java
// Book.aidl
package cn.ck.xjbl;

// Declare any non-default types here with import statements


parcelable Book;
```



```java
// IBookManager.aidl
package cn.ck.xjbl;
import cn.ck.xjbl.Book;
// Declare any non-default types here with import statements

interface IBookManager {
    List<Book> getBookList();
    void addBook(in Book book);
}
```



```java
/*
 * This file is auto-generated.  DO NOT MODIFY.
 * Original file: /Users/chengkun/Library/Android/project/XJBL/app/src/main/aidl/cn/ck/xjbl/IBookManager.aidl
 */
package cn.ck.xjbl;
// Declare any non-default types here with import statements

public interface IBookManager extends android.os.IInterface {
    /**
     * Local-side IPC implementation stub class.
     */
    public static abstract class Stub extends android.os.Binder implements cn.ck.xjbl.IBookManager {
        private static final java.lang.String DESCRIPTOR = "cn.ck.xjbl.IBookManager";

        /**
         * Construct the stub at attach it to the interface.
         */
        public Stub() {
            this.attachInterface(this, DESCRIPTOR);
        }

        /**
         * Cast an IBinder object into an cn.ck.xjbl.IBookManager interface,
         * generating a proxy if needed.
         */
        public static cn.ck.xjbl.IBookManager asInterface(android.os.IBinder obj) {
            if ((obj == null)) {
                return null;
            }
            android.os.IInterface iin = obj.queryLocalInterface(DESCRIPTOR);
            if (((iin != null) && (iin instanceof cn.ck.xjbl.IBookManager))) {
                return ((cn.ck.xjbl.IBookManager) iin);
            }
            return new cn.ck.xjbl.IBookManager.Stub.Proxy(obj);
        }

        @Override
        public android.os.IBinder asBinder() {
          //客户端通过asInterface返回接口
            return this;
        }

        @Override
        public boolean onTransact(int code, android.os.Parcel data, android.os.Parcel reply, int flags) throws android.os.RemoteException {
            switch (code) {
                case INTERFACE_TRANSACTION: {
                    reply.writeString(DESCRIPTOR);
                    return true;
                }
                case TRANSACTION_getBookList: {
                    data.enforceInterface(DESCRIPTOR);
                    java.util.List<cn.ck.xjbl.Book> _result = this.getBookList();
                    reply.writeNoException();
                    reply.writeTypedList(_result);
                    return true;
                }
                case TRANSACTION_addBook: {
                    data.enforceInterface(DESCRIPTOR);
                    cn.ck.xjbl.Book _arg0;
                    if ((0 != data.readInt())) {
                        _arg0 = cn.ck.xjbl.Book.CREATOR.createFromParcel(data);
                    } else {
                        _arg0 = null;
                    }
                    this.addBook(_arg0);
                    reply.writeNoException();
                    return true;
                }
            }
            return super.onTransact(code, data, reply, flags);
        }

      //代理类实现了IBookManager接口
        private static class Proxy implements cn.ck.xjbl.IBookManager {
            private android.os.IBinder mRemote;

            Proxy(android.os.IBinder remote) {
                mRemote = remote;
            }

            @Override
            public android.os.IBinder asBinder() {
                return mRemote;
            }

            public java.lang.String getInterfaceDescriptor() {
                return DESCRIPTOR;
            }

            @Override
            public java.util.List<cn.ck.xjbl.Book> getBookList() throws android.os.RemoteException {
                android.os.Parcel _data = android.os.Parcel.obtain();
                android.os.Parcel _reply = android.os.Parcel.obtain();
                java.util.List<cn.ck.xjbl.Book> _result;
                try {
                    _data.writeInterfaceToken(DESCRIPTOR);
                  //RPC调用
                    mRemote.transact(Stub.TRANSACTION_getBookList, _data, _reply, 0);
                    _reply.readException();
                    _result = _reply.createTypedArrayList(cn.ck.xjbl.Book.CREATOR);
                } finally {
                    _reply.recycle();
                    _data.recycle();
                }
                return _result;
            }

            @Override
            public void addBook(cn.ck.xjbl.Book book) throws android.os.RemoteException {
                android.os.Parcel _data = android.os.Parcel.obtain();
                android.os.Parcel _reply = android.os.Parcel.obtain();
                try {
                    _data.writeInterfaceToken(DESCRIPTOR);
                    if ((book != null)) {
                        _data.writeInt(1);
                        book.writeToParcel(_data, 0);
                    } else {
                        _data.writeInt(0);
                    }
                    mRemote.transact(Stub.TRANSACTION_addBook, _data, _reply, 0);
                    _reply.readException();
                } finally {
                    _reply.recycle();
                    _data.recycle();
                }
            }
        }

        static final int TRANSACTION_getBookList = (android.os.IBinder.FIRST_CALL_TRANSACTION + 0);
        static final int TRANSACTION_addBook = (android.os.IBinder.FIRST_CALL_TRANSACTION + 1);
    }

    public java.util.List<cn.ck.xjbl.Book> getBookList() throws android.os.RemoteException;

    public void addBook(cn.ck.xjbl.Book book) throws android.os.RemoteException;
}

```



```java
  <service
            android:name=".aidl.BookManagerService"
            android:enabled="true"
            android:exported="true"
            android:process=":remote" />
```



```java
//BookManagerService.java
package cn.ck.xjbl.aidl;

import android.app.Service;
import android.content.Intent;
import android.os.Binder;
import android.os.IBinder;
import android.os.RemoteException;

import java.util.ArrayList;
import java.util.List;

import cn.ck.xjbl.Book;
import cn.ck.xjbl.IBookManager;

//【服务端】
public class BookManagerService extends Service {
    public BookManagerService() {
    }

    private List<Book> mBookList = new ArrayList<>();

  	//【接口方法真正实现的地方】
    private Binder mBinder = new IBookManager.Stub() {
        @Override
        public List<Book> getBookList() throws RemoteException {
            return mBookList;
        }

        @Override
        public void addBook(Book book) throws RemoteException {
            mBookList.add(book);
        }
    };


    @Override
    public IBinder onBind(Intent intent) {
        return mBinder;
    }
}
```

```java
//AIDLDemo.java
package cn.ck.xjbl.aidl;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.IBinder;
import android.os.RemoteException;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;

import java.util.List;

import cn.ck.xjbl.Book;
import cn.ck.xjbl.IBookManager;
import cn.ck.xjbl.R;
import cn.ck.xjbl.common.app.MyApplication;
import cn.ck.xjbl.utils.ToastUtil;

public class AIDLDemo extends AppCompatActivity {

    private ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder service) {
          	//asInterface 区分进程返回 因为Stub类和Proxy类都实现了定义的接口【自始至终就一个接口】
            IBookManager iBookManager = IBookManager.Stub.asInterface(service);
            try {
                List<Book> list = iBookManager.getBookList();
                Log.i("ck", String.valueOf(list.size()));
                Book book = new Book(123, "ck");
                iBookManager.addBook(book);
                list = iBookManager.getBookList();
                Log.i("ck", list.get(0).getBookName() + "   " + list.get(0).getBookId());
                ToastUtil.show(MyApplication.AppContext,"aaa");
            } catch (RemoteException e) {
                e.printStackTrace();
            }
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {

        }
    };

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_aidldemo);
        Intent intent = new Intent(this, BookManagerService.class);
      	//绑定服务
        bindService(intent, mConnection, Context.BIND_AUTO_CREATE);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        unbindService(mConnection);
    }
}
```








---





https://www.jianshu.com/p/467016b4487c

### 面试题：AIDL是什么？你有使用过它吗，它支持哪些数据类型？

AIDL是Android Interface Definition Language的简写，即Android接口定义语言。我们知道Android系统为每一个应用开启一个独立的虚拟机，每个应用都运行在各自进程里（默认情况下），彼此之间相互独立，无法共享内存。当一个应用想要访问另一个应用的数据或调用其方法，就要用到Android系统提供的IPC机制。而AIDL就是Android实现IPC机制的方式之一。

除了AIDL，Android还提供了Messenger来实现跨进程通信，不过Messenger是以单线程串行方式（消息队列）来处理来自不同客户端的访问的，并不适合多线程并发访问。当需要提供跨进程以及多线程并发服务时就需要AIDL上场了。

> Messenger实际上也是以AIDL作为其底层结构。

其实要应对AIDL相关的面试题，除了了解清楚它的作用和注意的事项外，最有效的手段莫过于自己动手写几次。这里给大家定两个小目标：

- 会使用AIDL进行进程间通信；
- 会手写AIDL的编码，加深对Binder机制的理解。

### 创建AIDL

我们先实现第一个，在Android Studio中创建一个简单的AIDL项目，实现IPC通信。

Step1. 创建.aidl文件
我们在对应的src的Package下创建一个AIDL文件（Android Studio->File->New->AIDL->AIDL file），创建后Android Studio会自动把这个.aidl文件放到一个aidl的目录下。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glzyxe181zj30xc0hnjy3.jpg)

> Android SDK Tool会根据我们的.aidl文件自动生成一个同名的.java文件，如：AIDLTest/app/build/generated/source/aidl/debug/net/goeasyway/aidltest/IRemoteService.java

basicTypes方法中给我们展示了AIDL支持的基本数据类型，除此之外，AIDL还支持：CharSequence, List & Map（List和Map中的所有元素都必须是AIDL支持的数据类型、其他AIDL生成的接口或您声明的可打包类型。）

Step2. 创建一个Service暴露AIDL接口并实现AIDL的接口函数
如下代码，创建一个Service:



```java
public class RemoteService extends Service {
    public RemoteService() {
    }

    @Override
    public IBinder onBind(Intent intent) {
        return binder; //暴露给客户端
    }

    // 实现AIDL接口
    private final IRemoteService.Stub binder = new IRemoteService.Stub() {

        @Override
        public int getPid() throws RemoteException {
            return Process.myPid();
        }

        @Override
        public void basicTypes(int anInt, long aLong, boolean aBoolean, float aFloat,
                               double aDouble, String aString) throws RemoteException {

        }
    };
}
```

然后在MainActivity通过bindService绑定这个服务，即可以获得AIDL的接口调用的引用。运行前，我们设置一下AndroidManifest.xml文件记这个Service运行在一个单独的进程中：



```xml
        <service
            android:name=".RemoteService"
            android:process=":remote"
            android:enabled="true"
            android:exported="true"/>
```

现在来bindService:



```java
public class MainActivity extends AppCompatActivity {
    private final static String TAG = "MainActivity";
    private IRemoteService remoteService;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        Intent intent = new Intent();
        intent.setClass(this, RemoteService.class);
        bindService(intent, connection, Service.BIND_AUTO_CREATE); // 绑定服务
    }


    private ServiceConnection connection = new ServiceConnection() {
        public void onServiceConnected(ComponentName className, IBinder service) {
            remoteService = IRemoteService.Stub.asInterface(service); //获取AIDL的接口实现引用
            try {
                Log.i(TAG, "Client pid= " + Process.myPid());
                Log.i(TAG, "RemoteService pid= " + remoteService.getPid());
            } catch (RemoteException e) {
                e.printStackTrace();
            }
        }

        public void onServiceDisconnected(ComponentName className) {
            Log.e(TAG, "Service has unexpectedly disconnected");
            remoteService = null;
        }
    };
}
```

从输出的日志看到Service和Activity运行在两个不同的进程中：



```undefined
02-05 09:51:40.154 18992-18992/net.goeasyway.aidltest I/MainActivity: Client pid= 18992
02-05 09:51:40.154 18992-18992/net.goeasyway.aidltest I/MainActivity: RemoteService pid= 19022
```

到这里，我们完成了一个AIDL的范例，有几个地方可能会对我们造成困扰：

- Stub类：Binder的实现类，服务端需要实现这个类来提供服务。
- asInterface函数： 一个静态函数，用来将IBinder转换成对应的Binder的引用。先通过queryLocalInterface查询，如果服务端和客户端都是在同一个进程，那么就不需要跨进程了，直接将IRemoteService当做普通的对象来使用，否则会返回远程对象的代理对象（Proxy）。



```java
public static net.goeasyway.aidltest.IRemoteService asInterface(android.os.IBinder obj)
{
    if ((obj==null)) {
        return null;
    }
    android.os.IInterface iin = obj.queryLocalInterface(DESCRIPTOR);
    if (((iin!=null)&&(iin instanceof net.goeasyway.aidltest.IRemoteService))) {
        return ((net.goeasyway.aidltest.IRemoteService)iin);
    }
    return new net.goeasyway.aidltest.IRemoteService.Stub.Proxy(obj);
}
```

### 通过IPC传递对象

现在，我们加大一下难度，AIDL的接口使用一个我们自己定义的类为参数（或者返回值）。实现步骤如下：

- 添加一个自定义对象类，并且要实现Parcelable接口，如MyProcess.java；
- 在AIDL目录下的相同Pacage下添加一个同名的AIDL文件，如MyProcess.aidl；

> 注意：通过“Android Studio->File->New->AIDL->AIDL file”不让你创建和MyProcess.java同名的AIDL文件，你可以直接用通过“Android Studio->File->New->File”创建一个MyProcess.aidl。

- 在AIDL接口类中添加一个接口函数，使用MyProcess做为参数或者返回值；

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glzyxagnm7j30xc0eeq97.jpg)

其他的细节大家可以直接查看Github上的代码：[https://github.com/goeasyway/AIDL_Test](https://link.jianshu.com/?t=https://github.com/goeasyway/AIDL_Test) （或者查看提交的说明找到具体每次的代码区别：[https://github.com/goeasyway/AIDL_Test/commits/master](https://link.jianshu.com/?t=https://github.com/goeasyway/AIDL_Test/commits/master) ）

**in、out & inout**
这节我们看到“MyProcess getProcess(in MyProcess clientProcess);”这个接口的参数有一个“in”修饰符，这也是一个常见的面试题，可以考察一下对方是否真的写过AIDL的代码。

> 问题：AIDL中的接口函数有时会使用in、out或者inout的参数修饰符，它们各表示什么意思？在什么情况下要使用呢？

in、out和inout表示数据的流向。大家可以把AIDL的客户端和服务端理解成两个进程（其实大多数情况也是这样才会使用AIDL），从客户端流向服务端用in表示，表示这个对象是从客户端中传递到服务端，在服务端修改这个对象不会对客户端输入的对象产生影响。

而out则表示，数据只能从服务端影响客户端，即客户端输入这个参数时，服务端并不能获取到客户端的具体实例中的数据，而是生成一个默认数据，但是服务端对这个默认数据的修改会影响到客户端的这个类对象实例发生相应的改变。

理解了in、out之后，inout自然不需要再解释了。AIDL默认支持的数据类型使用in修饰符，对于我们自定义的Parcelable对象，一般情况下我们也是使用in，如果没有必要，应该尽量避免inout。

**Intent也是Parcelable实现**
也许你会想到，我们在Activity间可以通过Intent携带参数，其实你去看的源码的话会发现Intent也是一个Parcelable的实现类，而且在系统的工程中也有一个Intent.aidl文件（路径：/frameworks/base/core/java/android/content/Intent.aidl）。所以，它才可以在进程间传递。

> 注：目前Client端和Server端在同一工程中，如果分开在不同的工程的话，Client端所在的工程要把Server端提供的.aidl复制到同名Pacage的AIDL代码目录下。

### 手动方式创建AIDL（不依赖AIDL工具，手写远程AIDL的代码完成跨进程通信）

通过AIDL，可以让本地调用远程服务的接口就像调用本地接口那么简单，让用户无需关注内部细节，只需要实现自己的业务逻辑接口，内部复杂的参数序列化发送、接收、客户端调用服务端的逻辑，用户并不需要关心。

AIDL的代码生成器，已经根据.aidl文件自动帮我们生成Proxy、Stub（抽象类）两个类，并且把客户端代理mRemote的transact()过程以及服务器端的onTtransact()过程默认实现好了，我们只需要在服务端继承Stub，实现自己的业务方法即可。

但现在，为了进一步加深对Binder机制的理解，我们来做一个手动实现编写AIDL相关代码的练习。

具体的代码大家可以参考：[https://github.com/goeasyway/AIDL_Test/tree/master/app/src/main/java/net/goeasyway/aidltest/diy](https://link.jianshu.com/?t=https://github.com/goeasyway/AIDL_Test/tree/master/app/src/main/java/net/goeasyway/aidltest/diy)

这个包里有三个类：IRmote.java为接口，Stub.java为Binder实现类（Service端要实例化它并在onBind返回），Proxy.java为代理类，提供给客户端使用的，通过Binder驱动和服务端通信。

> 大家可以看到这个练习不需要.aidl文件。

在这几个代码中，大家需要搞清楚这个类（或者接口）的关系：

- Binder
  Binder本地对象。
- IBinder
  IBinder是一个接口，它代表了一种跨进程传输的能力。
- IInterface
  IBinder负责数据传输，那么client与server端的调用契约呢？这里的IInterface代表的就是远程server对象具有什么能力。具体来说，就是aidl里面的接口。
- Proxy
  代表远程进程的Binder对象的本地代理，继承自IBinder，因而具有跨进程传输的能力。实际上，在跨越进程的时候，Binder驱动会自动完成代理对象和本地对象的转换。
- Stub
  这个类继承了Binder, 说明它是一个Binder本地对象，它实现了IInterface接口，表明它具有远程Server承诺给Client的能力；Stub是一个抽象类，具体的IInterface的相关实现需要我们手动完成。

如果觉得有点难理解的话，不妨先动手写了再来看。

### 小结

如何使用AIDL应该是一个高级工程师必备技能，如果你之前不太了解它的话，那么我强烈建议你完成上面的两个小练习，再回过头去看它的解说。

之后，你将不再惧怕和AIDL相关的面试题。



134人点赞



[Android面试]()





作者：goeasyway
链接：https://www.jianshu.com/p/467016b4487c
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

---

**一、IPC的说明**

IPC是Inter-Process Communication的缩写，含义为进程间通信或跨进程通信，是指两个进程之间进行数据交换的过程。

IPC不是Android独有的，任何一个操作系统都需要有相应的IPC机制，比如Windows上可以通过剪贴板，管道和邮槽来进行进程间通信；Linux上可以通过命名管道、共享内容、信号量等进行进程间通信。

对于Android来说，它是一种基于Linux内核的移动操作系统，但它的进程间通信方式并不能完全继承自Linux；相反，它有自己的进程间通信方式。

在Android中可以通过Binder轻松的实现进程间通信。Android还支持Socket，通过Socket可以实现任意两个终端之间的通信，同一设备的两个进程通过Socket通信自然也是可以的。

 

**二、\**Android中\**IPC的使用**

首先，只有面对多进程的情况才需要考虑进程间通信。多进程的情况分为两种：

第一种是一个应用因为某些原因，自身需要采用多进程模式来实现，比如有些模块需要运行在单独的进程中、或者为了加大一个应用的可使用内存，需要通过多进程来获取多分内存空间。

另一种是当前应用需要向其他的应用获取数据，所以必须采用跨进程的方式来获取数据，比如使用系统的ContentProvider去查询数据。

通过给四大组件在AndroidMenifest中指定android:process属性，可以轻易地开启多进程模式。

 

**三、\**Android中\**IPC带来的问题**

两个应用共享数据：Android系统会为每个应用分配一个唯一的UID，具有相同UID的应用才能共享数据。两个应用通过ShareUID跑在同一个进程是有要求的，需要这两个应用有相同的ShareUID并且签名相同才可以。在这种情况下，他们可以相互访问对方的私有数据，比如data目录，组件信息等，不管他们是否跑在同一个进程。

Android系统为每个应用分配了一个独立的虚拟机，或者说为每一个进程都分配一个独立的虚拟机，不同的虚拟机在内存分配上有不同的地址空间，这就导致在不同的虚拟机中访问同一个对象会产生多分副本。所有运行在不同进程中的四大组件，只要它们之间需要通过内存来共享数据，都会共享失败，这也是多进程带来的主要影响。

一般来说，使用多进程会造成如下的问题：

（1）静态成员和单例模式完全失效（不同的虚拟机中访问同一个对象会产生多分副本）

（2）线程同步机制完全失效（不在同一块内存，不管是所对象还是锁全局类都无法保证线程同步）

（3）SharePreferences的可靠性下降（不支持两个进程同时写操作）

（4）Application会多次创建（因为创建新进程会分配独立虚拟机，相当于启动一个新的应用）

虽说不能直接的共享内存，但是通过跨进程通信还是可以实现数据交互。

 

**四、Android中各种IPC方式**

1、使用Bundle

四大组件中三大组件Activity、Service、Receiver都支持在Intent中传递Bundle数据。

由于Bundle实现了Parcelable接口，所以它可以很方便的在不同的进程间传输数据。当然我们传输的数据必须能够被序列化，比如基本类型、实现了Parcelable接口的对象、实现了Serializable接口的对象以及一些Android支持的特殊对象。

2、使用文件共享

两个进程通过读写同一个文件来交换数据，比如A进程把数据写入文件，B进程通过读取这个文件来获取数据。

Android系统基于Linux，使得并发读写文件可以没有限制的进行，甚至两个线程同时对文件读写操作都是允许的，尽管可能出问题，因此文件共享方式适合在对数据同步要求不高的进程间进行通信。

SharedPreferences也属于文件的一种，但是由于系统对它的读写有一定的缓存策略，即在内存中会有一份SharedPreferences文件的缓存；因此在多进程模式下，系统对它的读写就变得不可靠，会有很大几率丢失数据，不建议在进程间通信中使用SharedPreferences。

3、使用Messenger

Messenger可以理解为信使，通过它可以再不同进程中传递Message对象，在Message中放入我们需要传递的数据，就可以实现数据的进程间传递了。

Messenger是一种轻量级的IPC方案，它的底层实现是AIDL。由于它一次处理一个请求，因此在服务端不需要考虑线程同步的问题，因为服务端不存在并发执行的情形。

4、使用AIDL

AIDL是 Android Interface definition language的缩写，它是一种android内部进程通信接口的描述语言。AIDL可以处理发送到服务器端大量的并发请求（不同与Messenger的串行处理方式），也可以实现跨进程的方法调用。

在Android中使用方法：创建一个Service和一个AIDL接口，接着创建一个类继承自AIDL接口中的Stub类并实现Stub中的抽象方法，在Service的onBind方法中返回这个类的对象，然后客户端绑定服务端Service，建立连接后就可以访问远程服务器了。

5、使用ContentProvider

ContentProvider是Android中提供的专门用于不同应用间进行数据共享的方式，天生适合进程间通信。

ContentProvider的底层实现也是Binder，但是它的使用过程比AIDL简单的多，因为系统做了封装，使得无需关心细节即可轻松实现IPC。ContentProvider主要以表格的形式组织数据，和数据库很类似，但ContentProvider对底层的数据存储方式没有任何要求，既可以使用Sqlite数据库，也可以使用文件方式，甚至可以使用内存中的一个对象来存储。

6、使用Socket

Socket套接字，是网络通信中的概念，分为流式套接字和用户数据奥套接字两种，对应于网络的传输控制层中的TCP和UDP协议。

两个进程可以通过Socket来实现信息的传输，Socket本身可以支持传输任意字节流。

===选择合适的IPC方式===，如下表

| 名称            | 优点                                                         | 缺点                                                         | 适用场景                                         |
| --------------- | ------------------------------------------------------------ | ------------------------------------------------------------ | ------------------------------------------------ |
| Bundle          | 简单易用                                                     | 只能传输Bundle支持的数据类型                                 | 四大组件的进程间通信                             |
| 文件共享        | 简单易用                                                     | 不适合高并发场景，并且无法做到进程间即时通信                 | 无并发访问清醒，交换简单的数据，实时性不搞的场景 |
| AIDL            | 功能强大，支持一对多并发通信，支持实时通信                   | 使用稍复杂，需要处理好线程同步                               | 一对多通信且有RPC需求                            |
| Messenger       | 功能一般，支持一对多串行通信，支持实时通信                   | 不能很好的处理高并发情形，不支持RPC，数据通过Message进行传输，因此只能传输Bundle支持的数据类型 | 低并发的一对多即时通信，无RPC需求                |
| ContentProvider | 在数据源访问方面功能强大，支持一对多并发数据共享，可通过Call方法扩展其他操作 | 可以理解为受约束的AICL，主要提供数据的CRUD数据               | 一对多的进程间数据共享                           |
| Socket          | 功能强大，可以通过网络传输字节流，支持一对多并发实时通信     | 实现细节稍微繁琐，不支持直接的RPC                            | 网络数据交换                                     |

RPC(Remote Procedure Call，远程过程调用)是种C/S的编程模式，出于一种类比的愿望，在一台机器上运行的主程序，可以调用另一台机器上准备好的子程序。

通过RPC可以充分利用非共享内存的多处理器环境，可以简便地将你的应用分布在多台工作站上，应用程序就像运行在一个多处理器的计算机上一样。

 

 **五、Binder介绍**

Binder是Android系统进程间通信方式之一。Linux已经拥有的进程间通信IPC手段包括： 管道（Pipe）、信号（Signal）、跟踪（Trace）、插口（Socket）、报文队列（Message）、共享内存（Share Memory）和信号量（Semaphore）。

Binder框架定义了四个角色：Server，Client，ServiceManager以及Binder驱动。

其中Server，Client，ServiceManager运行于用户空间，驱动运行于内核空间。Binder就是一种把这四个组件粘合在一起的粘结剂了，其中，核心组件便是Binder驱动程序了，Service Manager提供了辅助管理的功能，Client和Server正是在Binder驱动和Service Manager提供的基础设施上，进行Client-Server之间的通信。这四个角色的关系和互联网类似：Server是服务器，Client是客户终端，ServiceManager是域名服务器（DNS），驱动是路由器。

Binder的理解：

1、从IPC角度来说，Binder是Android中的一种跨进程通信方式，该方式在Linux中没有；

2、从Android Framework角度来说，Binder是ServiceManager连接各种Manager和相应ManagerService的桥梁；

3、从Android应用层来说，Binder是客户端和服务端进行通许in的媒介，当BindService的时候，服务端会返回一个包含了服务端业务调用的Binder对象，通过这个Bind而对象，客户端就可以获取服务端提供的服务或者数据，这里的服务包括普通服务和基于AIDL的服务。

 

在Android开发中，Binder主要用在Service中，包括AIDL和Messenger，普通服务中的Binder不涉及进程间通信。

直观来讲，Binder是Android中的一个类，实现了IBinder接口。

Binder的工作机制：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glzyx30rqpj30kp07wdh2.jpg)

当客户端发起远程请求时，由于当前线程会被挂起直至服务端进程返回数据，所以一个远程方法是很耗时的，那么不能在UI线程中发起此远程请求；

由于服务端的Binder方法运行在Binder的线程池中，所以Binder方法不管是否耗时都应该采用同步的方式去实现，因为它已经运行在一个线程中了。

 

**六、具体实例**

[进程间通信Demo实例](https://github.com/PearLemon/IPCTest)

https://github.com/PearLemon/IPCTest

---

