[TOC]

在Android中，我们将线程大体上分为两种，一种是UI线程，即主线程，在应用启动的时候就分配的线程。它是我们加载UI界面，完成用户交互的线程。另一个就是WorkerThread，除了UI线程外的所有线程都可以这么称呼。在多线程编程中，我们必须遵循这么几个原则：

- 不能在UI线程中执行耗时操作（5s）
- 不能再WorkerThread去操控UI组件

因此我们通常会使用Handler来进行跨线程的通信，至于Handler的使用和原理在这里不是重点。本文介绍的是基于Handler的一个轻量级的异步类AsyncTask，它使得我们创建异步任务不会像Handler那样繁琐。

### AsyncTask使用
AsyncTask字面意思就是异步任务。在我们的UI线程运行的时候，AsyncTask允许我们的执行一个异步的任务在后台。我们可以将耗时的操作放在异步任务当中来执行，并随时将任务执行的结果返回给我们的UI线程来更新我们的UI控件。我们先来看看这个抽象类的原型：

```java
public abstract class AsyncTask<Params, Progress, Result>
```
从代码里面我们可以看到，这个AsyncTask创建的时候需要三个泛型。这三个泛型代表什么意思呢？其实这个字面翻译已经很明显了。

 * Params: 这个泛型指定的是我们传递给异步任务执行时的参数的类型
 * Progress: 这个泛型指定的是我们的异步任务在执行的时候将执行的进度返回给UI线程的参数的类型
 * Result: 这个泛型指定的异步任务执行完后返回给UI线程的结果的类型

这基本都是字面意思的翻译...

下来就是AsyncTask执行时的必要步骤

* onPreExecute(): 这个方法是在执行异步任务之前的时候执行，并且是在UI Thread当中执行的，通常我们在这个方法里做一些UI控件的初始化的操作，例如弹出要给ProgressDialog
* doInBackground(Params... params): 在onPreExecute()方法执行完之后，会马上执行这个方法，这个方法就是来处理异步任务的方法，Android操作系统会在后台的线程池当中开启一个worker thread来执行我们的这个方法，所以这个方法是在worker thread当中执行的，这个方法执行完之后就可以将我们的执行结果发送给我们的最后一个onPostExecute 方法，在这个方法里，我们可以从网络当中获取数据等一些耗时的操作
* onProgressUpdate(Progess... values): 这个方法也是在UI Thread当中执行的，我们在异步任务执行的时候，有时候需要将执行的进度返回给我们的UI界面，例如下载一张网络图片，我们需要时刻显示其下载的进度，就可以使用这个方法来更新我们的进度。这个方法在调用之前，我们需要在 **doInBackground 方法中调用一个 publishProgress(Progress)**的方法来将我们的进度时时刻刻传递给 onProgressUpdate 方法来更新
* onPostExecute(Result... result): 当我们的异步任务执行完之后，就会将结果返回给这个方法，这个方法也是在UI Thread当中调用的，我们可以将返回的结果显示在UI控件上

其中，我们必须要实现的方法是doInBackground，其实我们可以把它看做一个线程来用。为啥必须要实现这个方法呢？因为不实现这个方法的话那还用个毛AsyncTask。。。下面举例来说明这写方法的使用:

```java

    class TestTask extends AsyncTask<Void, Integer, Integer> {
        //开启线程处理
        @Override
        protected Integer doInBackground(Void... params) {
            int result = 8;
            while (true) {
            	   publishProgress(result);
                result += 1;
                SystemClock.sleep(1000);
                if (result == 10) {
                    break;
                }
            }
            return result;
        }

        //在doInBackground执行时调用publishProgress()来执行,运行在UI线程里
        @Override
        protected void onProgressUpdate(Integer... values) {
            super.onProgressUpdate(values);
            ToastUtil.showToast("" + values[0]);
        }

        //执行完后再主线程处理的任务
        @Override
        protected void onPostExecute(Integer integer) {
            super.onPostExecute(integer);
            ToastUtil.showToast(""+integer);
        }
    }
}

```
这是一个下拉加载隐藏ViewHead的一个异步处理，result初始值为8，每1秒来执行onProgressUpdate，显示一下result的值。最后当result到10的话，执行onPostExecute来显示此时的值。其中onProgressUpdata和onPostExecute都是在UI线程里面执行的。
AsyncTask的重要知识点

AsyncTask的一些其他知识点：

1.Cancelling a Task

我们可以在任何时刻来取消我们的异步任务的执行，通过调用 cancel(boolean)方法，调用完这个方法后系统会随后调用 isCancelled() 方法并且返回true。如果调用了这个方法，那么在 doInBackgroud() 方法执行完之后，就不会调用 onPostExecute() 方法了，取而代之的是调用 onCancelled() 方法。为了确保Task已经被取消了，我们需要经常调用 isCancelled() 方法来判断，如果有必要的话。

2.在使用AsyncTask做异步任务的时候必须要遵循的原则：

AsyncTask类必须在UI Thread当中加载，在Android Jelly_Bean版本后这些都是自动完成的
AsyncTask的对象必须在UI Thread当中实例化
execute方法必须在UI Thread当中调用
不要手动的去调用AsyncTask的onPreExecute, doInBackground, publishProgress, onProgressUpdate, onPostExecute方法，这些都是由Android系统自动调用的
AsyncTask任务只能被执行一次。这个只能执行一次的意思是：每一个new出的AsyncTask只能执行一次execute()方法，多次运行将会报错，如需多次，需要新new一个AsyncTask。至于为什么会这样呢？接下来我们会从内部去分析分析AsyncTask的实现原理。


