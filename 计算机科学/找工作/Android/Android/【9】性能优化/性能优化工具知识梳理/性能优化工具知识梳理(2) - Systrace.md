[TOC]

# 一、概述

`Systace`是`Android`推出的性能优化工具，通过这个工具我们可以在实时操作的情况下，获得某段时间内当前系统各个进程的运行时情况，通过分析所生成的报表，我们可以定位出`App`卡顿的原因。下面，我们分以下两部分介绍`Systrace`的相关知识：

- 获取分析报表
- 分析

# 二、获取分析报表

第一步，就是要获取分析报表，这个报表其实就是一个`html`网页的形式，需要用`Chrome`浏览器来打开，我们可以通过下面几种方法获取：

## 2.1 魅族手机提供的获取方式

- 第一步：在`Flyme6`的手机上，进入**/设置/辅助功能/开发者选项/性能优化/性能日志抓取**，可以设置是否打开开关以及抓取的时间长度，设置完毕后按下**音量下+电源键**，这时候手机会震动一次，说明系统就开始在后台进行跟踪了。

- 第二步：进入需要分析的应用，进行一系列的操作，在过了设置的时间之后，手机会再次震动一次。

- 第三步：进入文件管理器根目录下的

  ```
  PerformanceLog
  ```

  文件夹，查看距离当前时间最近的文件夹，我们可以看到里面有两个文件，里面就是我们上次抓取的结果，我们需要把它拷贝出来导出到电脑上面：

   

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn6ogr26j30gs01ot8u.jpg)

- 第四步：上面得到的这两个文件并不能直接打开，我们需要使用到`Android SDK`中提供的转换脚本转换上图当中的`systrace`文件，`systrace.py`，如果安装了`Android SDK`，那么进入`SDK目录/platform-tools/systrace/`，执行下面的命令：



```jsx
python systrace.py -o <转换后需要保存到的文件路径> --from-file=<手机导出的systrace文件路径> 
```

例如，执行下面的语句，得到转换后的分析报表`mobile_systrace`

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn6mfa7ej30xc025gmp.jpg)

在`Windows`下的路径写法为：



```java
python systrace.py -o .\result\1.html --from-file=.\source\20181027-105336.systrace
```

## 2.2 直接采用脚本抓取

上面`2.1`方法的优点是简便，随时随地都可以抓取日志，并且不要求手机有`root`权限，而如果我们是在电脑前面调试，那么可以通过上面这个脚本，运行更加细致的配置，例如配置抓取的时间，抓取的范围等等：



```jsx
python systrace.py --time=<抓取时间长度> -o <转换后所需要保存到的文件路径> sched gfx view wm
```

执行上面的命令后，然后去到要分析的界面进行操作，当到达指定的时间之后，就会在指定的目录下生成`*.html`文件，我们可以直接打开`python_systrace`文件，不需要进行转换：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn6iw93sj30xc02gq4e.jpg)

## 2.3 `systrace.py`可接受参数

`systrace.py`提供了很多可配置的参数，下面是官方文档中对于参数的说明，在使用`2.2`方式的时候，我们可以根据需要选取参数进行配置：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn6h0dr8j30f10l3tbj.jpg)



![img](https://upload-images.jianshu.io/upload_images/1949836-54fb03d7a95c2a6f.png?imageMogr2/auto-orient/strip|imageView2/2/w/535)



# 三、具体分析

## 3.1 分析动画卡顿问题

我们使用下面这段代码，在动画执行的过程中，同时进行`IO`操作，以模拟动画卡顿的问题：



```java
    private void startAnimator() {
        ValueAnimator valueAnimator = ValueAnimator.ofFloat(0, 1f);
        valueAnimator.addUpdateListener(new ValueAnimator.AnimatorUpdateListener() {

            @Override
            public void onAnimationUpdate(ValueAnimator animation) {
                mTv.setAlpha((float) animation.getAnimatedValue());
                writeSomething();
            }
        });
        valueAnimator.setDuration(1000);
        valueAnimator.start();
    }

    private void writeSomething() {
        OutputStream outputStream = null;
        try {
            outputStream = openFileOutput("systrace", MODE_APPEND);
            DataOutputStream dataOutputStream = new DataOutputStream(outputStream);
            for (int i = 0; i < 10000; i++) {
                dataOutputStream.write(10);
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                if (outputStream != null) {
                    outputStream.close();
                }
            } catch (Exception e) {
                e.printStackTrace();
            }

        }
    }
```

之后，按照上面的方法导出`systrace`文件，找到我们应用包名所对应的进程，可以看到许多红色`F`，这些就表明遇到了卡顿，如果正常时应当是绿色，如果出现了轻微卡顿，那么为黄色，严重卡顿则为红色：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn6f8t0aj30q206s74v.jpg)


而我们点击红色的`F`，它会给出我们对应的建议：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn6duqp1j30dr08974x.jpg)


我们也可以点击旁边的`alert`选项，它会列出所有的问题点：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn6cg62fj307l0cwglq.jpg)



------

## 更多文章，欢迎访问我的 **Android** 知识梳理系列：

- **Android** 知识梳理目录：[http://www.jianshu.com/p/fd82d18994ce](https://www.jianshu.com/p/fd82d18994ce)
- 个人主页：[http://lizejun.cn](http://lizejun.cn/)
- 个人知识总结目录：http://lizejun.cn/categories/



作者：泽毛
链接：https://www.jianshu.com/p/41bb27235921
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。