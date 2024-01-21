[TOC]

### **【ViewPager】**

[ViewPager](http://developer.android.com/reference/android/support/v4/view/ViewPager.html) 如其名所述，是负责翻页的一个 View。准确说是一个 [ViewGroup](http://developer.android.com/reference/android/view/ViewGroup.html)，包含多个 View 页，在手指横向滑动屏幕时，其负责对 View 进行切换。为了生成这些 View 页，需要提供一个 [PagerAdapter](http://developer.android.com/reference/android/support/v4/view/PagerAdapter.html) 来进行和数据绑定以及生成最终的 View 页。

- [**setAdapter()**](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/view/ViewPager.java#ViewPager.setAdapter%28android.support.v4.view.PagerAdapter%29)
  - ViewPager 通过 setAdapter() 来建立与 PagerAdapter 的联系。这个联系是双向的，一方面，ViewPager 会拥有 [PagerAdapter 对象](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/view/ViewPager.java#125)，从而可以在需要时调用 PagerAdapter 的方法；另一方面，ViewPager 会在 setAdapter() 中[调用 PagerAdapter 的 registerDataSetObserver() 方法](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/view/ViewPager.java#370)，注册一个[自己生成的 PagerObserver 对象](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/view/ViewPager.java#368)，从而在 PagerAdapter 有所需要时（如 [notifyDataSetChanged()](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/view/PagerAdapter.java#PagerAdapter.notifyDataSetChanged%28%29)或 notifyDataSetInvalidated() 时），可以调用 Observer 的 onChanged() 或 onInvalidated() 方法，从而实现 PagerAdapter 向 ViewPager 方向发送信息。
- [**dataSetChanged()**](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/view/ViewPager.java#ViewPager.dataSetChanged%28%29)
  - 在 [PagerObserver](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/view/ViewPager.java#ViewPager.PagerObserver).onChanged()，以及 PagerObserver.onInvalide() 中被调用。因此当 PagerAdapter.notifyDataSetChanged() 被触发时，ViewPager.dataSetChanged() 也可以被触发。该函数将[使用 getItemPosition() 的返回值来进行判断](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/view/ViewPager.java#714)，如果为 POSITION_UNCHANGED，则什么都不做；如果为 POSITION_NONE，则调用 PagerAdapter.destroyItem() 来去掉该对象，并设置为需要刷新 ([needPopulate = true](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/view/ViewPager.java#728)) 以便[触发](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/view/ViewPager.java#ViewPager.addNewItem%28int%2Cint%29)PagerAdapter.instantiateItem() 来生成新的对象。



### **【PagerAdapter】**

[PageAdapter](http://developer.android.com/reference/android/support/v4/view/PagerAdapter.html) 是 ViewPager 的支持者，ViewPager 将调用它来取得所需显示的页，而 PageAdapter 也会在数据变化时，通知 ViewPager。这个类也是[FragmentPagerAdapter](http://developer.android.com/reference/android/support/v13/app/FragmentPagerAdapter.html) 以及 [FragmentStatePagerAdapter](http://developer.android.com/reference/android/support/v13/app/FragmentStatePagerAdapter.html) 的基类。如果继承自该类，至少需要实现 instantiateItem(), destroyItem(), getCount() 以及 isViewFromObject()。

- [**getItemPosition()**](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/view/PagerAdapter.java#PagerAdapter.getItemPosition%28java.lang.Object%29)
  - 该函数用以返回给定对象的位置，给定对象是由 instantiateItem() 的返回值。
  - 在 ViewPager.dataSetChanged() 中将对该函数的返回值进行判断，以决定是否最终触发 PagerAdapter.instantiateItem() 函数。
  - 在 PagerAdapter 中的实现是直接传回 [POSITION_UNCHANGED](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/view/PagerAdapter.java#PagerAdapter.0POSITION_UNCHANGED)。如果该函数不被重载，则会一直返回 POSITION_UNCHANGED，从而导致 ViewPager.dataSetChanged() 被调用时，认为不必触发 PagerAdapter.instantiateItem()。很多人因为没有重载该函数，而导致调用
    PagerAdapter.notifyDataSetChanged() 后，什么都没有发生。
- [**instantiateItem()**](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/view/PagerAdapter.java#PagerAdapter.instantiateItem%28android.view.ViewGroup%2Cint%29)
  - 在每次 ViewPager 需要一个**用以显示**的 Object 的时候，该函数都会被 ViewPager.[addNewItem()](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/view/ViewPager.java#ViewPager.addNewItem%28int%2Cint%29) 调用。
- [notifyDataSetChanged()](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/view/PagerAdapter.java#PagerAdapter.notifyDataSetChanged%28%29)
  - 在数据集发生变化的时候，一般 Activity 会调用 PagerAdapter.notifyDataSetChanged()，以通知 PagerAdapter，而 PagerAdapter 则会通知在自己这里[注册过](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/view/PagerAdapter.java#PagerAdapter.registerDataSetObserver%28android.database.DataSetObserver%29)的所有 [DataSetObserver](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/database/DataSetObserver.java#DataSetObserver)。其中之一就是在 ViewPager.setAdapter() 中注册过的 PageObserver。PageObserver 则进而调用 ViewPager.dataSetChanged()，从而导致 ViewPager 开始触发更新**其内含 View** 的操作。



### **【FragmentPagerAdapter】**

[FragmentPagerAdapter](http://developer.android.com/reference/android/support/v13/app/FragmentPagerAdapter.html) 继承自 PagerAdapter。相比通用的 PagerAdapter，该类更专注于每一页均为 Fragment 的情况。如文档所述，**该类内的每一个生成的 Fragment 都将保存在内存之中**，因此适用于那些相对静态的页，数量也比较少的那种；如果需要处理有很多页，并且数据动态性较大、占用内存较多的情况，应该使用[FragmentStatePagerAdapter](http://developer.android.com/reference/android/support/v4/app/FragmentStatePagerAdapter.html)。FragmentPagerAdapter 重载实现了几个必须的函数，因此来自 PagerAdapter 的函数，我们只需要实现 getCount()，即可。且，由于 FragmentPagerAdapter.instantiateItem() 的实现中，调用了一个新增的虚函数 getItem()，因此，我们还至少需要实现一个 getItem()。因此，总体上来说，相对于继承自 PagerAdapter，更方便一些。

- [**getItem()**](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/app/FragmentPagerAdapter.java#FragmentPagerAdapter.getItem%28int%29)
  - 该类中新增的一个虚函数。函数的目的为**生成新的** Fragment 对象。重载该函数时需要注意这一点。在需要时，该函数将被 instantiateItem() 所调用。
  - 如果需要向 Fragment 对象传递相对**静态的数据**时，我们一般通过 Fragment.setArguments() 来进行，这部分代码应当放到 getItem()。它们只会在新生成 Fragment 对象时执行一遍。
  - 如果需要在生成 Fragment 对象后，将**数据集里面一些动态的数据**传递给该 Fragment，那么，这部分代码不适合放到 getItem() 中。因为当数据集发生变化时，往往对应的 Fragment 已经生成，如果传递数据部分代码放到了 getItem() 中，这部分代码将不会被调用。这也是为什么很多人发现调用 PagerAdapter.notifyDataSetChanged() 后，getItem() 没有被调用的一个原因。
- [**instantiateItem()**](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/app/FragmentPagerAdapter.java#FragmentPagerAdapter.instantiateItem%28android.view.ViewGroup%2Cint%29)
  - 函数中[判断一下](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/app/FragmentPagerAdapter.java#90)要生成的 Fragment 是否已经生成过了，如果生成过了，就使用旧的，[旧的将被 Fragment.attach()](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/app/FragmentPagerAdapter.java#95)；如果没有，[就调用 getItem() 生成一个新的](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/app/FragmentPagerAdapter.java#97)，[新的对象将被 FragmentTransation.add()](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/app/FragmentPagerAdapter.java#99)。
  - FragmentPagerAdapter **会将所有生成的 Fragment 对象通过 FragmentManager 保存起来备用**，以后需要该 Fragment 时，都会从 FragmentManager 读取，**而不会再次调用 getItem() 方法**。
  - 如果需要在生成 Fragment 对象后，将数据集中的一些数据传递给该 Fragment，这部分代码应该放到这个函数的重载里。在我们继承的子类中，重载该函数，并调用 FragmentPagerAdapter.instantiateItem() 取得该函数返回 Fragment 对象，然后，我们该 Fragment 对象中对应的方法，将数据传递过去，然后返回该对象。
  - 否则，如果将这部分传递数据的代码放到 getItem()中，在 PagerAdapter.notifyDataSetChanged() 后，这部分数据设置代码将不会被调用。
- [**destroyItem()**](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/app/FragmentPagerAdapter.java#FragmentPagerAdapter.destroyItem%28android.view.ViewGroup%2Cint%2Cjava.lang.Object%29)
  - 该函数被调用后，会对 Fragment **进行 [FragmentTransaction.detach()](http://developer.android.com/reference/android/support/v4/app/FragmentTransaction.html#detach(android.support.v4.app.Fragment))。这里不是 remove()**，只是 detach()，因此 Fragment 还在 FragmentManager 管理中，Fragment 所占用的资源不会被释放。



### **【FragmentStatePagerAdapter】**

[FragmentStatePagerAdapter](http://developer.android.com/reference/android/support/v4/app/FragmentStatePagerAdapter.html) 和前面的 FragmentPagerAdapter 一样，是继承子 PagerAdapter。但是，和 FragmentPagerAdapter 不一样的是，正如其类名中的 'State' 所表明的含义一样，该 PagerAdapter 的实现将只保留当前页面，当页面离开视线后，就会被消除，释放其资源；而在页面需要显示时，生成新的页面(就像 ListView 的实现一样)。这么实现的好处就是当拥有大量的页面时，不必在内存中占用大量的内存。

- [**getItem()**](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/app/FragmentStatePagerAdapter.java#FragmentStatePagerAdapter.getItem%28int%29)
  - 一个该类中新增的虚函数。
  - 函数的目的为生成**新的** Fragment 对象。
  - Fragment.setArguments() 这种只会在新建 Fragment 时执行一次的参数传递代码，可以放在这里。
  - 由于 FragmentStatePagerAdapter.instantiateItem() 在大多数情况下，都将调用 getItem() 来生成新的对象，因此如果在该函数中放置与数据集相关的 setter 代码，基本上都可以在 instantiateItem() 被调用时执行，但这和设计意图不符。毕竟还有[部分可能](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/app/FragmentStatePagerAdapter.java#94)是不会调用 getItem() 的。因此这部分代码应该放到 instantiateItem() 中。
- [**instantiateItem()**](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/app/FragmentStatePagerAdapter.java#FragmentStatePagerAdapter.instantiateItem%28android.view.ViewGroup%2Cint%29)
  - 除非碰到 FragmentManager 刚好从 SavedState 中恢复了对应的 Fragment 的情况外，**该函数将会调用 getItem() 函数，生成新的 Fragment 对象**。新的对象将被 FragmentTransaction.add()。
  - FragmentStatePagerAdapter 就是通过这种方式，**每次都创建一个新的 Fragment，而在不用后就立刻释放其资源，来达到节省内存占用的目的的。**
- [**destroyItem()**](http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/support/v4/app/FragmentStatePagerAdapter.java#FragmentStatePagerAdapter.destroyItem%28android.view.ViewGroup%2Cint%2Cjava.lang.Object%29)
  - 将 Fragment 移除，即调用 [FragmentTransaction.**remove()**](http://developer.android.com/reference/android/support/v4/app/FragmentTransaction.html#remove(android.support.v4.app.Fragment))，并释放其资源。