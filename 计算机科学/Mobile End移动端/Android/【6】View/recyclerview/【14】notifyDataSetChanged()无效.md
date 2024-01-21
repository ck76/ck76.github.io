最近遇到了notifyDataSetChanged()无效的情况。这是为什么呢？

我们知道setAdapter()的时候，我们传入的Adapter参数一般都会绑定一个集合作为数据源，如下：

```java
    mPopWAdapter = new ProductPopWAdapter(mPopWDatas, mPopWStatus);
    mPopWList.setAdapter(mPopWAdapter);
```
然后我们进行如下操作：

```java
    mPopWDatas = new ArrayList<>();
    mPopWAdapter.notifyDataSetChanged();
```
这时候发现notifyDataSetChanged()无效，这是因为一开始mPopWDatas指向了一个集合，mPopWAdapter绑定数据源的时候就是绑定了mPopWDatas指向的这个集合，而后来的操作只是改变了mPopWDatas的指向，让mPopWDatas指向了一个新的集合。

但是并没有改变mPopWDatas原来所指向的集合。而我们的mPopWAdapter绑定的是原来的集合，原来的集合没有产生任何变化，所以notifyDataSetChanged()无效。

比如把一个String作为了数据源，传给了Adapter，然后再改变String的值，notifyDataSetChanged()也是无效的，因为不能改变原来String对象本身。