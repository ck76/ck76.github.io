[TOC]

### 1、源码

> 唯一需要实现的就是getView方法，返回TagView的contentView

```java
public abstract class TagAdapter<T> {
    private List<T> mTagDatas;
    private OnDataChangedListener mOnDataChangedListener;

    public TagAdapter(List<T> datas) {
        mTagDatas = datas;
    }

    //隐藏了Deprecated代码以及OnDataChangedListener操作
    
    public int getCount() {
        return mTagDatas == null ? 0 : mTagDatas.size();
    }

    public void notifyDataChanged() {
        if (mOnDataChangedListener != null)
            mOnDataChangedListener.onChanged();
    }

    public T getItem(int position) {
        return mTagDatas.get(position);
    }

    //唯一需要实现的方法，返回TagView的Content
    public abstract View getView(FlowLayout parent, int position, T t);

    public void onSelected(int position, View view){
        Log.d("zhy","onSelected " + position);
    }

    public void unSelected(int position, View view){
        Log.d("zhy","unSelected " + position);
    }

    public boolean setSelected(int position, T t) {
        return false;
    }
}
```

