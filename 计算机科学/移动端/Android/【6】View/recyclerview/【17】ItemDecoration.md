[TOC]

```java
public class cc extends RecyclerView.ItemDecoration {
    @Override
    public void onDraw(@NonNull Canvas c, @NonNull RecyclerView parent, @NonNull RecyclerView.State state) {
        super.onDraw(c, parent, state);
    }

    @Override
    public void onDrawOver(@NonNull Canvas c, @NonNull RecyclerView parent, @NonNull RecyclerView.State state) {
        super.onDrawOver(c, parent, state);
    }

    @Override
    public void getItemOffsets(@NonNull Rect outRect, @NonNull View view, @NonNull RecyclerView parent,
                               @NonNull RecyclerView.State state) {
        super.getItemOffsets(outRect, view, parent, state);
    }
}
```



- `getItemOffests`可以通过`outRect.set(l,t,r,b)`设置指定itemview的`paddingLeft`，`paddingTop`， `paddingRight`， `paddingBottom

- `onDraw`可以通过一系列`c.drawXXX()`方法在绘制itemView**之前**绘制我们需要的内容。

- `onDrawOver`与`onDraw`类似，只不过是在绘制itemView**之后**绘制，具体表现形式，就是绘制的内容在**itemview上层**。



### 链接

- [CSDN](https://blog.csdn.net/briblue/article/details/70161917)