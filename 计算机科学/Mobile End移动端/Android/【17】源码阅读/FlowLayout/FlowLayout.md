[TOC]

### 1、源码

> 关键代码在布局和测量

```java
public class FlowLayout extends ViewGroup {
    private static final String TAG = "FlowLayout";
    private static final int LEFT = -1;
    private static final int CENTER = 0;
    private static final int RIGHT = 1;

    //所有的View，以行为单位
    protected List<List<View>> mAllViews = new ArrayList<List<View>>();
    //一行view
	private List<View> lineViews = new ArrayList<>();
    protected List<Integer> mLineHeight = new ArrayList<Integer>();
    protected List<Integer> mLineWidth = new ArrayList<Integer>();
    private int mGravity;

 	//...

    //见tips
    @Override
    public LayoutParams generateLayoutParams(AttributeSet attrs) {
        return new MarginLayoutParams(getContext(), attrs);
    }

    @Override
    protected LayoutParams generateDefaultLayoutParams() {
        return new MarginLayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
    }

    @Override
    protected LayoutParams generateLayoutParams(LayoutParams p) {
        return new MarginLayoutParams(p);
    }
}
```



### 2、onMeasure

> 精确模式用sizeWidth/Height
>
> AT_MOST模式就用width+padding
>
> width由每个TagVIew的宽度累加得到，在所有行中选出一个最大值

```java
	@Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        int sizeWidth = MeasureSpec.getSize(widthMeasureSpec);
        int modeWidth = MeasureSpec.getMode(widthMeasureSpec);
        int sizeHeight = MeasureSpec.getSize(heightMeasureSpec);
        int modeHeight = MeasureSpec.getMode(heightMeasureSpec);

        // 支持wrap_content，FlowLayout的宽和高
        int width = 0;
        int height = 0;
        
		//行宽和行高
        int lineWidth = 0;
        int lineHeight = 0;
		
        int cCount = getChildCount();

        //遍历所有的child
        for (int i = 0; i < cCount; i++) {
            View child = getChildAt(i);
            if (child.getVisibility() == View.GONE) {
                //避免只有一个child
                //FlowLayout指定为Wrap_Content
                //这个时候会出bug，FlowLayout显示不出来
                if (i == cCount - 1) {
                    width = Math.max(lineWidth, width);
                    height += lineHeight;
                }
                continue;
            }
            
            //对孩子进行测量
            measureChild(child, widthMeasureSpec, heightMeasureSpec);
            //获取孩子的LayoutParams用于处理Margin情况
            MarginLayoutParams lp = (MarginLayoutParams) child
                    .getLayoutParams();

            //孩子的宽和高，包括Margin
            int childWidth = child.getMeasuredWidth() + lp.leftMargin
                    + lp.rightMargin;
            int childHeight = child.getMeasuredHeight() + lp.topMargin
                    + lp.bottomMargin;
            
			//如果行宽+孩子的宽度比FlowLayout的宽度大
            //另起一行
            if (lineWidth + childWidth > sizeWidth - getPaddingLeft() - getPaddingRight()) {
                //这样做总是使FlowLayout的宽度保持为最大宽度
                width = Math.max(width, lineWidth);
                //此时另起一行，行宽为单独的一个的child的宽度
                lineWidth = childWidth;
                //FlowLayout高度++
                height += lineHeight;
                //行高=child高度
                lineHeight = childHeight;
            } 
            
            //正常情况下：
            //逐步累加child的宽度得到行宽
            else {
                lineWidth += childWidth;
                lineHeight = Math.max(lineHeight, childHeight);
            }
            
            //避免只有一个child
            //FlowLayout指定为Wrap_Content
            //这个时候会出bug，FlowLayout显示不出来
            if (i == cCount - 1) {
                width = Math.max(lineWidth, width);
                height += lineHeight;
            }
        }
        
        setMeasuredDimension(
                //
                modeWidth == MeasureSpec.EXACTLY ? sizeWidth : width + getPaddingLeft() + getPaddingRight(),
                modeHeight == MeasureSpec.EXACTLY ? sizeHeight : height + getPaddingTop() + getPaddingBottom()//
        );

    }
```



### 3、onLayout

> 通过循环对所有child进行布局，分为两部分：
>
> 将child添加到对应的集合中
>
> 双层循环对所有child根据约束进行布局

```java
	 @Override
    protected void onLayout(boolean changed, int l, int t, int r, int b) {
        mAllViews.clear();
        mLineHeight.clear();
        mLineWidth.clear();
        lineViews.clear();

        //FlowLayout的宽度
        int width = getWidth();
		//行宽行高
        int lineWidth = 0;
        int lineHeight = 0;

        int cCount = getChildCount();

        //遍历将child加到对应的集合
        for (int i = 0; i < cCount; i++) {
            View child = getChildAt(i);
            if (child.getVisibility() == View.GONE) continue;
            MarginLayoutParams lp = (MarginLayoutParams) child
                    .getLayoutParams();

            int childWidth = child.getMeasuredWidth();
            int childHeight = child.getMeasuredHeight();
			//一行的child超过行宽后
            //进行添加操作
            if (childWidth + lineWidth + lp.leftMargin + lp.rightMargin > width - getPaddingLeft() - getPaddingRight()) {
                mLineHeight.add(lineHeight);
                mAllViews.add(lineViews);
                mLineWidth.add(lineWidth);

                lineWidth = 0;
                lineHeight = childHeight + lp.topMargin + lp.bottomMargin;
                lineViews = new ArrayList<View>();
            }
            lineWidth += childWidth + lp.leftMargin + lp.rightMargin;
            lineHeight = Math.max(lineHeight, childHeight + lp.topMargin
                    + lp.bottomMargin);
            lineViews.add(child);

        }
        mLineHeight.add(lineHeight);
        mLineWidth.add(lineWidth);
        mAllViews.add(lineViews);


        int left = getPaddingLeft();
        int top = getPaddingTop();

        int lineNum = mAllViews.size();
		//行遍历
        for (int i = 0; i < lineNum; i++) {
            lineViews = mAllViews.get(i);
            lineHeight = mLineHeight.get(i);

            // set gravity
            int currentLineWidth = this.mLineWidth.get(i);
            switch (this.mGravity) {
                case LEFT:
                    left = getPaddingLeft();
                    break;
                case CENTER:
                    left = (width - currentLineWidth) / 2 + getPaddingLeft();
                    break;
                case RIGHT:
                    left = width - currentLineWidth + getPaddingLeft();
                    break;
            }
            //列遍历
            for (int j = 0; j < lineViews.size(); j++) {
                View child = lineViews.get(j);
                if (child.getVisibility() == View.GONE) {
                    continue;
                }

                MarginLayoutParams lp = (MarginLayoutParams) child
                        .getLayoutParams();

                int lc = left + lp.leftMargin;
                int tc = top + lp.topMargin;
                int rc = lc + child.getMeasuredWidth();
                int bc = tc + child.getMeasuredHeight();
				//对child进行布局
                child.layout(lc, tc, rc, bc);

                left += child.getMeasuredWidth() + lp.leftMargin
                        + lp.rightMargin;
            }
            top += lineHeight;
        }
    }
```



### tips

- generateDefaultLayoutParams()方法

  调用`addView`的方法时，不指定`LayoutParams`，是通过`generateDefaultLayoutParams()`获取默认的`LayoutParams`属性的。

  这个方法主要是用于父容器添加子View时调用，用于生成和此容器类型相匹配的布局参数类。