![View的滑动1](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/UHhJnqi4YSOXelfwaiIdmfdocJ6VrD6L8Kib7c7ezG0!/r/dFMBAAAAAAAA)

scroll本身就是滚动的内容，view本身的位置没有改变。至于正负关系的问题，可以这样解释： 在移动平台中，要明确知道**“滑动”与“滚动”的不同**，具体来说，滑动和滚动的方向总是相反的 

- **移动：**英文翻译为translate，在Android的动画中有移动的动画(但是其中的参数直接使用的from-&amp;gt;to的方式)，表示将对象移动至目的地 

- **滚动：**英文翻译为scroll，其实我们通常看滚动条就可以看出来，当滚动条向下移动时，屏幕原本显示的内容向上移动；当滚动条向左移动时，屏幕原本显示的内容向右移动。**这也就可以解释为什么scrollTo(int x, int y)中为什么x>0时，view向左移动；y>0时，view，向上移动了。**



### 链接

- https://www.jianshu.com/p/acdde6152cae

- https://blog.csdn.net/znouy/article/details/51338256

- https://blog.csdn.net/linmiansheng/article/details/17767795



```java
public class HorizontalView extends ViewGroup {
    private int lastX;
    private int lastY;
    private int currentIndex = 0; //当前子元素
    private int childWidth = 0;
    private Scroller scroller;
    private VelocityTracker tracker;    //增加速度检测,如果速度比较快的话,就算没有滑动超过一半的屏幕也可以
    private int lastInterceptX=0;
    private int lastInterceptY=0;
    public HorizontalView(Context context) {
        super(context);
        init();
    }
    public HorizontalView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }
    public HorizontalView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init();
    }

    public void init() {
        scroller = new Scroller(getContext());
        tracker = VelocityTracker.obtain();
    }

    @Override
    public boolean onInterceptTouchEvent(MotionEvent event) {
        boolean intercept = false;
        int x = (int) event.getX();
        int y = (int) event.getY();
        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                intercept = false;
                //如果scroller没有执行完毕,则打断
                if (!scroller.isFinished()) {
                    scroller.abortAnimation();
                }
                break;
            case MotionEvent.ACTION_MOVE:
                int deltaX = x - lastInterceptX;
                int deltaY = y - lastInterceptY;
                //水平方向距离长  MOVE中返回true一次,后续的MOVE和UP都不会收到此请求
                if (Math.abs(deltaX) - Math.abs(deltaY) > 0) {
                    intercept = true;
                } else {
                    intercept = false;
                }
                break;
            case MotionEvent.ACTION_UP:
                intercept = false;
                break;
        }
        lastX = x;
        lastY = y;
        lastInterceptX = x;
        lastInterceptY = y;
        return intercept;
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        tracker.addMovement(event);
        int x = (int) event.getX();
        int y = (int) event.getY();
        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                if (!scroller.isFinished()) {
                    scroller.abortAnimation();
                }
                break;
            case MotionEvent.ACTION_MOVE:
                //跟随手指滑动
                int deltaX = x - lastX;
                scrollBy(-deltaX, 0);
                break;
            //释放手指以后开始自动滑动到目标位置
            case MotionEvent.ACTION_UP:
                //相对于当前View滑动的距离,正为向左,负为向右
                int distance = getScrollX() - currentIndex * childWidth;

                //必须滑动的距离要大于1/2个宽度,否则不会切换到其他页面
                if (Math.abs(distance) > childWidth / 2) {
                    if (distance > 0) {
                        currentIndex++;
                    } else {
                        currentIndex--;
                    }
                } else {
                    tracker.computeCurrentVelocity(1000);
                    float xV = tracker.getXVelocity();
                    if (Math.abs(xV) > 50) {
                        if (xV > 0) {
                            currentIndex--;
                        } else {
                            currentIndex++;
                        }
                    }
                }
                currentIndex = currentIndex < 0 ? 0 : currentIndex > getChildCount() - 1 ? getChildCount() - 1 : currentIndex;
                smoothScrollTo(currentIndex * childWidth, 0);
                tracker.clear();
                break;
            default:
                break;
        }
        lastX = x;
        lastY = y;
        return true;
    }
    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
        int widthMode = MeasureSpec.getMode(widthMeasureSpec);
        int widthSize = MeasureSpec.getSize(widthMeasureSpec);
        int heightMode = MeasureSpec.getMode(heightMeasureSpec);
        int heightSize = MeasureSpec.getSize(heightMeasureSpec);
        //测量所有子元素
        measureChildren(widthMeasureSpec, heightMeasureSpec);
        //处理wrap_content的情况
        if (getChildCount() == 0) {
            setMeasuredDimension(0, 0);
        } else if (widthMode == MeasureSpec.AT_MOST && heightMode == MeasureSpec.AT_MOST) {
            View childOne = getChildAt(0);
            int childWidth = childOne.getMeasuredWidth();
            int childHeight = childOne.getMeasuredHeight();
            setMeasuredDimension(childWidth * getChildCount(), childHeight);
        } else if (widthMode == MeasureSpec.AT_MOST) {
            View childOne = getChildAt(0);
            int childWidth = childOne.getMeasuredWidth();
            setMeasuredDimension(childWidth * getChildCount(), heightSize);
        } else if (heightMode == MeasureSpec.AT_MOST) {
            int childHeight = getChildAt(0).getMeasuredHeight();
            setMeasuredDimension(widthSize, childHeight);
        }
    }
    @Override
    public void computeScroll() {
        super.computeScroll();
        if (scroller.computeScrollOffset()) {
            scrollTo(scroller.getCurrX(), scroller.getCurrY());
            postInvalidate();
        }
    }
    public void smoothScrollTo(int destX, int destY) {
        int deltaX = destX - getScrollX();
        int deltaY =  destY - getScrollY();
        scroller.startScroll(getScrollX(), getScrollY(), deltaX,deltaY, 1000);
        invalidate();
    }
    @Override
    protected void onLayout(boolean changed, int l, int t, int r, int b) {
        int childCount = getChildCount();
        int left = 0; //左边的距离
        View child;
        //遍历布局子元素
        for (int i = 0; i < childCount; i++) {
            child = getChildAt(i);
            if (child.getVisibility() != View.GONE) {
                int width = child.getMeasuredWidth();
                childWidth = width; //赋值给子元素宽度变量
                child.layout(left, 0, left + width, child.getMeasuredHeight());
                left += width;
            }
        }
    }
}
```

