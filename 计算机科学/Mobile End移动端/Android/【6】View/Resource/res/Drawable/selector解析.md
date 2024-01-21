

Selector是定义 `StateListDrawable` 的标签，该Drawable代表着一个Drawable的集合，每一个Drawable都对应着View的一个状态，系统会根据View的状态来选择相应的Drawable，该Drawable常常会被用于做View的背景。通过 `<Item>` 标签来定义View对象的状态。 

shape只能定义单一的形状，而实际应用中，很多地方比如按钮、Tab、ListItem等都是不同状态有不同的展示形状 ，而实际应用中，很多地方比如按钮、Tab、ListItem等都是不同状态有不同的展示形状。举个例子，一个按钮的背景，默认时是一个形状，按下时是一个形状，不可操作时又是另一个形状。有时候，不同状态下改变的不只是背景、图片等，文字颜色也会相应改变。而要处理这些不同状态下展示什么的问题，就要用selector来实现了。 

<!--more-->

代表View的状态和与状态相对应的Drawable。一个View主要以下的状态，每一个状态都是一个布尔值。

| 状态                                                         | 描述                                                         |
| ------------------------------------------------------------ | ------------------------------------------------------------ |
| [android:state_activated](https://developer.android.google.cn/reference/android/graphics/drawable/StateListDrawable.html#attr_android:state_activated) | 表示View是否处于激活状态                                     |
| [android:state_active](https://developer.android.google.cn/reference/android/graphics/drawable/StateListDrawable.html#attr_android:state_active) | 当一个View被视为是活动的                                     |
| [android:state_checkabl](https://developer.android.google.cn/reference/android/graphics/drawable/StateListDrawable.html#attr_android:state_checkable) | 是否是可以被想选中的，用于ChekBox                            |
| [android:state_checked](https://developer.android.google.cn/reference/android/graphics/drawable/StateListDrawable.html#attr_android:state_checked) | CheckBox/RadioButton是否被选中                               |
| [android:state_enabled](https://developer.android.google.cn/reference/android/graphics/drawable/StateListDrawable.html#attr_android:state_enabled) | View 是否是可用状态                                          |
| [android:state_first](https://developer.android.google.cn/reference/android/graphics/drawable/StateListDrawable.html#attr_android:state_first) | 当一个视图为有序列表的第一个元素的时候                       |
| [android:state_focused](https://developer.android.google.cn/reference/android/graphics/drawable/StateListDrawable.html#attr_android:state_focused) | 当View获得焦点的时候                                         |
| [android:state_last](https://developer.android.google.cn/reference/android/graphics/drawable/StateListDrawable.html#attr_android:state_last) | 当一个视图为有序列表的最后一个元素的时候                     |
| [android:state_middle](https://developer.android.google.cn/reference/android/graphics/drawable/StateListDrawable.html#attr_android:state_middle) | 当View位于有序集合的中间位置时                               |
| [android:state_pressed](https://developer.android.google.cn/reference/android/graphics/drawable/StateListDrawable.html#attr_android:state_pressed) | View是否被按压                                               |
| [android:state_selected](https://developer.android.google.cn/reference/android/graphics/drawable/StateListDrawable.html#attr_android:state_selected) | View是否被选中                                               |
| [android:state_single](https://developer.android.google.cn/reference/android/graphics/drawable/StateListDrawable.html#attr_android:state_single) | State value for StateListDrawable, set when a view or drawable is considered "single" by its host. |
| [android:state_window_focused](https://developer.android.google.cn/reference/android/graphics/drawable/StateListDrawable.html#attr_android:state_window_focused) | 当Window获得焦点时                                           |

- **android:state_enabled**: 设置触摸或点击事件是否可用状态，一般只在false时设置该属性，表示不可用状态
- **android:state_pressed**: 设置是否按压状态，一般在true时设置该属性，表示已按压状态，默认为false
- **android:state_selected**: 设置是否选中状态，true表示已选中，false表示未选中
- **android:state_checked**: 设置是否勾选状态，主要用于CheckBox和RadioButton，true表示已被勾选，false表示未被勾选
- **android:state_checkable**: 设置勾选是否可用状态，类似state_enabled，只是state_enabled会影响触摸或点击事件，而state_checkable影响勾选事件
- **android:state_focused**: 设置是否获得焦点状态，true表示获得焦点，默认为false，表示未获得焦点
- **android:state_window_focused**: 设置当前窗口是否获得焦点状态，true表示获得焦点，false表示未获得焦点，例如拉下通知栏或弹出对话框时，当前界面就会失去焦点；另外，ListView的ListItem获得焦点时也会触发true状态，可以理解为当前窗口就是ListItem本身
- **android:state_activated**: 设置是否被激活状态，true表示被激活，false表示未激活，API Level 11及以上才支持，可通过代码调用控件的setActivated(boolean)方法设置是否激活该控件
- **android:state_hovered**: 设置是否鼠标在上面滑动的状态，true表示鼠标在上面滑动，默认为false，API Level 14及以上才支持

### 一个例子

```xml
<?xml version="1.0" encoding="utf-8"?>
<selector xmlns:android="http://schemas.android.com/apk/res/android">

    <item android:state_pressed="true">
        <shape android:shape="rectangle">
            <solid android:color="@color/item_common_pressed"/>
            <corners android:radius="21dp" />
        </shape>
    </item>

    <item>
        <shape android:shape="rectangle">
            <solid android:color="#d2f5f8"/>
            <corners android:radius="21dp"/>
        </shape>
    </item>
</selector>
```

点击按钮背景由蓝绿色变为灰色。