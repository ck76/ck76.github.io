[TOC]

### 1、源码

> TagFlowLayout

```java
public class TagFlowLayout extends FlowLayout
		//实现了Adapter里的接口，在Adapter代用notifyDataChanged()时
		//会调用listener的onChange方法
        implements TagAdapter.OnDataChangedListener {

    private TagAdapter mTagAdapter;
    private int mSelectedMax = -1;//-1为不限制数量
    private static final String TAG = "TagFlowLayout";

    private Set<Integer> mSelectedView = new HashSet<Integer>();

	//选择Listener和点击Listener
    private OnSelectListener mOnSelectListener;
    private OnTagClickListener mOnTagClickListener;

	//setOnSelectListener
    //setOnTagClickListener
    public interface OnSelectListener {
        void onSelected(Set<Integer> selectPosSet);
    }
    public interface OnTagClickListener {
        boolean onTagClick(View view, int position, FlowLayout parent);
    }
    
    //构造函数...
	//onMeasure函数  tagContainer或者tagView为GONE的时候就把二者都设置成GONE
 	
 	//程序的入口方法
    public void setAdapter(TagAdapter adapter) {
        mTagAdapter = adapter;
        mTagAdapter.setOnDataChangedListener(this);
        mSelectedView.clear();
        changeAdapter();
    }

    @SuppressWarnings("ResourceType")
    private void changeAdapter() {
    	//ViewGroup方法，移除所有子View
        removeAllViews();
        TagAdapter adapter = mTagAdapter;
        TagView tagViewContainer = null;
        //从adapter获取预选项
        HashSet preCheckedList = mTagAdapter.getPreCheckedList();
        //循环，将所有的Adapter返回的View加入TagFlowLayout中
        for (int i = 0; i < adapter.getCount(); i++) {
            View tagView = adapter.getView(this, i, adapter.getItem(i));
            tagViewContainer = new TagView(getContext());
            //将此属性设置为 true 时，这个 View 将从其父容器而非自身获取绘制状态（焦点、点击等）
            tagView.setDuplicateParentStateEnabled(true);
            if (tagView.getLayoutParams() != null) {
                tagViewContainer.setLayoutParams(tagView.getLayoutParams());
            } else {
                MarginLayoutParams lp = new MarginLayoutParams(
                        LayoutParams.WRAP_CONTENT,
                        LayoutParams.WRAP_CONTENT);
                lp.setMargins(dip2px(getContext(), 5),
                        dip2px(getContext(), 5),
                        dip2px(getContext(), 5),
                        dip2px(getContext(), 5));
                tagViewContainer.setLayoutParams(lp);
            }
            LayoutParams lp = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
            tagView.setLayoutParams(lp);
            //将adapter返回的view加入TagView即Container
            tagViewContainer.addView(tagView);
            //将container（TagView）加入TagFlowLayout
            addView(tagViewContainer);

            if (preCheckedList.contains(i)) {
                setChildChecked(i, tagViewContainer);
            }
			//全选或者全不选，默认全不选
            if (mTagAdapter.setSelected(i, adapter.getItem(i))) {
                setChildChecked(i, tagViewContainer);
            }
            //设置点击事件
            tagView.setClickable(false);
            final TagView finalTagViewContainer = tagViewContainer;
            final int position = i;
            tagViewContainer.setOnClickListener(new OnClickListener() {
                @Override
                public void onClick(View v) {
                    doSelect(finalTagViewContainer, position);
                    if (mOnTagClickListener != null) {
                        mOnTagClickListener.onTagClick(finalTagViewContainer, position,
                                TagFlowLayout.this);
                    }
                }
            });
        }
        mSelectedView.addAll(preCheckedList);
    }
	//设置最大能选值
    public void setMaxSelectCount()
	//获取已经选择的集合
    public Set<Integer> getSelectedList() 
	//设置checked与unchecked
    private void setChildChecked(int position, TagView view) {
        view.setChecked(true);
        mTagAdapter.onSelected(position, view.getTagView());
    }

	//点击即调用
	//然后根据item的点击状态做出反应 
    private void doSelect(TagView child, int position) {
        if (!child.isChecked()) {
            //处理max_select=1的情况
            if (mSelectedMax == 1 && mSelectedView.size() == 1) {
                Iterator<Integer> iterator = mSelectedView.iterator();
                Integer preIndex = iterator.next();
                TagView pre = (TagView) getChildAt(preIndex);
                setChildUnChecked(preIndex, pre);
                setChildChecked(position, child);

                mSelectedView.remove(preIndex);
                mSelectedView.add(position);
            } else {
                if (mSelectedMax > 0 && mSelectedView.size() >= mSelectedMax) {
                    return;
                }
                //设置选择，加入mSelectedView
                setChildChecked(position, child);
                mSelectedView.add(position);
            }
        } else {
        	//反之
            setChildUnChecked(position, child);
            mSelectedView.remove(position);
        }
        if (mOnSelectListener != null) {
            mOnSelectListener.onSelected(new HashSet<Integer>(mSelectedView));
        }
    }

    public TagAdapter getAdapter() {
        return mTagAdapter;
    }

    private static final String KEY_CHOOSE_POS = "key_choose_pos";
    private static final String KEY_DEFAULT = "key_default";


    @Override
    protected Parcelable onSaveInstanceState() {
        Bundle bundle = new Bundle();
        bundle.putParcelable(KEY_DEFAULT, super.onSaveInstanceState());

        String selectPos = "";
        if (mSelectedView.size() > 0) {
            for (int key : mSelectedView) {
                selectPos += key + "|";
            }
            selectPos = selectPos.substring(0, selectPos.length() - 1);
        }
        bundle.putString(KEY_CHOOSE_POS, selectPos);
        return bundle;
    }

    @Override
    protected void onRestoreInstanceState(Parcelable state) {
        if (state instanceof Bundle) {
            Bundle bundle = (Bundle) state;
            String mSelectPos = bundle.getString(KEY_CHOOSE_POS);
            if (!TextUtils.isEmpty(mSelectPos)) {
                String[] split = mSelectPos.split("\\|");
                for (String pos : split) {
                    int index = Integer.parseInt(pos);
                    mSelectedView.add(index);

                    TagView tagView = (TagView) getChildAt(index);
                    if (tagView != null) {
                        setChildChecked(index, tagView);
                    }
                }

            }
            super.onRestoreInstanceState(bundle.getParcelable(KEY_DEFAULT));
            return;
        }
        super.onRestoreInstanceState(state);
    }


    @Override
    public void onChanged() {
        mSelectedView.clear();
        changeAdapter();
    }

    public static int dip2px(Context context, float dpValue) {
        final float scale = context.getResources().getDisplayMetrics().density;
        return (int) (dpValue * scale + 0.5f);
    }
}
```



### tips

- [android:duplicateParentState属性](https://blog.csdn.net/mrchx/article/details/49743881)

  将此属性设置为 true 时，这个 View 将从其父容器而非自身获取绘制状态（焦点、点击等）。

