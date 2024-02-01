[TOC]

以后用到就继续查吧。。。。。。。

<!--more-->

```java
SpannableString spannableString = new SpannableString("吧啦吧啦吧啦");//第一步

spannableString.setSpan(new ClickableSpan() {
    @Override
    public void onClick(View widget) {   //点击监听
   
    }
    @Override
    public void updateDrawState(TextPaint ds) {
        super.updateDrawState(ds);		
        ds.setColor(Color.parseColor("#ff8f8f8f"));		//设置颜色 
        ds.setUnderlineText(true);			//设置下划线
    }
}, 1, 2, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);       
tvMessage.setText(spannableString);			//给textView设置spannableString
tvMessage.setMovementMethod(LinkMovementMethod.getInstance());	//设置可点击
```



https://blog.csdn.net/u012702547/article/details/49895157