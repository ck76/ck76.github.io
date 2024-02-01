[TOC]

### 一、云想的webview是用的开源库agent

[项目地址](https://github.com/Justson/AgentWeb)



### 二、模块结构

```java
.
├── AndroidManifest.xml
├── java
│   └── com
│       └── neuqer
│           └── android
│               └── webview
│                   ├── SimpleWebLayout.java	//简单WebView容器
│                   └── WebViewActivity.java	//带有WebView的Activity，基于AgentWeb实现
└── res
    ├── layout
    │   └── simple_web_layout.xml				//自定义的webview布局
    └── values
        └── strings.xml
```



### 三、具体分析

#### 1、WebViewActivity--核心

```java
public abstract class WebViewActivity extends AbsActivity {

    /** WebView容器 */
    private ViewGroup mWebViewParent;
    /** AgentWebView */
    private AgentWeb.PreAgentWeb mPreAgentWeb;
    /** AgentWebView */
    private AgentWeb mAgentWeb;

    @Override
    protected void initView() {
        mWebViewParent = findViewById(getWebViewParent());
        if (mWebViewParent == null) {
            throw new IllegalStateException("WebView容器不能为空");
        }
        mPreAgentWeb = AgentWeb.with(this)
                .setAgentWebParent(mWebViewParent, new LinearLayout.LayoutParams(ViewGroup.LayoutParams
                        .MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT))
                .useDefaultIndicator()
                .setWebChromeClient(getWebChromeClient())
                .setWebViewClient(getWebViewClient())
                .setMainFrameErrorView(R.layout.agentweb_error_page, -1)
                .setSecurityType(AgentWeb.SecurityType.STRICT_CHECK)
                .setWebLayout(getWebLayout())
                .setOpenOtherPageWays(DefaultWebClient.OpenOtherPageWays.ASK)
                .interceptUnkownUrl()
                .createAgentWeb()
                .ready();
    }

    /**
     * 加载URL地址
     *
     * @param url url地址
     */
    protected void loadUrl(String url) {
        if (TextUtils.isEmpty(url)) {
            return;
        }
        if (mAgentWeb == null) {
            mAgentWeb = mPreAgentWeb.go(url);
        } else {
            mAgentWeb.getUrlLoader().loadUrl(url);
        }
    }

    /**
     * 获取WebViewClient，子类按需重写
     * 获取WebChromeClient，子类按需重写
     * 获取WebLayout，子类可重写，比如添加下拉刷新等操作
     */
    private IWebLayout getWebLayout() {
        return new SimpleWebLayout(this);
    }
    
    //......

    /**
     * 获取WebView容器
     */
    @IdRes
    protected abstract int getWebViewParent();

    //...pause/stop/destory操作

    //让WebView处理返回点击事件
    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (mAgentWeb != null && mAgentWeb.handleKeyEvent(keyCode, event)) {
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }
}
```



#### 2、使用-资讯详情页

```java
public class NewsDetailActivity extends WebViewActivity {

    private static final String KEY_URL = "url";
    private String mUrl;

    //静态方法跳转活动，这样写很清楚
    public static void start(Context context, String url) {
        if (context == null || TextUtils.isEmpty(url)) {
            return;
        }
        Intent intent = new Intent(context, NewsDetailActivity.class);
        intent.putExtra(KEY_URL, url);
        context.startActivity(intent);
    }

    @Override
    protected int getWebViewParent() {
        return R.id.web_view_container;
    }

    @Override
    protected int getLayoutRes() {
        return R.layout.activity_news_detail;
    }

    @Override
    protected void initVariable() {
        mUrl = getIntent().getStringExtra(KEY_URL);
    }

    @Override
    protected void loadData() {
        loadUrl(mUrl);
    }
}
```



### 四、链接

- [AgentWeb ， 一个简洁易用的 Android Web 库](https://www.jianshu.com/p/c80da1c41af7)

- [WebView 封装](https://www.jianshu.com/p/d657580ac643)
- [WebView 性能和用户体验优化](https://www.jianshu.com/p/fc7909e24178)