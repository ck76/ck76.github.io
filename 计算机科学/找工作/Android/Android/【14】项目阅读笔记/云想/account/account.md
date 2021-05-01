云想是个很有学习价值的项目，从一开始到全程跟着开发，可以学到很多东西。

[TOC]

### 一、类图

![account1](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/0APqYwhH7sMCVMlr.f*982WvdN3s.DhN8iVmILkLFRg!/r/dL8AAAAAAAAA)

![account2](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/ObW36zw4Xt22aTnbKKsu7nRyBlALY*qTGLqcpVzhEb0!/r/dFQBAAAAAAAA)



### 二、组成

```java
.
├── AccountConstants.java			//常量
├── CheckFormatHelper.java			//格式检查
├── data
│   ├── AccountApi.java				//Api接口
│   ├── AccountDataCenter.java		//登录模块数据中心
│   ├── AccountRepository.java		//用户模块数据管理
│   ├── ApiCallBack.java			//错误统一处理CallBack
│   └── IAccount.java				//网络请求回调接口基类
├── model
│   └── User.java					//User实体类
└── view
    └── AccountBaseActivity.java	//基类Activity
```



### 三、实例

以密码登录示例来讲解account模块

1. 写一个LoginActivity集成AccountBaseActivity，并且实现IAccount里的ILoginListener接口

2. 在登录按钮的点击事件里调用`mRepository.loginWithPwd(phone, password, this);`方法

3. AccountRepository的loginWithPwd方法内会调用mApi的loginWithPwd方法

   ```java
    public void loginWithPwd(String phone, String password, IAccount.ILoginListener listener) {
           mApi.loginWithPwd(phone, password).enqueue(new ApiCallBack<JsonObject>(listener) {
               @Override
               void onDataBack(ApiResponse<JsonObject> response) {
                   JsonObject jsonObject = response.getData();
                   String token = jsonObject.get(FIELD_TOKEN).getAsString();
                   int code = response.getCode();
                   SimpleCache.get(AppRuntime.getAppContext()).put(AccountConstants.KEY_TOKEN, token);
                   listener.onLoginSuccess(token, code);//回调listener的登陆成功方法
               }
           });
       }
   ```

4. 看一下ApiCallback.java

   ```java
   public abstract class ApiCallBack<T> implements Callback<ApiResponse<T>> {
   
       private IAccount mErrorListener;			//进行统一错误处理
   
       public ApiCallBack(IAccount errorListener) {
           mErrorListener = errorListener;
       }
   
       abstract void onDataBack(ApiResponse<T> response);
   
       @Override
       public void onResponse(Call<ApiResponse<T>> call, Response<ApiResponse<T>> response) {
           if (response != null && response.body() != null) {
               int code = response.body().getCode();
               if (code == AccountRepository.STATUS_CODE_OK) {
                   onDataBack(response.body());
               } else {
                   mErrorListener.onError(code);
               }
           }
       }
   
       @Override
       public void onFailure(Call call, Throwable t) {
           ToastUtil.show(AppRuntime.getAppContext(), ExceptionEngine.handleException(t).getMsg());
       }
   }
   ```

5. IAccount接口

   ```java
   package com.yunxiang.android.account.data;
   
   /**
    * 网络请求回调接口基类
    *
    * @author chengkun
    * @since 2018/11/3 20:40
    */
   public interface IAccount {
   
       /**
        * 请求失败回调
        *
        * @param status 失败状态码
        */
       void onError(int status);
       
   	//........省略代码
       
       interface ILoginListener extends IAccount {	//继承IAccount
   
           /**
            * 登录/注册成功
            *
            * @param token  token
            * @param status 状态码
            */
           void onLoginSuccess(String token, int status);
       }
   }
   
   ```


