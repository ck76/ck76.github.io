[TOC]

### 类图

![l](http://s191.photo.store.qq.com/psb?/V14L47VC0w3vOf/0rTSTKRQ8tBhFVTiCilbGEPuF*NiMDt.JQrXoWJ8yUc!/b/dL8AAAAAAAAA)

---

### 一、模块结构

```java
.
├── AbsSchemeDispatch.java		//Scheme分发器基类，所有分发器需要实现此类
├── ISchemeDispatch.java		//Scheme分发器抽象接口
├── MainSchemeDispatch.java		//Scheme分发器入口
├── Scheme.java					//Scheme分发统一工具类
├── SchemeCallback.java			//Scheme回调
├── SchemeEntity.java			//Scheme实体对象信息
├── SchemeFactory.java			//Scheme集合工厂
├── SchemeRuntime.java			// Scheme运行配置协议格式：
    //协议头://主模块.子模块/action?params={"param1":"value1"}&其他参数
└── SchemeUtil.java				//Scheme相关工具类
```

