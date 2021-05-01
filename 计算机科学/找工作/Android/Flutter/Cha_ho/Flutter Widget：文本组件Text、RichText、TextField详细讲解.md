[TOC]

# 1. Text

- 应用场景：普通文本

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgdy2m6yj307n0b2jrb.jpg)

示意图

- 属性设置



```kotlin
Text(this.data, {
    Key key,
    this.style, // 设置文字样式，具体见下面的说明
    this.textAlign, // 文字对齐方式：（center居中，left左对齐，right右对齐，justfy两端对齐）
    this.textDirection, // 文本方向（ltr从左至右，rtl从右至左）
    this.locale, 
    this.softWrap,// 是否自动换行（true自动换行，false单行显示，超出屏幕部分默认截断处理）
    this.overflow, // 文字超出屏幕之后的处理方式（clip裁剪，fade渐隐，ellipsis省略号）
    this.textScaleFactor, // 字体显示倍率
    this.maxLines, // maxLines 文字显示最大行数
    this.semanticsLabel, 
  }

 const TextStyle({
    this.inherit = true,
    this.color,//文本样式
    this.fontSize,//字体大小
    this.fontWeight,//绘制文本时的字体粗细
    this.fontStyle,//字体变体
    this.letterSpacing,//水平字母之间的空间间隔(逻辑像素为单位),可以负值
    this.wordSpacing,//单词之间添加的空间间隔(逻辑像素为单位),可以负值
    this.textBaseline,//对齐文本的水平线
    this.height,//文本行与行的高度,作为字体代销的倍数
    this.locale,//用于选择区域定字形的语言环境
    this.foreground,//文本的前景色，不能与color共同设置
    this.background,//文本背景色
    this.shadows,//Flutter Decoration背景设定(边框，圆角，阴影,渐变等)
    this.decoration,//绘制文本装饰，添加上下划线，删除线
    this.decorationColor,//文本装饰的颜色
    this.decorationStyle,//文本装饰的样式，控制画虚线，点，波浪线
    this.debugLabel,
    String fontFamily,//使用字体的名称
    String package,
  })
```

- 实例演示



```java
import 'package:flutter/material.dart'; // Material UI组件库

void main() => runApp(MyApp());

// 无状态控件显示
class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
// 主界面入口返回的组件 = MaterialApp
    return MaterialApp(
      title: 'Widget_Demo', //标题
      theme: ThemeData(primarySwatch: Colors.blue), //主题色
      home: MyHomePage(), // 返回一个Widget对象，用来定义当前应用打开的时候，所显示的界面
    );
  }
}

// 返回的Widget对象
class MyHomePage extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      //设置appbar
      appBar: new AppBar(
        title: new Text('Flutter Demo'),
      ),
      //主体
      body: new Center(
        //在屏幕中央显示一个文本
        child: Text(
          "carson ho Demo", // 显示的内容
          style: TextStyle( // 通过Style设置样式，可根据上述样式进行设置，此处仅作最简单属性设置
              color: Colors.blue, //颜色
              fontSize: 14, // 字体大小
              fontWeight: FontWeight.bold), // 字体加粗
          //文本背景颜色
        ),
      ),
    );
  }
}
```

- 特别注意：在 Flutter 中，并不是每个 Widget 都有点击事件，如上述说的文本Text 就没有
- 解决方案：需用一个 GestureDetector 组件包住Text 组件 & 实现onTap() 事件

------

# 2. RichText

- 应用场景：一段文本存在多种样式（大小、颜色等），类似Android的SpannableString

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgdvpujgj307n0b8dfz.jpg)

- 属性设置



```kotlin
RichText({
    Key key,
    @required this.text, // 区别于Text，RichText的text属性不是String类型，而是TextSpan，TextSpan用于指定文本片段的风格及手势交互，具体如下描述
    this.textAlign = TextAlign.start, // 文字对齐方式
    this.textDirection, // 文本方向（ltr从左至右，rtl从右至左）
    this.softWrap = true, // 是否自动换行（true自动换行，false单行显示，超出屏幕部分默认截断处理）
    this.overflow = TextOverflow.clip, // 文字超出屏幕之后的处理方式（clip裁剪，fade渐隐，ellipsis省略号）
    this.textScaleFactor = 1.0,// 字体显示倍率
    this.maxLines,// maxLines 文字显示最大行数
  })

// TextSpan是一个树状结构，children表示子节点。每个节点代表一个文本片段，祖先节点的style对所有子孙节点起作用。
// 注：当祖先节点的style中指定的值与自身节点的style发生冲突时，自身style中指定的值会覆盖掉前者

TextSpan({
    this.style, // 指定风格，类似text中的style
    this.text, // String，指定文本片段
    this.children, // List<TextSpan>类型，代表子节点，每个节点代表一个文本片段
    this.recognizer, // GestureRecognizer类型，指定该文本片段的手势交互，GestureRecognizer是一个抽象类（有许多子类，如点击监听器TapGestureRecognizer
  })
```

- 代码演示



```dart
import 'package:flutter/gestures.dart';
/**
 *  导入库
 **/
import 'package:flutter/material.dart'; // Material UI组件库

void main() => runApp(MyApp());

// 无状态控件显示
class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
// 主界面入口返回的组件 = MaterialApp
    return MaterialApp(
      title: 'Widget_Demo', //标题
      theme: ThemeData(primarySwatch: Colors.blue), //主题色
      home: MyHomePage(), // 返回一个Widget对象，用来定义当前应用打开的时候，所显示的界面
    );
  }
}

// 返回的Widget对象
class MyHomePage extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      //设置appbar
      appBar: new AppBar(
        title: new Text('Flutter Demo'),
      ),
      //主体
      body: new Center(
        //在屏幕中央显示一个文本
          child: RichText( // 根节点的设置会影响下面两个子节点
            text: TextSpan(style: TextStyle(fontSize: 20), children: [
              // 每个子节点的设置会覆盖根节点的设置
              TextSpan(text: "Android ", style: TextStyle(
                color: Colors.blue, fontSize: 60,),
                recognizer: TapGestureRecognizer() // 设置点击事件，此处以点击监听器TapGestureRecognizer()为例
                  ..onTap = () {
                    print("点击了Android");
                  },),
              TextSpan(text: "iOS ",
                  style: TextStyle(color: Colors.red, fontSize: 50,)),
              TextSpan(text: "Web", style: TextStyle(
                color: Colors.green, fontSize: 40,)),
            ]),
            textDirection: TextDirection.ltr,
          )
      ),
    );
  }
}
```

- 效果展示

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgdtelmgg30es0b0hdt.gif)

### 特别注意

Text控件中，有一个静态属性rich，可达到类似RichText的效果



```kotlin
Text.rich(
    this.textSpan, {
    Key key,
    this.style,
    this.strutStyle,
    this.textAlign,
    this.textDirection,
    this.locale,
    this.softWrap,
    this.overflow,
    this.textScaleFactor,
    this.maxLines,
    this.semanticsLabel,
  })
```

深入Text的源码build（）方法可知，Text控件实际上是对RichText控件的封装



```kotlin
  @override
  Widget build(BuildContext context) {
    ...
    Widget result = RichText(
      ...
      text: TextSpan(
        ...
        text: data,
        children: textSpan != null ? <TextSpan>[textSpan] : null,
      ),
    );
    ...
    return result;
  }
```

------

# 3. TextField

- 应用场景：文本输入框，类似于Android中的EditText

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgdqq30tg307q0b0av5.gif)

- 属性说明



```kotlin
const TextField({
    Key key,
    this.controller,    //编辑框的控制器，跟文本框的交互一般都通过该属性完成，如果不创建的话默认会自动创建
    this.focusNode,  //用于管理焦点
    this.decoration = const InputDecoration(),   //输入框的装饰器，用来修改外观
    TextInputType keyboardType,   //设置输入类型，不同的输入类型键盘不一样
    this.textInputAction,   //用于控制键盘动作（一般位于右下角，默认是完成）
    this.textCapitalization = TextCapitalization.none,
    this.style,    //输入的文本样式
    this.textAlign = TextAlign.start,   //输入的文本位置
    this.textDirection,    //输入的文字排列方向，一般不会修改这个属性
    this.autofocus = false,   //是否自动获取焦点
    this.obscureText = false,   //是否隐藏输入的文字，一般用在密码输入框中
    this.autocorrect = true,   //是否自动校验
    this.maxLines = 1,   //最大行
    this.maxLength,   //能输入的最大字符个数
    this.maxLengthEnforced = true,  //配合maxLength一起使用，在达到最大长度时是否阻止输入
    this.onChanged,  //输入文本发生变化时的回调
    this.onEditingComplete,   //点击键盘完成按钮时触发的回调，该回调没有参数，(){}
    this.onSubmitted,  //同样是点击键盘完成按钮时触发的回调，该回调有参数，参数即为当前输入框中的值。(String){}
    this.inputFormatters,   //对输入文本的校验
    this.enabled,    //输入框是否可用
    this.cursorWidth = 2.0,  //光标的宽度
    this.cursorRadius,  //光标的圆角
    this.cursorColor,  //光标的颜色
    this.keyboardAppearance,
    this.scrollPadding = const EdgeInsets.all(20.0),
    this.dragStartBehavior = DragStartBehavior.down,
    this.enableInteractiveSelection,
    this.onTap,    //点击输入框时的回调(){}
    this.buildCounter,
  })

// 此处主要介绍TextField.decoration属性
InputDecoration({
    this.icon,    //位于装饰器外部和输入框前面的图片
    this.labelText,  //用于描述输入框，例如这个输入框是用来输入用户名还是密码的，当输入框获取焦点时默认会浮动到上方，
    this.labelStyle,  // 控制labelText的样式,接收一个TextStyle类型的值
    this.helperText, //辅助文本，位于输入框下方，如果errorText不为空的话，则helperText不会显示
    this.helperStyle, //helperText的样式
    this.hintText,  //提示文本，位于输入框内部
    this.hintStyle, //hintText的样式
    this.hintMaxLines, //提示信息最大行数
    this.errorText,  //错误信息提示
    this.errorStyle, //errorText的样式
    this.errorMaxLines,   //errorText最大行数
    this.hasFloatingPlaceholder = true,  //labelText是否浮动，默认为true，修改为false则labelText在输入框获取焦点时不会浮动且不显示
    this.isDense,   //改变输入框是否为密集型，默认为false，修改为true时，图标及间距会变小
    this.contentPadding, //内间距
    this.prefixIcon,  //位于输入框内部起始位置的图标。
    this.prefix,   //预先填充的Widget,跟prefixText同时只能出现一个
    this.prefixText,  //预填充的文本，例如手机号前面预先加上区号等
    this.prefixStyle,  //prefixText的样式
    this.suffixIcon, //位于输入框后面的图片,例如一般输入框后面会有个眼睛，控制输入内容是否明文
    this.suffix,  //位于输入框尾部的控件，同样的不能和suffixText同时使用
    this.suffixText,//位于尾部的填充文字
    this.suffixStyle,  //suffixText的样式
    this.counter,//位于输入框右下方的小控件，不能和counterText同时使用
    this.counterText,//位于右下方显示的文本，常用于显示输入的字符数量
    this.counterStyle, //counterText的样式
    this.filled,  //如果为true，则输入使用fillColor指定的颜色填充
    this.fillColor,  //相当于输入框的背景颜色
    this.errorBorder,   //errorText不为空，输入框没有焦点时要显示的边框
    this.focusedBorder,  //输入框有焦点时的边框,如果errorText不为空的话，该属性无效
    this.focusedErrorBorder,  //errorText不为空时，输入框有焦点时的边框
    this.disabledBorder,  //输入框禁用时显示的边框，如果errorText不为空的话，该属性无效
    this.enabledBorder,  //输入框可用时显示的边框，如果errorText不为空的话，该属性无效
    this.border, //正常情况下的border
    this.enabled = true,  //输入框是否可用
    this.semanticCounterText,  
    this.alignLabelWithHint,
  })
```

- 代码演示



```dart
/**
 *  导入库
 **/
import 'package:flutter/material.dart';
import 'package:flutter/services.dart'; // Material UI组件库

void main() => runApp(MyApp());

// 无状态控件显示
class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
// 主界面入口返回的组件 = MaterialApp
    return MaterialApp(
      title: 'Widget_Demo', //标题
      theme: ThemeData(primarySwatch: Colors.blue), //主题色
      home: MyHomePage(), // 返回一个Widget对象，用来定义当前应用打开的时候，所显示的界面
    );
  }
}

// 返回的Widget对象
class MyHomePage extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      //设置appbar
      appBar: new AppBar(
        title: new Text('Flutter Demo'),
      ),
      //主体
        body: new TextField(
          decoration: InputDecoration(
              icon: Icon(Icons.person), // 输入框前加入图片
            hintText: "hintText", // 提示输入信息
              labelText: "labelText", // 描述输入框，当输入框获取焦点时默认会浮动到上方
              labelStyle: TextStyle( // labelText的样式
                color: Colors.red,
                fontSize: 20,
              ),
            hasFloatingPlaceholder: false,// labelText是否浮动，默认为true，修改为false则labelText在输入框获取焦点时不会浮动且不显示
            helperText: "helperText", // 输入框下方提示
            helperStyle: TextStyle( // 输入框下方提示样式
              color: Colors.green,//绿色
              fontSize: 20,//字体变大
            ),
            errorText: "errorText", // 错误提示，若该属性不为null，那么labelText失效
            prefixIcon: Icon(Icons.perm_identity), // 输入框前端预填充图片
            prefixText: "prefixText",// 预填充文字
            suffixIcon: Icon(Icons.remove_red_eye,), // 输入框后端预填充图片
            suffixText: "suffixText", // 输入框后端预填充文字
            counterText: "counterText", // 输入框右下方文字
            filled: true, // 颜色填充
            fillColor: Colors.grey,
            // 边界设置 - 外边界
              enabledBorder: OutlineInputBorder(
                /*边角*/
                borderRadius: BorderRadius.all(
                  Radius.circular(30), //边角为30
                ),
                borderSide: BorderSide(
                  color: Colors.amber, //边线颜色为黄色
                  width: 2, //边线宽度为2
                ),
              ),
              focusedBorder: OutlineInputBorder(
                  borderSide: BorderSide(
                    color: Colors.green, //边框颜色为绿色
                    width: 5, //宽度为5
                  )),
            // 边界设置 - 底边界
              errorBorder: UnderlineInputBorder(
                /*边角*/
                borderRadius: BorderRadius.all(
                  Radius.circular(30), //边角为30
                ),
                borderSide: BorderSide(
                  color: Colors.amber, //边线颜色为黄色
                  width: 2, //边线宽度为2
                ),
              ),
              focusedErrorBorder: UnderlineInputBorder(
                  borderSide: BorderSide(
                    color: Colors.green, //边框颜色为绿色
                    width: 5, //宽度为5
                  )),
          ),
          keyboardType: TextInputType.phone, // 只能输入手机号
          textInputAction: TextInputAction.search, // 键盘动作的按钮变为搜索

          onEditingComplete: (){ // 点击键盘的动作按钮时的回调（无参）
            print("点击了键盘上的动作按钮");
          },
          onSubmitted: (val){// 点击键盘的动作按钮时的回调（参数为输入框的值）
            print("点击了键盘上的动作按钮，当前输入框的值为：${val}");
          },

          onChanged: (val) { // 输入文本发生变化时的回调，参数即为输入框中的值
            print(val);
          },

          inputFormatters: [ // 输入限制
            WhitelistingTextInputFormatter(RegExp("[a-z]")),// 白名单校验，只能输入规定的字符，此处只能输入a-z
            BlacklistingTextInputFormatter(RegExp("[a-z]")), // 黑名单校验，除了规定的字符其他的都可以输入，此处即除了a-z不能输入其他都能输入
            LengthLimitingTextInputFormatter(5) // 限制输入的长度
          ],

        )
    );
  }
```

- 示意图

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgdl0hzgg30is0bgx6p.gif)

下面，额外展示一个属性设置：TextField controller，用于对输入框进进行赋值 & 取值操作



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgdjr35tg307o0b41dz.gif)



```java
/**
 *  导入库
 **/
import 'package:flutter/material.dart';// Material UI组件库

void main() => runApp(MyApp());

// 无状态控件显示
class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
// 主界面入口返回的组件 = MaterialApp
    return MaterialApp(
      title: 'Widget_Demo', //标题
      theme: ThemeData(primarySwatch: Colors.blue), //主题色
      home: MyHomePage(), // 返回一个Widget对象，用来定义当前应用打开的时候，所显示的界面
    );
  }
}

// 有状态控件显示
class MyHomePage extends StatefulWidget {
  @override
  State<StatefulWidget> createState() {
    return new _MyHomePageState();
  }
}

class _MyHomePageState extends State<MyHomePage>{
  
  // 创建TextEditingController对象
  TextEditingController _userEtController = TextEditingController(); 

  @override
  void initState() {
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      //设置appbar
      appBar: new AppBar(
        title: new Text('Flutter Demo'),
      ),
      //主体
      body: Container(
        padding: EdgeInsets.all(10),
        child: Column(
          children: <Widget>[
            TextField(
              controller: _userEtController,
            ),
            RaisedButton(
              child: Text("赋值"),
              onPressed: () {
                setState(() {
                  _userEtController.text = "carson.ho";// 通过_userEtController.text将设置的值设置到编辑框内
                });
              },
            ),
            Text(_userEtController.text),// 通过_userEtController.text获取到设置的值
          ],
        ),
      ),
    );
  }
}
```

# 4. 总结

- 本文主要讲解了Flutter中完整页面方面的Widget，包括Text、RichText、TextField
- 接下来推出的文章，我将继续讲解Flutter的相关知识，包括更多的Widget用法、实例应用等，感兴趣的读者可以继续关注我的博客哦：[Carson_Ho的Android博客](https://www.jianshu.com/users/383970bef0a0/latest_articles)

------

# 



作者：Carson_Ho
链接：https://www.jianshu.com/p/1dadad711dc7
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。