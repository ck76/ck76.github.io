---
title: values解析
date: 2018-08-12 03:18:15
categories: "Android-UI-Resource"
tags:
---

values是res文件夹下的又一个文件夹，里面通常放arrays，attrs，bools，colors，strings，styles文件。

<!--more-->

## arrays.xml

### 创建数组资源

在value目录下创建arrays.xml文件

然后在arrays.xml文件中使用&lt;string-array>或者&lt;integer-array>标签定义数组：

```xml
<?xml version="1.0" encoding="utf-8"?>
<resources>
    <string-array name="languages">
        <item>C语言</item>
        <item>Java </item>
        <item>C#</item>
        <item>PHP</item>
        <item>HTML</item>
    </string-array>
  
    <integer-array name="reminder_methods_values" translatable="false">
        <item>1</item>   
        <item>2</item>  
        <item>3</item>  
    </integer-array>
 
</resources>
```

### 引用数组资源：

### 在java代码中引用

```java
Resources res =getResources();
String[] languages = res.getStringArray(R.array.languages);
String[] reminder_methods = res.getIntArray(R.array.reminder_methods_values);
```

### 在xml中引用

这里以spinner控件为例，因为spinner的entries属性刚好需要的是数组资源

```xml
<Spinner
    android:id="@+id/spinner1"
    android:layout_width="match_parent"
    android:layout_height="wrap_content"
    android:entries="@array/languages"
  />
```

注意上面引用的时候是@array。

其实array资源并不一定非要定义在values/arrays.xml文件中，这只是一种不成文的约定，将上面的&lt;string-array name="languages">定义在strings.xml中也是可以的。

Android系统对所有的xml资源文件其实是不加区别的，觉得values资源类型的不在于放在什么什么文件，而在于标签名，比如string-array的标签名决定了这是一个array资源。

## colors.xml

```xml
<?xml version="1.0" encoding="utf-8"?>
<resources>
    <!--应用的主要色调，actionBar默认使用该颜色，Toolbar导航栏的底色-->
    <color name="colorPrimary">#1dcbdb</color>
    <!--应用的主要暗色调，statusBarColor默认使用该颜色-->
    <color name="colorPrimaryDark">#1dcbdb</color>
    <!--控件选中后的颜色-->
    <color name="colorAccent">#FF4081</color>

    <color name="item_common_pressed">#e0e0e0</color>
    <color name="common_btn_color">#dff9fa</color>

</resources>

```

## strings.xml

```xml
<resources>
    <string name="app_name">AppName</string>
</resources>

```

## styles.xml

1. 每个页面标题栏的标题基本会有一样的字体大小、颜色、对齐方式、内间距、外间距等，这就可以定义成样式；
2. 很多按钮也都使用一致的背景、内间距、文字颜色、文字大小、文字的对齐方式等，这也可以定义成样式；
3. 网络加载的进度条基本也都是一样的，同样可以定义成样式；
4. 不喜欢系统的弹出框样式，那也可以自定义样式。



Android的样式一般定义在**res/values/styles.xml**文件中，其中有一个根元素&lt;resource>**，而具体的每种样式定义则是通过**&lt;resource>**下的子标签**&lt;style>**来完成，**&lt;style>**通过添加多个**&lt;item>**来设置样式不同的属性。**另外，样式是可以继承的，可通过**&lt;style>**标签的**parent**属性声明要继承的样式，也可通过点前缀 (.) 继承，点前面为父样式名称，后面为子样式名称。点前缀方式只适用于自定义的样式，若要继承Android内置的样式，则只能通过**parent**属性声明。 

```xml
<resources>

    <!-- Base application theme. -->
    <style name="AppTheme" parent="Theme.AppCompat.Light.NoActionBar">
        <!-- Customize your theme here. -->
        <!--应用的主要色调，actionBar默认使用该颜色，Toolbar导航栏的底色-->
        <item name="colorPrimary">@color/colorPrimary</item>
        <!--应用的主要暗色调，statusBarColor默认使用该颜色-->
        <item name="colorPrimaryDark">@color/colorPrimaryDark</item>
        <!--控件选中后的颜色-->
        <item name="colorAccent">@color/colorAccent</item>
        <!--是否设置滑动退出-->
        <item name="android:windowIsTranslucent">true</item>
        <item name="android:windowBackground">@android:color/transparent</item>
    </style>

    <style name="AppSplash" parent="AppTheme">
        <item name="android:windowDisablePreview">false</item>
        <item name="android:windowNoTitle">true</item>
        <item name="android:windowFullscreen">true</item>
        <item name="android:windowIsTranslucent">false</item>
        <item name="android:windowBackground">@drawable/start_1</item>
    </style>

    <style name="MainActivity" parent="AppTheme">
        <item name="android:windowIsTranslucent">false</item>
        <item name="android:windowBackground">@android:color/white</item>
    </style>

    <style name="loading_dialog" parent="@android:style/Theme.Dialog">
        <item name="android:windowFrame">@null</item>
        <item name="android:windowIsFloating">true</item>
        <item name="android:windowIsTranslucent">true</item>
        <item name="android:windowNoTitle">true</item>
        <item name="android:background">@android:color/transparent</item>
        <item name="android:windowBackground">@android:color/transparent</item>
        <item name="android:windowContentOverlay">@null</item>
        <item name="android:backgroundDimEnabled">true</item>
        <item name="android:backgroundDimAmount">0.6</item>
    </style>

    <style name="Transparent" parent="AppTheme">
        <item name="android:windowBackground">@android:color/transparent</item>
        <item name="android:colorBackgroundCacheHint">@null</item>
        <item name="android:windowIsTranslucent">true</item>
        <item name="android:windowAnimationStyle">@android:style/Animation</item>
        <item name="android:windowNoTitle">true</item>
        <item name="android:windowContentOverlay">@null</item>
    </style>

    <style name="courseTableText">
        <item name="android:textColor">#646262</item>
        <item name="android:textSize">12sp</item>
        <item name="android:gravity">center</item>
        <item name="android:padding">2dp</item>
    </style>

    <style name="weekViewTimeText">
        <item name="android:textColor">@android:color/black</item>
        <item name="android:textSize">10sp</item>
    </style>

    <style name="weekViewNumText">
        <item name="android:textColor">@color/black2</item>
        <item name="android:textSize">10sp</item>
    </style>

    <style name="CustomDialog" parent="android:style/Theme.Dialog">
        <!--背景颜色及透明程度-->
        <item name="android:windowBackground">@android:color/transparent</item>
        <!--是否有标题 -->
        <item name="android:layout_width">match_parent</item>
        <item name="android:layout_height">match_parent</item>
        <item name="android:windowNoTitle">true</item>
        <!--是否浮现在activity之上-->
        <item name="android:windowIsFloating">true</item>
        <!--是否模糊-->
        <item name="android:backgroundDimEnabled">true</item>
    </style>

    <style name="VideoViewH5" parent="AppTheme">
        <item name="android:windowIsTranslucent">false</item>
        <item name="android:windowFullscreen">true</item>
        <item name="windowNoTitle">true</item>
    </style>

</resources>

```



### 部分属性值

```java
窗口主题设置：（这里主要对于Activity设置，用到系统自动主题内容） 
•android:theme=”@android:style/Theme.Dialog” 将一个Activity显示为能话框模式 
•android:theme=”@android:style/Theme.NoTitleBar” 不显示应用程序标题栏 
•android:theme=”@android:style/Theme.NoTitleBar.Fullscreen” 不显示应用程序标题栏，并全屏 
•android:theme=”Theme.Light” 背景为白色 
•android:theme=”Theme.Light.NoTitleBar” 白色背景并无标题栏 
•android:theme=”Theme.Light.NoTitleBar.Fullscreen” 白色背景，无标题栏，全屏 
•android:theme=”Theme.Black” 背景黑色 
•android:theme=”Theme.Black.NoTitleBar” 黑色背景并无标题栏 
•android:theme=”Theme.Black.NoTitleBar.Fullscreen” 黑色背景，无标题栏，全屏 
•android:theme=”Theme.Wallpaper” 用系统桌面为应用程序背景 
•android:theme=”Theme.Wallpaper.NoTitleBar” 用系统桌面为应用程序背景，且无标题栏 
•android:theme=”Theme.Wallpaper.NoTitleBar.Fullscreen” 用系统桌面为应用程序背景，无标题栏，全屏 
•android:theme=”Translucent” 半透明 
•android:theme=”Theme.Translucent.NoTitleBar” 
•android:theme=”Theme.Translucent.NoTitleBar.Fullscreen” 
•android:theme=”Theme.Panel” 
•android:theme=”Theme.Light.Panel”
```

**Android平台定义了三种字体大小：**

```java
"?android:attr/textAppearanceLarge"
"?android:attr/textAppearanceMedium"
"?android:attr/textAppearanceSmall"
```

**Android字体颜色：**

```java
android:textColor="?android:attr/textColorPrimary"
android:textColor="?android:attr/textColorSecondary"
android:textColor="?android:attr/textColorTertiary"
android:textColor="?android:attr/textColorPrimaryInverse"
android:textColor="?android:attr/textColorSecondaryInverse"
```

**Android的ProgressBar样式：**

```java
style="?android:attr/progressBarStyleHorizontal"
style="?android:attr/progressBarStyleLarge"
style="?android:attr/progressBarStyleSmall"
style="?android:attr/progressBarStyleSmallTitle"
```

### 常用theme项

```xml
  <style name="BaseTheme" parent="Theme.AppCompat.Light.NoActionBar">
        <!--主色-->
        <item name="colorPrimary">@color/theme_primary</item>
        <!--深色主色-->
        <item name="colorPrimaryDark">@color/theme_primary</item>
        <!--醒目主色-->
        <item name="colorAccent">@color/theme_accent</item>
        <!--抽屉开关样式-->
        <item name="drawerArrowStyle">@style/DrawerArrowStyle</item>
    </style>

    <style name="AppTheme" parent="BaseTheme">
        <!--窗体透明-->
        <item name="android:windowIsTranslucent">true</item>
        <!--窗体背景-->
        <item name="android:windowBackground">@android:color/transparent</item>
    </style>

    <style name="LaunchTheme" parent="Theme.AppCompat.NoActionBar">
        <!--窗体背景，这个背景能在第一时间显示，可以用作启动界面，但此windows不可再使用view-->
        <item name="android:background">@drawable/bg_launch</item>
    </style>

    <style name="MainTheme" parent="BaseTheme">
        <!--窗体动画-->
        <item name="android:windowAnimationStyle">@android:style/Animation.Translucent</item>
    </style>

    <style name="DrawerArrowStyle" parent="Widget.AppCompat.DrawerArrowToggle">
        <!--抽屉开关箭头颜色-->
        <item name="color">@android:color/white</item>
    </style>
```

### 所有theme项

```xml
<item name="windowActionBar">true</item>
        <item name="windowActionBarOverlay">false</item>

        <!-- Used by MediaRouter -->
        <item name="isLightTheme">false</item>

        <item name="selectableItemBackground">@drawable/abc_item_background_holo_dark</item>
        <item name="selectableItemBackgroundBorderless">?attr/selectableItemBackground</item>
        <item name="homeAsUpIndicator">@drawable/abc_ic_ab_back_mtrl_am_alpha</item>

        <item name="dividerVertical">@drawable/abc_list_divider_mtrl_alpha</item>
        <item name="dividerHorizontal">@drawable/abc_list_divider_mtrl_alpha</item>

        <!-- Action Bar Styles -->
        <item name="actionBarTabStyle">@style/Widget.AppCompat.ActionBar.TabView</item>
        <item name="actionBarTabBarStyle">@style/Widget.AppCompat.ActionBar.TabBar</item>
        <item name="actionBarTabTextStyle">@style/Widget.AppCompat.ActionBar.TabText</item>
        <item name="actionButtonStyle">@style/Widget.AppCompat.ActionButton</item>
        <item name="actionOverflowButtonStyle">@style/Widget.AppCompat.ActionButton.Overflow</item>
        <item name="actionOverflowMenuStyle">@style/Widget.AppCompat.PopupMenu.Overflow</item>
        <item name="actionBarStyle">@style/Widget.AppCompat.ActionBar.Solid</item>
        <item name="actionBarSplitStyle">?attr/actionBarStyle</item>
        <item name="actionBarWidgetTheme">@null</item>
        <item name="actionBarTheme">@style/ThemeOverlay.AppCompat.ActionBar</item>
        <item name="actionBarSize">@dimen/abc_action_bar_default_height_material</item>
        <item name="actionBarDivider">?attr/dividerVertical</item>
        <item name="actionBarItemBackground">?attr/selectableItemBackgroundBorderless</item>
        <item name="actionMenuTextAppearance">@style/TextAppearance.AppCompat.Widget.ActionBar.Menu</item>
        <item name="actionMenuTextColor">?android:attr/textColorPrimaryDisableOnly</item>

        <!-- Dropdown Spinner Attributes -->
        <item name="actionDropDownStyle">@style/Widget.AppCompat.Spinner.DropDown.ActionBar</item>

        <!-- Action Mode -->
        <item name="actionModeStyle">@style/Widget.AppCompat.ActionMode</item>
        <item name="actionModeBackground">@drawable/abc_cab_background_top_material</item>
        <item name="actionModeSplitBackground">?attr/colorPrimaryDark</item>
        <item name="actionModeCloseDrawable">@drawable/abc_ic_ab_back_mtrl_am_alpha</item>
        <item name="actionModeCloseButtonStyle">@style/Widget.AppCompat.ActionButton.CloseMode</item>

        <item name="actionModeCutDrawable">@drawable/abc_ic_menu_cut_mtrl_alpha</item>
        <item name="actionModeCopyDrawable">@drawable/abc_ic_menu_copy_mtrl_am_alpha</item>
        <item name="actionModePasteDrawable">@drawable/abc_ic_menu_paste_mtrl_am_alpha</item>
        <item name="actionModeSelectAllDrawable">@drawable/abc_ic_menu_selectall_mtrl_alpha</item>
        <item name="actionModeShareDrawable">@drawable/abc_ic_menu_share_mtrl_alpha</item>

        <!-- Panel attributes -->
        <item name="panelMenuListWidth">@dimen/abc_panel_menu_list_width</item>
        <item name="panelMenuListTheme">@style/Theme.AppCompat.CompactMenu</item>
        <item name="panelBackground">@drawable/abc_menu_hardkey_panel_mtrl_mult</item>
        <item name="android:panelBackground">@android:color/transparent</item>
        <item name="listChoiceBackgroundIndicator">@drawable/abc_list_selector_holo_dark</item>

        <!-- List attributes -->
        <item name="textAppearanceListItem">@style/TextAppearance.AppCompat.Subhead</item>
        <item name="textAppearanceListItemSmall">@style/TextAppearance.AppCompat.Subhead</item>
        <item name="listPreferredItemHeight">64dp</item>
        <item name="listPreferredItemHeightSmall">48dp</item>
        <item name="listPreferredItemHeightLarge">80dp</item>
        <item name="listPreferredItemPaddingLeft">16dip</item>
        <item name="listPreferredItemPaddingRight">16dip</item>

        <!-- Required for use of support_simple_spinner_dropdown_item.xml -->
        <item name="spinnerDropDownItemStyle">@style/Widget.AppCompat.DropDownItem.Spinner</item>
        <item name="dropdownListPreferredItemHeight">?attr/listPreferredItemHeightSmall</item>

        <!-- Popup Menu styles -->
        <item name="popupMenuStyle">@style/Widget.AppCompat.PopupMenu</item>
        <item name="textAppearanceLargePopupMenu">@style/TextAppearance.AppCompat.Widget.PopupMenu.Large</item>
        <item name="textAppearanceSmallPopupMenu">@style/TextAppearance.AppCompat.Widget.PopupMenu.Small</item>
        <item name="listPopupWindowStyle">@style/Widget.AppCompat.ListPopupWindow</item>
        <item name="dropDownListViewStyle">@style/Widget.AppCompat.ListView.DropDown</item>

        <!-- SearchView attributes -->
        <item name="searchViewStyle">@style/Widget.AppCompat.SearchView</item>
        <item name="android:dropDownItemStyle">@style/Widget.AppCompat.DropDownItem.Spinner</item>
        <item name="textColorSearchUrl">@color/abc_search_url_text</item>
        <item name="textAppearanceSearchResultTitle">@style/TextAppearance.AppCompat.SearchResult.Title</item>
        <item name="textAppearanceSearchResultSubtitle">@style/TextAppearance.AppCompat.SearchResult.Subtitle</item>

        <!-- ShareActionProvider attributes -->
        <item name="activityChooserViewStyle">@style/Widget.AppCompat.ActivityChooserView</item>

        <!-- Toolbar styles -->
        <item name="toolbarStyle">@style/Widget.AppCompat.Toolbar</item>
        <item name="toolbarNavigationButtonStyle">@style/Widget.AppCompat.Toolbar.Button.Navigation</item>

        <item name="android:editTextStyle">@style/Widget.AppCompat.EditText</item>
        <item name="editTextBackground">@drawable/abc_edit_text_material</item>
        <item name="editTextColor">?android:attr/textColorPrimary</item>
        <item name="android:autoCompleteTextViewStyle">@style/Widget.AppCompat.AutoCompleteTextView</item>

        <!-- Color palette -->
        <item name="colorPrimaryDark">@color/primary_dark_material_dark</item>
        <item name="colorPrimary">@color/primary_material_dark</item>
        <item name="colorAccent">@color/accent_material_dark</item>

        <item name="colorControlNormal">?android:attr/textColorSecondary</item>
        <item name="colorControlActivated">?attr/colorAccent</item>
        <item name="colorControlHighlight">@color/ripple_material_dark</item>
        <item name="colorButtonNormal">@color/button_material_dark</item>
        <item name="colorSwitchThumbNormal">@color/switch_thumb_normal_material_dark</item>

        <item name="drawerArrowStyle">@style/Widget.AppCompat.DrawerArrowToggle</item>

        <item name="switchStyle">@style/Widget.AppCompat.CompoundButton.Switch</item>

        <item name="android:ratingBarStyle">@style/Widget.AppCompat.RatingBar</item>

        <!-- Button styles -->
        <item name="android:buttonStyle">@style/Widget.AppCompat.Button</item>
        <item name="android:buttonStyleSmall">@style/Widget.AppCompat.Button.Small</item>
        <item name="android:textAppearanceButton">@style/TextAppearance.AppCompat.Button</item>
```



## attrs.xml

要使用属性，首先这个属性应该存在，所以如果我们要使用自己的属性，必须要先把他定义出来才能使用。但我们平时在写布局文件的时候好像没有自己定义属性，但我们照样可以用很多属性，这是为什么？我想大家应该都知道：系统定义好的属性我们就可以拿来用呗，但是你们知道系统定义了哪些属性吗？哪些属性是我们自定义控件可以直接使用的，哪些不能使用？什么样的属性我们能使用？这些问题我想大家不一定都弄得清除，下面我们去一一解开这些谜团。    系统定义的所有属性我们可以在\sdk\platforms\android-xx\data\res\values目录下找到attrs.xml这个文件，这就是系统自带的所有属性 .

```xml
<?xml version="1.0" encoding="utf-8"?>
<resources>
    <declare-styleable name="RatioImageView">
        <attr name="ratio" format="float"/>
    </declare-styleable>

    <declare-styleable name="PictureLayout">
        <attr name="space" format="dimension"/>
    </declare-styleable>

    <!-- RichText -->
    <declare-styleable name="RichText">
        <attr name="drawable_src" format="reference" />
        <attr name="drawable_height" format="dimension" />
        <attr name="drawable_width" format="dimension" />
        <attr name="drawable_location">
            <enum name="left" value="1" />
            <enum name="top" value="2" />
            <enum name="right" value="3" />
            <enum name="bottom" value="4" />
        </attr>
    </declare-styleable>
</resources>
```



