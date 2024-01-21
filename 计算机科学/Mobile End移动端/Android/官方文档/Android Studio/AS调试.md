```java
for (int i = 0; i < 20; i++) {//断点1
            i++;//断点2
        }
```



![AS调试](<http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/ycvct.nTu2JsgTZ6Qi*akN28MJDnGfpzmoEB9GDUOzQ!/r/dMAAAAAAAAAA>)

- step over（F8）

  程序向下执行一行（如果当前行有方法调用，这个方法将被执行完毕返回，然后到下一行。就是说不会进入到调用的其他方法中去）。 

- step into（F7）

  程序向下执行一行。跟step over不同的是：如果该行有方法调用且为自定义方法，则运行进入自定义方法（不会进入官方类库的方法）。 

- force step into

  - 比step into 多了可以进入系统类库方法

- step out 

  如果在调试的时候你进入了一个方法(如Add())，并觉得该方法没有问题，你就可以使用step out跳出该方法，返回到该方法被调用处的下一行语句。值得注意的是，该方法已执行完毕。 没有问题，点击step out按钮，跳出该方法，该方法执行完毕。

- run to cursor

  设置多个断点时，可利用 run to Cursor按钮在两个断点之间跳转。 

- watches

  变量太多的时候在variables中已经看不过来了，可以邮寄变量add to watches或者在watches窗口中直接添加

- view breakpoints

  在这里可以查看设置过的所有断点（箭头所指）。另外，还可以在这里设置条件断点，日志断点，异常断点等

- 小蜘蛛右边第三个Attach调试

  你的APK如果已经运行在普通模式（非Debug）的情况下，你突然想Debug，而又不想重新运行浪费时间，就用attach debug