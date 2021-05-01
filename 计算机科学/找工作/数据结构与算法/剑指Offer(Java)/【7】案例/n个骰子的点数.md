[TOC]

### 题目

**把n个骰子扔到地上，所有骰子朝上一面的点数之后为s. 输入n，打印出s所有可能的值出现的概率。（每个骰子6个面，点数从1到6）**

关键点

- 总共出现6的n次幂钟组合
- 最小的和是n
- 最大的和是6n
- 总共6n-n+1种可能



### **解法一：基于递归，时间效率不高**(自顶向下分解)

递归的思想一般是分而治之，**把n个骰子分为第一个和剩下的n-1个**。先计算第一个骰子每个点数出现的次数，再计算剩余n-1个骰子出现的点数之和。求n-1个骰子的点数之的方法和前面讲的一样，即再次把n-1个骰子分成两堆------第一个和剩下的n-2个。n个骰子，每个骰子6个面，总共有6n个组合。这6n个组合之中肯定有重复的，我们知道其范围是n~6n,对于每种情况我们可以用缓存机制记录下来，每当其发生一次我们令其对应的单元加1。

我们定义一个长度为6n-n+1的数组，和为s的点数出现的次数保存到数组第s-n个元素里。为什么是6n-n+1呢？因为n个骰子的**和最少是n，最大是6n，**介于这两者之间的每一个情况都可能会发生，总共6n-n+1种情况。下面是java源码：

- n=1 循环次数6
- n=2 循环次数72
- n=3 循环次数468
- n=4 循环次数2844
- n=5 循环次数17160



```java
public class ck {
    public static void main(String[] args) {
        PrintProbability(2);
    }

    //n是不变的，到时候可以把参数省略
    private static final int g_maxValue = 6;
    private static int count = 0;
    private static int[] result;

    //基于递归求骰子点数，时间效率不高
    public static void PrintProbability(int n) {
        if (n < 1) return;
        //最大的数字
        int maxSum = n * g_maxValue;
        //6n-n+1
        result = new int[maxSum - n + 1];
        //初始化，开始统计之前都为0次
        for (int i = n; i <= maxSum; i++) {
            result[i - n] = 0;
        }
        //总共有多少种组合
        double total = Math.pow(g_maxValue, n);
        //probability(number,result);
        probability(n, result);
        //这个函数计算n~6n每种情况出现的次数
        for (int i = n; i <= maxSum; i++) {
            double ratio = result[i - n] / total;
            System.out.println("i: " + i + " ratio: " + ratio);
        }
        System.out.println("次数" + count);
    }

    public static void probability(int n, int[] result) {
        for (int i = 1; i <= g_maxValue; i++) {
            //循环6次，改变sum的值从1~6
            probability(n, n, i);
        }
    }

    /**
     * @param n       总共有多少骰子
     * @param current 当前第几个由n递减到1
     * @param sum     current个骰子的和 6次循环值从1~6
     */
    public static void probability(int n, int current, int sum) {
        //current==1时走此路
        if (current == 1) {
            //sum-n
            count++;
            result[sum - n]++; 				//关键点1
        } else {
            //计算抛去一个后的n-1个的可能性
            for (int i = 1; i <= g_maxValue; i++) {
                probability(n, current - 1, sum + i);   	//关键点2
                count++;
            }
        }
    }
}
```

这种方法思路非常简洁，但是递归实现会存在子问题重复求解的情况发生，所以当number很大的时候，其性能会慢的让人不能接受。



### 解法二：基于循环，时间性能好

**递归一般是自顶向下的分析求解**，而基于循环的方法则是自底向上。基于循环的一般需要更少的空间和更少的时间，性能较好，但是一般代码比较难懂。

开两行数组初始化为0，下一次投骰子的时候，假设第一个数组中的第n个数字表示骰子和为n出现的次数，那么此时和为n的骰子出现次数应该等于上一次循环中骰子点数和为**n-1,n-2,n-3,n-4,n-5,n-6**的次数总和，所以我们把**另一个数组**的第**n**个数字设为前一个数组对应的n-1,n-2,n-3,n-4,n-5,n-6之和

- 第j次掷骰子，和最小为j，小于j的情况是不可能发生的！所以另不可能发生的次数设置为0！

- 第j次掷骰子，和最小为j，最大为g_maxValue*j，做二层循环

  - ```java
     for (int j = 2; j <= n; j++) {
                //第j次掷骰子，和最小为j，小于j的情况是不可能发生的！所以另不可能发生的次数设置为0！
                for (int i = 0; i < j; i++) {
                    result[1 - flag][i] = 0;
                }
                //第j次掷骰子，和最小为j，最大为g_maxValue*j
                for (int i = j; i <= g_maxValue * j; i++) {
                    //初始化，因为这个数组要重复使用，上一次的值要清0
                    result[1 - flag][i] = 0;
                    for (int k = 1; k <= i && k <= g_maxValue; k++) {
                        //进行赋值的核心代码
                        result[1 - flag][i] += result[flag][i - k];
                        count++;
                    }
                }
                //flag在0与1之间切换
                flag = 1 - flag;
            }
    ```

    

```java
public class ck {
    public static void main(String[] args) {
        probability(2);
    }

    private static int count = 0;
    private static int flag = 0;
    private static final int g_maxValue = 6;
    private static int[][] result;

    //基于循环求骰子点数
    public static void probability(int n) {
        if (n < 1) {
            return;
        }
        //初始化数组 2行6n+1列的数组,因为下面的i从1开始
        //两行数组来回切换用于省空间
        //每次都换一行数组做上一次
        result = new int[2][g_maxValue * n + 1];
        for (int i = 0; i < g_maxValue; i++) {
            result[0][i] = 0;
            result[1][i] = 0;
        }
        //当第一次抛掷骰子时，有6种可能，每种可能出现一次
        for (int i = 1; i <= g_maxValue; i++) {
            result[0][i] = 1;
        }
        // 从第二次开始掷骰子，假设第一个数组中的第n个数字表示骰子和为n出现的次数，
        // 在下一循环中，我们加上一个新骰子，此时和为n的骰子出现次数应该等于上一次循环中骰子点数和为
        // n-1,n-2,n-3,n-4,n-5，n-6的次数总和，
        // 所以我们把另一个数组的第n个数字设为前一个数组对应的n-1,n-2,n-3,n-4,n-5，n-6之和
        for (int j = 2; j <= n; j++) {
            //第j次掷骰子，和最小为j，小于j的情况是不可能发生的！所以另不可能发生的次数设置为0！
            for (int i = 0; i < j; i++) {
                result[1 - flag][i] = 0;
            }
            //第j次掷骰子，和最小为j，最大为g_maxValue*j
            for (int i = j; i <= g_maxValue * j; i++) {
                //初始化，因为这个数组要重复使用，上一次的值要清0
                result[1 - flag][i] = 0;
                for (int k = 1; k <= i && k <= g_maxValue; k++) {
                    //进行赋值的核心代码
                    result[1 - flag][i] += result[flag][i - k];
                    count++;
                }
            }
            //flag在0与1之间切换
            //1-flag是当前要计算的，flag是上次的
            flag = 1 - flag;
        }

        //计算最终结果
        double total = Math.pow(g_maxValue, n);
        for (int i = n; i <= g_maxValue * n; i++) {
            double ratio = result[flag][i] / total;
            System.out.println("sum: " + i + " ratio: " + ratio);
        }
        System.out.println("次数" + count);
    }
}
```



