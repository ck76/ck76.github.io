[TOC]

- https://lfool.github.io/LFool-Notes/algorithm/%E6%9C%80%E9%95%BF%E5%85%AC%E5%85%B1%E5%AD%90%E5%BA%8F%E5%88%97-%E6%A8%A1%E7%89%88-%E8%BE%93%E5%87%BA.html



[1143. 最长公共子序列](https://leetcode.cn/problems/longest-common-subsequence/)

[583. 两个字符串的删除操作](https://leetcode.cn/problems/delete-operation-for-two-strings/)

[712. 两个字符串的最小ASCII删除和](https://leetcode.cn/problems/minimum-ascii-delete-sum-for-two-strings/)

 

**「最长递增子序列」**针对一个字符串，求出其最长递增子序列 (废话文学！！) **详细介绍可见 [动态规划设计：最长递增子序列](https://lfool.github.io/LFool-Notes/algorithm/动态规划设计：最长递增子序列.html)**

**「最长重复子数组」**针对两个数组，求出其最长重复子数组 (子数组必须要连着)  **详细介绍可见 [最长重复子数组](https://lfool.github.io/LFool-Notes/algorithm/秒杀子数组类题目.html#最长重复子数组)** 

而我们今天要介绍的是**「最长公共子序列」**，它是针对两个字符串，求出其最长公共子序列 (子序列可以不用连着)

### 模版归纳

首先结合题目 **[最长公共子序列](https://leetcode.cn/problems/longest-common-subsequence/)**，归纳总结出「最长公共子序列」问题的模版

毫无疑问这种类型的题目需要使用「DP」去解决！！这里给出一个例子`s1 = "abcde", s2 = "ace"`，下面所有分析都围绕该样例展开

先给出`dp[][]`数组的定义：`dp[i][j]`表示子串`s1[0..i]`和`s2[0..j]`最长公共子序列的长度

那么「状态转移方程」是什么呢？

- 如果`s1[i] = s2[j]`，`dp[i][j] = dp[i - 1][j - 1] + 1`
- 如果`s1[i] != s2[j]`，`dp[i][j] = Math.max(dp[i][j - 1], dp[i - 1][j])`

为什么就是这样转移的呢？直接看下图：

![5](https://cdn.jsdelivr.net/gh/LFool/image-hosting@master/20220627/2049531656334193pgcIaD5.svg)

![image-20220915085906754](https://tva1.sinaimg.cn/large/e6c9d24egy1h66ynnidk8j20ve0mswgz.jpg)

那么「base case」是什么呢？

![6](https://cdn.jsdelivr.net/gh/LFool/image-hosting@master/20220627/21003216563348325Wnk1O6.svg)

![image-20220915085919532](https://tva1.sinaimg.cn/large/e6c9d24egy1h66ynvaw8sj20h60b8dgm.jpg)

如上图粉色标记出来的就是 base case。橙色标记出来的是相等的情况，其余是不等的情况

#### 完整模版

```java
public int longestCommonSubsequence(String text1, String text2) {
    int n1 = text1.length(), n2 = text2.length();
    int[][] dp = new int[n1 + 1][n2 + 1];
    for (int i = 1; i <= n1; i++) {
        for (int j = 1; j <= n2; j++) {
            char c1 = text1.charAt(i - 1);
            char c2 = text2.charAt(j - 1);
            // 相等情况
            if (c1 == c2) dp[i][j] = dp[i - 1][j - 1] + 1;
            // 不等情况
            else dp[i][j] = Math.max(dp[i][j - 1], dp[i - 1][j]);
        }
    }
    return dp[n1][n2];
}
```

#### 如何输出最长公共子序列

「最长公共子序列」问题基本都是要求返回一个最值即可，但是有时候面试官喜欢不按常理出牌，让你输出最长公共子序列

我们可以通过构造出来的二维`dp`数组来得到最长公共子序列。如下图所示，从最后一个点开始往左上角的方向遍历

![7](https://cdn.jsdelivr.net/gh/LFool/image-hosting@master/20220627/2106171656335177ipOoys7.svg)

![image-20220915085943639](https://tva1.sinaimg.cn/large/e6c9d24egy1h66yoa95g7j20hg0b40t4.jpg)

如果`s1[i] = s2[j]`，那么当前字符肯定在最长公共子序列中；否在我们就向左或者向上遍历

至于选择「向左」还是「向上」的方向，这就要和构造`dp`的时候联系起来。我们是挑了一个最大值，所以遍历的方向也是谁大就往谁的方向遍历

```java
public int longestCommonSubsequence(String text1, String text2) {
    
    // 同上面的模版
    
    /* ------- print ------- */
    int i = n1, j = n2;
    StringBuffer sb = new StringBuffer();
    while (i > 0 && j > 0) {
        char c1 = text1.charAt(i - 1);
        char c2 = text2.charAt(j - 1);
        if (c1 == c2) {
            sb.append(c1);
            // 向左上角遍历
            i--; j--;
        } else {
            // 向上
            if (dp[i - 1][j] > dp[i][j - 1]) i--;
            // 向左
            else j--;
        }
    }
    System.out.println(sb.reverse());
    /* ------- end ------- */
    return dp[n1][n2];
}
```

### 两个字符串的最小ASCII删除和

**题目详情可见 [两个字符串的最小ASCII删除和](https://leetcode.cn/problems/minimum-ascii-delete-sum-for-two-strings/)**

其实这个题目的底层也是「最长公共子序列」，只是问法稍微变化了一点

「需要被删除的字符 = 原字符串 - 最长公共子序列」

结合这个题目我们把`dp[][]`数组的定义稍微改改：`dp[i][j]`表示子串`s1[0..i]`和`s2[0..j]`最小 ASCII 删除和

那么「状态转移方程」是什么呢？(有点逆过程的意思！！！)

- 如果`s1[i] = s2[j]`，`dp[i][j] = dp[i - 1][j - 1]` (不需要被删除)
- 如果`s1[i] != s2[j]`，`dp[i][j] = Math.min(dp[i - 1][j] + s1[i], dp[i][j - 1] + s2[j])`

那么「base case」是什么呢？

![8](https://cdn.jsdelivr.net/gh/LFool/image-hosting@master/20220627/2125241656336324wx8fTB8.svg)

![image-20220915085928020](https://tva1.sinaimg.cn/large/e6c9d24egy1h66yo0d7yvj20g80c2aa8.jpg)

如上图粉色标记出来的就是 base case，`e`表示 e 的 ASCII 值

```java
public int minimumDeleteSum(String s1, String s2) {
    int n1 = s1.length(), n2 = s2.length();
    int[][] dp = new int[n1 + 1][n2 + 1];
    // base case
    for (int i = 1; i <= n1; i++) dp[i][0] = dp[i - 1][0] + s1.charAt(i - 1);
    for (int i = 1; i <= n2; i++) dp[0][i] = dp[0][i - 1] + s2.charAt(i - 1);
    for (int i = 1; i <= n1; i++) {
        for (int j = 1; j <= n2; j++) {
            int c1 = s1.charAt(i - 1);
            int c2 = s2.charAt(j - 1);
            // 相等情况
            if (c1 == c2) dp[i][j] = dp[i - 1][j - 1];
            // 不等情况
            else dp[i][j] = Math.min(dp[i][j - 1] + c2, dp[i - 1][j] + c1);
        }
    }
    return dp[n1][n2];
}
```

 