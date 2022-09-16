

[TOC]

- #### [698. 划分为k个相等的子集](https://leetcode.cn/problems/partition-to-k-equal-sum-subsets/)



# leetcode一博主

```java
//对每个数组元素，都有k种加入方法，所以选择列表即可定为 k个集合
class Solution {
    public boolean canPartitionKSubsets(int[] nums, int k) {
        int[] coll = new int[k];//记录k个集合
        Arrays.sort(nums);
        int len = nums.length;
        for (int i = 0; i < len/2; i++) {
            // 交换 nums[i] 和 nums[j]
            int temp = nums[i];
            nums[i] = nums[len - i - 1];
            nums[len - i - 1] = temp;
        }
        int sum = 0;
        for(int temp : nums)sum += temp;
        if((sum % k) != 0)//无法将数组之和平均分配到k个集合中
            return false; 
        int target = sum / k;//target为每个集合中的数据的目标值大小
        return backTrack(nums,coll,target,0);
        
    }
    //回溯暴力解决分配问题
    private boolean backTrack(int[] nums,int[] coll,int target,int index){
        //base case
        if(index == nums.length)
            return true;
        //每个数字都可以选择加入k个集合中的任意一个，直接暴力判断数字应该加入哪一个集合
        //选择列表：coll集合的k组
        for(int i = 0;i < coll.length;i++){
            //递归前做选择
            if(nums[index]+coll[i] > target)//剪枝,避免多次递归——若当前的nums[index]加入集合之后超过target了，那么向后继续找合适的coll[i]
                continue;
            
            // 【bucket[i]和bucket[i-1]是相对同一cur层而言的，如果num[cur]放入bucket[i-1]中，最终并没有成功，那么回溯到cur层，num[cur]将被放入bucket[i]中，由于bucket[i-1]和bucket[i]容量相同，最终结果也不会成功】
            if(i>0 && coll[i]==coll[i-1]) 
                continue;

            coll[i] += nums[index];

            //递归穷举下一个数字的选择
            if(backTrack(nums,coll,target,index+1))
                return true;

            //递归后撤销选择
            coll[i] -= nums[index];
            //剪枝：跟着评论区大佬写的，大概的意思就是：将所有的nums[i]都放到该coll中了，但是都没有满足条件的情况出现，因此撤销选择后就变成了0,；例如：target=8，7加入coll[i]之后，在nums[]中找寻1没找到，因此撤销选择后后就成为了0，这种情况可以直接return false
            // if(coll[i] == 0)
            //    break;
        }
        return false;
    }
}
```



之前说过回溯算法是笔试中最好用的算法，只要你没什么思路，就用回溯算法暴力求解，即便不能通过所有测试用例，多少能过一点。

回溯算法的技巧也不难，前文 [回溯算法框架套路](http://mp.weixin.qq.com/s?__biz=MzAxODQxMDM0Mw==&mid=2247484709&idx=1&sn=1c24a5c41a5a255000532e83f38f2ce4&chksm=9bd7fb2daca0723be888b30345e2c5e64649fc31a00b05c27a0843f349e2dd9363338d0dac61&scene=21#wechat_redirect) 说过，回溯算法就是穷举一棵决策树的过程，只要在递归之前「做选择」，在递归之后「撤销选择」就行了。

但是，就算暴力穷举，不同的思路也有优劣之分。

本文就来看一道非常经典的回溯算法问题，子集划分问题，可以帮你更深刻理解回溯算法的思维，得心应手地写出回溯函数。

题目非常简单：

给你输入一个数组`nums`和一个正整数`k`，请你判断`nums`是否能够被平分为元素和相同的`k`个子集。

函数签名如下：

```
boolean canPartitionKSubsets(int[] nums, int k);
```

我们之前 [背包问题之子集划分](http://mp.weixin.qq.com/s?__biz=MzAxODQxMDM0Mw==&mid=2247485103&idx=1&sn=8a9752e18ed528e5c18d973dcd134260&chksm=9bd7f8a7aca071b14c736a30ef7b23b80914c676414b01f8269808ef28da48eb13e90a432fff&scene=21#wechat_redirect) 写过一次子集划分问题，不过那道题只需要我们把集合划分成两个相等的集合，可以转化成背包问题用动态规划技巧解决。

但是如果划分成多个相等的集合，解法一般只能通过暴力穷举，时间复杂度爆表，是练习回溯算法和递归思维的好机会。

### 一、思路分析

把装有`n`个数字的数组`nums`分成`k`个和相同的集合，你可以想象将`n`个数字分配到`k`个「桶」里，最后这`k`个「桶」里的数字之和要相同。

前文 [回溯算法框架套路](http://mp.weixin.qq.com/s?__biz=MzAxODQxMDM0Mw==&mid=2247484709&idx=1&sn=1c24a5c41a5a255000532e83f38f2ce4&chksm=9bd7fb2daca0723be888b30345e2c5e64649fc31a00b05c27a0843f349e2dd9363338d0dac61&scene=21#wechat_redirect) 说过，回溯算法的关键在哪里？

关键是要知道怎么「做选择」，这样才能利用递归函数进行穷举。

那么回想我们这个问题，将`n`个数字分配到`k`个桶里，我们可以有两种视角：

**视角一，如果我们切换到这`n`个数字的视角，每个数字都要选择进入到`k`个桶中的某一个**。

**视角二，如果我们切换到这`k`个桶的视角，对于每个桶，都要遍历`nums`中的`n`个数字，然后选择是否将当前遍历到的数字装进自己这个桶里**。

你可能问，这两种视角有什么不同？

**用不同的视角进行穷举，虽然结果相同，但是解法代码的逻辑完全不同；对比不同的穷举视角，可以帮你更深刻地理解回溯算法，我们慢慢道来**。

### 二、以数字的视角

用 for 循环迭代遍历`nums`数组大家肯定都会：

```java
for (int index = 0; index < nums.length; index++) {
    System.out.println(nums[index]);
}
```

递归遍历数组你会不会？其实也很简单：

```java
void traverse(int[] nums, int index) {
    if (index == nums.length) {
        return;
    }
    System.out.println(nums[index]);
    traverse(nums, index + 1);
}
```

只要调用`traverse(nums, 0)`，和 for 循环的效果是完全一样的。

那么回到这道题，以数字的视角，选择`k`个桶，用 for 循环写出来是下面这样：

```java
// k 个桶（集合），记录每个桶装的数字之和
int[] bucket = new int[k];

// 穷举 nums 中的每个数字
for (int index = 0; index < nums.length; index++) {
    // 穷举每个桶
    for (int i = 0; i < k; i++) {
        // nums[index] 选择是否要进入第 i 个桶
        // ...
    }
}
```

如果改成递归的形式，就是下面这段代码逻辑：

```java
// k 个桶（集合），记录每个桶装的数字之和
int[] bucket = new int[k];

// 穷举 nums 中的每个数字
void backtrack(int[] nums, int index) {
    // base case
    if (index == nums.length) {
        return;
    }
    // 穷举每个桶
    for (int i = 0; i < bucket.length; i++) {
        // 选择装进第 i 个桶
        bucket[i] += nums[index];
        // 递归穷举下一个数字的选择
        backtrack(nums, index + 1);
        // 撤销选择
        bucket[i] -= nums[index];
    }
}
```

虽然上述代码仅仅是穷举逻辑，还不能解决我们的问题，但是只要略加完善即可：

```java
// 主函数
public boolean canPartitionKSubsets(int[] nums, int k) {
    // 排除一些基本情况
    if (k > nums.length) return false;
    int sum = 0;
    for (int v : nums) sum += v;
    if (sum % k != 0) return false;

    // k 个桶（集合），记录每个桶装的数字之和
    int[] bucket = new int[k];
    // 理论上每个桶（集合）中数字的和
    int target = sum / k;
    // 穷举，看看 nums 是否能划分成 k 个和为 target 的子集
    return backtrack(nums, 0, bucket, target);
}

// 递归穷举 nums 中的每个数字
boolean backtrack(
    int[] nums, int index, int[] bucket, int target) {

    if (index == nums.length) {
        // 检查所有桶的数字之和是否都是 target
        for (int i = 0; i < bucket.length; i++) {
            if (bucket[i] != target) {
                return false;
            }
        }
        // nums 成功平分成 k 个子集
        return true;
    }

    // 穷举 nums[index] 可能装入的桶
    for (int i = 0; i < bucket.length; i++) {
        // 剪枝，桶装装满了
        if (bucket[i] + nums[index] > target) {
            continue;
        }
        // 将 nums[index] 装入 bucket[i]
        bucket[i] += nums[index];
        // 递归穷举下一个数字的选择
        if (backtrack(nums, index + 1, bucket, target)) {
            return true;
        }
        // 撤销选择
        bucket[i] -= nums[index];
    }

    // nums[index] 装入哪个桶都不行
    return false;
}
```

有之前的铺垫，相信这段代码是比较容易理解的。这个解法虽然能够通过，但是耗时比较多，其实我们可以再做一个优化。

主要看`backtrack`函数的递归部分：

```java
for (int i = 0; i < bucket.length; i++) {
    // 剪枝
    if (bucket[i] + nums[index] > target) {
        continue;
    }

    if (backtrack(nums, index + 1, bucket, target)) {
        return true;
    }
}
```

**如果我们让尽可能多的情况命中剪枝的那个 if 分支，就可以减少递归调用的次数，一定程度上减少时间复杂度**。

如何尽可能多的命中这个 if 分支呢？要知道我们的`index`参数是从 0 开始递增的，也就是递归地从 0 开始遍历`nums`数组。

如果我们提前对`nums`数组排序，把大的数字排在前面，那么大的数字会先被分配到`bucket`中，对于之后的数字，`bucket[i] + nums[index]`会更大，更容易触发剪枝的 if 条件。

所以可以在之前的代码中再添加一些代码：

```java
public boolean canPartitionKSubsets(int[] nums, int k) {
    // 其他代码不变
    // ...
    /* 降序排序 nums 数组 */
    Arrays.sort(nums);
    int i = 0, j = nums.length - 1;
    for (; i < j; i++, j--) {
        // 交换 nums[i] 和 nums[j]
        int temp = nums[i];
        nums[i] = nums[j];
        nums[j] = temp;
    }
    /*******************/
    return backtrack(nums, 0, bucket, target);
}
```

由于 Java 的语言特性，这段代码通过先升序排序再反转，达到降序排列的目的。

### 三、以桶的视角

文章开头说了，**以桶的视角进行穷举，每个桶需要遍历`nums`中的所有数字，决定是否把当前数字装进桶中；当装满一个桶之后，还要装下一个桶，直到所有桶都装满为止**。

这个思路可以用下面这段代码表示出来：

```java
// 装满所有桶为止
while (k > 0) {
    // 记录当前桶中的数字之和
    int bucket = 0;
    for (int i = 0; i < nums.length; i++) {
        // 决定是否将 nums[i] 放入当前桶中
        bucket += nums[i] or 0;
        if (bucket == target) {
            // 装满了一个桶，装下一个桶
            k--;
            break;
        }
    }
}
```

那么我们也可以把这个 while 循环改写成递归函数，不过比刚才略微复杂一些，首先写一个`backtrack`递归函数出来：

```java
boolean backtrack(int k, int bucket, 
    int[] nums, int start, boolean[] used, int target);
```

不要被这么多参数吓到，我会一个个解释这些参数。**如果你能够透彻理解本文，也能得心应手地写出这样的回溯函数**。

这个`backtrack`函数的参数可以这样解释：

现在`k`号桶正在思考是否应该把`nums[start]`这个元素装进来；目前`k`号桶里面已经装的数字之和为`bucket`；`used`标志某一个元素是否已经被装到桶中；`target`是每个桶需要达成的目标和。

根据这个函数定义，可以这样调用`backtrack`函数：

```java
public boolean canPartitionKSubsets(int[] nums, int k) {
    // 排除一些基本情况
    if (k > nums.length) return false;
    int sum = 0;
    for (int v : nums) sum += v;
    if (sum % k != 0) return false;

    boolean[] used = new boolean[nums.length];
    int target = sum / k;
    // k 号桶初始什么都没装，从 nums[0] 开始做选择
    return backtrack(k, 0, nums, 0, used, target);
}
```

实现`backtrack`函数的逻辑之前，再重复一遍，从桶的视角：

1、需要遍历`nums`中所有数字，决定哪些数字需要装到当前桶中。

2、如果当前桶装满了（桶内数字和达到`target`），则让下一个桶开始执行第 1 步。

下面的代码就实现了这个逻辑：

```java
boolean backtrack(int k, int bucket, 
    int[] nums, int start, boolean[] used, int target) {
    // base case
    if (k == 0) {
        // 所有桶都被装满了，而且 nums 一定全部用完了
        // 因为 target == sum / k
        return true;
    }
    if (bucket == target) {
        // 装满了当前桶，递归穷举下一个桶的选择
        // 让下一个桶从 nums[0] 开始选数字
        return backtrack(k - 1, 0 ,nums, 0, used, target);
    }

    // 从 start 开始向后探查有效的 nums[i] 装入当前桶
    for (int i = start; i < nums.length; i++) {
        // 剪枝
        if (used[i]) {
            // nums[i] 已经被装入别的桶中
            continue;
        }
        if (nums[i] + bucket > target) {
            // 当前桶装不下 nums[i]
            continue;
        }
        // 做选择，将 nums[i] 装入当前桶中
        used[i] = true;
        bucket += nums[i];
        // 递归穷举下一个数字是否装入当前桶
        if (backtrack(k, bucket, nums, i + 1, used, target)) {
            return true;
        }
        // 撤销选择
        used[i] = false;
        bucket -= nums[i];
    }
    // 穷举了所有数字，都无法装满当前桶
    return false;
}
```

至此，这道题的第二种思路也完成了。

### 四、最后总结

本文写的这两种思路都可以通过所有测试用例，不过第一种解法即便经过了排序优化，也明显比第二种解法慢很多，这是为什么呢？

我们来分析一下这两个算法的时间复杂度，假设`nums`中的元素个数为`n`。

先说第一个解法，也就是从数字的角度进行穷举，`n`个数字，每个数字有`k`个桶可供选择，所以组合出的结果个数为`k^n`，时间复杂度也就是`O(k^n)`。

第二个解法，每个桶要遍历`n`个数字，选择「装入」或「不装入」，组合的结果有`2^n`种；而我们有`k`个桶，所以总的时间复杂度为`O(k*2^n)`。

**当然，这是理论上的最坏复杂度，实际的复杂度肯定要好一些，毕竟我们添加了这么多剪枝逻辑**。不过，从复杂度的上界已经可以看出第一种思路要慢很多了。

所以，谁说回溯算法没有技巧性的？虽然回溯算法就是暴力穷举，但穷举也分聪明的穷举方式和低效的穷举方式，关键看你以谁的「视角」进行穷举。

通俗来说，我们应该尽量「少量多次」，就是说宁可多做几次选择，也不要给太大的选择空间；宁可「二选一」选`k`次，也不要 「`k`选一」选一次。

这道题我们从两种视角进行穷举，虽然代码量看起来多，但核心逻辑都是类似的，相信你通过本文能够更深刻地理解回溯算法。



---

- https://leetcode.cn/problems/partition-to-k-equal-sum-subsets/solution/by-lfool-d9o7/
- https://leetcode.cn/problems/partition-to-k-equal-sum-subsets/solution/by-lfool-d9o7/
- https://leetcode.cn/problems/partition-to-k-equal-sum-subsets/solution/by-lfool-d9o7/



[698. 划分为k个相等的子集](https://leetcode-cn.com/problems/partition-to-k-equal-sum-subsets/)

[473. 火柴拼正方形](https://leetcode.cn/problems/matchsticks-to-square/)

[2305. 公平分发饼干](https://leetcode.cn/problems/fair-distribution-of-cookies/)

------

## 回顾框架

前面我们介绍了「**[回溯算法的框架](https://leetcode.cn/link/?target=https%3A%2F%2Flfool.github.io%2FLFool-Notes%2Falgorithm%2F回溯(DFS).html)**」「**[排列/组合/子集 问题](https://leetcode.cn/link/?target=https%3A%2F%2Flfool.github.io%2FLFool-Notes%2Falgorithm%2F排列-组合-子集问题.html)**」「**[秒杀所有岛屿题目(DFS)](https://leetcode.cn/link/?target=https%3A%2F%2Flfool.github.io%2FLFool-Notes%2Falgorithm%2F秒杀所有岛屿题目(DFS).html)**」

首先我们来复习一下回溯的思想 (因为今天的内容很硬核！！！) 关于回溯的具体内容可点击上述链接查看

在「回溯算法框架」中给出了解决回溯问题需要思考的 3 个问题：

- **路径**：已经做出的选择
- **选择列表**：当前可以做的选择
- **结束条件**：到达决策树底层，无法再做选择的条件

我们先结合下面的决策树，根据「排列」问题来详细分析一下如何理解「**路径**」和「**选择列表**」

- 当我们处于 `第 0 层` 的时候，其实可以做的选择有 3 种，即：选择 1 or 2 or 3
- 假定我们在 `第 0 层` 的时候选择了 1，那么当我们处于 `第 1 层` 的时候，可以做的选择有 2 种，即：2 or 3
- 假定我们在 `第 1 层` 的时候选择了 2，那么当我们处于 `第 2 层` 的时候，可以做的选择有 1 种，即：3
- 当我们到达 `第 3 层` 的时候，我们面前的选择依次为：1，2，3。这正好构成了一个完整的「**路径**」，也正是我们需要的结果

经过上面的分析，我们可以很明显的知道「**结束条件**」，即：所有数都被选择

```
// 结束条件：已经处理完所有数
if (track.size() == nums.length) {
    // 处理逻辑
}
```

![1500581649055658RPWy3c1.svg](https://pic.leetcode-cn.com/1650612125-duEOAF-1500581649055658RPWy3c1.svg)

## 引入问题

**题目详情可见 [划分为k个相等的子集](https://leetcode-cn.com/problems/partition-to-k-equal-sum-subsets/)**

我们先给出一个样例：`nums = [1, 2, 2, 4, 3, 3], k = 3`，和题目中的样例不同，下面的所有分析都围绕这个样例展开

## 数据预处理

我们先对数据进行预处理，主要就是计算每个子集的和是多少！直接给出代码

```java
// 求总和
int sum = 0;
for (int i = 0; i < nums.length; i++) sum += nums[i];
// 不能刚好分配的情况
if (sum % k != 0) return false;
// target 即每个子集所需要满足的和
int target = sum / k;
```

## 问题分析

我们先对问题进行一层抽象：有 n 个球，k 个桶，如何分配球放入桶中使得每个桶中球的总和均为 `target`。如下图所示：

![3.svg](https://pic.leetcode-cn.com/1650612185-BjJVjS-3.svg)

为了可以更好的理解「回溯」的思想，我们这里提供两种不同的视角进行分析对比

**视角一**：我们站在球的视角，每个球均需要做出三种选择，即：选择放入 1 号桶、2 号桶、3 号桶。所有球一共需要做 k^n*k**n* 次选择 (分析时间复杂度会用到)

这里提一个点：由于回溯就是一种暴力求解的思想，所以对于每个球的三种选择，只有执行了该选择才知道该选择是否为最优解。说白了就是依次执行这三种选择，如果递归到下面后发现该选择为非最优解，然后开始回溯，执行其他选择，直到把所有选择都遍历完

**视角二**：我们站在桶的视角，每个桶均需要做出六次选择，即：是否选择 1 号球放入、是否选择 2 号球放入、...、是否选择 6 号球放入。所有的桶一共需要做 k 2^n*k*2*n* 次选择

## 视角一：球视角

如下图所示，「球」选择「桶」

![4.svg](https://pic.leetcode-cn.com/1650612256-bolKmo-4.svg)

下面给出「球视角」下的决策树

首先解释一下这棵决策树，`第 i 层` 为 `第 i 个球` 做选择，可做的选择：`选择 1 or 2 or 3 号桶`，直到 `第 n 个球` 做完选择后结束

由于，每个桶可以放下不止一个球，所以不存在某一个球选择了 1 号桶，另一个球就不能放入 1 号桶。判断是否可以放下的条件为：放入该球后，桶是否溢出？

![6.svg](https://pic.leetcode-cn.com/1650612305-HpUTlI-6.svg)

同样的，根据本文开头给出的框架，详细分析一下如何理解「**路径**」和「**选择列表**」

- 当我们处于 `第 1 层` 的时候，即值为「1」的球开始做选择，可以做的选择有 3 种，即：选择放入 1 or 2 or 3 号桶
- 假定我们在 `第 1 层` 的时候选择了放入 1 号桶，那么当我们处于 `第 2 层` 的时候，即值为「2」的球开始做选择，可以做的选择有 3 种，即：选择放入 1 or 2 or 3 号桶
- 假定我们在 `第 2 层` 的时候选择了放入 1 号桶，那么当我们处于 `第 3 层` 的时候，即值为「2」的球开始做选择，可以做的选择有 3 种，即：选择放入 1 or 2 or 3 号桶
- 假定我们在 `第 3 层` 的时候选择了放入 1 号桶，那么当我们处于 `第 4 层` 的时候，即值为「4」的球开始做选择，可以做的选择有 2 种，即：选择放入 2 or 3 号桶 (原因：1 号桶放入了 1 2 2，已经满了)
- 假定我们在 `第 4 层` 的时候选择了放入 2 号桶，那么当我们处于 `第 5 层` 的时候，即值为「3」的球开始做选择，可以做的选择有 1 种，即：选择放入 3 号桶 (原因：2 号桶放入了 4，容纳不下 3 了)
- 假定我们在 `第 5 层` 的时候选择了放入 3 号桶，那么当我们处于 `第 6 层` 的时候，即值为「3」的球开始做选择，可以做的选择有 0 种 (原因：3 号桶放入了 3，容纳不下 3 了)
- 此时我们已经到达了最后一层！！我们来梳理一下选择的路径，即：「1 号桶：1 2 2」「 2 号桶：4」「3 号桶：3」。显然这条路径是不符合要求的，所以就开始回溯，回溯到 `第 5 层`，改变 `第 5 层` 的选择，以此类推，直到得出「最优解」

经过上面的分析，我们可以很明显的知道「**结束条件**」，即：所有球都做了选择后结束

```java
// 结束条件：已经处理完所有球
if (index == nums.length) {
    // 处理逻辑
}
```

这里来一个小插曲。根据上面的分析可以知道，其实我们每一层做选择的球都是按顺序执行的。我们可以很容易的用迭代的方法遍历一个数组，那么如何递归的遍历一个数组呢？？？

```java
// 迭代
private void traversal(int[] nums) {
    for (int i = 0; i < nums.length; i++) {
        System.out.println(nums[i]);
    }
}

// 递归
private void traversal(int[] nums, int index) {
    if (index == nums.length) return ;
    // 处理当前元素
    System.out.println(nums[i]);
    // 递归处理 index + 1 后的元素
    traversal(nums, index + 1);
}
traversal(nums, 0);
```

### 第一版代码

好了，下面给出第一版代码：**(温馨提示：结合注释以及上面的分析一起看，便于理解，整个流程高度吻合)**

```java
public boolean canPartitionKSubsets(int[] nums, int k) {
    int sum = 0;
    for (int i = 0; i < nums.length; i++) sum += nums[i];
    if (sum % k != 0) return false;
    int target = sum / k;
    int[] bucket = new int[k + 1];
    return backtrack(nums, 0, bucket, k, target);
}
// index : 第 index 个球开始做选择
// bucket : 桶
private boolean backtrack(int[] nums, int index, int[] bucket, int k, int target) {

    // 结束条件：已经处理完所有球
    if (index == nums.length) {
        // 判断现在桶中的球是否符合要求 -> 路径是否满足要求
        for (int i = 0; i < k; i++) {
            if (bucket[i] != target) return false;
        }
        return true;
    }

    // nums[index] 开始做选择
    for (int i = 0; i < k; i++) {
        // 剪枝：放入球后超过 target 的值，选择一下桶
        if (bucket[i] + nums[index] > target) continue;
        // 做选择：放入 i 号桶
        bucket[i] += nums[index];

        // 处理下一个球，即 nums[index + 1]
        if (backtrack(nums, index + 1, bucket, k, target)) return true;

        // 撤销选择：挪出 i 号桶
        bucket[i] -= nums[index];
    }

    // k 个桶都不满足要求
    return false;
}
```

这里有一个好消息和一个坏消息，想先听哪一个呢？？哈哈哈哈哈

好消息：代码没问题

坏消息：超时没通过

![2200471650549647JAf0ikimage-20220421220047241.png](https://tva1.sinaimg.cn/large/e6c9d24egy1h66yuw84gnj21kk0g20um.jpg)

回到最上面的分析，我们必须一直回溯到 `第 2 层`，让第一个值为「2」的球选择 2 号桶，才更接近我们的最优解，其他的以此类推！

现在超时的原因就很明显了，由于我们的时间复杂度为 O(k^n)*O*(*k**n*)，呈指数增加，直接爆掉了

### 第一次尝试剪枝

我们有一个优化的思路，先看剪枝部分的代码：

```
// 剪枝：放入球后超过 target 的值，选择一下桶
if (bucket[i] + nums[index] > target) continue;
```

如果我们让 `nums[]` 内的元素递减排序，先让值大的元素选择桶，这样可以增加剪枝的命中率，从而降低回溯的概率

```java
public boolean canPartitionKSubsets(int[] nums, int k) {
    // 其余代码不变
    
    // 降序排列
    Arrays.sort(nums);
    int left = 0, right= nums.length - 1;
    while (left < right) {
        int temp = nums[left];
        nums[left] = nums[right];
        nums[right] = temp;
        left++;
        right--;
    }
    
    return backtrack(nums, 0, bucket, k, target);
}
```

很遗憾，还是超时，**但肯定比第一版的快点**

其实主要原因还是在于这种思路的时间复杂度太高，无论怎么优化，还是很高！！！直接 O(k^n)*O*(*k**n*)，这谁顶得住呀！！！

### 第二次尝试剪枝

🔥🔥🔥 **发现新大陆！！！**

突然看到了一种新的「剪枝」思路，这个剪枝绝了，不得不更新一下文章

首先需要优化的第一个点：

```java
// 结束条件：已经处理完所有球
if (index == nums.length) {
    // 有人提出，其实这个地方不需要判断，因为当 index == num.length 时，所有球已经按要求装入所有桶，所以肯定是一个满足要求的解
    // 即：每个桶内球的和一定为 target
    /** // 判断现在桶中的球是否符合要求 -> 路径是否满足要求
    for (int i = 0; i < k; i++) {
        if (bucket[i] != target) return false;
    }**/
    return true;
}
```

其次可以优化的第二个点和 **[排列/组合/子集 问题](https://leetcode.cn/link/?target=https%3A%2F%2Flfool.github.io%2FLFool-Notes%2Falgorithm%2F排列-组合-子集问题.html%23元素可重不可复选)** 中「元素可重不可复选」情况下「子集」的处理情况很相似！！！

```java
for (int i = 0; i < k; i++) {
    // 如果当前桶和上一个桶内的元素和相等，则跳过
    // 原因：如果元素和相等，那么 nums[index] 选择上一个桶和选择当前桶可以得到的结果是一致的
    if (i > 0 && bucket[i] == bucket[i - 1]) continue;
    // 其他逻辑不变
}
```

最后可以优化的第三个点，对于第一个球，任意放到某个桶中的效果都是一样的，所以我们规定放到第一个桶中

```java
for (int i = 0; i < k; i++) {
    if (i > 0 && index == 0) break ;
    // 其他逻辑不变
}
```

下面给出优化后的执行时间：(惊不惊喜，意不意外！！！！)

![0934461652837686XuLMWVimage-20220518093445847.png](https://tva1.sinaimg.cn/large/e6c9d24egy1h66yuyj8xvj20xg0lwac0.jpg)

## 视角二：桶视角

现在来介绍另外一种视角，如下图所示，「桶」选择「球」

![8.svg](https://pic.leetcode-cn.com/1650612523-XIwJRu-8.svg)

下面给出「桶视角」下的决策树

首先解释一下这棵决策树，`第 i 层`为`j 号桶`做出`第 x 次选择`，可做的选择：`是否选择 1 ～ 6 号球`，直到`k 个桶`均装满后结束

由于，每个球只能被一个桶选择，所以当某一个球被某一个桶选择后，另一个桶就不能选择该球，如下图红色标注出的分支

判断是否可以选择某个球的条件为：(1) 该球是否已经被选择？ (2) 放入该球后，桶是否溢出？

这里还需要强调的一点是，我们是根据每个桶可以做的最多次选择来绘制的决策树，即 6 次选择，但在实际中可能经过两三次选择后桶就装满了，然后下一个桶开始选择。之所以会有 6 次选择，是因为可能在后面回溯的过程过中进行其他选择

![1.svg](https://pic.leetcode-cn.com/1652702269-xGJhIl-1.svg)

其中黄色框出来的分支是当前桶选择的冗余分支！！！举个简单例子，「1」号桶开始先选值为「1」的球，再选值为「2」的球 和 「1」号桶开始先选值为「2」的球，再选值为「1」的球是等价的，因为没有顺序的约束！！

同样的，根据本文开头给出的框架，详细分析一下如何理解「**路径**」和「**选择列表**」

- 当我们处于 `第 1 层` 的时候，即「1」号桶开始第「1」次选择，可以做的选择有 6 种，即：选择值为「1 or 2 or 2 or 4 or 3 or 3」的球
- 假定我们在 `第 1 层` 的时候「1」号桶选择了值为「1」的球，那么当我们处于 `第 2 层` 的时候，即「1」号桶开始第「2」次选择，可以做的选择有 5 种，即：选择值为「2 or 2 or 4 or 3 or 3」的球
- 假定我们在 `第 2 层` 的时候「1」号桶选择了值为「2」的球，那么当我们处于 `第 3 层` 的时候，即「1」号桶开始第「3」次选择，可以做的选择有 4 种，即：选择值为「2 or 4 or 3 or 3」的球
- 假定我们在 `第 3 层` 的时候「1」号桶选择了值为「2」的球，那么当我们处于 `第 4 层` 的时候，开始下一个桶开始选择 (原因：1 号桶选择了 1 2 2，已经满了) 即「2」号桶开始第「1」次选择，可以做的选择有 3 种，即：选择值为「4 or 3 or 3」的球
- ......
- 开始回溯......以此类推，直到得出「最优解」

假定得到了「最优解」，我们来梳理一下此时选择的路径，即：「1 号桶：1 4」「 2 号桶：2 3」「3 号桶：2 3」，具体如下图所示：

![101.svg](https://pic.leetcode-cn.com/1662953827-YnOFuM-101.svg)

经过上面的分析，我们可以很明显的知道「**结束条件**」，即：所有均装满后结束

```java
if (k == 0) {
    // 处理逻辑
}
```

现在我们再次回到本文最开始复习的「回溯」框架需要思考的三个问题，即：「**路径**」「**选择列表**」「**结束条件**」

有没有发现一个很有意思的现象，这难道不是和求「树的所有从根到叶子节点的路径」如出一辙嘛！！不信的话可以先去写一下 **[257. 二叉树的所有路径](https://leetcode-cn.com/problems/binary-tree-paths/)**

我们先复习一下求「树的所有路径」的思路

- **选择列表**：每次我们都可以选择当前节点的「左***」或「右***」
- **结束条件**：遇到叶子节点
- **路径**：在遍历过程中记录我们所有的选择的一条路

回到「回溯」问题上，相比于「树的所有路径」

- 只不过这棵树需要我们自己抽象出来而已，即「决策树」
- 只不过结束条件需要我们根据题目意思自己确定而已
- 只不过我们需要的是一条「最优解」路径，即：从所有路径中得到最优解路径
- 同时，我们需要通过「剪枝」来减少对「决策树」的递归遍历而已

### 第一版代码

好了，下面给出第一版代码：**(温馨提示：结合注释以及上面的分析一起看，便于理解，整个流程高度吻合)**

```java
private boolean backtrack(int[] nums, int start, int[] bucket, int k, int target, boolean[] used) {
    // k 个桶均装满
    if (k == 0) return true;
    // 当前桶装满了，开始装下一个桶
    if (bucket[k] == target) {
        // 注意：桶从下一个开始；球从第一个开始
        return backtrack(nums, 0, bucket, k - 1, target, used);
    }
    // 第 k 个桶开始对每一个球选择进行选择是否装入
    for (int i = start; i < nums.length; i++) {
        // 如果当前球已经被装入，则跳过
        if (used[i]) continue;
        // 如果装入当前球，桶溢出，则跳过
        if (bucket[k] + nums[i] > target) continue;

        // 装入 && 标记已使用
        bucket[k] += nums[i];
        used[i] = true;

        // 开始判断是否选择下一个球
        // 注意：桶依旧是当前桶；球是下一个球
        // 注意：是 i + 1
        if (backtrack(nums, i + 1, bucket, k, target, used)) return true;

        // 拿出 && 标记未使用
        bucket[k] -= nums[i];
        used[i] = false;
    }
    // 如果所有球均不能使所有桶刚好装满
    return false;
}
```

可是可是可是，虽然可以过，但是执行时间吓死个人！！！

![image.png](https://tva1.sinaimg.cn/large/e6c9d24egy1h66yuwyopwj20y202odfv.jpg)

### 第一次尝试优化

前文分析过该视角下的时间复杂度为 k 2^n*k*2*n*。其实我们上面的代码在递归的过程中存在很多冗余的计算，导致超时

现在我们假设一种情况，`num = {1, 2, 4, 3, ......}, target = 5`

第一个桶会首先选择 1 4。如下图橙色所示

![15.svg](https://pic.leetcode-cn.com/1650612646-CPHudp-15.svg)

第二个桶会选择 2 3。如下图绿色所示

![16.svg](https://pic.leetcode-cn.com/1650612660-LtfDiH-16.svg)

现在假设后面的元素无法完美组合成目标和，程序会进行回溯！！假设当前回溯到了「1」号桶开始第「1」次选择，故「1」号桶的第「1」次选择会发生改变 1 -> 2。如下图橙色所示

![17.svg](https://pic.leetcode-cn.com/1650612669-SSIMiP-17.svg)

接着第二个桶的选择也会改变。如下图绿色所示

![18.svg](https://pic.leetcode-cn.com/1650612679-EFjkye-18.svg)

显然，虽然这一次的回溯结果中「1」号桶和「2」号桶选择的元素发生了改变，但是它们组合起来的选择没有变化，依旧是 1 2 4 3，剩下的元素未发生改变，所以依旧无法完美组合成目标和

如果我们把这样的组合记录下来，下次遇到同样的组合则直接跳过。那如何记录这种状态呢？？？--> **借助 `used[]` 数组**

可以看到上述四张图片中的 `used[]` 状态分别为：

- 图片一：「true, false, true, false, false, ....」
- 图片二：「true, true, true, true, false, ....」
- 图片三：「false, true, false, true, false, ....」
- 图片四：「true, true, true, true, false, ....」

第一次优化代码如下：

```java
// 备忘录，存储 used 数组的状态
private HashMap<String, Boolean> memo = new HashMap<>();
private boolean backtrack(int[] nums, int start, int[] bucket, int k, int target, boolean[] used) {
    // k 个桶均装满
    if (k == 0) return true;

    // 将 used 的状态转化成形如 [true, false, ...] 的字符串
    // 便于存入 HashMap
    String state = Arrays.toString(used);

    // 当前桶装满了，开始装下一个桶
    if (bucket[k] == target) {
        // 注意：桶从下一个开始；球从第一个开始
        boolean res = backtrack(nums, 0, bucket, k - 1, target, used);
        memo.put(state, res);
        return res;
    }

    if (memo.containsKey(state)) {
        // 如果当前状态曾今计算过，就直接返回，不要再递归穷举了
        return memo.get(state);
    }
    
    // 其他逻辑不变！！
}
```

虽然耗时少了很多，但效率依然是比较低

![image.png](https://tva1.sinaimg.cn/large/e6c9d24egy1h66yuzdwzyj20xy02udfv.jpg)

### 第二次尝试优化

这次不是因为算法逻辑上的冗余计算，而是代码实现上的问题

**因为每次递归都要把 `used` 数组转化成字符串，这对于编程语言来说也是一个不小的消耗，所以我们还可以进一步优化**

结合题目意思，可以知道 `1 <= len(nums) <= 16`，所以我们可以用 16 位二进制来记录元素的使用情况，即：如果第 i 个元素使用了，则第 i 位二进制设为 1，否则为 0

关于位运算技巧，**详情可见 [位运算技巧](https://leetcode.cn/link/?target=https%3A%2F%2Flfool.github.io%2FLFool-Notes%2Falgorithm%2F位运算技巧.html%23任意位置置-1-or-0)**

下面给出第二次优化代码 (最终完整代码)，如下：

```java
// 备忘录，存储 used 的状态
private HashMap<Integer, Boolean> memo = new HashMap<>();

public boolean canPartitionKSubsets(int[] nums, int k) {
    int sum = 0;
    for (int i = 0; i < nums.length; i++) sum += nums[i];
    if (sum % k != 0) return false;
    int target = sum / k;
    // 使用位图技巧
    int used = 0;
    int[] bucket = new int[k + 1];
    return backtrack(nums, 0, bucket, k, target, used);
}
private boolean backtrack(int[] nums, int start, int[] bucket, int k, int target, int used) {
    // k 个桶均装满
    if (k == 0) return true;

    // 当前桶装满了，开始装下一个桶
    if (bucket[k] == target) {
        // 注意：桶从下一个开始；球从第一个开始
        boolean res = backtrack(nums, 0, bucket, k - 1, target, used);
        memo.put(used, res);
        return res;
    }

    if (memo.containsKey(used)) {
        // 如果当前状态曾今计算过，就直接返回，不要再递归穷举了
        return memo.get(used);
    }

    // 第 k 个桶开始对每一个球选择进行选择是否装入
    for (int i = start; i < nums.length; i++) {
        // 如果当前球已经被装入，则跳过
        if (((used >> i) & 1) == 1) continue;
        // 如果装入当前球，桶溢出，则跳过
        if (bucket[k] + nums[i] > target) continue;

        // 装入 && 标记已使用
        bucket[k] += nums[i];
        // 将第 i 位标记为 1
        used |= 1 << i;

        // 开始判断是否选择下一个球
        // 注意：桶依旧是当前桶；球是下一个球
        // 注意：是 i + 1
        if (backtrack(nums, i + 1, bucket, k, target, used)) return true;

        // 拿出 && 标记未使用
        bucket[k] -= nums[i];
        // 将第 i 位标记为 0
        used ^= 1 << i;
    }
    // 如果所有球均不能使所有桶刚好装满
    return false;
}
```

至此，终于还算完美的通过了，太不容易了！！！😭😭😭😭

![image.png](https://tva1.sinaimg.cn/large/e6c9d24egy1h66yuvczojj20xy0ludhs.jpg)

### 牛的不行的剪枝

🔥🔥🔥 **再次发现新大陆！！！**

刚刚有个小伙伴给出了一种新的剪枝策略，在这里感谢一下这位小伙伴 [@yttttt-e](https://leetcode.cn/u/yttttt-e/)

先看代码：(温馨提醒：前提是数组需要有序)

```
for (int i = start; i < nums.length; i++) {
    // 其他逻辑不变 ....
    used ^= 1 << i;
    
    // 下一个如果和当前的相同肯定也不行
    while (i + 1 < nums.length && nums[i + 1] == nums[i]) i++;
}
```

解释一下是什么意思！当我们在处理第 `i` 个球的时候发现无法满足要求，如果这个时候下一个球和当前球的值是一样的，那么我们就可以直接跳过下一个球

按照惯例看一下耗时：(有很大的提高！！再次证明了回溯问题全靠剪枝)

![image.png](https://tva1.sinaimg.cn/large/e6c9d24egy1h66yuxi8iij20ta0lyjt6.jpg)

## 时间复杂度分析

对于两种不同视角下的时间复杂度，前文也给出了简约的分析！！

对于视角一 (球视角) 和视角二 (桶视角)，前者为 O(k^n)*O*(*k**n*)，后者为 O(k2^n)*O*(*k*2*n*)。其实差距还是挺大的，尤其是当 k*k* 越大时，这种差距越明显！！

现在结合回溯的每一次选择分析时间复杂度，「尽可能让每一次的可选择项少」才能使时间复杂度降低维度！！

- 对于视角一 (球视角)，每一次的可选择项都为「所有桶」，所以每一次可选择项均为桶的数量 k*k*。故时间复杂度指数的底数为 k*k*
- 对于视角二 (桶视角)，每一次的可选择项都为「是否装入某个球」，所以每一次可选择项均为「装入」or「不装入」。故时间复杂度指数的底数为 22

所以，通俗来说，我们应该尽量「少量多次」，就是说宁可多做几次选择，也不要给太大的选择空间；宁可 `n` 次「二选一」重复 `k` 次 (O(k2^n)*O*(*k*2*n*))，也不要 `n` 次「`k` 选一」仅重复一次 (O(k^n)*O*(*k**n*))

**最后感谢一下「东哥」，一直都是跟着他学习的！！[labuladong 的算法小抄](https://leetcode.cn/link/?target=https%3A%2F%2Flabuladong.github.io%2Falgo%2F)**



### -----------------------------------------------------------------------------------

# labuladong

PS：本文是前文 [回溯算法牛逼！](http://mp.weixin.qq.com/s?__biz=MzAxODQxMDM0Mw==&mid=2247490981&idx=1&sn=db17864d2f6f189d63e051e434c05460&chksm=9bd7e3adaca06abbe32166196b812589de302cf38c419b1dba81ca7d6d094278235e42eb0ed2&scene=21#wechat_redirect) 的修订版，首先添加了两种回溯思想的来源，即排列公式的两种推导思路；另外，有读者反映力扣添加了测试用例，以前的解法代码现在会超时，所以我进一步优化了代码实现，使之能够通过力扣的测试用例。

以下是正文。

我经常说回溯算法是笔试中最好用的算法，只要你没什么思路，就用回溯算法暴力求解，即便不能通过所有测试用例，多少能过一点。

回溯算法的技巧也不难，前文 [回溯算法框架套路](https://mp.weixin.qq.com/s?__biz=MzAxODQxMDM0Mw==&mid=2247484709&idx=1&sn=1c24a5c41a5a255000532e83f38f2ce4&scene=21#wechat_redirect) 说过，回溯算法就是穷举一棵决策树的过程，只要在递归之前「做选择」，在递归之后「撤销选择」就行了。

**但是，就算暴力穷举，不同的思路也有优劣之分**。

本文就来看一道非常经典的回溯算法问题：子集划分问题。这道题可以帮你更深刻理解回溯算法的思维，得心应手地写出回溯函数。

题目非常简单：

给你输入一个数组 `nums` 和一个正整数 `k`，请你判断 `nums` 是否能够被平分为元素和相同的 `k` 个子集。

函数签名如下：

```
boolean canPartitionKSubsets(int[] nums, int k);
```

我们之前 [背包问题之子集划分](https://mp.weixin.qq.com/s?__biz=MzAxODQxMDM0Mw==&mid=2247485103&idx=1&sn=8a9752e18ed528e5c18d973dcd134260&scene=21#wechat_redirect) 写过一次子集划分问题，不过那道题只需要我们把集合划分成两个相等的集合，可以转化成背包问题用动态规划技巧解决。

但是如果划分成多个相等的集合，解法一般只能通过暴力穷举，时间复杂度爆表，是练习回溯算法和递归思维的好机会。

### 一、思路分析

**首先，我们回顾一下以前学过的排列组合知识：**

1、`P(n, k)`（也有很多书写成 `A(n, k)`）表示从 `n` 个不同元素中拿出 `k`个元素的排列（Permutation/Arrangement）总数；`C(n, k)` 表示从 `n` 个不同元素中拿出 `k` 个元素的组合（Combination）总数。

2、「排列」和「组合」的主要区别在于是否考虑顺序的差异。

3、排列、组合总数的计算公式：

![Image](https://tva1.sinaimg.cn/large/e6c9d24egy1h68q9t3bqcj20jc09qjrg.jpg)

好，现在我问一个问题，这个排列公式 `P(n, k)` 是如何推导出来的？为了搞清楚这个问题，我需要讲一点组合数学的知识。

**排列组合问题的各种变体都可以抽象成「球盒模型**」，`P(n, k)` 就可以抽象成下面这个场景：

![Image](https://tva1.sinaimg.cn/large/e6c9d24egy1h68q9xnvhhj20u00gwjs6.jpg)

即，将 `n` 个标记了不同序号的球（标号为了体现顺序的差异），放入 `k` 个标记了不同序号的盒子中（其中 `n >= k`，每个盒子最终都装有恰好一个球），共有 `P(n, k)` 种不同的方法。

现在你来，往盒子里放球，你怎么放？其实有两种视角。

**首先，你可以站在盒子的视角**，每个盒子必然要选择一个球。

这样，第一个盒子可以选择 `n` 个球中的任意一个，然后你需要让剩下 `k - 1` 个盒子在 `n - 1` 个球中选择：

![Image](https://tva1.sinaimg.cn/large/e6c9d24egy1h68q9yk2g4j20u00gwdgp.jpg)

**另外，你也可以站在球的视角**，因为并不是每个球都会被装进盒子，所以球的视角分两种情况：

1、第一个球可以不装进任何一个盒子，这样的话你就需要将剩下 `n - 1` 个球放入 `k` 个盒子。

2、第一个球可以装进 `k` 个盒子中的任意一个，这样的话你就需要将剩下 `n - 1` 个球放入 `k - 1` 个盒子。

结合上述两种情况，可以得到：

![Image](https://tva1.sinaimg.cn/large/e6c9d24egy1h68q9y4onfj20u00gw75b.jpg)

你看，两种视角得到两个不同的递归式，但这两个递归式解开的结果都是我们熟知的阶乘形式：

![Image](https://tva1.sinaimg.cn/large/e6c9d24egy1h68q9td3xnj20fy06ygln.jpg)

至于如何解递归式，涉及数学的内容比较多，这里就不做深入探讨了，有兴趣的读者可以自行学习组合数学相关知识。

回到正题，这道算法题让我们求子集划分，子集问题和排列组合问题有所区别，但我们可以借鉴「球盒模型」的抽象，用两种不同的视角来解决这道子集划分问题。

把装有 `n` 个数字的数组 `nums` 分成 `k` 个和相同的集合，你可以想象将 `n` 个数字分配到 `k` 个「桶」里，最后这 `k` 个「桶」里的数字之和要相同。

前文 [回溯算法框架套路](https://mp.weixin.qq.com/s?__biz=MzAxODQxMDM0Mw==&mid=2247484709&idx=1&sn=1c24a5c41a5a255000532e83f38f2ce4&scene=21#wechat_redirect) 说过，回溯算法的关键在哪里？

关键是要知道怎么「做选择」，这样才能利用递归函数进行穷举。

那么模仿排列公式的推导思路，将 `n` 个数字分配到 `k` 个桶里，我们也可以有两种视角：

**视角一，如果我们切换到这 `n` 个数字的视角，每个数字都要选择进入到 `k` 个桶中的某一个**。

![Image](https://tva1.sinaimg.cn/large/e6c9d24egy1h68q9vx3s5j20u00gwmxz.jpg)

**视角二，如果我们切换到这 `k` 个桶的视角，对于每个桶，都要遍历 `nums` 中的 `n` 个数字，然后选择是否将当前遍历到的数字装进自己这个桶里**。

![Image](https://tva1.sinaimg.cn/large/e6c9d24egy1h68q9uv4s9j20u00gw0tk.jpg)

你可能问，这两种视角有什么不同？

**用不同的视角进行穷举，虽然结果相同，但是解法代码的逻辑完全不同，进而算法的效率也会不同；对比不同的穷举视角，可以帮你更深刻地理解回溯算法，我们慢慢道来**。

### 二、以数字的视角

用 for 循环迭代遍历 `nums` 数组大家肯定都会：

```
for (int index = 0; index < nums.length; index++) {
    System.out.println(nums[index]);
}
```

递归遍历数组你会不会？其实也很简单：

```
void traverse(int[] nums, int index) {
    if (index == nums.length) {
        return;
    }
    System.out.println(nums[index]);
    traverse(nums, index + 1);
}
```

只要调用 `traverse(nums, 0)`，和 for 循环的效果是完全一样的。

那么回到这道题，以数字的视角，选择 `k` 个桶，用 for 循环写出来是下面这样：

```
// k 个桶（集合），记录每个桶装的数字之和
int[] bucket = new int[k];

// 穷举 nums 中的每个数字
for (int index = 0; index < nums.length; index++) {
    // 穷举每个桶
    for (int i = 0; i < k; i++) {
        // nums[index] 选择是否要进入第 i 个桶
        // ...
    }
}
```

如果改成递归的形式，就是下面这段代码逻辑：

```
// k 个桶（集合），记录每个桶装的数字之和
int[] bucket = new int[k];

// 穷举 nums 中的每个数字
void backtrack(int[] nums, int index) {
    // base case
    if (index == nums.length) {
        return;
    }
    // 穷举每个桶
    for (int i = 0; i < bucket.length; i++) {
        // 选择装进第 i 个桶
        bucket[i] += nums[index];
        // 递归穷举下一个数字的选择
        backtrack(nums, index + 1);
        // 撤销选择
        bucket[i] -= nums[index];
    }
}
```

虽然上述代码仅仅是穷举逻辑，还不能解决我们的问题，但是只要略加完善即可：

```
// 主函数
boolean canPartitionKSubsets(int[] nums, int k) {
    // 排除一些基本情况
    if (k > nums.length) return false;
    int sum = 0;
    for (int v : nums) sum += v;
    if (sum % k != 0) return false;

    // k 个桶（集合），记录每个桶装的数字之和
    int[] bucket = new int[k];
    // 理论上每个桶（集合）中数字的和
    int target = sum / k;
    // 穷举，看看 nums 是否能划分成 k 个和为 target 的子集
    return backtrack(nums, 0, bucket, target);
}

// 递归穷举 nums 中的每个数字
boolean backtrack(
    int[] nums, int index, int[] bucket, int target) {

    if (index == nums.length) {
        // 检查所有桶的数字之和是否都是 target
        for (int i = 0; i < bucket.length; i++) {
            if (bucket[i] != target) {
                return false;
            }
        }
        // nums 成功平分成 k 个子集
        return true;
    }

    // 穷举 nums[index] 可能装入的桶
    for (int i = 0; i < bucket.length; i++) {
        // 剪枝，桶装装满了
        if (bucket[i] + nums[index] > target) {
            continue;
        }
        // 将 nums[index] 装入 bucket[i]
        bucket[i] += nums[index];
        // 递归穷举下一个数字的选择
        if (backtrack(nums, index + 1, bucket, target)) {
            return true;
        }
        // 撤销选择
        bucket[i] -= nums[index];
    }

    // nums[index] 装入哪个桶都不行
    return false;
}
```

有之前的铺垫，相信这段代码是比较容易理解的。这个解法虽然能够通过，但是耗时比较多，其实我们可以再做一个优化。

主要看 `backtrack` 函数的递归部分：

```
for (int i = 0; i < bucket.length; i++) {
    // 剪枝
    if (bucket[i] + nums[index] > target) {
        continue;
    }

    if (backtrack(nums, index + 1, bucket, target)) {
        return true;
    }
}
```

**如果我们让尽可能多的情况命中剪枝的那个 if 分支，就可以减少递归调用的次数，一定程度上减少时间复杂度**。

如何尽可能多的命中这个 if 分支呢？要知道我们的 `index` 参数是从 0 开始递增的，也就是递归地从 0 开始遍历 `nums` 数组。

如果我们提前对 `nums` 数组排序，把大的数字排在前面，那么大的数字会先被分配到 `bucket` 中，对于之后的数字，`bucket[i] + nums[index]` 会更大，更容易触发剪枝的 if 条件。

所以可以在之前的代码中再添加一些代码：

```
boolean canPartitionKSubsets(int[] nums, int k) {
    // 其他代码不变
    // ...
    /* 降序排序 nums 数组 */
    Arrays.sort(nums);
    for (i = 0, j = nums.length - 1; i < j; i++, j--) {
        // 交换 nums[i] 和 nums[j]
        int temp = nums[i];
        nums[i] = nums[j];
        nums[j] = temp;
    }
    /*******************/
    return backtrack(nums, 0, bucket, target);
}
```

由于 Java 的语言特性，这段代码通过先升序排序再反转，达到降序排列的目的。

### 三、以桶的视角

文章开头说了，**以桶的视角进行穷举，每个桶需要遍历 `nums` 中的所有数字，决定是否把当前数字装进桶中；当装满一个桶之后，还要装下一个桶，直到所有桶都装满为止**。

这个思路可以用下面这段代码表示出来：

```
// 装满所有桶为止
while (k > 0) {
    // 记录当前桶中的数字之和
    int bucket = 0;
    for (int i = 0; i < nums.length; i++) {
        // 决定是否将 nums[i] 放入当前桶中
        bucket += nums[i] or 0;
        if (bucket == target) {
            // 装满了一个桶，装下一个桶
            k--;
            break;
        }
    }
}
```

那么我们也可以把这个 while 循环改写成递归函数，不过比刚才略微复杂一些，首先写一个 `backtrack` 递归函数出来：

```
boolean backtrack(int k, int bucket, 
    int[] nums, int start, boolean[] used, int target);
```

不要被这么多参数吓到，我会一个个解释这些参数。**如果你能够透彻理解本文，也能得心应手地写出这样的回溯函数**。

这个 `backtrack` 函数的参数可以这样解释：

现在 `k` 号桶正在思考是否应该把 `nums[start]` 这个元素装进来；目前 `k` 号桶里面已经装的数字之和为 `bucket`；`used` 标志某一个元素是否已经被装到桶中；`target` 是每个桶需要达成的目标和。

根据这个函数定义，可以这样调用 `backtrack` 函数：

```
boolean canPartitionKSubsets(int[] nums, int k) {
    // 排除一些基本情况
    if (k > nums.length) return false;
    int sum = 0;
    for (int v : nums) sum += v;
    if (sum % k != 0) return false;

    boolean[] used = new boolean[nums.length];
    int target = sum / k;
    // k 号桶初始什么都没装，从 nums[0] 开始做选择
    return backtrack(k, 0, nums, 0, used, target);
}
```

实现 `backtrack` 函数的逻辑之前，再重复一遍，从桶的视角：

1、需要遍历 `nums` 中所有数字，决定哪些数字需要装到当前桶中。

2、如果当前桶装满了（桶内数字和达到 `target`），则让下一个桶开始执行第 1 步。

下面的代码就实现了这个逻辑：

```
boolean backtrack(int k, int bucket, 
    int[] nums, int start, boolean[] used, int target) {
    // base case
    if (k == 0) {
        // 所有桶都被装满了，而且 nums 一定全部用完了
        // 因为 target == sum / k
        return true;
    }
    if (bucket == target) {
        // 装满了当前桶，递归穷举下一个桶的选择
        // 让下一个桶从 nums[0] 开始选数字
        return backtrack(k - 1, 0 ,nums, 0, used, target);
    }

    // 从 start 开始向后探查有效的 nums[i] 装入当前桶
    for (int i = start; i < nums.length; i++) {
        // 剪枝
        if (used[i]) {
            // nums[i] 已经被装入别的桶中
            continue;
        }
        if (nums[i] + bucket > target) {
            // 当前桶装不下 nums[i]
            continue;
        }
        // 做选择，将 nums[i] 装入当前桶中
        used[i] = true;
        bucket += nums[i];
        // 递归穷举下一个数字是否装入当前桶
        if (backtrack(k, bucket, nums, i + 1, used, target)) {
            return true;
        }
        // 撤销选择
        used[i] = false;
        bucket -= nums[i];
    }
    // 穷举了所有数字，都无法装满当前桶
    return false;
}
```

**这段代码是可以得出正确答案的，但是效率很低，我们可以思考一下是否还有优化的空间**。

首先，在这个解法中每个桶都可以认为是没有差异的，但是我们的回溯算法却会对它们区别对待，这里就会出现重复计算的情况。

什么意思呢？我们的回溯算法，说到底就是穷举所有可能的组合，然后看是否能找出和为 `target` 的 `k` 个桶（子集）。

那么，比如下面这种情况，`target = 5`，算法会在第一个桶里面装 `1, 4`：

![Image](https://tva1.sinaimg.cn/large/e6c9d24egy1h68q9wg48rj20u00gwwf0.jpg)

现在第一个桶装满了，就开始装第二个桶，算法会装入 `2, 3`：

![Image](https://tva1.sinaimg.cn/large/e6c9d24egy1h68q9x8gcfj20u00gwaal.jpg)

然后以此类推，对后面的元素进行穷举，凑出若干个和为 5 的桶（子集）。

但问题是，如果最后发现无法凑出和为 `target` 的 `k` 个子集，算法会怎么做？

回溯算法会回溯到第一个桶，重新开始穷举，现在它知道第一个桶里装 `1, 4` 是不可行的，它会尝试把 `2, 3` 装到第一个桶里：

![Image](https://tva1.sinaimg.cn/large/e6c9d24egy1h68q9twgcqj20u00gw3z1.jpg)

现在第一个桶装满了，就开始装第二个桶，算法会装入 `1, 4`：

![Image](https://tva1.sinaimg.cn/large/e6c9d24egy1h68q9vc7nyj20u00gw74t.jpg)

好，到这里你应该看出来问题了，这种情况其实和之前的那种情况是一样的。也就是说，到这里你其实已经知道不需要再穷举了，必然凑不出来和为 `target` 的 `k` 个子集。

但我们的算法还是会傻乎乎地继续穷举，因为在她看来，第一个桶和第二个桶里面装的元素不一样，那这就是两种不一样的情况呀。

那么我们怎么让算法的智商提高，识别出这种情况，避免冗余计算呢？

你注意这两种情况的 `used` 数组肯定长得一样，所以 `used` 数组可以认为是回溯过程中的「状态」。

**所以，我们可以用一个 `memo` 备忘录，在装满一个桶时记录当前 `used` 的状态，如果当前 `used` 的状态是曾经出现过的，那就不用再继续穷举，从而起到剪枝避免冗余计算的作用**。

有读者肯定会问，`used` 是一个布尔数组，怎么作为键进行存储呢？这其实是小问题，比如我们可以把数组转化成字符串，这样就可以作为哈希表的键进行存储了。

看下代码实现，只要稍微改一下 `backtrack` 函数即可：

```
// 备忘录，存储 used 数组的状态
HashMap<String, Boolean> memo = new HashMap<>();

boolean backtrack(int k, int bucket, int[] nums, int start, boolean[] used, int target) {        
    // base case
    if (k == 0) {
        return true;
    }
    // 将 used 的状态转化成形如 [true, false, ...] 的字符串
    // 便于存入 HashMap
    String state = Arrays.toString(used);

    if (bucket == target) {
        // 装满了当前桶，递归穷举下一个桶的选择
        boolean res = backtrack(k - 1, 0, nums, 0, used, target);
        // 将当前状态和结果存入备忘录
        memo.put(state, res);
        return res;
    }

    if (memo.containsKey(state)) {
        // 如果当前状态曾今计算过，就直接返回，不要再递归穷举了
        return memo.get(state);
    }

    // 其他逻辑不变...
}
```

这样提交解法，发现执行效率依然比较低，这次不是因为算法逻辑上的冗余计算，而是代码实现上的问题。

**因为每次递归都要把 `used` 数组转化成字符串，这对于编程语言来说也是一个不小的消耗，所以我们还可以进一步优化**。

注意题目给的数据规模 `nums.length <= 16`，也就是说 `used` 数组最多也不会超过 16，那么我们完全可以用「位图」的技巧，用一个 int 类型的 `used` 变量来替代 `used` 数组。

具体来说，我们可以用整数 `used` 的第 `i` 位（`(used >> i) & 1`）的 1/0 来表示 `used[i]` 的 true/false。

这样一来，不仅节约了空间，而且整数 `used` 也可以直接作为键存入 HashMap，省去数组转字符串的消耗。

看下最终的解法代码：

```
public boolean canPartitionKSubsets(int[] nums, int k) {
    // 排除一些基本情况
    if (k > nums.length) return false;
    int sum = 0;
    for (int v : nums) sum += v;
    if (sum % k != 0) return false;

    int used = 0; // 使用位图技巧
    int target = sum / k;
    // k 号桶初始什么都没装，从 nums[0] 开始做选择
    return backtrack(k, 0, nums, 0, used, target);
}

HashMap<Integer, Boolean> memo = new HashMap<>();

boolean backtrack(int k, int bucket,
                  int[] nums, int start, int used, int target) {        
    // base case
    if (k == 0) {
        // 所有桶都被装满了，而且 nums 一定全部用完了
        return true;
    }
    if (bucket == target) {
        // 装满了当前桶，递归穷举下一个桶的选择
        // 让下一个桶从 nums[0] 开始选数字
        boolean res = backtrack(k - 1, 0, nums, 0, used, target);
        // 缓存结果
        memo.put(used, res);
        return res;
    }

    if (memo.containsKey(used)) {
        // 避免冗余计算
        return memo.get(used);
    }

    for (int i = start; i < nums.length; i++) {
        // 剪枝
        if (((used >> i) & 1) == 1) { // 判断第 i 位是否是 1
            // nums[i] 已经被装入别的桶中
            continue;
        }
        if (nums[i] + bucket > target) {
            continue;
        }
        // 做选择
        used |= 1 << i; // 将第 i 位置为 1
        bucket += nums[i];
        // 递归穷举下一个数字是否装入当前桶
        if (backtrack(k, bucket, nums, i + 1, used, target)) {
            return true;
        }
        // 撤销选择
        used ^= 1 << i; // 使用异或运算将第 i 位恢复 0
        bucket -= nums[i];
    }

    return false;
}
```

至此，这道题的第二种思路也完成了。

### 四、最后总结

本文写的这两种思路都可以算出正确答案，不过第一种解法即便经过了排序优化，也明显比第二种解法慢很多，这是为什么呢？

我们来分析一下这两个算法的时间复杂度，假设 `nums` 中的元素个数为 `n`。

先说第一个解法，也就是从数字的角度进行穷举，`n` 个数字，每个数字有 `k` 个桶可供选择，所以组合出的结果个数为 `k^n`，时间复杂度也就是 `O(k^n)`。

第二个解法，每个桶要遍历 `n` 个数字，对每个数字有「装入」或「不装入」两种选择，所以组合的结果有 `2^n` 种；而我们有 `k` 个桶，所以总的时间复杂度为 `O(k*2^n)`。

**当然，这是对最坏复杂度上界的粗略估算，实际的复杂度肯定要好很多，毕竟我们添加了这么多剪枝逻辑**。不过，从复杂度的上界已经可以看出第一种思路要慢很多了。

所以，谁说回溯算法没有技巧性的？虽然回溯算法就是暴力穷举，但穷举也分聪明的穷举方式和低效的穷举方式，关键看你以谁的「视角」进行穷举。

通俗来说，我们应该尽量「少量多次」，就是说宁可多做几次选择，也不要给太大的选择空间；宁可「二选一」选 `k` 次，也不要 「`k` 选一」选一次。

好了，这道题我们从两种视角进行穷举，虽然代码量看起来多，但核心逻辑都是类似的，相信你通过本文能够更深刻地理解回溯算法。