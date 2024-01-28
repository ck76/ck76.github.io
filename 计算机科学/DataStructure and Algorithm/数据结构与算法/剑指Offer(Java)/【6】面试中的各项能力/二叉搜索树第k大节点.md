### 题目

给定一颗二叉搜索树，请找出其中的第k大的结点。



### 题解

```java
class Solution {

    TreeNode getKthNode(TreeNode head, int k) {
        if (head == null || k == 0) {
            return null;
        }
        return kthNodeCore(head, k);
    }

    private TreeNode kthNodeCore(TreeNode head, int k) {
        TreeNode result = null;
        //中序遍历先到最左下
        if (head.leftChild != null) {
            result = kthNodeCore(head.leftChild, k);
        }
        //到了一个左孩子为空的节点，计数器减一
        if (result == null) {
            if (k == 1) {
                return head;
            }
            k--;
        }
        //head的左孩子为空，右孩子不为空
        if (result == null && head.rightChild != null) {
            result = kthNodeCore(head.rightChild, k);
        }
        return result;
    }

}
```

