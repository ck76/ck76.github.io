**题目：**输入一个链表，输出该链表中倒数第k个结点。为了符合大多数人的习惯，本题从1开始计数，即链表的尾结点是倒数第1个结点。例如一个链表有6个结点，从头结点开始它们的值依次是1、2、3、4、5、6。这个链表的倒数第3个结点是值为4的结点。



```java
public class Solution {
    public ListNode FindKthToTail(ListNode head,int k) {
        if(head==null||k<=0){
            return null;
        }
        ListNode p1=head;
        ListNode p2=head;       
        //p1先到达第k个节点处
        for(int i=1;i<k;i++){
            if(p1.next!=null){
                p1=p1.next;
            }else{
                return null;
            }
        }
        //p1走到链尾时p2正为倒数第k个节点
        while(p1.next!=null){
            p1=p1.next;
            p2=p2.next;
        }
        return last;
    }
}
```



