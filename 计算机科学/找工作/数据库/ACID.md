原子性（Atomicity）、一致性（Consistency）、隔离性（Isolation）、持久性（Durability）.

**1.8 数据库**

```
Q：数据库中的事务了解吗？事务的四大特性？
```

> - 技术点：事务
> - 参考回答：
>   - 事务是并发控制的单位，是用户定义的一个操作序列。它指这些操作要么都做，要么都不做，以便服务器保持数据的完整性。
>   - 事务通常是以BEGIN TRANSACTION开始，以COMMIT或ROLLBACK结束。
>   - 事务的四大特性（ACID特性）：
>   - 原子性（Atomicity）表示事务中包括的诸操作**要么全做，要么全不做；**
>   - 一致性（Consistency）表示事务执行的结果必须是使数据库**从一个一致性状态变到另一个一致性状态**；
>   - 隔离性（Isolation）表示一个事务的执行**不能被其他事务干扰；**
>   - 持续性（Durability）表示一个事务一旦提交，**它对数据库中数据的改变就应该是永久性的**
> - 引申：谈谈[数据库事务的并发控制](https://www.jianshu.com/p/478c6dca1b74)

```
Q：如何理解数据库的范式？
```

> - 技术点：范式
> - 思路：详见实例讲解[数据库的3大范式和5大约束](https://links.jianshu.com/go?to=https%3A%2F%2Fblog.csdn.net%2Fqq_33862644%2Farticle%2Fdetails%2F79692652)
> - 参考回答：
>   - 第一范式（1NF）：数据表中的**每个字段必须是不可拆分的最小单元**，即确保每一列的原子性
>   - 第二范式（2NF）：满足1NF后，要求表中的**所有列，都必须依赖于主键，**而不能有任何一列与主键没有关系
>   - 第三范式（3NF）：必须先满足第二范式，要求表中的每一列只**与主键直接相关而不是间接相关**，即表中的每一列**只能依赖于主键**