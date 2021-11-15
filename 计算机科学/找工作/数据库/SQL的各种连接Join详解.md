[TOC]

#### [SQL的各种连接Join详解](https://www.cnblogs.com/reaptomorrow-flydream/p/8145610.html)



SQL JOIN 子句用于把来自两个或多个表的行结合起来，基于这些表之间的共同字段。

最常见的 JOIN 类型：**SQL INNER JOIN（简单的 JOIN）、SQL LEFT JOIN、SQL RIGHT JOIN、SQL FULL JOIN，**其中前一种是内连接，后三种是外链接。

假设我们有两张表，Table A是左边的表，Table B是右边的表。

| id   | name     |
| ---- | -------- |
| 1    | Google   |
| 2    | 淘宝     |
| 3    | 微博     |
| 4    | Facebook |

| id   | address |
| ---- | ------- |
| 1    | 美国    |
| 5    | 中国    |
| 3    | 中国    |
| 6    | 美国    |

 

 

## **一、INNER JOIN**

------

 

内连接是最常见的一种连接，只连接匹配的行。

inner join语法

```sql
select column_name(s)
from table 1
INNER JOIN table 2
ON
table 1.column_name=table 2.column_name
```

**注释：**INNER JOIN与JOIN是相同

![img](https://tva1.sinaimg.cn/large/008i3skNly1gwgach2xnsj306f04ia9y.jpg)

INNER JOIN产生的结果集中，是1和2的交集。

```sql
select * from Table A inner join Table B
on Table A.id=Table B.id
```

 执行以上SQL输出结果如下：

| id   | name   | address |
| ---- | ------ | ------- |
| 1    | Google | 美国    |
| 3    | 微博   | 中国    |

 

 

## **二、LEFT JOIN**

------

 

LEFT JOIN返回左表的全部行和右表满足ON条件的行，如果左表的行在右表中没有匹配，那么这一行右表中对应数据用NULL代替。

LEFT JOIN 语法

```sql
select column_name(s)
from table 1
LEFT JOIN table 2
ON table 1.column_name=table 2.column_name
```

**注释：**在某些数据库中，LEFT JOIN 称为LEFT OUTER JOIN

![img](https://tva1.sinaimg.cn/large/008i3skNly1gwgace9r9uj306s04cjra.jpg)

LEFT JOIN产生表1的完全集，而2表中匹配的则有值，没有匹配的则以null值取代。

```sql
select * from Table A left join Table B
on Table A.id=Table B.id
```

 执行以上SQL输出结果如下：

| id   | name     | address |
| ---- | -------- | ------- |
| 1    | Google   | 美国    |
| 2    | 淘宝     | null    |
| 3    | 微博     | 中国    |
| 4    | Facebook | null    |



 

##  **三、RIGHT JOIN**

------

 

RIGHT JOIN返回右表的全部行和左表满足ON条件的行，如果右表的行在左表中没有匹配，那么这一行左表中对应数据用NULL代替。

RIGHT JOIN语法

```sql
select column_name(s)
from table 1
RIGHT JOIN table 2
ON table 1.column_name=table 2.column_name
```

**注释：**在某些数据库中，RIGHT JOIN 称为RIGHT OUTER JOIN

![img](https://tva1.sinaimg.cn/large/008i3skNly1gwgacaicu4j306j04at8m.jpg)

RIGHT JOIN产生表2的完全集，而1表中匹配的则有值，没有匹配的则以null值取代。

```sql
select * from Table A right join Table B
on Table A.id=Table B.id
```

执行以上SQL输出结果如下：

| id   | name   | address |
| ---- | ------ | ------- |
| 1    | Google | 美国    |
| 5    | null   | 中国    |
| 3    | 微博   | 中国    |
| 6    | null   | 美国    |

 

 

## **四、FULL OUTER JOIN**

------

 

FULL JOIN 会从左表 和右表 那里返回所有的行。如果其中一个表的数据行在另一个表中没有匹配的行，那么对面的数据用NULL代替

FULL OUTER JOIN语法

```sql
select column_name(s)
from table 1
FULL OUTER JOIN table 2
ON table 1.column_name=table 2.column_name
```

 

![img](https://tva1.sinaimg.cn/large/008i3skNly1gwgac6o7wgj3068046dfr.jpg)

FULL OUTER JOIN产生1和2的并集。但是需要注意的是，对于没有匹配的记录，则会以null做为值。

```sql
select * from Table A full outer join Table B
on Table A.id=Table B.id
```

执行以上SQL输出结果如下：

| id   | name     | address |
| ---- | -------- | ------- |
| 1    | Google   | 美国    |
| 2    | 淘宝     | null    |
| 3    | 微博     | 中国    |
| 4    | Facebook | null    |
| 5    | null     | 中国    |
| 6    | null     | 美国    |