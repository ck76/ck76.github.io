



## 一、[笛卡尔积](https://so.csdn.net/so/search?q=笛卡尔积&spm=1001.2101.3001.7020)：CROSS JOIN

CROSS JOIN**使两张表的所有字段直接进行笛卡尔积**，假设表1有m条数据，表2有n条数据，则结果数量为m*n条

```sql
SELECT * FROM tab1 CROSS JOIN tab2
```

## 二、内连接：INNER JOIN

内连接需要用ON来指定两张表需要比较的字段，最终结果只显示**满足条件**的数据

```sql
SELECT * FROM tab1 INNER JOIN tab2 ON tab1.id1 = tab2.id2
```

## 三、左连接：LEFT JOIN

左连接可以看做在内连接的基础上，把左表中不满足ON条件的数据也显示出来，但结果中的右表部分中的数据为NULL

```sql
SELECT * FROM tab1 LEFT JOIN tab2 ON tab1.id1 = tab2.id2
```

## 四、右连接：RIGHT JOIN

右连接就是与左连接完全相反

```sql
SELECT * FROM tab1 RIGHT JOIN tab2 ON tab1.id1 = tab2.id2
```

## 五、全连接：OUTER JOIN

全连接就是左连接和右连接的并集，但是MySQL中并不支持全连接的写法

```sql
SELECT * FROM tab1 OUTER JOIN tab2 ON tab1.id1 = tab2.id2
```

不过可以用UNION联合左连接和右连接的结果来代替

```sql
SELECT * FROM tab1 LEFT JOIN tab2 ON tab1.id1 = tab2.id2
UNION
SELECT * FROM tab1 RIGHT JOIN tab2 ON tab1.id1 = tab2.id2
```