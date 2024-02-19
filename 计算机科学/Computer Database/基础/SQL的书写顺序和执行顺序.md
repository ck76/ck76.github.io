- https://typora.io/releases/all
- typora测试版可以凑合着用



[TOC]

主要是SQL的书写顺序和执行顺序

**执行顺序**

```sql
(8) SELECT (9)DISTINCT<Select_list>
(1) FROM <left_table> (3) <join_type>JOIN<right_table>
(2) ON<join_condition>
(4) WHERE<where_condition>
(5) GROUP BY<group_by_list>
(6) WITH {CUBE|ROLLUP}
(7) HAVING<having_condtion>
(10) ORDER BY<order_by_list>
(11) LIMIT<limit_number>
```

我们可以看出，SELECT子句是必选的，其它子句如WHERE子句、GROUP BY子句等是可选的。
一个SELECT语句中，子句的顺序是固定的。必须严格按照上述的顺序书写。

所有的查询语句都是从FROM开始执行的，在执行过程中，每个步骤都会为下一个步骤生成一个**虚拟表**，这个虚拟表将作为下一个执行步骤的输入。

1. 先对FROM子句中的两个表执行一个笛卡尔乘，此时生成虚拟表 virtual table 1（选择相对小的表做基础表）
2. 然后是应用ON条件筛选器，将ON中的逻辑表达式将应用到 virtual table 1中的各个行，筛选出满足ON中的逻辑表达式的行，生成虚拟表 virtual table 2
3. 根据连接方式进行进一步的操作。如果是OUTER JOIN，那么这一步就将添加外部行

- LEFT OUTER JOIN就把左表在第二步中筛选掉的行添加进来
- RIGHT OUTER JOIN就将右表在第二步中筛选掉的行添加进来 这样生成虚拟表 virtual table 3

如果 FROM子句中的表数目大于2，那么就将virtual table 3和第三个表连接从而计算笛卡尔积，生成虚拟表，该过程就是一个重复1-3的步骤，最终得到一个新的虚拟表virtual table 3

1. 应用WHERE筛选器，对上一步生产的virtual table 3用WHERE筛选器筛选，生成虚拟表virtual table 4
   **在这有个比较重要的细节需要提一下，如果我们有一个condition需要去筛选，应该在在ON条件筛选器还是用WHERE筛选器指定condition逻辑表达式呢？**
   ON和WHERE的最大区别在于，如果在ON应用逻辑表达式那么在第三步OUTER JOIN中还可以把移除的行再次添加回来，而WHERE的移除的不可挽回的
2. GROUP BY子句将具有相同属性的row组合成为一组，得到虚拟表virtual table 5
   如果应用了GROUP BY，那么后面的所有步骤都只能得到的virtual table 5的列或者是聚合函数，并且分组的时候是将列中唯一的值分成一组，同时只为每一组返回一行记录，这一点请牢记。
3. 应用CUBE或者ROLLUP选项，为virtual table 5生成超组，生成virtual table 6. 这个暂时还没了解，先放到这里吧。
4. 应用HAVING筛选器，生成virtual table 7
   HAVING筛选器是唯一一个用来筛选组的筛选器
5. 处理SELECT子句。将virtual table 7中的并且在Select_list中的列筛选出来，生成virtual table 8
6. 应用DISTINCT子句，virtual table 8中移除相同的行，生成virtual table 9
   事实上如果应用了GROUP BY子句，那么DISTINCT是多余的，原因同样在于，分组的时候是将列中唯一的值分成一组，同时只为每一组返回一行记录，那么所以的记录都将是不相同的。
7. 应用ORDER BY子句。按照order_by_condition排序virtual table 10，此时返回的一个游标，而不是虚拟表。SQL是基于集合的，集合不会预先对行进行排序，它只是成员的逻辑集合，成员的顺序是无关紧要的。对表进行排序的查询可以返回一个对象，这个对象包含特定的物理顺序的逻辑组织。这个对象就叫游标。正因为返回值是游标，那么使用ORDER BY子句查询不能应用于表达式。



---

# [【SQL】SQL 中Select语句完整的执行顺序](https://www.cnblogs.com/HDK2016/p/6884191.html)

SQL Select语句完整的执行顺序： 

1、from子句组装来自不同数据源的数据； 

2、where子句基于指定的条件对记录行进行筛选； 

3、group by子句将数据划分为多个分组； 

4、使用聚集函数进行计算； 

5、使用having子句筛选分组； 

6、计算所有的表达式； 

7、select 的字段；

8、使用order by对结果集进行排序。

[SQL语言](http://www.easthome.com/Course/Details2_233.html)不同于其他编程语言的最明显特征是处理代码的顺序。在大多[数据库](http://lib.csdn.net/base/mysql)语言中，代码按编码顺序被处理。但在SQL语句中，第一个被处理的子句式FROM，而不是第一出现的SELECT。[SQL](http://www.easthome.com/Course/Details2_171.html)查询处理的步骤序号：

(1) FROM <left_table>

(3) <join_type> JOIN <right_table>

(2) ON <join_condition>

(4) WHERE <where_condition>

(5) GROUP BY <group_by_list>

(6) WITH {CUBE | ROLLUP}

(7) HAVING <having_condition>

(8) SELECT

(9) DISTINCT

(9) ORDER BY <order_by_list>

(10) <TOP_specification> <select_list>

 

　　以上每个步骤都会产生一个虚拟表，该虚拟表被用作下一个步骤的输入。这些虚拟表对调用者(客户端应用程序或者外部查询)不可用。只有最后一步生成的表才会会给调用者。如果没有在查询中指定某一个子句，将跳过相应的步骤。

　　逻辑查询处理阶段简介：

　　1、 FROM：对FROM子句中的前两个表执行笛卡尔积(交叉联接)，生成虚拟表VT1。

　　2、 ON：对VT1应用ON筛选器，只有那些使为真才被插入到TV2。

　　3、 OUTER (JOIN):如果指定了OUTER JOIN(相对于CROSS JOIN或INNER JOIN)，保留表中未找到匹配的行将作为外部行添加到VT2，生成TV3。如果FROM子句包含两个以上的表，则对上一个联接生成的结果表和下一个表重复执行步骤1到步骤3，直到处理完所有的表位置。

　　4、 WHERE：对TV3应用WHERE筛选器，只有使为true的行才插入TV4。

　　5、 GROUP BY：按GROUP BY子句中的列列表对TV4中的行进行分组，生成TV5。

　　6、 CUTE|ROLLUP：把超组插入VT5，生成VT6。

　　7、 HAVING：对VT6应用HAVING筛选器，只有使为true的组插入到VT7。

　　8、 SELECT：处理SELECT列表，产生VT8。

　　9、 DISTINCT：将重复的行从VT8中删除，产品VT9。

　　10、ORDER BY：将VT9中的行按ORDER BY子句中的列列表顺序，生成一个游标(VC10)。

11、TOP：从VC10的开始处选择指定数量或比例的行，生成表TV11，并返回给调用者。

 

where子句中的条件书写顺序：

```
FROM -> CONNECT BY -> WHERE -> GROUP BY -> HAVING -> SELECT -> ORDER BY
```

 

排序规则来着[Query Processing Order](http://oracle.readthedocs.io/en/latest/sql/basics/query-processing-order.html).

---



今天遇到一个问题就是mysql中insert into 和update以及delete语句中能使用as别名吗？目前还在查看，但是在查阅资料时发现了一些有益的知识，给大家分享一下，就是关于sql以及MySQL语句执行顺序：

sql和mysql执行顺序，发现内部机制是一样的。最大区别是在别名的引用上。 


一、sql执行顺序 
from 
join 
on 
where 
group by(开始使用select中的别名，后面的语句中都可以使用)
 avg,sum.... 
having 
select 
distinct 
order by
limit 

从这个顺序中我们不难发现，所有的 查询语句都是从from开始执行的，在执行过程中，每个步骤都会为下一个步骤生成一个虚拟表，这个虚拟表将作为下一个执行步骤的输入。 


第一步：首先对from子句中的前两个表执行一个笛卡尔乘积，此时生成虚拟表 vt1（选择相对小的表做基础表）。 
第二步：接下来便是应用on筛选器，on 中的逻辑表达式将应用到 vt1 中的各个行，筛选出满足on逻辑表达式的行，生成虚拟表 vt2 。
第三步：如果是outer join 那么这一步就将添加外部行，left outer jion 就把左表在第二步中过滤的添加进来，如果是right outer join 那么就将右表在第二步中过滤掉的行添加进来，这样生成虚拟表 vt3 。

第四步：如果 from 子句中的表数目多余两个表，那么就将vt3和第三个表连接从而计算笛卡尔乘积，生成虚拟表，该过程就是一个重复1-3的步骤，最终得到一个新的虚拟表 vt3。 

第五步：应用where筛选器，对上一步生产的虚拟表引用where筛选器，生成虚拟表vt4，在这有个比较重要的细节不得不说一下，对于包含outer join子句的查询，就有一个让人感到困惑的问题，到底在on筛选器还是用where筛选器指定逻辑表达式呢？on和where的最大区别在于，如果在on应用逻辑表达式那么在第三步outer join中还可以把移除的行再次添加回来，而where的移除的最终的。举个简单的例子，有一个学生表（班级,姓名）和一个成绩表(姓名,成绩)，我现在需要返回一个x班级的全体同学的成绩，但是这个班级有几个学生缺考，也就是说在成绩表中没有记录。为了得到我们预期的结果我们就需要在on子句指定学生和成绩表的关系（学生.姓名=成绩.姓名）那么我们是否发现在执行第二步的时候，对于没有参加考试的学生记录就不会出现在vt2中，因为他们被on的逻辑表达式过滤掉了,但是我们用left outer join就可以把左表（学生）中没有参加考试的学生找回来，因为我们想返回的是x班级的所有学生，如果在on中应用学生.班级='x'的话，left outer join会把x班级的所有学生记录找回（感谢网友康钦谋__康钦苗的指正），所以只能在where筛选器中应用学生.班级='x' 因为它的过滤是最终的。 

第六步：group by 子句将中的唯一的值组合成为一组，得到虚拟表vt5。如果应用了group by，那么后面的所有步骤都只能得到的vt5的列或者是聚合函数（count、sum、avg等）。原因在于最终的结果集中只为每个组包含一行。这一点请牢记。 

第七步：应用cube或者rollup选项，为vt5生成超组，生成vt6. 
第八步：应用having筛选器，生成vt7。having筛选器是第一个也是为唯一一个应用到已分组数据的筛选器。 
第九步：处理select子句。将vt7中的在select中出现的列筛选出来。生成vt8. 

第十步：应用distinct子句，vt8中移除相同的行，生成vt9。事实上如果应用了group by子句那么distinct是多余的，原因同样在于，分组的时候是将列中唯一的值分成一组，同时只为每一组返回一行记录，那么所以的记录都将是不相同的。 

第十一步：应用order by子句。按照order_by_condition排序vt9，此时返回的一个游标，而不是虚拟表。sql是基于集合的理论的，集合不会预先对他的行排序，它只是成员的逻辑集合，成员的顺序是无关紧要的。对表进行排序的查询可以返回一个对象，这个对象包含特定的物理顺序的逻辑组织。这个对象就叫游标。正因为返回值是游标，那么使用order by 子句查询不能应用于表表达式。排序是很需要成本的，除非你必须要排序，否则最好不要指定order by，最后，在这一步中是第一个也是唯一一个可以使用select列表中别名的步骤。 

第十二步：应用top选项。此时才返回结果给请求者即用户。 

 

二、mysql的执行顺序 

1、SELECT语句定义 
一个完成的SELECT语句包含可选的几个子句。SELECT语句的定义如下： 
SQL代码 

```sql
<SELECT clause> [<FROM clause>] [<WHERE clause>] [<GROUP BY clause>] [<HAVING clause>] [<ORDER BY clause>] [<LIMIT clause>] 
```




2、SELECT语句执行顺序 
SELECT语句中子句的执行顺序与SELECT语句中子句的输入顺序是不一样的，所以并不是从SELECT子句开始执行的，而是按照下面的顺序执行： 

开始->FROM子句->WHERE子句->GROUP BY子句->HAVING子句->ORDER BY子句->SELECT子句->LIMIT子句->最终结果 

每个子句执行后都会产生一个中间结果，供接下来的子句使用，如果不存在某个子句，就跳过 
对比了一下，mysql和sql执行顺序基本是一样的, 标准顺序的 SQL 语句为: 

```sql
select 考生姓名, max(总成绩) as max总成绩 

from tb_Grade 

where 考生姓名 is not null 

group by 考生姓名 

having max(总成绩) > 600 

order by max总成绩 
```



 在上面的示例中 SQL 语句的执行顺序如下: 

　　 (1). 首先执行 FROM 子句, 从 tb_Grade 表组装数据源的数据 

　　 (2). 执行 WHERE 子句, 筛选 tb_Grade 表中所有数据不为 NULL 的数据 

　　 (3). 执行 GROUP BY 子句, 把 tb_Grade 表按 "学生姓名" 列进行分组(注：这一步开始才可以使用select中的别名，他返回的是一个游标，而不是一个表，所以在where中不可以使用select中的别名，而having却可以使用，感谢网友  zyt1369  提出这个问题)
　　 (4). 计算 max() 聚集函数, 按 "总成绩" 求出总成绩中最大的一些数值 

　　 (5). 执行 HAVING 子句, 筛选课程的总成绩大于 600 分的. 

　　 (7). 执行 ORDER BY 子句, 把最后的结果按 "Max 成绩" 进行排序. 



---

### 一、查询的逻辑执行顺序



(1) FROM left_table

(3) join_type JOIN right_table (2) ON join_condition

(4) WHERE where_condition

(5) GROUP BY group_by_list

(6) WITH {cube | rollup}

(7) HAVING having_condition

(8) SELECT (9) DISTINCT (11) top_specification select_list

(9) ORDER BY order_by_list



标准的 SQL 的解析顺序为:



(1) FROM 子句 组装来自不同数据源的数据



(2) WHERE 子句 基于指定的条件对记录进行筛选



(3) GROUP BY 子句 将数据划分为多个分组



(4) 使用聚合函数进行计算



(5) 使用HAVING子句筛选分组



(6) 计算所有的表达式



(7) 使用ORDER BY对结果集进行排序



### 二、执行顺序



\1. FROM：对FROM子句中前两个表执行笛卡尔积生成虚拟表vt1



\2. ON: 对vt1表应用ON筛选器只有满足 join_condition 为真的行才被插入vt2



\3. OUTER(join)：如果指定了 OUTER JOIN保留表(preserved table)中未找到的行将行作为外部行添加到vt2，生成t3，如果from包含两个以上表，则对上一个联结生成的结果表和下一个表重复执行步骤和步骤直接结束。



\4. WHERE：对vt3应用 WHERE 筛选器只有使 where_condition 为true的行才被插入vt4



\5. GROUP BY：按GROUP BY子句中的列列表对vt4中的行分组生成vt5



\6. CUBE|ROLLUP：把超组(supergroups)插入vt6，生成vt6



\7. HAVING：对vt6应用HAVING筛选器只有使 having_condition 为true的组才插入vt7



\8. SELECT：处理select列表产生vt8



\9. DISTINCT：将重复的行从vt8中去除产生vt9



\10. ORDER BY：将vt9的行按order by子句中的列列表排序生成一个游标vc10



\11. TOP：从vc10的开始处选择指定数量或比例的行生成vt11 并返回调用者



看到这里，那么用过Linq to SQL的语法有点相似啊？如果我们我们了解了SQL Server执行顺序，那么我们就接下来进一步养成日常SQL的好习惯，也就是在实现功能的同时有考虑性能的思想，数据库是能进行集合运算的工具，我们应该尽量的利用这个工具，所谓集合运算实际就是批量运算，就是尽量减少在客户端进行大数据量的循环操作，而用SQL语句或者存储过程代替。

---

- https://www.cnblogs.com/HDK2016/p/6884191.html
- https://zhuanlan.zhihu.com/p/77847158
- https://developer.aliyun.com/article/56359
- 

