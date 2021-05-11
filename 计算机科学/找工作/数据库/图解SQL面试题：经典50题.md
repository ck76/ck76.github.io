[TOC]

已知有如下4张表：

学生表：student(学号,学生姓名,出生年月,性别)

成绩表：score(学号,课程号,成绩)

课程表：course(课程号,课程名称,教师号)

教师表：teacher(教师号,教师姓名)



根据以上信息按照下面要求写出对应的SQL语句。

ps：这些题考察SQL的编写能力，对于这类型的题目，需要你先把4张表之间的关联关系搞清楚了，最好的办法是自己在草稿纸上画出关联图，然后再编写对应的SQL语句就比较容易了。下图是我画的这4张表的关系图，可以看出它们之间是通过哪些外键关联起来的：

![img](https://pic1.zhimg.com/80/v2-86fd263583a6cead51675982c1735e68_1440w.jpg)



**一、创建数据库和表**

为了演示题目的运行过程，我们先按下面语句在客户端navicat中创建数据库和表。

（如何你还不懂什么是数据库，什么是客户端navicat，可以先学习这个：

[从零学会SQL：入门](https://www.zhihu.com/lives/1092472616434180096)）

![img](https://pic1.zhimg.com/80/v2-9fb2b06e6889bc7db47d4ce61164ad10_1440w.jpg)

**1.创建表**

1）创建学生表（student）

按下图在客户端navicat里创建学生表



![img](https://pic3.zhimg.com/80/v2-2ac878b436d11d5e1999e14fa9f5c902_1440w.jpg)

学生表的“学号”列设置为主键约束，下图是每一列设置的数据类型和约束

![img](https://pic3.zhimg.com/80/v2-b6664b30b2603c2542eb440d6b11f22e_1440w.jpg)

创建完表，点击“保存”

![img](https://pic3.zhimg.com/80/v2-61ab935e55aa2cab90eab18d3aebb4ea_1440w.jpg)

2）创建成绩表（score）

同样的步骤，创建"成绩表“。“课程表的“学号”和“课程号”一起设置为主键约束（联合主键），“成绩”这一列设置为数值类型（float，浮点数值）

![img](https://pic1.zhimg.com/80/v2-9433b477d43ce110c557e723f83b5bb0_1440w.jpg)

3）创建课程表（course）

课程表的“课程号”设置为主键约束

![img](https://pic1.zhimg.com/80/v2-5b29ed6f6d99a9f011a0c1b72f594764_1440w.jpg)

4）教师表（teacher）

教师表的“教师号”列设置为主键约束，

教师姓名这一列设置约束为“null”（红框的地方不勾选），表示这一列允许包含空值（null）

![img](https://pic1.zhimg.com/80/v2-a46bfc8eaea54894ee2e44cf9cd5a7c0_1440w.jpg)



2.向表中添加数据

![img](https://pic1.zhimg.com/80/v2-7054b72fc575d2ffcaa6152883609050_1440w.jpg)

1）向学生表里添加数据

添加数据的sql

```mysql
insert into student(学号,姓名,出生日期,性别) 
values('0001' , '猴子' , '1989-01-01' , '男');

insert into student(学号,姓名,出生日期,性别) 
values('0002' , '猴子' , '1990-12-21' , '女');

insert into student(学号,姓名,出生日期,性别) 
values('0003' , '马云' , '1991-12-21' , '男');

insert into student(学号,姓名,出生日期,性别) 
values('0004' , '王思聪' , '1990-05-20' , '男');
```

在客户端navicat里的操作

![img](https://pic3.zhimg.com/80/v2-f6a1e2708dd61bce92833e5a1e7a77a2_1440w.jpg)

![img](https://pic1.zhimg.com/v2-900bfe4cf778d43991b7f1528f45ea14_b.jpg)

![img](https://pic3.zhimg.com/80/v2-a57052cbc56adab675957a541f6f41a2_1440w.jpg)

2）成绩表（score）

添加数据的sql

```text
insert into score(学号,课程号,成绩) 
values('0001' , '0001' , 80);

insert into score(学号,课程号,成绩) 
values('0001' , '0002' , 90);

insert into score(学号,课程号,成绩) 
values('0001' , '0003' , 99);

insert into score(学号,课程号,成绩) 
values('0002' , '0002' , 60);

insert into score(学号,课程号,成绩) 
values('0002' , '0003' , 80);

insert into score(学号,课程号,成绩) 
values('0003' , '0001' , 80);

insert into score(学号,课程号,成绩) 
values('0003' , '0002' , 80);

insert into score(学号,课程号,成绩) 
values('0003' , '0003' , 80);
```

客户端navicat里的操作

![img](https://pic1.zhimg.com/80/v2-6934ec47421515f4bda493a719692298_1440w.jpg)

3）课程表

添加数据的sql

```text
insert into course(课程号,课程名称,教师号)
values('0001' , '语文' , '0002');

insert into course(课程号,课程名称,教师号)
values('0002' , '数学' , '0001');

insert into course(课程号,课程名称,教师号)
values('0003' , '英语' , '0003');
```

客户端navicat里的操作

![img](https://pic3.zhimg.com/80/v2-ebb8013550066d7152b97caf35645f0a_1440w.jpg)

4）教师表里添加数据

添加数据的sql

```text
-- 教师表：添加数据
insert into teacher(教师号,教师姓名) 
values('0001' , '孟扎扎');

insert into teacher(教师号,教师姓名) 
values('0002' , '马化腾');

-- 这里的教师姓名是空值（null）
insert into teacher(教师号,教师姓名) 
values('0003' , null);

-- 这里的教师姓名是空字符串（''）
insert into teacher(教师号,教师姓名) 
values('0004' , '');
```

客户端navicat里操作

![img](https://pic3.zhimg.com/80/v2-dfca63e21dba75dc994535be752e00be_1440w.jpg)

添加结果

![img](https://pic2.zhimg.com/80/v2-a097a5f3bb31b43da3571bae614362f9_1440w.jpg)



**三、50道面试题**

为了方便学习，我将50道面试题进行了分类

**1.简单查询**

![img](https://pic2.zhimg.com/80/v2-5a377ec35954190b6ee6b07e2c74c575_1440w.jpg)

查询姓“猴”的学生名单

![img](https://pic2.zhimg.com/80/v2-74404bfe16150f5a9f4a473297e1e079_1440w.jpg)

查询姓“孟”老师的个数

```text
select count(教姓名)
from teacher
where 教师姓名 like '孟%';
```

如果上面题目不会做，可以复习这部分涉及到的sql知识：

[从零学会SQL：简单查询www.zhihu.com![图标](https://pic2.zhimg.com/v2-12ef91a3f1e91e70bd3480d755e058b1_ipico.jpg)](https://www.zhihu.com/lives/1097094125874200576)

**2.汇总分析**

![img](https://pic3.zhimg.com/80/v2-983199041b422ac426e919544ae4450e_1440w.jpg)

面试题：查询课程编号为“0002”的总成绩

```mysql
*
分析思路
select 查询结果 [总成绩:汇总函数sum]
from 从哪张表中查找数据[成绩表score]
where 查询条件 [课程号是0002]
*/
select sum(成绩)
from score
where 课程号 = '0002';
```

查询选了课程的学生人数

```text
/*
这个题目翻译成大白话就是：查询有多少人选了课程
select 学号，成绩表里学号有重复值需要去掉
from 从课程表查找score;
*/
select count(distinct 学号) as 学生人数 
from score;
```

![img](https://pic2.zhimg.com/80/v2-a5f8fc41ba4bf08d49b2dcfac4094831_1440w.jpg)

查询各科成绩最高和最低的分， 以如下的形式显示：课程号，最高分，最低分

```text
/*
分析思路
select 查询结果 [课程ID：是课程号的别名,最高分：max(成绩) ,最低分：min(成绩)]
from 从哪张表中查找数据 [成绩表score]
where 查询条件 [没有]
group by 分组 [各科成绩：也就是每门课程的成绩，需要按课程号分组];
*/
select 课程号,max(成绩) as 最高分,min(成绩) as 最低分
from score
group by 课程号;
```

查询每门课程被选修的学生数

```text
/*
分析思路
select 查询结果 [课程号，选修该课程的学生数：汇总函数count]
from 从哪张表中查找数据 [成绩表score]
where 查询条件 [没有]
group by 分组 [每门课程：按课程号分组];
*/
select 课程号, count(学号)
from score
group by 课程号;
```

查询男生、女生人数

```text
/*
分析思路
select 查询结果 [性别，对应性别的人数：汇总函数count]
from 从哪张表中查找数据 [性别在学生表中，所以查找的是学生表student]
where 查询条件 [没有]
group by 分组 [男生、女生人数：按性别分组]
having 对分组结果指定条件 [没有]
order by 对查询结果排序[没有];
*/
select 性别,count(*)
from student
group by 性别;
```

![img](https://pic1.zhimg.com/80/v2-e272c0f9609047fa50073f46f84139c4_1440w.jpg)

查询平均成绩大于60分学生的学号和平均成绩

```text
/* 
题目翻译成大白话：
平均成绩：展开来说就是计算每个学生的平均成绩
这里涉及到“每个”就是要分组了
平均成绩大于60分，就是对分组结果指定条件

分析思路
select 查询结果 [学号，平均成绩：汇总函数avg(成绩)]
from 从哪张表中查找数据 [成绩在成绩表中，所以查找的是成绩表score]
where 查询条件 [没有]
group by 分组 [平均成绩：先按学号分组，再计算平均成绩]
having 对分组结果指定条件 [平均成绩大于60分]
*/
select 学号, avg(成绩)
from score
group by 学号
having avg(成绩)>60;
```

查询至少选修两门课程的学生学号

```text
/* 
翻译成大白话：
第1步，需要先计算出每个学生选修的课程数据，需要按学号分组
第2步，至少选修两门课程：也就是每个学生选修课程数目>=2，对分组结果指定条件

分析思路
select 查询结果 [学号,每个学生选修课程数目：汇总函数count]
from 从哪张表中查找数据 [课程的学生学号：课程表score]
where 查询条件 [至少选修两门课程：需要先计算出每个学生选修了多少门课，需要用分组，所以这里没有where子句]
group by 分组 [每个学生选修课程数目：按课程号分组，然后用汇总函数count计算出选修了多少门课]
having 对分组结果指定条件 [至少选修两门课程：每个学生选修课程数目>=2]
*/
select 学号, count(课程号) as 选修课程数目
from score
group by 学号
having count(课程号)>=2;
```

查询同名同姓学生名单并统计同名人数

```text
/* 
翻译成大白话，问题解析：
1）查找出姓名相同的学生有谁，每个姓名相同学生的人数
查询结果：姓名,人数
条件：怎么算姓名相同？按姓名分组后人数大于等于2，因为同名的人数大于等于2
分析思路
select 查询结果 [姓名,人数：汇总函数count(*)]
from 从哪张表中查找数据 [学生表student]
where 查询条件 [没有]
group by 分组 [姓名相同：按姓名分组]
having 对分组结果指定条件 [姓名相同：count(*)>=2]
order by 对查询结果排序[没有];
*/

select 姓名,count(*) as 人数
from student
group by 姓名
having count(*)>=2;
```

查询不及格的课程并按课程号从大到小排列

```text
/* 
分析思路
select 查询结果 [课程号]
from 从哪张表中查找数据 [成绩表score]
where 查询条件 [不及格：成绩 <60]
group by 分组 [没有]
having 对分组结果指定条件 [没有]
order by 对查询结果排序[课程号从大到小排列：降序desc];
*/
select 课程号
from score 
where 成绩<60
order by 课程号 desc;
```

查询每门课程的平均成绩，结果按平均成绩升序排序，平均成绩相同时，按课程号降序排列

```text
/* 
分析思路
select 查询结果 [课程号,平均成绩：汇总函数avg(成绩)]
from 从哪张表中查找数据 [成绩表score]
where 查询条件 [没有]
group by 分组 [每门课程：按课程号分组]
having 对分组结果指定条件 [没有]
order by 对查询结果排序[按平均成绩升序排序:asc，平均成绩相同时，按课程号降序排列:desc];
*/
select 课程号, avg(成绩) as 平均成绩
from score
group by 课程号
order by 平均成绩 asc,课程号 desc;
```

检索课程编号为“0004”且分数小于60的学生学号，结果按按分数降序排列

```text
/* 
分析思路
select 查询结果 []
from 从哪张表中查找数据 [成绩表score]
where 查询条件 [课程编号为“04”且分数小于60]
group by 分组 [没有]
having 对分组结果指定条件 []
order by 对查询结果排序[查询结果按按分数降序排列];
*/
select 学号
from score
where 课程号='04' and 成绩 <60
order by 成绩 desc;
```

统计每门课程的学生选修人数(超过2人的课程才统计)

要求输出课程号和选修人数，查询结果按人数降序排序，若人数相同，按课程号升序排序

```text
/* 
分析思路
select 查询结果 [要求输出课程号和选修人数]
from 从哪张表中查找数据 []
where 查询条件 []
group by 分组 [每门课程：按课程号分组]
having 对分组结果指定条件 [学生选修人数(超过2人的课程才统计)：每门课程学生人数>2]
order by 对查询结果排序[查询结果按人数降序排序，若人数相同，按课程号升序排序];
*/
select 课程号, count(学号) as '选修人数'
from score
group by 课程号
having count(学号)>2
order by count(学号) desc,课程号 asc;
```

查询两门以上不及格课程的同学的学号及其平均成绩

```text
/*
分析思路
先分解题目：
1）[两门以上][不及格课程]限制条件
2）[同学的学号及其平均成绩]，也就是每个学生的平均成绩，显示学号，平均成绩
分析过程：
第1步：得到每个学生的平均成绩，显示学号，平均成绩
第2步：再加上限制条件：
1）不及格课程
2）两门以上[不及格课程]：课程数目>2


/* 
第1步：得到每个学生的平均成绩，显示学号，平均成绩
select 查询结果 [学号,平均成绩：汇总函数avg(成绩)]
from 从哪张表中查找数据 [涉及到成绩：成绩表score]
where 查询条件 [没有]
group by 分组 [每个学生的平均：按学号分组]
having 对分组结果指定条件 [没有]
order by 对查询结果排序[没有];
*/
select 学号, avg(成绩) as 平均成绩
from score
group by 学号;


/* 
第2步：再加上限制条件：
1）不及格课程
2）两门以上[不及格课程]
select 查询结果 [学号,平均成绩：汇总函数avg(成绩)]
from 从哪张表中查找数据 [涉及到成绩：成绩表score]
where 查询条件 [限制条件：不及格课程，平均成绩<60]
group by 分组 [每个学生的平均：按学号分组]
having 对分组结果指定条件 [限制条件：课程数目>2,汇总函数count(课程号)>2]
order by 对查询结果排序[没有];
*/
select 学号, avg(成绩) as 平均成绩
from score
where 成绩 <60
group by 学号
having count(课程号)>2;
```

![img](https://pic1.zhimg.com/80/v2-75c7395d5559c5e47e528aa7ebbab2a0_1440w.jpg)

查询学生的总成绩并进行排名

```text
【知识点】分组查询
/*
分析思路
select 查询结果 [总成绩：sum(成绩), 学号]
from 从哪张表中查找数据 [成绩表score]
where 查询条件 [没有]
group by 分组 [学生的总成绩：按照每个学生学号进行分组]
order by 排序 [按照总成绩进行排序：sum(成绩)];
/*
select 学号 ,sum(成绩) from score 
group by 学号
order by sum(成绩);
```

查询平均成绩大于60分的学生的学号和平均成绩

```text
【知识点】分组+条件
/*
分析思路
select 查询结果 [学号, 平均成绩: avg(成绩)]
from 从哪张表中查找数据 [成绩表score]
where 查询条件 [没有]
group by 分组 [学号]
having 分组条件 [平均成绩大于60分：avg(成绩 ) >60]
order by 排序 [没有];
/*
select 学号 ,avg(成绩) from score 
group by 学号  
having avg(成绩 ) >60;
```

如果上面题目不会做，可以复习这部分涉及到的sql知识：

[从零学会SQL：汇总分析www.zhihu.com![图标](https://pic2.zhimg.com/v2-12ef91a3f1e91e70bd3480d755e058b1_ipico.jpg)](https://www.zhihu.com/lives/1097094227900571648)

**3.复杂查询**

查询所有课程成绩小于60分学生的学号、姓名

```text
【知识点】子查询

1.翻译成大白话
1）查询结果：学生学号，姓名
2）查询条件：所有课程成绩 < 60 的学生，需要从成绩表里查找，用到子查询

第1步，写子查询（所有课程成绩 < 60 的学生）
select 查询结果[学号]
from 从哪张表中查找数据[成绩表：score]
where 查询条件[成绩 < 60]
group by 分组[没有]
having 对分组结果指定条件[没有]
order by 对查询结果排序[没有]
limit 从查询结果中取出指定行[没有];

select 学号 
from student
where 成绩 < 60;

第2步，查询结果：学生学号，姓名，条件是前面1步查到的学号

select 查询结果[学号,姓名]
from 从哪张表中查找数据[学生表:student]
where 查询条件[用到运算符in]
group by 分组[没有]
having 对分组结果指定条件[没有]
order by 对查询结果排序[没有]
limit 从查询结果中取出指定行[没有];
*/
select 学号,姓名
from student
where  学号 in (
select 学号 
from student
where 成绩 < 60
);
```

查询没有学全所有课的学生的学号、姓名|

```text
/*
查找出学号，条件：没有学全所有课，也就是该学生选修的课程数 < 总的课程数
【考察知识点】in，子查询
*/
select 学号,姓名
from student
where 学号 in(
select 学号 
from score
group by 学号
having count(课程号) < (select count(课程号) from course)
);
```

查询出只选修了两门课程的全部学生的学号和姓名|

```mysql
select 学号,姓名
from student
where 学号 in(
select 学号
from score
group by 学号
having count(课程号)=2
);
```

1990年出生的学生名单

![img](https://pic2.zhimg.com/80/v2-ebb49780c0ad104e4557e2a9581a41cd_1440w.jpg)

```mysql
/*
查找1990年出生的学生名单
学生表中出生日期列的类型是datetime
*/
select 学号,姓名 
from student 
where year(出生日期)=1990; 
```

查询各科成绩前两名的记录

这类问题其实就是常见的：分组取每组最大值、最小值，每组最大的N条（top N）记录。我专门写了一篇文章来回答这类问题：

[sql面试题：topN问题mp.weixin.qq.com![图标](https://pic2.zhimg.com/v2-1a8d46d131fff88b0f48d07814cd50d1_180x120.jpg)](https://link.zhihu.com/?target=https%3A//mp.weixin.qq.com/s/MuxjlFV0gi1GydOrYfiSeQ)



查询各学生的年龄（精确到月份）

```text
/*
【知识点】时间格式转化
*/
select 学号 ,timestampdiff(month ,出生日期 ,now())/12 
from student ;
```

-查询本月过生日的学生

```text
select * 
from student 
where month (出生日期 ) = month(now())+2;
```

如果上面题目不会做，可以复习这部分涉及到的sql知识：

[从零学会SQL：复杂查询www.zhihu.com![图标](https://pic2.zhimg.com/v2-12ef91a3f1e91e70bd3480d755e058b1_ipico.jpg)](https://www.zhihu.com/lives/1104157397408395264)

**4.多表查询**

![img](https://pic1.zhimg.com/80/v2-d3517e45d5a4ca25655133b714486550_1440w.jpg)

查询所有学生的学号、姓名、选课数、总成绩

```text
selecta.学号,a.姓名,count(b.课程号) as 选课数,sum(b.成绩) as 总成绩
from student as a left join score as b
on a.学号 = b.学号
group by a.学号;
```

查询平均成绩大于85的所有学生的学号、姓名和平均成绩

```text
select a.学号,a.姓名, avg(b.成绩) as 平均成绩
from student as a left join score as b
on a.学号 = b.学号
group by a.学号
having avg(b.成绩)>85;
```

查询学生的选课情况：学号，姓名，课程号，课程名称

```text
select a.学号, a.姓名, c.课程号,c.课程名称
from student a inner join score b on a.学号=b.学号
inner join course c on b.课程号=c.课程号;
```

查询出每门课程的及格人数和不及格人数

```text
-- 考察case表达式
select 课程号,
sum(case when 成绩>=60 then 1 
	 else 0 
    end) as 及格人数,
sum(case when 成绩 <  60 then 1 
	 else 0 
    end) as 不及格人数
from score
group by 课程号;
```

使用分段[100-85],[85-70],[70-60],[<60]来统计各科成绩，分别统计：各分数段人数，课程号和课程名称

```text
-- 考察case表达式
select a.课程号,b.课程名称,
sum(case when 成绩 between 85 and 100 
	 then 1 else 0 end) as '[100-85]',
sum(case when 成绩 >=70 and 成绩<85 
	 then 1 else 0 end) as '[85-70]',
sum(case when 成绩>=60 and 成绩<70  
	 then 1 else 0 end) as '[70-60]',
sum(case when 成绩<60 then 1 else 0 end) as '[<60]'
from score as a right join course as b 
on a.课程号=b.课程号
group by a.课程号,b.课程名称;
```

查询课程编号为0003且课程成绩在80分以上的学生的学号和姓名|

```text
select a.学号,a.姓名
from student  as a inner join score as b on a.学号=b.学号
where b.课程号='0003' and b.成绩>80;
```

下面是学生的成绩表（表名score，列名：学号、课程号、成绩）

![img](https://pic1.zhimg.com/80/v2-4d2a6f3afb29d0a5b596a4fd57f82cd0_1440w.jpg)

使用sql实现将该表行转列为下面的表结构

![img](https://pic2.zhimg.com/80/v2-a628c7d239662b9bee8e1a3892810861_1440w.jpg)

【面试题类型总结】这类题目属于行列如何互换，解题思路如下：

[sql面试题：行列如何互换？mp.weixin.qq.com![图标](https://pic1.zhimg.com/v2-0cbb643f6cc20cd34c69e5332abe7340_180x120.jpg)](https://link.zhihu.com/?target=https%3A//mp.weixin.qq.com/s/6Kll4Q6Xp37i2PiLUh4cMA)

![img](https://pic2.zhimg.com/80/v2-250fb5e9eb490bec82c872cdfb988abd_1440w.jpg)

-检索"0001"课程分数小于60，按分数降序排列的学生信息

思路如图：

![img](https://pic1.zhimg.com/80/v2-a72c969a9a2bd7867897dcfd75d87148_1440w.jpg)

```text
select a.*,b.成绩 
from student as a 
inner join score as b 
on a.学号 =b.学号 
where b.成绩 <60 and b.课程号 =01
order by b.成绩 desc;
```

-查询不同老师所教不同课程平均分从高到低显示

【知识点】分组+条件+排序+多表连接，思路如图

![img](https://pic4.zhimg.com/80/v2-74822d9d4428ece3e3a95269ddbffa37_1440w.jpg)

```mysql
select a.教师号,a.教师姓名,avg(c.成绩) 
from  teacher as a 
inner join course as b 
on a.教师号= b.教师号
inner join score  c on b.课程号= c.课程号
group by a.教师姓名
order by avg(c.成绩) desc;
```

-查询课程名称为"数学"，且分数低于60的学生姓名和分数

【知识点】多表连接，思路如图

![img](https://pic1.zhimg.com/80/v2-1414cafaf21a49079900f49b6c8f06e0_1440w.jpg)

```text
select a.姓名,b.成绩 
from student as a 
inner join score as b 
on a.学号 =b.学号 
inner join course c on b.课程号 =c.课程号 
where b.成绩  <60 and c.课程名称 ='数学';
```

-查询任何一门课程成绩在70分以上的姓名、课程名称和分数（与上题类似）

```text
select a.姓名,c.课程名称 ,b.成绩 
from student as a 
inner join score as b 
on a.学号=b.学号
inner join course c on b.课程号 =c.课程号 
where b.成绩 >70;
```

-查询两门及其以上不及格课程的同学的学号，姓名及其平均成绩

【知识点】分组+条件+多表连接

翻译成大白话:计算每个学号不及格分数个数，筛选出大于2个的学号并找出姓名，平均成绩，思路如图：

![img](https://pic4.zhimg.com/80/v2-e1e06e7678a85c0b2a0495cd22bea68f_1440w.jpg)

```text
select b.姓名,avg(a.成绩),a.学号  
from score as a
inner join student as b 
on a.学号 =b.学号 
where a.成绩 <60
group by a.学号 
having count(a.学号 ) >=2;
```

-查询不同课程成绩相同的学生的学生编号、课程编号、学生成绩

![img](https://pic2.zhimg.com/80/v2-db138a4aee39b2ff0f1409699b63c031_1440w.jpg)

```text
select distinct a.学号 ,a.成绩 ,a.课程号 
from score as a 
inner join score as b 
on a.学号 =b.学号 
where a.成绩 =b.成绩 and a.课程号 != b.课程号 ;
```

-查询课程编号为“0001”的课程比“0002”的课程成绩高的所有学生的学号

【知识点】多表连接+条件，思路如图

![img](https://pic1.zhimg.com/80/v2-ef875a66cbc6264fa347cb5ad07524a4_1440w.jpg)

```text
select a.学号  
from 
(select 学号 ,成绩 from score where 课程号=01) as a
inner join 
(select 学号 ,成绩 from score where 课程号=02) as b
on a.学号 =b.学号 
inner join student c on c.学号 =a.学号 
where a.成绩 >b.成绩 ;
```

-查询学过编号为“0001”的课程并且也学过编号为“0002”的课程的学生的学号、姓名

思路如图

![img](https://pic4.zhimg.com/80/v2-9e6b61a2a7f56b25ed53b80c70a11ecb_1440w.jpg)

```text
select a.学号  
from 
(select 学号 ,成绩 from score where 课程号=01) as a
inner join 
(select 学号 ,成绩 from score where 课程号=02) as b
on a.学号 =b.学号 
inner join student c on c.学号 =a.学号 
where a.成绩 >b.成绩 ;
```

-查询学过“孟扎扎”老师所教的所有课的同学的学号、姓名

思路如图

![img](https://pic1.zhimg.com/80/v2-40736fd147722579ecfe5ea3c9d5de40_1440w.jpg)

```text
select s.学号 ,s.姓名,a.学号 ,b.课程号,c.教师号 ,c.教师姓名
from student as s  
inner join score as a  
on s.学号 =a.学号 
inner join  course  b on a.课程号 =b.课程号
inner join  teacher c  on b.教师号 = c.教师号
where c.教师姓名 ='孟扎扎';
```

-查询没学过"孟扎扎"老师讲授的任一门课程的学生姓名（与上题类似，"没学过"用not in来实现)

```text
select 姓名 ,学号 
from student 
where 学号 not in (
select a.学号 
from student as a 
inner join score as b
on a.学号 =b.学号 
inner join course as c on b.课程号 =c.课程号 
inner join teacher as d on c.教师号 =d.教师号 
where d.教师姓名 ='孟扎扎');
```

-查询没学过“孟扎扎”老师课的学生的学号、姓名（与上题类似）

```text
select 学号, 姓名 
from student 
where 学号 not in 
(select 学号 from score where 课程号=
(select 课程号 from course  where 教师号 = 
(select 教师号 from teacher where 教师姓名 ='孟扎扎')
)
);
```

-查询选修“孟扎扎”老师所授课程的学生中成绩最高的学生姓名及其成绩（与上题类似,用成绩排名，用 limit 1得出最高一个）

```text
select a.姓名,b.成绩 
from student as a 
inner join score as b on a.学号=b.学号
inner join course as c on b.课程号 =c.课程号 
inner join teacher as d on c.教师号 = d.教师号 
where d.教师姓名 = '孟扎扎'
order by b.成绩 desc limit 1;
```

-查询至少有一门课与学号为“0001”的学生所学课程相同的学生的学号和姓名

```text
select 学号 ,姓名 
from student 
where 学号 in
(select distinct(学号) from score where 课程号 in 
(select 课程号 from score where 学号=0001))
 and 学号 !=0001;
```

-按平均成绩从高到低显示所有学生的所有课程的成绩以及平均成绩

【知识点】多表连接 新建字段 ，思路如图

![img](https://pic1.zhimg.com/80/v2-8cb878abdc0df9b799dab2bcc568b1f0_1440w.jpg)

```text
select a.学号,avg(a.成绩 ),
max(case when b.课程名称  = '数学' then a.成绩 else null end ) as '数学',
max(case when b.课程名称  = '语文' then a.成绩 else null end ) as '语文',
max(case when b.课程名称  = '英语' then a.成绩 else null end ) as '英语'
from score as a
inner join course as b 
on a.课程号 =b.课程号 
group by a.学号 ;
```

如果上面题目不会做，可以复习这部分涉及到的sql知识：

[从零学会SQL：多表查询www.zhihu.com![图标](https://pic2.zhimg.com/v2-12ef91a3f1e91e70bd3480d755e058b1_ipico.jpg)](https://www.zhihu.com/lives/1105128761158352896)



**5.SQL高级功能：窗口函数**

![img](https://pic4.zhimg.com/80/v2-432f5eec866aa94cd0c21ab10ee329d3_1440w.jpg)

-查询学生平均成绩及其名次

【知识点】窗口函数排名，思路如图

![img](https://pic1.zhimg.com/80/v2-9ad6ea22ce6d2545ff8092a14c2ec978_1440w.jpg)

```text
select 学号 ,avg(成绩),
row_number () over( order by avg(成绩) desc)
from score
group by 学号  ;
```

-按各科成绩进行排序，并显示排名

![img](https://pic1.zhimg.com/80/v2-8b35405df988929d5039891f1d449ec0_1440w.jpg)

```text
select 课程号 ,
row_number () over(partition by 课程号 order by 成绩 )
from score ;
```

-查询每门功成绩最好的前两名学生姓名

【知识点】窗口函数排名+多表连接+条件

![img](https://pic1.zhimg.com/80/v2-cc60234de0d2e1320920cb3e5dd693dc_1440w.jpg)

```text
select a.课程号 ,b.姓名 ,a.成绩,a.ranking from (
select 课程号 ,学号 ,成绩 ,
row_number () over(partition by 课程号 order by 成绩 desc) as ranking
from  score) as a 
inner join student b on a.学号 =b.学号 
where a.ranking <3 ;
```

-查询所有课程的成绩第2名到第3名的学生信息及该课程成绩（与上一题相似）

```text
select b.姓名 ,a.课程号 ,a.成绩 
from (
select 课程号 ,学号 ,成绩 ,
row_number () over( partition by 课程号 order by 成绩 desc) as ranking
from  score ) as a 
inner join student as b 
on a.学号 =b.学号 
where a.ranking in( 2,3) ;
```

-查询各科成绩前三名的记录（不考虑成绩并列情况）（与上一题相似）

```text
select b.姓名 ,a.课程号 ,a.成绩 
from (
select 课程号 ,学号 ,成绩 ,
row_number () over( partition by 课程号 order by 成绩 desc) as 'ranking'
from  score ) as a 
inner join student as b 
on a.学号 =b.学号 
where a.ranking <4 ;
```