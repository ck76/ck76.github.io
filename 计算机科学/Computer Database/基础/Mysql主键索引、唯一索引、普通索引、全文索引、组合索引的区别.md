

Mysql索引概念：
说说Mysql索引，看到一个很少比如：索引就好比一本书的目录，它会让你更快的找到内容，显然目录（索引）并不是越多越好，假如这本书1000页，有500也是目录，它当然效率低，目录是要占纸张的,而索引是要占磁盘空间的。


Mysql索引主要有两种结构：B+树和hash.

hash:hsah索引在mysql比较少用,他以把数据的索引以hash形式组织起来,因此当查找某一条记录的时候,速度非常快.当时因为是hash结构,每个键只对应一个值,而且是散列的方式分布.所以他并不支持范围查找和排序等功能.

B+树:b+tree是mysql使用最频繁的一个索引数据结构,数据结构以平衡树的形式来组织,因为是树型结构,所以更适合用来处理排序,范围查找等功能.相对hash索引,B+树在查找单条记录的速度虽然比不上hash索引,但是因为更适合排序等操作,所以他更受用户的欢迎.毕竟不可能只对数据库进行单条记录的操作. 


Mysql常见索引有：主键索引、唯一索引、普通索引、全文索引、组合索引

PRIMARY KEY（主键索引）  ALTER TABLE `table_name` ADD PRIMARY KEY ( `column` ) UNIQUE(唯一索引)     ALTER TABLE `table_name` ADD UNIQUE (`column`)
INDEX(普通索引)     ALTER TABLE `table_name` ADD INDEX index_name ( `column` ) FULLTEXT(全文索引)      ALTER TABLE `table_name` ADD FULLTEXT ( `column` )
组合索引   ALTER TABLE `table_name` ADD INDEX index_name ( `column1`, `column2`, `column3` ) 

Mysql各种索引区别：
普通索引：最基本的索引，没有任何限制
唯一索引：与"普通索引"类似，不同的就是：索引列的值必须唯一，但允许有空值。
主键索引：它 是一种特殊的唯一索引，不允许有空值。 
全文索引：仅可用于 MyISAM 表，针对较大的数据，生成全文索引很耗时好空间。
组合索引：为了更多的提高mysql效率可建立组合索引，遵循”最左前缀“原则。