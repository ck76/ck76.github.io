

Organizations often use cloud-based applications to analyze large amounts of data, including system and application logs, business metrics, external data sources, public data sets and many others.

AWS, the largest public cloud provider, has more than a dozen data analytics offerings. These services occasionally have overlapping functionality, which can make it harder to know which one to choose.

Let's look at three services for data analysis on AWS -- Amazon Redshift, [Amazon EMR](https://www.techtarget.com/searchaws/definition/Amazon-Elastic-MapReduce-Amazon-EMR) and Amazon Athena -- to help find the right fit for your data analysis needs.  

### Amazon Redshift

Redshift is a managed data warehouse that stores and performs data analysis queries in a centralized location. The work is done in a Redshift cluster that consists of one or more compute nodes that also store data. While Redshift also supports analyzing data stored in Amazon S3 using [Amazon Redshift Spectrum](https://www.techtarget.com/searchaws/definition/Amazon-Redshift-Spectrum), its main focus is on analyzing data stored in the cluster itself.

Redshift is designed to pull together data from lots of different sources and store it in a structured fashion. This builds data consistency rules directly into the tables of the database. Amazon Redshift is the best service to use when you need to perform complex queries on massive collections of structured and semi-structured data with fast performance. This makes it ideal for, say, a retail business that needs to regularly create performance reports based on data from inventory, financial and retail sales systems.

However, this model makes it difficult to reduce the size of a Redshift cluster based on demand, because the data is stored directly in the cluster. This typically results in high cost, given that Redshift clusters are often always on. IT teams can alleviate this issue with reserved node offerings, which are billed at discounted hourly rates for either a one-year or three-year period.

### Amazon Athena

Athena is a serverless service for data analysis on AWS mainly geared towards accessing data stored in Amazon S3. But since it can access data defined in AWS Glue catalogues, it also supports Amazon DynamoDB, ODBC/JDBC drivers and Redshift.

Data analysts use Athena, which is built on [Presto](https://prestodb.io/overview.html), to execute queries using SQL syntax. Unlike Redshift and EMR, users don't have to explicitly configure the underlying compute infrastructure and they only pay for data scanned ($5 per terabyte in most regions), which makes it a cost-effective tool in most cases.

Athena is widely used to analyze log data stored in S3 for services such as [Application Load Balancer](https://www.techtarget.com/searchaws/definition/application-load-balancer), Amazon CloudFront, AWS CloudTrail, Amazon Kinesis Data Firehose and any type of log data exported into S3. While this service is the easiest way to get visibility into data stored in S3, services such as EMR can potentially bring better performance -- albeit at a potentially higher cost -- since developers control the underlying infrastructure.

Athena is a good fit for infrequent or ad hoc data analysis needs, since users don't have to launch any infrastructure and the service is always ready to query data.

### Amazon EMR

Amazon EMR provides [managed deployments](https://www.techtarget.com/searchitoperations/tip/Use-Amazon-EMR-with-Apache-Airflow-to-simplify-processes) of popular data analytics platforms, such as Presto, Spark, Hadoop, Hive and HBase, among others. EMR automates the launch of compute and storage nodes powered by Amazon EC2 instances, and more recently AWS Fargate.

While data can be stored inside EC2 instances using an [HDFS](https://www.techtarget.com/searchaws/tip/Compare-AWS-data-lakes-with-S3-against-HDFS) (Hadoop Distributed File System), the service also supports querying data stored in sources outside the cluster, such as relational databases or S3.

This makes it possible to reduce the cluster size based on demand and therefore optimize cost. The service is a good fit for teams that prefer or need to use any of the popular platforms supported in EMR (i.e. Presto, Spark, etc.). It also supports Reserved Instances and Savings Plans for EC2 clusters and Savings Plans for Fargate, which can help lower cost.

EMR is a good fit for predictable data analysis tasks, typically on clusters that need to be available for extended periods of time. This includes data loads in which having control over the underlying infrastructure -- EC2 instances and S3 storage -- would optimize performance and justify the additional work.

………………

---

组织经常使用基于云的应用程序来分析大量数据，包括系统和应用程序日志、业务指标、外部数据源、公共数据集等等。

AWS 是最大的公共云提供商，拥有十几种数据分析产品。这些服务有时具有重叠的功能，这会使您更难知道该选择哪一个。

让我们看一下在 AWS 上进行数据分析的三项服务——Amazon Redshift、[Amazon EMR](https://www.techtarget.com/searchaws/definition/Amazon-Elastic-MapReduce-Amazon-EMR)和 Amazon Athena——以帮助找到最适合您的数据分析需求的服务。  

### 亚马逊红移

Redshift 是一个托管数据仓库，可在集中位置存储和执行数据分析查询。这项工作在一个 Redshift 集群中完成，该集群由一个或多个还存储数据的计算节点组成。[虽然 Redshift 还支持使用Amazon Redshift Spectrum](https://www.techtarget.com/searchaws/definition/Amazon-Redshift-Spectrum)分析存储在 Amazon S3 中的数据，但其主要重点是分析存储在集群本身中的数据。

Redshift 旨在将来自许多不同来源的数据汇集在一起，并以结构化方式存储。这会将数据一致性规则直接构建到数据库的表中。当您需要以快速的性能对大量结构化和半结构化数据的集合执行复杂查询时，Amazon Redshift 是最好的服务。这使其成为需要定期根据库存、财务和零售销售系统数据创建绩效报告的零售企业的理想选择。

但是，这种模型很难根据需求减少 Redshift 集群的大小，因为数据是直接存储在集群中的。鉴于 Redshift 集群通常始终处于开启状态，这通常会导致高成本。IT 团队可以通过预留节点产品来缓解这个问题，这些产品在一年或三年期间按小时打折收费。

### 亚马逊雅典娜

Athena 是 AWS 上用于数据分析的无服务器服务，主要用于访问存储在 Amazon S3 中的数据。但由于它可以访问 AWS Glue 目录中定义的数据，它还支持 Amazon DynamoDB、ODBC/JDBC 驱动程序和 Redshift。

[数据分析师使用基于Presto](https://prestodb.io/overview.html)构建的 Athena使用 SQL 语法执行查询。与 Redshift 和 EMR 不同，用户不必显式配置底层计算基础设施，他们只需为扫描的数据付费（大多数地区每 TB 5 美元），这使其在大多数情况下成为一种具有成本效益的工具。

Athena 广泛用于分析存储在 S3 中的日志数据，用于[应用程序负载均衡器](https://www.techtarget.com/searchaws/definition/application-load-balancer)、Amazon CloudFront、AWS CloudTrail、Amazon Kinesis Data Firehose 等服务以及导出到 S3 的任何类型的日志数据。虽然这项服务是了解存储在 S3 中的数据的最简单方法，但诸如 EMR 之类的服务可能会带来更好的性能——尽管成本可能更高——因为开发人员控制着底层基础设施。

Athena 非常适合不频繁或临时的数据分析需求，因为用户不必启动任何基础架构，并且该服务始终准备好查询数据。

### 亚马逊电子病历

Amazon EMR 提供流行数据分析平台的[托管部署](https://www.techtarget.com/searchitoperations/tip/Use-Amazon-EMR-with-Apache-Airflow-to-simplify-processes)，例如 Presto、Spark、Hadoop、Hive 和 HBase 等。EMR 自动启动由 Amazon EC2 实例和最近的 AWS Fargate 提供支持的计算和存储节点。

虽然数据可以使用[HDFS](https://www.techtarget.com/searchaws/tip/Compare-AWS-data-lakes-with-S3-against-HDFS)（Hadoop 分布式文件系统）存储在 EC2 实例中，但该服务还支持查询存储在集群外部源中的数据，例如关系数据库或 S3。

这使得可以根据需求减少集群大小，从而优化成本。该服务非常适合喜欢或需要使用 EMR 支持的任何流行平台（即 Presto、Spark 等）的团队。它还支持 EC2 集群的预留实例和 Savings Plans 以及 Fargate 的 Savings Plans，这有助于降低成本。

EMR 非常适合可预测的数据分析任务，通常用于需要长时间可用的集群。这包括控制底层基础设施（EC2 实例和 S3 存储）的数据负载，可以优化性能并证明额外工作的合理性。



https://www.techtarget.com/searchcloudcomputing/answer/Compare-EMR-Redshift-and-Athena-for-data-analysis-on-AWS