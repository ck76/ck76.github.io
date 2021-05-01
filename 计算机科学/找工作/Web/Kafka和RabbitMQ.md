https://www.zhihu.com/question/353858758



https://zhuanlan.zhihu.com/p/60288391



**在应用场景方面**
**RabbitMQ**
RabbitMQ遵循AMQP协议，由内在高并发的erlanng语言开发，用在实时的对可靠性要求比较高的消息传递上，适合企业级的消息发送订阅，也是比较受到大家欢迎的。
**kafka**
kafka是Linkedin于2010年12月份开源的消息发布订阅系统,它主要用于处理活跃的流式数据,大数据量的数据处理上。常用日志采集，数据采集上。
**ActiveMQ**

- 异步调用
- 一对多通信
- 做多个系统的集成，同构、异构
- 作为RPC的替代
- 多个应用相互解耦
- 作为事件驱动架构的幕后支撑
- 为了提高系统的可伸缩性

**在架构模型方面，**
**RabbitMQ**
RabbitMQ遵循AMQP协议，RabbitMQ的broker由Exchange,Binding,queue组成，其中exchange和binding组成了消息的路由键；客户端Producer通过连接channel和server进行通信，Consumer从queue获取消息进行消费（长连接，queue有消息会推送到consumer端，consumer循环从输入流读取数据）。rabbitMQ以broker为中心；有消息的确认机制。
**kafka**
kafka遵从一般的MQ结构，producer，broker，consumer，以consumer为中心，消息的消费信息保存的客户端consumer上，consumer根据消费的点，从broker上批量pull数据；无消息确认机制。
**在吞吐量**
**kafka**
kafka具有高的吞吐量，内部采用消息的批量处理，zero-copy机制，数据的存储和获取是本地磁盘顺序批量操作，具有O(1)的复杂度，消息处理的效率很高。
**rabbitMQ**
rabbitMQ在吞吐量方面稍逊于kafka，他们的出发点不一样，rabbitMQ支持对消息的可靠的传递，支持事务，不支持批量的操作；基于存储的可靠性的要求存储可以采用内存或者硬盘。
**在可用性方面，**
**rabbitMQ**
rabbitMQ支持miror的queue，主queue失效，miror queue接管。
**kafka**
kafka的broker支持主备模式。
**在集群负载均衡方面，**
**kafka**
kafka采用zookeeper对集群中的broker、consumer进行管理，可以注册topic到zookeeper上；通过zookeeper的协调机制，producer保存对应topic的broker信息，可以随机或者轮询发送到broker上；并且producer可以基于语义指定分片，消息发送到broker的某分片上。
**rabbitMQ**
rabbitMQ的负载均衡需要单独的loadbalancer进行支持。