https://medium.com/awesome-cloud/aws-vpc-difference-between-internet-gateway-and-nat-gateway-c9177e710af6

https://serverfault.com/questions/556363/what-is-the-difference-between-a-public-and-private-subnet-in-a-amazon-vpc

The main difference is the route for 0.0.0.0/0 in the associated route table.

A private subnet sets that route to a NAT instance. Private subnet instances only need a private ip and internet traffic is routed through the NAT in the public subnet. You could also have no route to 0.0.0.0/0 to make it a truly **private** subnet with no internet access in or out.

A public subnet routes 0.0.0.0/0 through an Internet Gateway (igw). Instances in a public subnet require public IPs to talk to the internet.

The warning appears even for private subnets, but the instance is only accessible inside your vpc.

- External traffic can only reach the instance if: It has a public IP assigned, it is on a subnet with the default route for 0.0.0.0/0 pointed at an internet gateway (aka 'public subnet'), the assigned security group allows the incoming traffic on the specified port from 0.0.0.0/0, and if the network ACLs allow the ip/port. If ANY of these are not set correctly, public traffic will not reach the instance.

- **PUBLIC SUBNET** If a subnet's traffic is routed to an internet gateway, the subnet is known as a public subnet. **PRIVATE SUBNET**If a subnet doesn't have a route to the internet gateway, the subnet is known as a private subnet.

- **Can Private Subnet go To the Internet?**

  So, answer is by **default no**,
  If you wanted to access To the Internet through a Private Subnet or a Subnet having no Public IP or a Subnet having disabled **Auto-assign Public IPv4**, you need to create NAT instance or a NAT-gateway, and that NAT-gateway must have Public IP so, your service/Instance with Private IP will go to the Internet and can download updates,software and packages. Private IP will route traffic to NAT-gateway and NAT-gateway will route to Internet Gateway. Then your communication from Private Subnets to Internet can start, you can check your IP by simply running command ***curl wgetIP.com\***, it will give you NAT-gateway Public IP, because NAT-gateway will go to the Internet for you and Internet gateway is a path of outside communication to the world and its a term in networking as well to pass the traffic from inbound to outbound. You can simply understand its main gate of the building for the exit.

  **Conclusion** The instances in the public subnet can send outbound traffic directly to the Internet, whereas the instances in the private subnet can't. Instead, the instances in the private subnet can access the Internet by using a network address translation (NAT) gateway that resides in the public subnet. The database servers can connect to the Internet for software updates using the NAT gateway, but the Internet cannot establish connections to the database servers."**.....said, AWS**

----

主要区别在于相关路由表中0.0.0.0/0的路由。

一个私有子网将该路由设置为一个NAT实例。私有子网实例只需要一个私有ip，互联网流量通过公共子网的NAT进行路由。你也可以不设置0.0.0.0/0的路由，使其成为一个真正的**私人**子网，没有互联网接入或流出。

公共子网通过互联网网关（igw）对0.0.0.0/0进行路由。公共子网中的实例需要公共IP来与互联网对话。

即使是私有子网也会出现这个警告，但是这个实例只能在你的vpc内部访问。

- 外部流量只有在以下情况下才能到达该实例。它有一个分配的公共IP，它在一个子网中，0.0.0.0/0的默认路由指向互联网网关（又称 "公共子网"），分配的安全组允许从0.0.0.0/0的指定端口传入流量，并且如果网络ACL允许该IP/端口。如果其中任何一项设置不正确，公共流量将无法到达实例。

- 如果一个子网的流量被路由到互联网网关，该子网被称为公共子网。**如果一个子网没有到互联网网关的路由，那么这个子网就被称为私有子网。

- 私有子网可以进入互联网吗？

  所以，答案是：**默认不可以**。
  如果你想通过一个私有子网或一个没有公共IP的子网或一个禁用**自动分配公共IPv4的子网访问互联网，你需要创建NAT实例或一个NAT-网关，该NAT-网关必须有公共IP，这样，你的服务/私有IP的实例将进入互联网并可以下载更新、软件和软件包。私有IP将路由流量到NAT网关，NAT网关将路由到互联网网关。你可以通过简单地运行***curl wgetIP.com/***命令来检查你的IP，它会给你NAT-gateway公共IP，因为NAT-gateway将为你进入互联网，互联网网关是一个外部通信的路径，它在网络中也是一个术语，将流量从入境传到出境。你可以简单地理解为建筑物的主门，用于出口。

  **结论**公共子网中的实例可以直接向互联网发送出站流量，而私有子网中的实例则不能。相反，私有子网中的实例可以通过使用居住在公共子网中的网络地址转换（NAT）网关来访问互联网。数据库服务器可以使用NAT网关连接到互联网进行软件更新，但互联网不能建立与数据库服务器的连接。"**.....said, AWS**。 使用www.DeepL.com/Translator翻译（免费版）



---

![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h4g14w51cjj20xy0qqtcq.jpg)