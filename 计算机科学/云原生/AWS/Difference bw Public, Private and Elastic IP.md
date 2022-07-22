https://www.linkedin.com/pulse/aws-difference-bw-public-private-elastic-ip-chirag-jain/

Whenever we launch an EC2 instance, a public and a private IP get allocated to it and can be viewed via:

> AWS console -> EC2 instance -> Description

**Public DNS/IP:** It is allocated from a pool of available IP's and it is mandatory to let you connect from anywhere around the globe to your EC2 instance.

**Private IP:** Its allocation is based on vpc/subnet in which EC2 is setup. Every subnet has a range of IP's, out of which one IP gets allocated to the launched EC2. Scope or visibility of this IP is only under the defined VPC. Hence, to communicate between two or more EC2 instances using private IP, all must be under the same vpc.

**Note:** Private IP designated to an EC2 remains same until vpc is same.

**Elastic IP:** - It is similar to static IP and can be assign to any EC2 instance. Once we assign it, existing public IP gets released and replaced with the newly assigned Elastic IP. They are allocated to the AWS account so that we can release it from specific EC2 and re-assign it to any other EC2 instances (if needed).

- There are many reasons for why we should use Elastic IP. For example - Public IP & DNS records changes if we stop-start an instance. This would lead to a non-functional website until DNS records are re-updated. To avoid such issues, we make use of Elastic IP.

```
Path: AWS console -> EC2 -> Network & Security -> Elastic IP's
```

To assign an Elastic IP, navigate to above path and click on '***Allocate New Address***', followed by specifying the required EC2 instance.

![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h4g0or89zgj20sy09o0ug.jpg)

Elastic IP can be dissociated anytime per need. Once dissociated, AWS re-assign a new public IP from its pool whereas Elastic IP gets listed in dissociated IP list.

**Effects of Restart | stop-start | terminate of EC2 instances on IP addresses:**

1. Restart of EC2: All 3 IP's remains same as virtual machines remain unchanged.
2. Stop-Start: Existing Public IP is released as AWS take away the vm. On start, new public IP/vm is provisioned. It is validated only for EBS-backed AMI. Private IP remains same, whereas if an EC2 is designated with Elastic IP, both Elastic/Public IP will remain same.
3. Terminate: Both Public and Private IP are released, whereas Elastic IP gets dissociated, which can be associated again with any other EC2 instance.

---

每当我们启动 EC2 实例时，都会为其分配一个公共 IP 和一个私有 IP，可以通过以下方式查看：

> AWS 控制台 -> EC2 实例 -> 说明

**公共 DNS/IP：**它是从可用 IP 池中分配的，必须让您从全球任何地方连接到您的 EC2 实例。

**私有 IP：**其分配基于设置 EC2 的 vpc/子网。每个子网都有一个 IP 范围，其中一个 IP 被分配给启动的 EC2。此 IP 的范围或可见性仅在定义的 VPC 下。因此，要使用私有 IP 在两个或多个 EC2 实例之间进行通信，所有实例都必须在同一个 vpc 下。

**注意：**指定给 EC2 的私有 IP 保持不变，直到 vpc 相同。

**弹性 IP：** - 类似于静态 IP，可以分配给任何 EC2 实例。一旦我们分配它，现有的公共 IP 就会被释放并替换为新分配的弹性 IP。它们被分配到 AWS 账户，以便我们可以从特定的 EC2 释放它并将其重新分配给任何其他 EC2 实例（如果需要）。

- 我们应该使用弹性 IP 的原因有很多。例如 - 如果我们停止启动实例，公共 IP 和 DNS 记录会发生变化。在重新更新 DNS 记录之前，这将导致网站无法正常运行。为避免此类问题，我们使用弹性 IP。

```
路径：AWS 控制台 -> EC2 -> 网络和安全 -> 弹性IP
```

要分配弹性 IP，请导航到上述路径并单击“***分配新地址***”，然后指定所需的 EC2 实例。

![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h4g0or89zgj20sy09o0ug.jpg)

弹性 IP 可以根据需要随时分离。一旦分离，AWS 会从其池中重新分配一个新的公共 IP，而弹性 IP 会列在分离的 IP 列表中。

**重启的影响 | 停止启动 | 终止 IP 地址上的 EC2 实例：**

1. EC2 重启：所有 3 个 IP 保持不变，因为虚拟机保持不变。
2. 停止启动：现有公共 IP 在 AWS 带走 vm 时被释放。开始时，会提供新的公共 IP/vm。它仅针对 EBS 支持的 AMI 进行验证。私有 IP 保持不变，而如果使用弹性 IP 指定 EC2，则弹性/公共 IP 将保持不变。
3. 终止：公共和私有 IP 都被释放，而弹性 IP 被分离，可以再次与任何其他 EC2 实例关联。