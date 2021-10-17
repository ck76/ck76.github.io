# 使用Terraform在GCP上部署GKE

简单学习了一下[Terraform](https://www.terraform.io/)的入门教程。拿[GKE](https://cloud.google.com/kubernetes-engine/)练练手，顺便整理记录一下步骤。本文主旨搭建一套GKE。将来目标持续优化代码，融入到CI/CD中实现Provision Infrastructure的步骤。

先决条件：Google,GCP。
 因为使用了公司内部资源所以会有各种打码.文中Google的Project ID 都将会使用GCP Project ID 的码代替

找到官方模版[google-gke-cluster](https://github.com/terraform-providers/terraform-provider-kubernetes/tree/master/_examples/google-gke-cluster)看了一眼后发现username 和 password 将来在CI/CD 上使用不是很方便就注视掉了。（其实主要是我也不知道username 和 password 在什么地方可以找到。GCP上一般我用的是json文件做认证）

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvie58eoiwj60d809t0t602.jpg)

image.png

根据[Readme](https://github.com/terraform-providers/terraform-provider-kubernetes/blob/master/_examples/google-gke-cluster/README.md)中的提示specifically look at arguments credentials and project后添加了credentials 和 project 变量。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvie5hmfl5j60bq07it8w02.jpg)

image.png


 使用gcloud 创建GCP ServiceAccount





```bash
gcloud iam service-accounts create terraform --display-name "Terraform admin account"
```

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvie5k1yq3j60kn013t8o02.jpg)

image.png



创建Service Account Key 并导出json文件，因为没有找到创建GKE都需要那些权限所以准备后续看报错慢慢添加对应的permission



```bash
gcloud iam service-accounts keys create ./terraform.json --iam-account terraform@project_id.iam.gserviceaccount.com
```

Terraform 初始化下载provider



```bash
Terraform ini
```

查看GKE里 Kubernetes Engine versions [release-notes](https://cloud.google.com/kubernetes-engine/release-notes)选择了最新的版本去apply



```bash
terraform apply \
        -var 'kubernetes_version=1.11.6-gke.6' \
        -var 'cluster_name=terraform-example-cluster' \
        -var 'region=us-west1
```

因为没有给SA 赋权所以apply 时报错了



![img](https://tva1.sinaimg.cn/large/008i3skNly1gvie5lv75oj60xc05bt9602.jpg)

image.png

添加viewer权限



```bash
gcloud projects add-iam-policy-binding project_id --member serviceAccount:terraform@project_id.iam.gserviceaccount.com --role roles/viewer
```

然后就成功了～



![img](https://tva1.sinaimg.cn/large/008i3skNly1gvie5n09zxj60me0lq77602.jpg)

image.png

然而 yes 后报错提示node_version and min_master_version must be set to equivalent values on create

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvie5p47kcj60pp06475102.jpg)

image.png


 查看了官方说明 [node_version](https://www.terraform.io/docs/providers/google/r/container_cluster.html#node_version)修改tf文件添加min_master_version参数

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvie5qi754j60cb05cjrm02.jpg)

image.png


 yes后继续提示权限不够

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvie5rxw35j60xc05b75102.jpg)

image.png


 查了一下直接给了[GKE Cluster Admin](https://cloud.google.com/kubernetes-engine/docs/how-to/iam#predefined) role 后继续报错。然后又给了iam.serviceAccountUser role





```bash
gcloud projects add-iam-policy-binding project_id --member serviceAccount:terraform@project_id.iam.gserviceaccount.com --role roles/container.clusterAdmin

gcloud projects add-iam-policy-binding project_id --member serviceAccount:terraform@project_id.iam.gserviceaccount.com --role roles/iam.serviceAccountUser
```

然后完了。发现terraform 还是蛮方便的



![img](https://tva1.sinaimg.cn/large/008i3skNly1gvie5tfc79j60g505jq3602.jpg)

image.png



![img](https://tva1.sinaimg.cn/large/008i3skNly1gvie5uqcp8j60px08674v02.jpg)

image.png

最后毁尸灭迹



```bash
terraform destroy \
        -var 'kubernetes_version=1.11.6-gke.6' \
        -var 'cluster_name=terraform-example-cluster' \
        -var 'region=us-west1'
```

Next: 下一步需要测试如下
 1.本想创建的size是3.结果发现创建的Kubernetes Cluster的size 是6， 明明initial_node_count = 3。可能需要添加修改 node_count = "3"

2.[autoscaling](https://nickcharlton.net/posts/kubernetes-terraform-google-cloud.html) {
 min_node_count = 2
 max_node_count = 5
 }

3.使用[Remote State](https://www.terraform.io/docs/state/remote.html)在[GCS](https://www.terraform.io/docs/backends/types/gcs.html) 上来维护多用户操作



作者：中饭吃什么啊
链接：https://www.jianshu.com/p/97366ef4726a
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

