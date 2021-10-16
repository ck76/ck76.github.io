[TOC]

## 通过Rancher一键部署k8s服务

- 地址:  [https://rancher.com/quick-start/](https://links.jianshu.com/go?to=https%3A%2F%2Francher.com%2Fquick-start%2F)
- 命令



```base
sudo docker run --privileged -d --restart=unless-stopped -p 80:80 -p 443:443 rancher/rancher
```

- 然后访问你的docker机器的IP地址就可以看到一个叫local的k8s环境，不过不知道是不是我操作不对,我不能使用这个集群
- 点击添加集群,自定义一通默认到最后给你一个docker命令执行后我们就又得到一个自定义的k8s集群了（弄一个单节点集群的话最后一步把3个都要勾选上）
- 下载kubectl [https://kubernetes.io/docs/tasks/tools/install-kubectl/](https://links.jianshu.com/go?to=https%3A%2F%2Fkubernetes.io%2Fdocs%2Ftasks%2Ftools%2Finstall-kubectl%2F)
- 创建一个～/.kube/config文件，把[https://k8s](https://links.jianshu.com/go?to=https%3A%2F%2Fk8s)集群部署IP/c/local/monitoring 里面的Kubeconfig File按钮里面的文本拷贝进去OK

## 部署一个 nginx

nginx.yaml



```base
apiVersion: apps/v1
kind: Deployment
metadata:
  name: nginx-deployment
  labels:
    app: nginx
spec:
 selector:
   matchLabels:
    app: nginx
 replicas: 1
 template:
   metadata:
    labels:
     app: nginx
   spec:
    containers:
    - name: nginx
      image: nginx
      ports:
      - containerPort: 80
```

#### 运行



```base
>kubectl apply -f nginx.yml
deployment.apps/nginx configured

>kubectl get pods
NAME                     READY   STATUS    RESTARTS   AGE
nginx-deployment-7f4768c97b-69t65   1/1     Running   0          105s

//删除 deployment
kubectl delete deployment nginx-deployment
//删除 pod
kubectl.exe delete pod nginx-deployment-7f4768c97b-69t65
//查看 pod 日志
kubectl.exe logs -f nginx-deployment-7f4768c97b-69t65
```

#### 现在外网还无法访问要把pod内的端口通过service给暴露出来

nginx_service.yaml



```base
apiVersion: v1
kind: Service
metadata:
  name: nginx-service
  labels:
   app: nginx
spec:
 selector:
   app: nginx
 ports:
  - name: nginx-port
    protocol: TCP
    port: 80             
    targetPort: 80          
    nodePort: 30080        
 type: NodePort   
```

#### 运行



```base
>kubectl apply -f nginx_service.yaml


>kubectl get service
NAME            TYPE        CLUSTER-IP      EXTERNAL-IP   PORT(S)        AGE
kubernetes      ClusterIP   10.43.0.1       <none>        443/TCP        83m
nginx-service   NodePort    10.43.207.110   <none>        80:30080/TCP   29m
```

#### 访问 [http://k8s](https://links.jianshu.com/go?to=http%3A%2F%2Fk8s)集群部署IP:30080 就可以看到熟悉的 nginx 欢迎界面了

## 在k8s中部署 golang 服务

### 代码



```base
package main
import "net/http"
func main() {
    http.HandleFunc("/k8s", func(writer http.ResponseWriter, request *http.Request) {
        _, _ = writer.Write([]byte("hello world k8s"))
    })
    _ = http.ListenAndServe(":8080", nil)
}
```

### Dockerfile



```base
FROM scratch
ADD app /var/app
WORKDIR /var
ENTRYPOINT ["/var/app"]
```

### 构建docker



```base
//docker image build -t [username(dockerhub 用户名)]/[repository（在dockerhub新建的仓库）]:[tag（版本号）] .
docker image build -t hwholiday/app:v1 .
docker run  -p 8080:8080 --name app -d  hwholiday/app:v1
//访问定义好的api能正常输出就代表一切正常接下来我们把镜像传到 Dockerhub
docker push hwholiday/app:v1
```

### 在k8s运行服务

app.yml



```base
apiVersion: apps/v1
kind: Deployment
metadata:
  name: app-deployment
  labels:
    app: app
spec:
  selector:
    matchLabels:
      app: app
  replicas: 1
  template:
    metadata:
      labels:
        app: app
    spec:
      containers:
        - name: app
          image: hwholiday/app:v1
          ports:
            - containerPort: 8080
```

app_service.yml



```base
apiVersion: v1
kind: Service
metadata:
  name: app-service
  labels:
    app: app
spec:
  selector:
    app: app
  ports:
    - name: app-port
      protocol: TCP
      port: 8080
      targetPort: 8080
      nodePort: 31080
  type: NodePort
```

命令



```base
kubectl.exe apply -f app.yml
kubectl.exe apply -f app_service.yml
```

#### 访问   [http://k8s](https://links.jianshu.com/go?to=http%3A%2F%2Fk8s)集群部署IP:31080/k8s   正常输出结果（hello world k8s） 部署完成

### [完整代码地址](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2Fhwholiday%2Flearning_tools%2Ftree%2Fmaster%2Fk8s)



作者：hwholiday
链接：https://www.jianshu.com/p/e2071b7e0757
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

- https://www.jianshu.com/p/e2071b7e0757?utm_campaign=haruki&utm_content=note&utm_medium=reader_share&utm_source=qq
- https://www.jianshu.com/u/e35388d0f8f8
- https://github.com/hwholiday/learning_tools/tree/master/k8s
- 