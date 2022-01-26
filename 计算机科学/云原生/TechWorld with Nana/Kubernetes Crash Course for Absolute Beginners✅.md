[TOC]

- https://www.youtube.com/watch?v=s_o8dwzRlu4
- https://gitlab.com/nanuchi/k8s-in-1-hour

```c
▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro and Course Overview
1:44 - What is Kubernetes
4:33 -  Kubernetes Architecture
9:29 - Node and Pod
8:58 - Main K8s Components
  09:29 - Node & Pod
  12:19 - Service & Ingress
  14:31 - ConfigMap & Secret
  17:52 - Volume
  19:46 - Deployment & StatefulSet
26:28 - Kubernetes Configuration
32:39 - Minikube and Kubectl - Setup K8s cluster locally
41:17 - Complete Demo Project: Deploy WebApp with MongoDB
1:05:40 - Interacting with Kubernetes Cluster
1:11:03 - Congrats! You made it to the end 🎉
```

![image-20220126160626133](https://tva1.sinaimg.cn/large/008i3skNly1gyr4z7tndij31fm0u0dj4.jpg)



![image-20220126160755812](https://tva1.sinaimg.cn/large/008i3skNly1gyr50rkwrhj31lg0u0wiu.jpg)



![image-20220126160859984](https://tva1.sinaimg.cn/large/008i3skNly1gyr51vff7zj31ff0u0gpd.jpg)



![image-20220126160917439](https://tva1.sinaimg.cn/large/008i3skNly1gyr528a8wvj31ma0u0jvv.jpg)



![image-20220126161051607](https://tva1.sinaimg.cn/large/008i3skNly1gyr53t3jowj31bs0u0acu.jpg)



![image-20220126161307223](https://tva1.sinaimg.cn/large/008i3skNly1gyr566eo5nj31l70u0tbg.jpg)



![image-20220126161506173](https://tva1.sinaimg.cn/large/008i3skNly1gyr588al8zj31i50u0q6m.jpg)



![image-20220126161605278](https://tva1.sinaimg.cn/large/008i3skNly1gyr599kmwbj31gy0u00w2.jpg)



![image-20220126162001585](https://tva1.sinaimg.cn/large/008i3skNly1gyr5dcusrej31nt0u0tcm.jpg)



![image-20220126162028746](https://tva1.sinaimg.cn/large/008i3skNly1gyr5dtnb72j31pv0u077t.jpg)



![image-20220126162050027](https://tva1.sinaimg.cn/large/008i3skNly1gyr5e6ui6bj31t00sygpe.jpg)



![image-20220126162107047](https://tva1.sinaimg.cn/large/008i3skNly1gyr5ehmgurj31ri0u0gpx.jpg)



![image-20220126162116490](https://tva1.sinaimg.cn/large/008i3skNly1gyr5en4h83j31u90u0n1a.jpg)



![image-20220126162224392](https://tva1.sinaimg.cn/large/008i3skNly1gyr5ftomr3j31i50u0jvc.jpg)



![image-20220126162242383](https://tva1.sinaimg.cn/large/008i3skNly1gyr5g4ut9sj31nq0scdjq.jpg)



![image-20220126162336376](https://tva1.sinaimg.cn/large/008i3skNly1gyr5h36k31j31dw0u00w9.jpg)



![image-20220126162343701](https://tva1.sinaimg.cn/large/008i3skNly1gyr5h7b3fcj31cq0u0gp1.jpg)



![image-20220126162411500](https://tva1.sinaimg.cn/large/008i3skNly1gyr5hojv1ij31ei0u0jv7.jpg)



![image-20220126162458822](https://tva1.sinaimg.cn/large/008i3skNly1gyr5iid6fvj31kp0u0q76.jpg)



![image-20220126162509532](https://tva1.sinaimg.cn/large/008i3skNly1gyr5ioo1pmj31lj0u076u.jpg)





- [**mongo-config.yaml**](https://gitlab.com/nanuchi/k8s-in-1-hour/-/blob/master/mongo-config.yaml)

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: mongo-config
data:
  mongo-url: mongo-service
```

- [**mongo-secret.yaml**](https://gitlab.com/nanuchi/k8s-in-1-hour/-/blob/master/mongo-secret.yaml)

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: mongo-secret
type: Opaque
data:
  mongo-user: bW9uZ291c2Vy
  mongo-password: bW9uZ29wYXNzd29yZA==
```

- [**mongo.yaml**](https://gitlab.com/nanuchi/k8s-in-1-hour/-/blob/master/mongo.yaml)

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: mongo-deployment
  labels:
    app: mongo
spec:
  replicas: 1
  selector:
    matchLabels:
      app: mongo
  template:
    metadata:
      labels:
        app: mongo
    spec:
      containers:
      - name: mongodb
        image: mongo:5.0
        ports:
        - containerPort: 27017
        env:
        - name: MONGO_INITDB_ROOT_USERNAME
          valueFrom:
            secretKeyRef:
              name: mongo-secret
              key: mongo-user
        - name: MONGO_INITDB_ROOT_PASSWORD
          valueFrom:
            secretKeyRef:
              name: mongo-secret
              key: mongo-password  
---
apiVersion: v1
kind: Service
metadata:
  name: mongo-service
spec:
  selector:
    app: mongo
  ports:
    - protocol: TCP
      port: 27017
      targetPort: 27017
```

- [**webapp.yaml**](https://gitlab.com/nanuchi/k8s-in-1-hour/-/blob/master/webapp.yaml)

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: webapp-deployment
  labels:
    app: webapp
spec:
  replicas: 1
  selector:
    matchLabels:
      app: webapp
  template:
    metadata:
      labels:
        app: webapp
    spec:
      containers:
      - name: webapp
        image: nanajanashia/k8s-demo-app:v1.0
        ports:
        - containerPort: 3000
        env:
        - name: USER_NAME
          valueFrom:
            secretKeyRef:
              name: mongo-secret
              key: mongo-user
        - name: USER_PWD
          valueFrom:
            secretKeyRef:
              name: mongo-secret
              key: mongo-password 
        - name: DB_URL
          valueFrom:
            configMapKeyRef:
              name: mongo-config
              key: mongo-url
---
apiVersion: v1
kind: Service
metadata:
  name: webapp-service
spec:
  type: NodePort
  selector:
    app: webapp
  ports:
    - protocol: TCP
      port: 3000
      targetPort: 3000
      nodePort: 30100
```



![image-20220126162658577](https://tva1.sinaimg.cn/large/008i3skNly1gyr5klv6edj31kq0u042k.jpg)



![image-20220126162813814](https://tva1.sinaimg.cn/large/008i3skNly1gyr5lvrfu0j31q90u0wic.jpg)



![image-20220126162914271](https://tva1.sinaimg.cn/large/008i3skNly1gyr5mxml2yj31oy0u0q82.jpg)



![image-20220126163053994](https://tva1.sinaimg.cn/large/008i3skNly1gyr5onpw49j31m00u00x8.jpg)



![image-20220126163348623](https://tva1.sinaimg.cn/large/008i3skNly1gyr5rovcsjj31ye0rwwlk.jpg)



![image-20220126163513894](https://tva1.sinaimg.cn/large/008i3skNly1gyr5t67i0sj31sh0u0qaa.jpg)



![image-20220126163531520](https://tva1.sinaimg.cn/large/008i3skNly1gyr5thacurj31i00u0418.jpg)



![image-20220126163621879](/Users/chengkun02/Library/Application%20Support/typora-user-images/image-20220126163621879.png)