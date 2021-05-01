1. 获取序列号：
   adb get-serialno
2. 查看连接计算机的设备：
   adb devices
3. 重启机器：
   adb reboot
4. 重启到bootloader，即刷机模式：
   adb reboot boot loader
5. 重启到recovery，即恢复模式：
   adb reboot recovery
6. 查看log：
   adb logcat (-s)
7. 终止adb服务进程：
   adb kill-server
8. 重启adb服务进程：
   adb start-server
9. 获取机器MAC地址：
   adb shell cat /sys/class/net/wlan0/address
10. 获取CPU序列号：
    adb shell cat /proc/cpuinfo
11. 安装APK：
    adb install \<apkfile>
12. 保留数据和缓存文件，重新安装apk：
    adb install -r \<apkfile>
13. 安装apk到sd卡：
    adb install -s \<apkfile>
14. 卸载APK：
    adb uninstall \<package>
15. 卸载app但保留数据和缓存文件：
    adb uninstall -k \<package> 
16. 启动应用：
    adb shell am start -n <package_name>/.<activity_class_name>
17. 查看设备cpu和内存占用情况：
    adb shell top
18. 查看占用内存前6的app：
    adb shell top -m 6
19. 刷新一次内存信息，然后返回：
    adb shell top -n 1
20. 查询各进程内存使用情况：
    adb shell pro crank
21. 杀死一个进程：
    adb shell kill [pid]
22. 查看进程列表：
    adb shell ps
23. 查看指定进程状态：
    adb shell ps -x [PID]
24. 查看后台services信息：
    adb shell service list
25. 查看当前内存占用：
    adb shell cat /proc/meminfo
26. 查看IO内存分区：
    adb shell cat /proc/iomem
27. 将system分区重新挂载为可读写分区：
    add remount
28. 从本地复制文件到设备：
    adb push \<local>\ <remote>
29. 从设备复制文件到本地：
    adb pull \<remote>\ <local>
30. 列出目录下的文件和文件夹，等同于dos中的dir命令：
    adb shell ls
31. 进入文件夹，等同于dos中的cd 命令：
    adb shell cd\ <folder>
32. 重命名文件：
    adb shell rename path/oldfilename path/newfilename
33. 删除system/avi.apk：
    adb shell rm /system/avi.apk
34. 删除文件夹及其下面所有文件：
    adb shell rm -r \<folder>
35. 移动文件：
    adb shell mv path/file newpath/file
36. 设置文件权限：
    adb shell chmod 777 /system/fonts/DroidSansFallback.ttf
37. 新建文件夹：
    adb shell mkdir path/foldelname
38. 查看文件内容：
    adb shell cat \<file>
39. 查看wifi密码：
    adb shell cat /data/misc/wifi/*.conf
40. 清除log缓存：
    adb logcat -c
41. 查看bug报告：
    adb bugreport
42. 获取设备名称：
    adb shell cat /system/build.prop
43. 查看ADB帮助：
    adb help
44. 运行monkey：
    adb shell monkey -v -p [your.package.name](http://your.package.name/) 500