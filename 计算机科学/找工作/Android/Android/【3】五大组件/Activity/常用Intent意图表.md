

```java
(1).调用拨号程序
Uri uri = Uri.parse("tel:10086"); 
Intent intent = new Intent(Intent.ACTION_DIAL, uri); 
startActivity(intent); 
(2).发送短信或者彩信
//发生短信
Uri uri = Uri.parse("smsto:10086"); 
Intent intent = new Intent(Intent.ACTION_SENDTO, uri); 
intent.putExtra("sms_body", "Hello"); 
startActivity(intent); 
//发送彩信，相当于发送带附件的短信
Intent intent = new Intent(Intent.ACTION_SEND); 
intent.putExtra("sms_body", "Hello"); 
Uri uri = Uri.parse("content://media/external/images/media/23"); 
intent.putExtra(Intent.EXTRA_STREAM, uri); 
intent.setType("image/png"); 
startActivity(intent); 
(3).通过浏览器打开网页
Uri uri = Uri.parse("http://www.google.com"); 
Intent intent  = new Intent(Intent.ACTION_VIEW, uri); 
startActivity(intent);
(4).发送电子邮件
Uri uri = Uri.parse("mailto:someone@domain.com"); 
Intent intent = new Intent(Intent.ACTION_SENDTO, uri); 
startActivity(intent); 
//给someone@domain.com发邮件发送内容为“Hello”的邮件 
Intent intent = new Intent(Intent.ACTION_SEND); 
intent.putExtra(Intent.EXTRA_EMAIL, "someone@domain.com"); 
intent.putExtra(Intent.EXTRA_SUBJECT, "Subject"); 
intent.putExtra(Intent.EXTRA_TEXT, "Hello"); 
intent.setType("text/plain"); 
startActivity(intent); 
// 给多人发邮件 
Intent intent=new Intent(Intent.ACTION_SEND); 
String[] tos = {"1@abc.com", "2@abc.com"}; // 收件人 
String[] ccs = {"3@abc.com", "4@abc.com"}; // 抄送 
String[] bccs = {"5@abc.com", "6@abc.com"}; // 密送 
intent.putExtra(Intent.EXTRA_EMAIL, tos); 
intent.putExtra(Intent.EXTRA_CC, ccs); 
intent.putExtra(Intent.EXTRA_BCC, bccs); 
intent.putExtra(Intent.EXTRA_SUBJECT, "Subject"); 
intent.putExtra(Intent.EXTRA_TEXT, "Hello"); 
intent.setType("message/rfc822"); 
startActivity(intent); 
(5).显示地图与路径规划
// 打开Google地图中国北京位置（北纬39.9，东经116.3） 
Uri uri = Uri.parse("geo:39.9,116.3"); 
Intent intent = new Intent(Intent.ACTION_VIEW, uri); 
startActivity(intent); 
// 路径规划：从北京某地（北纬39.9，东经116.3）到上海某地（北纬31.2，东经121.4） 
Uri uri = Uri.parse("http://maps.google.com/maps?f=d&saddr=39.9 116.3&daddr=31.2 121.4"); 
Intent intent = new Intent(Intent.ACTION_VIEW, uri); 
startActivity(intent); 
(6).播放多媒体
Intent intent = new Intent(Intent.ACTION_VIEW); 
Uri uri = Uri.parse("file:///sdcard/foo.mp3"); 
intent.setDataAndType(uri, "audio/mp3"); 
startActivity(intent); 
Uri uri = Uri.withAppendedPath(MediaStore.Audio.Media.INTERNAL_CONTENT_URI, "1"); 
Intent intent = new Intent(Intent.ACTION_VIEW, uri); 
startActivity(intent); 
(7).拍照
// 打开拍照程序 
Intent intent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);  
startActivityForResult(intent, 0); 
// 取出照片数据 
Bundle extras = intent.getExtras();  
Bitmap bitmap = (Bitmap) extras.get("data"); 
(8).获取并剪切图片
// 获取并剪切图片 
Intent intent = new Intent(Intent.ACTION_GET_CONTENT); 
intent.setType("image/*"); 
intent.putExtra("crop", "true"); // 开启剪切 
intent.putExtra("aspectX", 1); // 剪切的宽高比为1：2 
intent.putExtra("aspectY", 2); 
intent.putExtra("outputX", 20); // 保存图片的宽和高 
intent.putExtra("outputY", 40);  
intent.putExtra("output", Uri.fromFile(new File("/mnt/sdcard/temp"))); // 保存路径 
intent.putExtra("outputFormat", "JPEG");// 返回格式 
startActivityForResult(intent, 0); 
// 剪切特定图片 
Intent intent = new Intent("com.android.camera.action.CROP");  
intent.setClassName("com.android.camera", "com.android.camera.CropImage");  
intent.setData(Uri.fromFile(new File("/mnt/sdcard/temp")));  
intent.putExtra("outputX", 1); // 剪切的宽高比为1：2 
intent.putExtra("outputY", 2); 
intent.putExtra("aspectX", 20); // 保存图片的宽和高 
intent.putExtra("aspectY", 40); 
intent.putExtra("scale", true); 
intent.putExtra("noFaceDetection", true);  
intent.putExtra("output", Uri.parse("file:///mnt/sdcard/temp"));  
startActivityForResult(intent, 0); 
(9).打开Google Market
// 打开Google Market直接进入该程序的详细页面 
Uri uri = Uri.parse("market://details?id=" + "com.demo.app"); 
Intent intent = new Intent(Intent.ACTION_VIEW, uri); 
startActivity(intent); 
(10).安装和卸载程序
Uri uri = Uri.fromParts("package", "com.demo.app", null);   
Intent intent = new Intent(Intent.ACTION_DELETE, uri);   
startActivity(intent); 
(11).进入设置界面
// 进入无线网络设置界面（其它可以举一反三）   
Intent intent = new Intent(android.provider.Settings.ACTION_WIRELESS_SETTINGS);
startActivityForResult(intent, 0); 
```

