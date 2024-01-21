

自定义Convert......就是预处理一下返回的Json信息，但是结合rxjava后还有另一种更优秀的解决方案，所以自定义Convert进行数据加密。。也许可以。

通常从服务端拿到的JSON数据格式大概如下:

```java
  {
    "code":1,
    "message":"查询成功",
    "deta":{"aa":"123","bb":"123","cc":"123"}
  }
```

因此通常我们会定义一个实体类来解析对应的json:

```java
public class ApiResponse<T> {

    /**
     * 状态码
     */
    @SerializedName("code")
    private int code;

    /**
     * 返回信息
     */
    @SerializedName("message")
    private String msg;

    /**
     * 返回具体数据
     */
    @SerializedName("data")
    private T data;

	//省略getter和setter方法...
}

```

其中的code字段表示状态,比如以下值可能代表了不同的含义

- code = 1, 表示成功, 不等于1代表错误
- code = -101, 表示token过期
- code = -102, 表示手机号码已经注册
- 等等等

GsonConverter怎么写的, 由三个类组成:

- GsonConverterFactory    // GsonConverter 工厂类, 用来创建GsonConverter
- GsonResponseBodyConverter  //  处理ResponseBody
- GsonRequestBodyConverter    //   处理RequestBody

下面看一下子这三个都咋写：

- GsonRequestBodyConverter

```java
final class GsonRequestBodyConverter<T> implements Converter<T, RequestBody>{


    /** 设置Content-Type数据格式 */
    private static final MediaType MEDIA_TYPE = MediaType.parse("application/json; charset=UTF-8");
    /** 定义 Charset的默认Unicode编码是UTF-8 */
    private static final Charset UTF_8 = Charset.forName("UTF-8");

    private final Gson mGson;
    private final TypeAdapter<T> mAdapter;

    
    public GsonRequestBodyConverter(Gson gson, TypeAdapter<T> adapter) {
        mGson = gson;
        mAdapter = adapter;
    }

    /**
     * 将泛型转化成RequestBody
     *
     * @param value
     * @return
     * @throws IOException
     */
    @Override
    public RequestBody convert(T value) throws IOException {

        Buffer buffer = new Buffer();
        Writer writer = new OutputStreamWriter(buffer.outputStream(),UTF_8);
        JsonWriter jsonWriter = mGson.newJsonWriter(writer);
        mAdapter.write(jsonWriter,value);
        jsonWriter.close();
        return RequestBody.create(MEDIA_TYPE,buffer.readByteString());
    }
}
```

- GsonResponseBodyConverter

```java
public class GsonResponseBodyConverter<T> implements Converter<ResponseBody, T> {

    private final Gson mGson;
    private final Type mType;

    public GsonResponseBodyConverter(Gson gson, Type type) {
        this.mGson = gson;
        this.mType = type;
    }

    @Override
    public T convert(ResponseBody value) throws IOException {
        //将返回的json数据储存在String类型的response中
        String response = value.string();
        LogUtilPlus.json(response);
        //将外层的数据解析到ApiResponse中
        ApiResponse apiResponse = mGson.fromJson(response, ApiResponse.class);

        //遍历正确码
        //正确则解析，不正确则导致此次请求失败排除异常，执行Failure方法。
        for (int code : Config.NET_CORRECT_CODE) {
            if (apiResponse.getCode() == code) {
                //直接解析，正确请求不会导致json解析异常
                return mGson.fromJson(response, mType);
            }
        }
        //通过抛出自定义异常传递错误码及错误信息
        throw (ApiException) mGson.fromJson(response, ApiException.class);
    }
}
```

- ResponseConverterFactory也就是上面说的GsonConverterFactory

```java
/**
*json解析转换器
*/

public class ResponseConverterFactory extends Converter.Factory{


    private final Gson gson;
    private static final String NULL_GSON = "gson == null";

    private ResponseConverterFactory(Gson gson) {

        if (gson == null) {
            throw new NullPointerException(NULL_GSON);
        }
        this.gson = gson;
    }

    /**
     * 单例
     * @return
     */
    public static ResponseConverterFactory create() {
        return create(new Gson());
    }

    public static ResponseConverterFactory create(Gson gson) {
        return new ResponseConverterFactory(gson);
    }


    @Nullable
    @Override
    public Converter<ResponseBody, ?> responseBodyConverter(Type type, Annotation[] annotations, Retrofit retrofit) {
        return new GsonResponseBodyConverter<>(gson, type);
    }

    @Nullable
    @Override
    public Converter<?, RequestBody> requestBodyConverter(Type type, Annotation[] parameterAnnotations, Annotation[]
            methodAnnotations, Retrofit retrofit) {
        TypeAdapter<?> adapter = gson.getAdapter(TypeToken.get(type));
        return new GsonRequestBodyConverter<>(gson, adapter);
    }
}
```

