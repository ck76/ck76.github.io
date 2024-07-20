



<img src="https://p.ipic.vip/qwcelg.png" alt="9ceac8ee5f84190bd55cf8991579def3" style="zoom:50%;" />

这个图展示了Haskell中常见的类型类（type classes）的继承关系。我们可以从图中了解哪些类型类继承了其他类型类，从而了解每个类型类的功能和用途。以下是对这些类型类及其关系的详细解释：

1. **Eq**: 定义相等性检查的类型类。实现了 `==` 和 `/=` 操作符。
   - `Eq` 的所有类型都可以检查相等性。

2. **Ord**: 定义顺序关系的类型类。实现了 `<`, `<=`, `>`, `>=` 等操作符。
   - `Ord` 类型类继承了 `Eq`，意味着所有可比较的类型也必须是可检查相等性的类型。

3. **Enum**: 定义可枚举类型的类型类。实现了 `succ`, `pred`, `toEnum`, `fromEnum` 等方法。
   - `Enum` 类型类继承了 `Ord`。

4. **Integral**: 定义整数类型的类型类。
   - `Integral` 类型类继承了 `Enum` 和 `Num`。

5. **Num**: 定义数值类型的类型类。实现了 `+`, `-`, `*`, `abs`, `signum`, `fromInteger` 等方法。
   - `Num` 类型类继承了 `Eq` 和 `Show`。

6. **Real**: 定义实数类型的类型类。提供将实数转换为有理数的方法 `toRational`。
   - `Real` 类型类继承了 `Num` 和 `Ord`。

7. **Fractional**: 定义分数类型的类型类。实现了 `/`, `recip`, `fromRational` 等方法。
   - `Fractional` 类型类继承了 `Num`。

8. **Floating**: 定义浮点数类型的类型类。实现了 `pi`, `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh` 等方法。
   - `Floating` 类型类继承了 `Fractional`。

9. **RealFrac**: 定义实数分数类型的类型类。提供了 `properFraction`, `truncate`, `round`, `ceiling`, `floor` 等方法。
   - `RealFrac` 类型类继承了 `Real` 和 `Fractional`。

10. **RealFloat**: 定义实数浮点类型的类型类。提供了 `floatRadix`, `floatDigits`, `floatRange`, `decodeFloat`, `encodeFloat`, `exponent`, `significand`, `scaleFloat`, `isNaN`, `isInfinite`, `isDenormalized`, `isNegativeZero`, `isIEEE` 等方法。
    - `RealFloat` 类型类继承了 `RealFrac` 和 `Floating`。

11. **Show**: 定义类型可以转换为字符串的类型类。实现了 `show` 方法。
    - `Show` 类型类独立存在。

12. **Read**: 定义类型可以从字符串解析的类型类。实现了 `read` 方法。
    - `Read` 类型类独立存在。

13. **Bounded**: 定义类型有上界和下界的类型类。提供了 `minBound` 和 `maxBound`。
    - `Bounded` 类型类独立存在。

14. **Monad** 和 **Functor**: 定义了函子和单子，分别提供了 `fmap` 和 `>>=`, `return` 等方法。
    - `Monad` 和 `Functor` 类型类在这个图中独立存在，未展示与其他类型类的继承关系。

这个类型类的继承图帮助我们理解每个类型类的功能及其与其他类型类的关系。当你为一个新的数据类型实现这些类型类时，图中展示的继承关系也有助于确定需要实现的具体方法和功能。