



![img](https://p.ipic.vip/s4ea6e.png)

下面是对图片中八种λ立方体系统的详细解释。

λ立方体由八个系统组成，每个系统通过不同的参数组合定义依赖关系。每个系统在图中以两个或三个参数（$\star$, $\square$）表示：

1. **$\lambda \to$**: ($\star, \star$)
   - 只包含项到项的依赖关系。即，项依赖于项。

2. **$\lambda 2$**: ($\star, \star$), ($\square, \star$)
   - 包含项到项的依赖关系以及项到类型的依赖关系。即，项依赖于项，项也可以依赖于类型。

3. **$\lambda P$**: ($\star, \star$), ($\star, \square$)
   - 包含项到项的依赖关系以及类型到类型的依赖关系。即，项依赖于项，类型也可以依赖于类型。

4. **$\lambda \underline{\omega}$**: ($\star, \star$), ($\square, \square$)
   - 包含项到项的依赖关系以及类型到类型的依赖关系。即，项依赖于项，类型也可以依赖于类型。

5. **$\lambda P2$**: ($\star, \star$), ($\square, \star$), ($\star, \square$)
   - 包含项到项、项到类型以及类型到类型的依赖关系。即，项依赖于项，项可以依赖于类型，类型也可以依赖于类型。

6. **$\lambda P\omega$**: ($\star, \star$), ($\star, \square$), ($\square, \square$)
   - 包含项到项、项到类型以及类型到类型的依赖关系。即，项依赖于项，项可以依赖于类型，类型也可以依赖于类型。

7. **$\lambda \omega$**: ($\star, \star$), ($\square, \star$), ($\square, \square$)
   - 包含项到项、项到类型以及类型到类型的依赖关系。即，项依赖于项，项可以依赖于类型，类型也可以依赖于类型。

8. **$\lambda C$**: ($\star, \star$), ($\square, \star$), ($\star, \square$), ($\square, \square$)
   - 包含所有可能的依赖关系。即，项依赖于项，项可以依赖于类型，类型可以依赖于项，类型也可以依赖于类型。

### 详解各个系统及其变换

1. **$\lambda \to$**:
   - 这是最基本的系统，只有项依赖于项。所有项都具有类型，类型可以是简单类型或函数类型。

2. **$\lambda 2$**:
   - 增加了项到类型的依赖关系。这样，类型可以作为参数传递，并用于定义多态函数。

3. **$\lambda P$**:
   - 增加了类型到类型的依赖关系。这允许类型函数，它们可以生成新的类型。

4. **$\lambda \underline{\omega}$**:
   - 包括类型到类型的依赖关系，允许在更高层次上抽象和构造类型。

5. **$\lambda P2$**:
   - 包含了项到类型和类型到类型的依赖关系，结合了上述两个系统的特点，使得项和类型都可以具有更高的抽象能力。

6. **$\lambda P\omega$**:
   - 包含项到类型和类型到类型的依赖关系，允许类型作为参数传递，生成更高阶的类型。

7. **$\lambda \omega$**:
   - 包含项到类型和类型到类型的依赖关系，允许定义非常复杂的类型系统。

8. **$\lambda C$**:
   - 包含所有可能的依赖关系，是最通用和最强大的系统。

这些系统通过增加更多层次的依赖关系，使得类型系统变得越来越强大和复杂，从简单的项到项的依赖，逐步增加到项到类型、类型到项、类型到类型的依赖关系。

### 可能的路径

从 λ→ 到 λC，一共有 14 条路径。这里是每条路径的解释：

1. $\lambda \to \to \lambda 2$
   - 增加项到类型的依赖。
2. $\lambda \to \to \lambda P$
   - 增加类型到类型的依赖。
3. $\lambda 2 \to \lambda P2$
   - 在 $\lambda 2$ 的基础上增加类型到类型的依赖。
4. $\lambda 2 \to \lambda P\omega$
   - 在 $\lambda 2$ 的基础上增加类型到类型的依赖，形成更高阶的类型。
5. $\lambda P \to \lambda P2$
   - 在 $\lambda P$ 的基础上增加项到类型的依赖。
6. $\lambda P \to \lambda \underline{\omega}$
   - 在 $\lambda P$ 的基础上增加类型到类型的依赖，形成更高阶的类型。
7. $\lambda \underline{\omega} \to \lambda \omega$
   - 在 $\lambda \underline{\omega}$ 的基础上增加项到类型的依赖。
8. $\lambda \underline{\omega} \to \lambda P\omega$
   - 在 $\lambda \underline{\omega}$ 的基础上增加项到类型的依赖。
9. $\lambda \omega \to \lambda C$
   - 在 $\lambda \omega$ 的基础上增加类型到类型的依赖。
10. $\lambda P2 \to \lambda C$
    - 在 $\lambda P2$ 的基础上增加类型到类型的依赖。
11. $\lambda P2 \to \lambda \omega$
    - 在 $\lambda P2$ 的基础上增加类型到类型的依赖。
12. $\lambda P\omega \to \lambda C$
    - 在 $\lambda P\omega$ 的基础上增加类型到类型的依赖。
13. $\lambda P\omega \to \lambda \omega$
    - 在 $\lambda P\omega$ 的基础上增加类型到类型的依赖。
14. $\lambda \underline{\omega} \to \lambda C$
    - 在 $\lambda \underline{\omega}$ 的基础上增加项到类型和类型到类型的依赖，形成最复杂的系统。