[toc]

### Go 编译器简介

`cmd/compile` 包含了构成 Go 编译器的主要软件包。编译器可以逻辑上分为四个阶段，以下是每个阶段的简要描述以及相关代码所在的软件包列表。

通常会听到"前端"和"后端"来描述编译器。粗略来说，前两阶段是前端，后两阶段是后端。还有一个术语 "中端"（middle-end），指的是大部分发生在第二阶段的工作。

需要注意的是，`go/*` 家族的软件包，例如 `go/parser` 和 `go/types`，大多数情况下并未被编译器使用。因为编译器最初是用 C 语言编写的，`go/*` 软件包是为了支持像 `gofmt` 和 `vet` 这样的工具而开发的。不过，随着时间的推移，编译器的内部 API 慢慢演变为更符合 `go/*` 用户习惯的形式。

需要澄清的是，“gc” 代表的是“Go 编译器”（Go Compiler），与大写的“GC”（Garbage Collection，垃圾回收）没有关系。

#### 1. 解析

* `cmd/compile/internal/syntax`（词法分析器，解析器，语法树）

在编译的第一个阶段，源码被标记化（词法分析），解析（语法分析），并为每个源文件构造语法树。

每个语法树是相应源文件的精确表示，节点对应于源代码中的各种元素，如表达式、声明和语句。语法树还包括位置信息，用于错误报告和生成调试信息。

#### 2. 类型检查

* `cmd/compile/internal/types2`（类型检查）

`types2` 包是 `go/types` 的移植版本，使用的是 `syntax` 包中的 AST（抽象语法树）而非 `go/ast`。

#### 3. IR 构建（“noding”）

* `cmd/compile/internal/types`（编译器类型）
* `cmd/compile/internal/ir`（编译器 AST）
* `cmd/compile/internal/noder`（创建编译器 AST）

编译器的中端使用它自己的 AST 定义和 Go 类型表示法，这源自它用 C 语言编写的历史遗留代码。所有的代码都是基于这些来写的，所以在类型检查之后的下一步是将 `syntax` 和 `types2` 的表示转换为 `ir` 和 `types`。这个过程称为“noding”。

#### 4. 中端

* `cmd/compile/internal/inline`（函数调用内联）
* `cmd/compile/internal/devirtualize`（已知接口方法调用的去虚拟化）
* `cmd/compile/internal/escape`（逃逸分析）

在 IR 表示上执行了一些优化传递：死代码消除、去虚拟化、函数调用内联和逃逸分析。

#### 5. Walk（走查）

* `cmd/compile/internal/walk`（求值顺序，语法糖处理）

IR 表示上的最终传递是“walk”，它有两个主要目的：
1. 分解复杂语句为简单语句，引入临时变量并尊重求值顺序。
2. 将 Go 高级结构转换为更简单的结构。例如，将 `switch` 语句转换为二进制搜索或跳转表。

#### 6. 泛 SSA

* `cmd/compile/internal/ssa`（SSA 传递和规则）
* `cmd/compile/internal/ssagen`（将 IR 转换为 SSA）

在此阶段，IR 被转换为静态单赋值形式（SSA），这是一种更底层的中间表示，使得实现优化和生成机器代码变得更容易。

#### 7. 生成机器码

* `cmd/compile/internal/ssa`（SSA 降级和特定架构的传递）
* `cmd/internal/obj`（生成机器码）

编译器的机器相关阶段从“降级”传递开始，它将通用值转换为机器特定的变体。

#### 8. 导出

编译器除了为链接器写入目标代码文件外，还会为下游编译单元写入“导出数据”文件。





### -----------------

### Go 编译器简介

`cmd/compile` 包含了构成 Go 编译器的主要软件包。编译器可以逻辑上分为四个阶段，我们将在这里简要描述这些阶段以及包含相应代码的软件包列表。

你可能会听到关于编译器的 "前端" 和 "后端" 术语。粗略来说，这分别对应于我们将要列出的前两个阶段和后两个阶段。第三个术语 "中端"（middle-end）通常指的是第二阶段中的大部分工作。

需要注意的是，`go/*` 家族的软件包，如 `go/parser` 和 `go/types`，大多未被编译器使用。因为编译器最初是用 C 语言编写的，`go/*` 软件包是为编写处理 Go 代码的工具（如 `gofmt` 和 `vet`）开发的。然而，随着时间的推移，编译器的内部 API 逐渐演变成更符合 `go/*` 用户习惯的形式。

需要澄清的是，名称 "gc" 代表 "Go 编译器"（Go compiler），与大写的 "GC"（Garbage Collection，即垃圾回收）无关。

### 1. 解析

* `cmd/compile/internal/syntax`（词法分析器、解析器、语法树）

在编译的第一个阶段，源代码被标记化（词法分析），解析（语法分析），并为每个源文件构建一个语法树。

每个语法树都是相应源文件的精确表示，节点对应于源代码中的各种元素，例如表达式、声明和语句。语法树还包含位置信息，用于错误报告和生成调试信息。

### 2. 类型检查

* `cmd/compile/internal/types2`（类型检查）

`types2` 包是 `go/types` 的移植版本，使用的是 `syntax` 包的 AST（抽象语法树）而不是 `go/ast`。

### 3. IR 构建（"noding"）

* `cmd/compile/internal/types`（编译器类型）
* `cmd/compile/internal/ir`（编译器 AST）
* `cmd/compile/internal/noder`（创建编译器 AST）

编译器的中端使用它自己的 AST 定义和 Go 类型表示法，这源自编译器最初使用 C 语言编写的历史遗留代码。所有代码都基于这些内容编写，因此在类型检查之后的下一步是将 `syntax` 和 `types2` 的表示转换为 `ir` 和 `types`。这个过程称为 "noding"。

Noding 使用一个称为统一 IR（Unified IR）的过程，它通过序列化类型检查代码来构建节点表示。统一 IR 还用于包的导入/导出以及内联处理。

### 4. 中端

* `cmd/compile/internal/inline`（函数调用内联）
* `cmd/compile/internal/devirtualize`（已知接口方法调用的去虚拟化）
* `cmd/compile/internal/escape`（逃逸分析）

在 IR 表示上执行了多个优化步骤：死代码消除、去虚拟化、函数调用内联和逃逸分析。

早期的死代码消除与统一 IR 写入阶段集成在一起。

### 5. Walk 阶段

* `cmd/compile/internal/walk`（求值顺序处理，语法糖处理）

在 IR 表示上的最后一个步骤是 "walk"，该步骤有两个目的：

1. 分解复杂语句为更简单的单个语句，引入临时变量并确保求值顺序。这一步也被称为 "顺序"（order）。
2. 将 Go 的高级结构转化为更基础的结构。例如，将 `switch` 语句转换为二进制搜索或跳转表，并将对映射和通道的操作替换为运行时调用。

### 6. 泛 SSA

* `cmd/compile/internal/ssa`（SSA 传递和规则）
* `cmd/compile/internal/ssagen`（将 IR 转换为 SSA）

在这个阶段，IR 被转换为静态单赋值（SSA）形式，这是一种更底层的中间表示，其特定性质使得实现优化和最终生成机器代码更为容易。

在此转换过程中，编译器内建函数（intrinsics）也会被应用。这些是编译器通过特殊优化替换的函数。

在 AST 转换为 SSA 的过程中，一些节点会被降低为更简单的组件，以便其余部分的编译器能够处理。例如，`copy` 内置函数被替换为内存移动操作，而 `range` 循环则被重写为 `for` 循环。这些更改中的一些出于历史原因在转换为 SSA 之前就完成了，但长期计划是将它们全部移到这里。

然后，应用一系列与机器无关的优化步骤和规则。这些不涉及任何单一的计算机架构，因此在所有 `GOARCH` 变体上运行。这些步骤包括死代码消除、删除不必要的 nil 检查以及删除未使用的分支。通用的重写规则主要与表达式相关，例如用常量值替换一些表达式，并优化乘法和浮点运算。

### 7. 生成机器代码

* `cmd/compile/internal/ssa`（SSA 降级和架构特定传递）
* `cmd/internal/obj`（生成机器代码）

编译器的机器相关阶段从 "降级"（lower）传递开始，它将通用值重写为特定机器架构的变体。例如，在 amd64 架构上可以使用内存操作数，因此许多加载-存储操作可以合并。

需要注意的是，降级传递应用了所有架构特定的重写规则，因此当前也执行了许多优化。

一旦 SSA 已经被 "降级" 并且更加适合目标架构，就会运行最终的代码优化步骤。这包括再次进行死代码消除、将值移动到更靠近其使用的地方、删除从未读取的局部变量以及寄存器分配。

在 SSA 生成阶段结束时，Go 函数已经转换为一系列 `obj.Prog` 指令。这些指令传递给汇编器（`cmd/internal/obj`），汇编器将其转换为机器代码并写出最终的目标文件。目标文件还会包含反射数据、导出数据以及调试信息。



### 7a. 导出

除了为链接器编写目标代码文件外，编译器还会为下游编译单元写入“导出数据”文件。导出数据文件包含在编译包 P 时计算出的所有信息，这些信息在编译直接导入包 P 的包 Q 时可能会用到。它包括所有导出声明的类型信息、内联候选函数体的 IR、可以在其他包中实例化的泛型函数体的 IR 以及函数参数逃逸分析的结果摘要。

导出数据文件的格式经历了多次迭代。当前的形式称为“统一”（unified），它是对象图的序列化表示，并带有一个索引，允许对整个对象图的部分进行延迟解码（因为大多数导入只需提供少量符号）。

GOROOT 仓库包含统一格式的读写器，它可以从编译器的 IR 进行编码和解码。`golang.org/x/tools` 仓库还提供了一个公共 API，用于导出数据读取器（使用 `go/types` 表示），该读取器始终支持当前的编译器文件格式以及少量历史版本。（`x/tools/go/packages` 在需要类型信息但不需要带注释的语法时使用它。）

`x/tools` 仓库还提供了用于读取和写入导出类型信息的公共 API（但仅限此用途），使用的是旧的“索引”（indexed）格式。（例如，`gopls` 使用此版本作为其工作空间信息的数据库，其中包括类型。）

导出数据通常提供一个“深层”摘要，以便编译包 Q 时只需读取每个直接导入的导出数据文件，并确保这些文件提供了关于间接导入中声明的所有必要信息，如 P 的公共 API 中引用的类型的方法和结构体字段。深层导出数据对于构建系统来说更简单，因为每个直接依赖只需一个文件。然而，在大型代码库中，当沿着导入图上升时，它往往会变得越来越大：如果存在一组常用的大型 API 类型，几乎每个包的导出数据都会包含它们的副本。这个问题促使了“索引”设计，它允许按需部分加载。

（`gopls` 对每次导入做的工作比编译器少，因此对导出数据的开销更敏感。出于这个原因，它使用“浅层”导出数据，其中间接信息完全不记录。这需要对所有依赖项的导出数据文件进行随机访问，因此不适合分布式构建系统。）

### 8. 提示

#### 入门

* 如果你从未为编译器贡献过代码，一个简单的入门方法是添加日志语句或 `panic("here")`，以便对你正在调查的问题获得初步了解。

* 编译器本身提供了日志记录、调试和可视化功能，例如：

  ```bash
  $ go build -gcflags=-m=2                   # 打印优化信息，包括内联和逃逸分析
  $ go build -gcflags=-d=ssa/check_bce/debug # 打印边界检查信息
  $ go build -gcflags=-W                     # 打印类型检查后的内部语法树
  $ GOSSAFUNC=Foo go build                   # 为函数 Foo 生成 ssa.html 文件
  $ go build -gcflags=-S                     # 打印汇编代码
  $ go tool compile -bench=out.txt x.go      # 打印编译器阶段的时间
  ```

  一些标志会更改编译器行为，例如：

  ```bash
  $ go tool compile -h file.go               # 遇到第一个编译错误时触发 panic
  $ go build -gcflags=-d=checkptr=2          # 启用额外的 unsafe 指针检查
  ```

  还有更多其他标志。可以通过以下命令获取一些描述：

  ```bash
  $ go tool compile -h              # 编译器标志，例如 go build -gcflags='-m=1 -l'
  $ go tool compile -d help         # 调试标志，例如 go build -gcflags=-d=checkptr=2
  $ go tool compile -d ssa/help     # ssa 标志，例如 go build -gcflags=-d=ssa/prove/debug=2
  ```

  关于 `-gcflags` 和 `go build` 与 `go tool compile` 之间差异的其他详细信息，可以在下面的 [相关部分](#-gcflags-and-go-build-vs-go-tool-compile) 中找到。

* 一般来说，当你在编译器中调查一个问题时，通常需要从最简单的重现开始，并准确理解它是如何发生的。

#### 测试你的修改

* 请务必阅读 Go 贡献指南中的[快速测试你的修改](https://go.dev/doc/contribute#quick_test)部分。

* 一些测试位于 `cmd/compile` 包中，可以通过 `go test ./...` 或类似方式运行，但许多 `cmd/compile` 测试位于顶层 [test](https://github.com/golang/go/tree/master/test) 目录中：

  ```bash
  $ go test cmd/internal/testdir                           # 运行 'test' 目录中的所有测试
  $ go test cmd/internal/testdir -run='Test/escape.*.go'   # 测试 'test' 目录中的特定文件
  ```

  有关详细信息，请参阅 [testdir README](https://github.com/golang/go/tree/master/test#readme)。`testdir_test.go` 中的 `errorCheck` 方法（[链接](https://github.com/golang/go/blob/master/src/cmd/internal/testdir/testdir_test.go)）有助于理解许多测试中使用的 `ERROR` 注释的描述。

  此外，标准库中的 `go/types` 包和 `cmd/compile/internal/types2` 在 `src/internal/types/testdata` 中共享测试，如果这些地方发生任何更改，两个类型检查器都需要进行检查。

* 新的 [基于应用程序的覆盖率分析](https://go.dev/testing/coverage/) 也可以用于编译器，例如：

  ```bash
  $ go install -cover -coverpkg=cmd/compile/... cmd/compile  # 用覆盖率仪器构建编译器
  $ mkdir /tmp/coverdir                                      # 选择覆盖数据的位置
  $ GOCOVERDIR=/tmp/coverdir go test [...]                   # 使用编译器，保存覆盖率数据
  $ go tool covdata textfmt -i=/tmp/coverdir -o coverage.out # 转换为传统的覆盖率格式
  $ go tool cover -html coverage.out                         # 使用传统工具查看覆盖率
  ```

#### 切换编译器版本

* 许多编译器测试使用的是 PATH 中找到的 `go` 命令及其相应的 `compile` 二进制文件。

* 如果你在一个分支中，并且你的 PATH 包含 `<go-repo>/bin`，那么执行 `go install cmd/compile` 会使用你分支中的代码构建编译器，并将其安装到正确的位置，这样后续的 `go build` 或 `go test ./...` 命令将使用你刚刚构建的编译器。

* [toolstash](https://pkg.go.dev/golang.org/x/tools/cmd/toolstash) 提供了一种保存、运行和恢复已知良好的 Go 工具链的方式。例如，最好的一种做法是先构建你的分支，保存该工具链的版本，然后恢复已知的良好版本来编译你正在进行的编译器版本。

  示例设置步骤：

  ```bash
  $ go install golang.org/x/tools/cmd/toolstash@latest
  $ git clone https://go.googlesource.com/go
  $ cd go
  $ git checkout -b mybranch
  $ ./src/all.bash               # 构建并确认良好的起点
  $ export PATH=$PWD/bin:$PATH
  $ toolstash save               # 保存当前工具
  ```

  在此之后，你的编辑/编译/测试循环可能类似于：

  ```bash
  <... 修改 cmd/compile 源代码 ...>
  $ toolstash restore && go install cmd/compile   # 恢复已知的良好工具以构建编译器
  <... 使用 'go build'，'go test' 等命令 ...>   # 使用新编译器
  ```

* toolstash 还允许比较已安装的编译器和存储的编译器，例如，如果你希望在重构后行为等效。例如，检查更改后的编译器在构建标准库时是否生成与存储的编译器相同的目标文件：

  ```bash
  $ toolstash restore && go install cmd/compile   # 构建最新编译器
  $ go build -toolexec "toolstash -cmp" -a -v std # 比较最新编译器与已保存编译器
  ```

* 如果版本似乎不同步（例如，出现类似 `linked object header mismatch` 的错误，版本字符串为 `devel go1.21

-db3f952b1f`），你可能需要执行 `toolstash restore && go install cmd/...` 来更新 `cmd` 下的所有工具。

#### 其他有用的工具

* [compilebench](https://pkg.go.dev/golang.org/x/tools/cmd/compilebench) 可以对编译器的速度进行基准测试。

* [benchstat](https://pkg.go.dev/golang.org/x/perf/cmd/benchstat) 是报告由于编译器修改而导致性能变化的标准工具，包括是否有任何改进具有统计显著性：

  ```bash
  $ go test -bench=SomeBenchmarks -count=20 > new.txt   # 使用新编译器
  $ toolstash restore                                   # 恢复旧编译器
  $ go test -bench=SomeBenchmarks -count=20 > old.txt   # 使用旧编译器
  $ benchstat old.txt new.txt                           # 比较旧版与新版
  ```

* [bent](https://pkg.go.dev/golang.org/x/benchmarks/cmd/bent) 可用于在 Docker 容器中运行一组大型的 Go 项目基准测试。

* [perflock](https://github.com/aclements/perflock) 帮助在 Linux 上通过调整 CPU 频率缩放设置来获得更一致的基准测试结果。

* [view-annotated-file](https://github.com/loov/view-annotated-file)（社区工具）会将内联、边界检查和逃逸信息叠加回源代码上。

* [godbolt.org](https://go.godbolt.org) 是一个广泛用于查看和分享来自多个编译器的汇编输出的平台，包括 Go 编译器。它还可以[比较](https://go.godbolt.org/z/5Gs1G4bKG)不同版本函数或 Go 编译器版本之间的汇编代码，这在调查问题和生成 bug 报告时非常有用。

#### `-gcflags` 和 'go build' 与 'go tool compile'

* `-gcflags` 是 Go 命令的[构建标志](https://pkg.go.dev/cmd/go#hdr-Compile_packages_and_dependencies)。`go build -gcflags=<args>` 将传递给底层 `compile` 调用的 `<args>`，同时继续执行 `go build` 命令通常处理的所有事情（例如，处理构建缓存、模块等）。相比之下，`go tool compile <args>` 只请求 `go` 命令调用 `compile <args>` 一次，而不涉及标准 `go build` 的机制。在某些情况下，减少运行中的部分会更有帮助，例如当你有一个可以不依赖 `go build` 辅助编译的小的独立源文件时。在其他情况下，将 `-gcflags` 传递给构建命令（如 `go build`、`go test` 或 `go install`）会更方便。

* `-gcflags` 默认适用于命令行上指定的软件包，但也可以使用包模式，例如 `-gcflags='all=-m=1 -l'`，或使用多个包模式，例如 `-gcflags='all=-m=1' -gcflags='fmt=-m=2'`。详情见[cmd/go 文档](https://pkg.go.dev/cmd/go#hdr-Compile_packages_and_dependencies)。

### 进一步阅读

要深入了解 SSA 包的工作方式，包括其传递和规则，请参阅 [cmd/compile/internal/ssa/README.md](internal/ssa/README.md)。

最后，如果本 README 或 SSA README 中的某些内容不清楚，或者你有改进的想法，欢迎在 [issue 30074](https://go.dev/issue/30074) 留言。

### ------------------------

<!---
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
-->

## Introduction to the Go compiler

`cmd/compile` contains the main packages that form the Go compiler. The compiler
may be logically split in four phases, which we will briefly describe alongside
the list of packages that contain their code.

You may sometimes hear the terms "front-end" and "back-end" when referring to
the compiler. Roughly speaking, these translate to the first two and last two
phases we are going to list here. A third term, "middle-end", often refers to
much of the work that happens in the second phase.

Note that the `go/*` family of packages, such as `go/parser` and
`go/types`, are mostly unused by the compiler. Since the compiler was
initially written in C, the `go/*` packages were developed to enable
writing tools working with Go code, such as `gofmt` and `vet`.
However, over time the compiler's internal APIs have slowly evolved to
be more familiar to users of the `go/*` packages.

It should be clarified that the name "gc" stands for "Go compiler", and has
little to do with uppercase "GC", which stands for garbage collection.

### 1. Parsing

* `cmd/compile/internal/syntax` (lexer, parser, syntax tree)

In the first phase of compilation, source code is tokenized (lexical analysis),
parsed (syntax analysis), and a syntax tree is constructed for each source
file.

Each syntax tree is an exact representation of the respective source file, with
nodes corresponding to the various elements of the source such as expressions,
declarations, and statements. The syntax tree also includes position information
which is used for error reporting and the creation of debugging information.

### 2. Type checking

* `cmd/compile/internal/types2` (type checking)

The types2 package is a port of `go/types` to use the syntax package's
AST instead of `go/ast`.

### 3. IR construction ("noding")

* `cmd/compile/internal/types` (compiler types)
* `cmd/compile/internal/ir` (compiler AST)
* `cmd/compile/internal/noder` (create compiler AST)

The compiler middle end uses its own AST definition and representation of Go
types carried over from when it was written in C. All of its code is written in
terms of these, so the next step after type checking is to convert the syntax
and types2 representations to ir and types. This process is referred to as
"noding."

Noding using a process called Unified IR, which builds a node representation
using a serialized version of the typechecked code from step 2.
Unified IR is also involved in import/export of packages and inlining.

### 4. Middle end

* `cmd/compile/internal/inline` (function call inlining)
* `cmd/compile/internal/devirtualize` (devirtualization of known interface method calls)
* `cmd/compile/internal/escape` (escape analysis)

Several optimization passes are performed on the IR representation:
dead code elimination, (early) devirtualization, function call
inlining, and escape analysis.

The early dead code elimination pass is integrated into the unified IR writer phase.

### 5. Walk

* `cmd/compile/internal/walk` (order of evaluation, desugaring)

The final pass over the IR representation is "walk," which serves two purposes:

1. It decomposes complex statements into individual, simpler statements,
   introducing temporary variables and respecting order of evaluation. This step
   is also referred to as "order."

2. It desugars higher-level Go constructs into more primitive ones. For example,
   `switch` statements are turned into binary search or jump tables, and
   operations on maps and channels are replaced with runtime calls.

### 6. Generic SSA

* `cmd/compile/internal/ssa` (SSA passes and rules)
* `cmd/compile/internal/ssagen` (converting IR to SSA)

In this phase, IR is converted into Static Single Assignment (SSA) form, a
lower-level intermediate representation with specific properties that make it
easier to implement optimizations and to eventually generate machine code from
it.

During this conversion, function intrinsics are applied. These are special
functions that the compiler has been taught to replace with heavily optimized
code on a case-by-case basis.

Certain nodes are also lowered into simpler components during the AST to SSA
conversion, so that the rest of the compiler can work with them. For instance,
the copy builtin is replaced by memory moves, and range loops are rewritten into
for loops. Some of these currently happen before the conversion to SSA due to
historical reasons, but the long-term plan is to move all of them here.

Then, a series of machine-independent passes and rules are applied. These do not
concern any single computer architecture, and thus run on all `GOARCH` variants.
These passes include dead code elimination, removal of
unneeded nil checks, and removal of unused branches. The generic rewrite rules
mainly concern expressions, such as replacing some expressions with constant
values, and optimizing multiplications and float operations.

### 7. Generating machine code

* `cmd/compile/internal/ssa` (SSA lowering and arch-specific passes)
* `cmd/internal/obj` (machine code generation)

The machine-dependent phase of the compiler begins with the "lower" pass, which
rewrites generic values into their machine-specific variants. For example, on
amd64 memory operands are possible, so many load-store operations may be combined.

Note that the lower pass runs all machine-specific rewrite rules, and thus it
currently applies lots of optimizations too.

Once the SSA has been "lowered" and is more specific to the target architecture,
the final code optimization passes are run. This includes yet another dead code
elimination pass, moving values closer to their uses, the removal of local
variables that are never read from, and register allocation.

Other important pieces of work done as part of this step include stack frame
layout, which assigns stack offsets to local variables, and pointer liveness
analysis, which computes which on-stack pointers are live at each GC safe point.

At the end of the SSA generation phase, Go functions have been transformed into
a series of obj.Prog instructions. These are passed to the assembler
(`cmd/internal/obj`), which turns them into machine code and writes out the
final object file. The object file will also contain reflect data, export data,
and debugging information.

### 7a. Export

In addition to writing a file of object code for the linker, the
compiler also writes a file of "export data" for downstream
compilation units. The export data file holds all the information
computed during compilation of package P that may be needed when
compiling a package Q that directly imports P. It includes type
information for all exported declarations, IR for bodies of functions
that are candidates for inlining, IR for bodies of generic functions
that may be instantiated in another package, and a summary of the
findings of escape analysis on function parameters.

The format of the export data file has gone through a number of
iterations. Its current form is called "unified", and it is a
serialized representation of an object graph, with an index allowing
lazy decoding of parts of the whole (since most imports are used to
provide only a handful of symbols).

The GOROOT repository contains a reader and a writer for the unified
format; it encodes from/decodes to the compiler's IR.
The golang.org/x/tools repository also provides a public API for an export
data reader (using the go/types representation) that always supports the
compiler's current file format and a small number of historic versions.
(It is used by x/tools/go/packages in modes that require type information
but not type-annotated syntax.)

The x/tools repository also provides public APIs for reading and
writing exported type information (but nothing more) using the older
"indexed" format. (For example, gopls uses this version for its
database of workspace information, which includes types.)

Export data usually provides a "deep" summary, so that compilation of
package Q can read the export data files only for each direct import,
and be assured that these provide all necessary information about
declarations in indirect imports, such as the methods and struct
fields of types referred to in P's public API. Deep export data is
simpler for build systems, since only one file is needed per direct
dependency. However, it does have a tendency to grow as one gets
higher up the import graph of a big repository: if there is a set of
very commonly used types with a large API, nearly every package's
export data will include a copy. This problem motivated the "indexed"
design, which allowed partial loading on demand.
(gopls does less work than the compiler for each import and is thus
more sensitive to export data overheads. For this reason, it uses
"shallow" export data, in which indirect information is not recorded
at all. This demands random access to the export data files of all
dependencies, so is not suitable for distributed build systems.)


### 8. Tips

#### Getting Started

* If you have never contributed to the compiler before, a simple way to begin
  can be adding a log statement or `panic("here")` to get some
  initial insight into whatever you are investigating.

* The compiler itself provides logging, debugging and visualization capabilities,
  such as:
   ```
   $ go build -gcflags=-m=2                   # print optimization info, including inlining, escape analysis
   $ go build -gcflags=-d=ssa/check_bce/debug # print bounds check info
   $ go build -gcflags=-W                     # print internal parse tree after type checking
   $ GOSSAFUNC=Foo go build                   # generate ssa.html file for func Foo
   $ go build -gcflags=-S                     # print assembly
   $ go tool compile -bench=out.txt x.go      # print timing of compiler phases
   ```

  Some flags alter the compiler behavior, such as:
   ```
   $ go tool compile -h file.go               # panic on first compile error encountered
   $ go build -gcflags=-d=checkptr=2          # enable additional unsafe pointer checking
   ```

  There are many additional flags. Some descriptions are available via:
   ```
   $ go tool compile -h              # compiler flags, e.g., go build -gcflags='-m=1 -l'
   $ go tool compile -d help         # debug flags, e.g., go build -gcflags=-d=checkptr=2
   $ go tool compile -d ssa/help     # ssa flags, e.g., go build -gcflags=-d=ssa/prove/debug=2
   ```

  There are some additional details about `-gcflags` and the differences between `go build`
  vs. `go tool compile` in a [section below](#-gcflags-and-go-build-vs-go-tool-compile).

* In general, when investigating a problem in the compiler you usually want to
  start with the simplest possible reproduction and understand exactly what is
  happening with it.

#### Testing your changes

* Be sure to read the [Quickly testing your changes](https://go.dev/doc/contribute#quick_test)
  section of the Go Contribution Guide.

* Some tests live within the cmd/compile packages and can be run by `go test ./...` or similar,
  but many cmd/compile tests are in the top-level
  [test](https://github.com/golang/go/tree/master/test) directory:

  ```
  $ go test cmd/internal/testdir                           # all tests in 'test' dir
  $ go test cmd/internal/testdir -run='Test/escape.*.go'   # test specific files in 'test' dir
  ```
  For details, see the [testdir README](https://github.com/golang/go/tree/master/test#readme).
  The `errorCheck` method in [testdir_test.go](https://github.com/golang/go/blob/master/src/cmd/internal/testdir/testdir_test.go)
  is helpful for a description of the `ERROR` comments used in many of those tests.

  In addition, the `go/types` package from the standard library and `cmd/compile/internal/types2`
  have shared tests in `src/internal/types/testdata`, and both type checkers
  should be checked if anything changes there.

* The new [application-based coverage profiling](https://go.dev/testing/coverage/) can be used
  with the compiler, such as:

  ```
  $ go install -cover -coverpkg=cmd/compile/... cmd/compile  # build compiler with coverage instrumentation
  $ mkdir /tmp/coverdir                                      # pick location for coverage data
  $ GOCOVERDIR=/tmp/coverdir go test [...]                   # use compiler, saving coverage data
  $ go tool covdata textfmt -i=/tmp/coverdir -o coverage.out # convert to traditional coverage format
  $ go tool cover -html coverage.out                         # view coverage via traditional tools
  ```

#### Juggling compiler versions

* Many of the compiler tests use the version of the `go` command found in your PATH and
  its corresponding `compile` binary.

* If you are in a branch and your PATH includes `<go-repo>/bin`,
  doing `go install cmd/compile` will build the compiler using the code from your
  branch and install it to the proper location so that subsequent `go` commands
  like `go build` or `go test ./...` will exercise your freshly built compiler.

* [toolstash](https://pkg.go.dev/golang.org/x/tools/cmd/toolstash) provides a way
  to save, run, and restore a known good copy of the Go toolchain. For example, it can be
  a good practice to initially build your branch, save that version of
  the toolchain, then restore the known good version of the tools to compile
  your work-in-progress version of the compiler.

  Sample set up steps:
  ```
  $ go install golang.org/x/tools/cmd/toolstash@latest
  $ git clone https://go.googlesource.com/go
  $ cd go
  $ git checkout -b mybranch
  $ ./src/all.bash               # build and confirm good starting point
  $ export PATH=$PWD/bin:$PATH
  $ toolstash save               # save current tools
  ```
  After that, your edit/compile/test cycle can be similar to:
  ```
  <... make edits to cmd/compile source ...>
  $ toolstash restore && go install cmd/compile   # restore known good tools to build compiler
  <... 'go build', 'go test', etc. ...>           # use freshly built compiler
  ```

* toolstash also allows comparing the installed vs. stashed copy of
  the compiler, such as if you expect equivalent behavior after a refactor.
  For example, to check that your changed compiler produces identical object files to
  the stashed compiler while building the standard library:
  ```
  $ toolstash restore && go install cmd/compile   # build latest compiler
  $ go build -toolexec "toolstash -cmp" -a -v std # compare latest vs. saved compiler
  ```

* If versions appear to get out of sync (for example, with errors like
  `linked object header mismatch` with version strings like
  `devel go1.21-db3f952b1f`), you might need to do
  `toolstash restore && go install cmd/...` to update all the tools under cmd.

#### Additional helpful tools

* [compilebench](https://pkg.go.dev/golang.org/x/tools/cmd/compilebench) benchmarks
  the speed of the compiler.

* [benchstat](https://pkg.go.dev/golang.org/x/perf/cmd/benchstat) is the standard tool
  for reporting performance changes resulting from compiler modifications,
  including whether any improvements are statistically significant:
  ```
  $ go test -bench=SomeBenchmarks -count=20 > new.txt   # use new compiler
  $ toolstash restore                                   # restore old compiler
  $ go test -bench=SomeBenchmarks -count=20 > old.txt   # use old compiler
  $ benchstat old.txt new.txt                           # compare old vs. new
  ```

* [bent](https://pkg.go.dev/golang.org/x/benchmarks/cmd/bent) facilitates running a
  large set of benchmarks from various community Go projects inside a Docker container.

* [perflock](https://github.com/aclements/perflock) helps obtain more consistent
  benchmark results, including by manipulating CPU frequency scaling settings on Linux.

* [view-annotated-file](https://github.com/loov/view-annotated-file) (from the community)
   overlays inlining, bounds check, and escape info back onto the source code.

* [godbolt.org](https://go.godbolt.org) is widely used to examine
  and share assembly output from many compilers, including the Go compiler. It can also
  [compare](https://go.godbolt.org/z/5Gs1G4bKG) assembly for different versions of
  a function or across Go compiler versions, which can be helpful for investigations and
  bug reports.

#### -gcflags and 'go build' vs. 'go tool compile'

* `-gcflags` is a go command [build flag](https://pkg.go.dev/cmd/go#hdr-Compile_packages_and_dependencies).
  `go build -gcflags=<args>` passes the supplied `<args>` to the underlying
  `compile` invocation(s) while still doing everything that the `go build` command
  normally does (e.g., handling the build cache, modules, and so on). In contrast,
  `go tool compile <args>` asks the `go` command to invoke `compile <args>` a single time
  without involving the standard `go build` machinery. In some cases, it can be helpful to have
  fewer moving parts by doing `go tool compile <args>`, such as if you have a
  small standalone source file that can be compiled without any assistance from `go build`.
  In other cases, it is more convenient to pass `-gcflags` to a build command like
  `go build`, `go test`, or `go install`.

* `-gcflags` by default applies to the packages named on the command line, but can
  use package patterns such as `-gcflags='all=-m=1 -l'`, or multiple package patterns such as
  `-gcflags='all=-m=1' -gcflags='fmt=-m=2'`. For details, see the
  [cmd/go documentation](https://pkg.go.dev/cmd/go#hdr-Compile_packages_and_dependencies).

### Further reading

To dig deeper into how the SSA package works, including its passes and rules,
head to [cmd/compile/internal/ssa/README.md](internal/ssa/README.md).

Finally, if something in this README or the SSA README is unclear
or if you have an idea for an improvement, feel free to leave a comment in
[issue 30074](https://go.dev/issue/30074).
