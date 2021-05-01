https://www.jianshu.com/p/63ce92182dbf

# 源码编译安装

## 环境准备

官方要求具备的环境：

- `g++` 5.1 or later or `clang++` 3.5 or later
- `python` 2.7 (but not 3.x)
- GNU `make` 3.81 or later
- `cmake` 3.4.3 or later
- `curl`
- `git`
- `ssl` which comes in `libssl-dev` or `openssl-devel`（注意是dev版）
- `pkg-config` if you are compiling on Linux and targeting Linux

先甭管都是用来干啥的，装上就是：



```ruby
$ sudo apt install g++
$ sudo apt install python2.7
$ sudo apt install make
$ sudo apt install cmake
$ sudo apt install curl
$ sudo apt install git
$ sudo apt install libssl-dev
$ sudo apt install pkg-config
```

## 源码获取

本来可以直接`git clone https://github.com/rust-lang/rust.git`获取源码，但网速太慢，而且我们也不是要最新版，就自行从`https://static.rust-lang.org/dist/rustc-1.34.2-src.tar.gz`下载了。

放到`~/`下解压：



```shell
$ tar -zxvf rustc-1.34.2-src.tar.gz
$ mv rustc-1.34.2-src rust
$ cd rust
```

## 构建与安装

借用官方提供的`config.toml`：



```shell
$ cp config.toml.example config.toml
```

鉴于`/usr/local`的权限问题，在`config.toml`中把安装目录配置到我们的工作目录：



```toml
[install]
# Instead of installing to /usr/local, install to this path instead.
prefix = "~/rust"
```

构建并安装：



```shell
$ ./x.py build && ./x.py install
```

`build`会在`~\rust\build\x86_64-unknown-linux-gnu`目录下生成stage0～stage2的工具。`install`会将`rustc`等程序装入`~/rust/bin`，将库文件装入`~/rust/lib`。

`cargo`需要另行安装：



```shell
$ ./x.py install cargo
```

或者在一开始就修改`config.toml`配置，构建及安装所有工具：



```toml
# Enable a build of the extended rust tool set which is not only the compiler
# but also tools such as Cargo. This will also produce "combined installers"
# which are used to install Rust and Cargo together. This is disabled by
# default.
extended = true
```

## 遇到的坑

**- 1 -**

获取官方`crates`时可能太慢卡住了，像这样：



```shell
    Updating crates.io index
       Fetch [>                                                      ]   2.81%
```

改用镜像站，新建`~/.cargo/config`，贴入：



```toml
[source.crates-io]
replace-with = 'ustc'
[source.ustc]
registry = "https://mirrors.ustc.edu.cn/crates.io-index"
```

`https`可以换成`git`，还有一个阿里云的`https://code.aliyun.com/rustcc/crates.io-index.git`，但有点问题似乎暂时不能用。

**- 2 -**

下载`crates`期间遇到报错可能是网络不稳，重新运行会继续下。

**- 3 -**

编译Rust源码需要用到更早版本的Rust：



```shell
https://static.rust-lang.org/dist/2019-02-28/rust-std-1.33.0-x86_64-unknown-linux-gnu.tar.gz
https://static.rust-lang.org/dist/2019-02-28/rustc-1.33.0-x86_64-unknown-linux-gnu.tar.gz
https://static.rust-lang.org/dist/2019-02-28/cargo-0.34.0-x86_64-unknown-linux-gnu.tar.gz
```

网速太慢经常下载失败，可自行下载后放到`~/rust/build/cache/2019-02-28`。

**- 4 -**



```shell
error: failed to run custom build command for `compiler_builtins v0.1.23`
process didn't exit successfully: `/home/yizhi/rust/build/x86_64-unknown-linux-gnu/stage0-std/release/build/compiler_builtins-b25c1064dfc3ea33/build-script-build` (exit code: 101)
--- stdout
cargo:rerun-if-changed=build.rs
cargo:compiler-rt=/home/yizhi/.cargo/registry/src/mirrors.ustc.edu.cn-12df342d903acd47/compiler_builtins-0.1.23/compiler-rt
cargo:rustc-cfg=feature="unstable"

--- stderr
thread 'main' panicked at 'RUST_COMPILER_RT_ROOT is not set', /home/yizhi/.cargo/registry/src/mirrors.ustc.edu.cn-12df342d903acd47/compiler_builtins-0.1.23/build.rs:423:21
note: Run with `RUST_BACKTRACE=1` environment variable to display a backtrace.
```

添加环境变量



```shell
$ export RUST_COMPILER_RT_ROOT=~/rust/vendor/compiler_builtins/compiler-rt/
```

**- 5 -**



```shell
error[E0635]: unknown feature `compiler_builtins_lib`===============>  ] 35/36: std
   --> src/libstd/lib.rs:244:12
    |
244 | #![feature(compiler_builtins_lib)]
    |            ^^^^^^^^^^^^^^^^^^^^^

error: aborting due to previous error

For more information about this error, try `rustc --explain E0635`.
error: Could not compile `std`.
```

把`src/libstd/lib.rs`中的`#![feature(compiler_builtins_lib)]`注释掉。

**- 6 -**



```shell
error: cannot satisfy dependencies so `unicode_width` only shows up once31/149: rustc
  |
  = help: having upstream crates all available in one format will likely make this go away

error: aborting due to previous error

error: Could not compile `rustc`.
```

这个问题实在无解，更新了一下系统就过了，而且连前面的4和5都没遇到了。

**- 7 -**

安装`cargo`期间会遇到系统目录无权限的问题：



```shell
cp: cannot create regular file '/etc/bash_completion.d/cargo': Permission denied
chmod: cannot access '/etc/bash_completion.d/cargo': No such file or directory
install: error: file creation failed. see logs at '/home/yizhi/rust/lib/rustlib/install.log'
```

那是因为`config.toml`中`[install]`下的几个子路径并不都是默认相对路径，有些个尝试装到系统目录下了，改成相对路径即可。

# Rustup安装

Rustup是Rust的官方版本管理工具，是安装Rust的首选，一行命令搞定：



```shell
$ curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

如果网速慢，设置镜像站：



```shell
$ export RUSTUP_DIST_SERVER=https://mirrors.ustc.edu.cn/rust-static
$ export RUSTUP_UPDATE_ROOT=https://mirrors.ustc.edu.cn/rust-static/rustup
```

如果想安装指定的版本，使用`toolchain`子命令：



```shell
$ rustup toolchain install stable-x86_64-unknown-linux-gnu stable-2019-05-14 1.34.2
```

会逐一下载各工具，其中体积比较大的经常会下载失败，又需要手动下载，比如：



```shell
https://static.rust-lang.org/dist/2019-05-14/rust-docs-1.34.2-x86_64-unknown-linux-gnu.tar.xz
https://static.rust-lang.org/dist/2019-05-14/rust-std-1.34.2-x86_64-unknown-linux-gnu.tar.xz
https://static.rust-lang.org/dist/2019-05-14/rustc-1.34.2-x86_64-unknown-linux-gnu.tar.xz
https://static.rust-lang.org/dist/2020-03-12/rust-std-1.42.0-x86_64-unknown-linux-gnu.tar.xz
https://static.rust-lang.org/dist/2020-03-12/rustc-1.42.0-x86_64-unknown-linux-gnu.tar.xz
```

然后放到`~/.rustup/downloads/`下，重命名成对应的SHA256值（用`sha256sum`命令查看）。
 

 2020年3月23日 无锡

------



# 离线安装（没有网络怎么办？）

事先下载好：

- `cmake`等环境工具
- `rustc-1.34.2-src.tar.gz`
- `rust-std-1.33.0-x86_64-unknown-linux-gnu.tar.gz`
- `rustc-1.33.0-x86_64-unknown-linux-gnu.tar.gz`
- `cargo-0.34.0-x86_64-unknown-linux-gnu.tar.gz`

按上文步骤操作。

由于无法连接`crates`站点，利用源码`vendor`目录中现成的`crates`，在`config.toml`中做如下配置：



```toml
[build]
# Indicate whether the vendored sources are used for Rust dependencies or not
vendor = true
```



 2020年3月27日 无锡

------



# 从源码构建交叉编译器

> 在前文的离线安装基础上，构建Rust交叉编译器。
>
> - host：x86_64-unknown-linux-gnu
> - target：aarch64-unknown-linux-gnu

## gcc交叉编译环境

首先，我们需要`gcc`交叉编译工具`aarch64-linux-gnu-gcc`。在[这里](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.archlinux.org%2Fpackages%2Fcommunity%2Fx86_64%2Faarch64-linux-gnu-gcc%2F)下载如下几个包：

- `aarch64-linux-gnu-gcc-9.3.0-1-x86_64.pkg.tar.zst`
- `aarch64-linux-gnu-glibc-2.31-1-any.pkg.tar.zst`
- `aarch64-linux-gnu-binutils-2.34-1-x86_64.pkg.tar.zst`
- `aarch64-linux-gnu-linux-api-headers-5.5-1-any.pkg.tar.zst`

当然还有它们所依赖的`mpfr`、`glibc`等基础包，根据自己的环境按需下载。

以`aarch64-linux-gnu-gcc-9.3.0-1-x86_64.pkg.tar.zst`为例，把上述包逐一安顿到一个文件夹里：



```shell
$ mkdir aarch64-linux-gnu-gcc
$ zstd -d aarch64-linux-gnu-gcc-9.3.0-1-x86_64.pkg.tar.zst
$ tar -xvf aarch64-linux-gnu-gcc-9.3.0-1-x86_64.pkg.tar --directory=aarch64-linux-gnu-gcc
```

将该文件夹添加至环境变量：



```shell
$ export PATH=$HOME/aarch64-linux-gnu-gcc/usr/bin:$PATH
```

## 构建Rust交叉编译器

生成交叉编译的`config.toml`和`Makefile`：



```shell
$ cd rust
$ ./configure --target=aarch64-unknown-linux-gnu
```

构建：



```shell
$ make -j$(nproc)
```

此举会在`build/x86_64-unknown-linux-gnu/stage2/lib/rustlib`目录下生成target库，将其拷贝至`lib/rustlib`即可：



```shell
$ cp -r build/x86_64-unknown-linux-gnu/stage2/lib/rustlib/aarch64-unknown-linux-gnu lib/rustlib
```

## 使用

让我们写一个简单的测试程序`hello.rs`，对其交叉编译：



```shell
$ ~/rust/bin/rustc --target=aarch64-unknown-linux-gnu -C linker=aarch64-linux-gnu-gcc hello.rs
```

也可以对Rust源码中自带的测试集进行交叉编译测试：



```shell
$ cd ~/rust
$ ./x.py test --target=aarch64-unknown-linux-gnu
```

不过程序在本地也跑不起来，这么用的意义似乎也不大，目前主要靠自己写脚本批量编译后`scp`到目标平台。





作者：逸之
链接：https://www.jianshu.com/p/63ce92182dbf
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

# Rust 各版源码及工具链下载地址

|  版本  |                             源码                             |                            工具链                            |
| :----: | :----------------------------------------------------------: | :----------------------------------------------------------: |
| 1.34.2 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.34.2-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.34.2-src.tar.xz) | [2019-05-14](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2019-05-14) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2019-05-14%2Frust-1.34.2-x86_64-unknown-linux-gnu.tar.xz) |
| 1.35.0 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.35.0-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.35.0-src.tar.xz) | [2019-05-23](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2019-05-23) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2019-05-23%2Frust-1.35.0-x86_64-unknown-linux-gnu.tar.xz) |
| 1.36.0 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.36.0-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.36.0-src.tar.xz) | [2019-07-04](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2019-07-04) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2019-07-04%2Frust-1.36.0-x86_64-unknown-linux-gnu.tar.xz) |
| 1.37.0 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.37.0-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.37.0-src.tar.xz) | [2019-08-15](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2019-08-15) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2019-08-15%2Frust-1.37.0-x86_64-unknown-linux-gnu.tar.xz) |
| 1.38.0 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.38.0-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.38.0-src.tar.xz) | [2019-09-26](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2019-09-26) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2019-09-26%2Frust-1.38.0-x86_64-unknown-linux-gnu.tar.xz) |
| 1.39.0 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.39.0-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.39.0-src.tar.xz) | [2019-11-07](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2019-11-07) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2019-11-07%2Frust-1.39.0-x86_64-unknown-linux-gnu.tar.xz) |
| 1.40.0 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.40.0-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.40.0-src.tar.xz) | [2019-12-19](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2019-12-19) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2019-12-19%2Frust-1.40.0-x86_64-unknown-linux-gnu.tar.xz) |
| 1.41.0 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.41.0-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.41.0-src.tar.xz) | [2020-01-30](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-01-30) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-01-30%2Frust-1.41.0-x86_64-unknown-linux-gnu.tar.xz) |
| 1.41.1 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.41.1-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.41.1-src.tar.xz) | [2020-02-27](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-02-27) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-02-27%2Frust-1.41.1-x86_64-unknown-linux-gnu.tar.xz) |
| 1.42.0 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.42.0-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.42.0-src.tar.xz) | [2020-03-12](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-03-12) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-03-12%2Frust-1.42.0-x86_64-unknown-linux-gnu.tar.xz) |
| 1.43.0 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.43.0-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.43.0-src.tar.xz) | [2020-04-23](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-04-23) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-04-23%2Frust-1.43.0-x86_64-unknown-linux-gnu.tar.xz) |
| 1.43.1 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.43.1-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.43.1-src.tar.xz) | [2020-05-07](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-05-07) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-05-07%2Frust-1.43.1-x86_64-unknown-linux-gnu.tar.xz) |
| 1.44.0 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.44.0-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.44.0-src.tar.xz) | [2020-06-04](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-06-04) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-06-04%2Frust-1.44.0-x86_64-unknown-linux-gnu.tar.xz) |
| 1.44.1 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.44.1-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.44.1-src.tar.xz) | [2020-06-18](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-06-18) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-06-18%2Frust-1.44.1-x86_64-unknown-linux-gnu.tar.xz) |
| 1.45.0 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.45.0-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.45.0-src.tar.xz) | [2020-07-16](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-07-16) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-07-16%2Frust-1.45.0-x86_64-unknown-linux-gnu.tar.xz) |
| 1.45.1 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.45.1-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.45.1-src.tar.xz) | [2020-07-30](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-07-30) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-07-30%2Frust-1.45.1-x86_64-unknown-linux-gnu.tar.xz) |
| 1.45.2 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.45.2-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.45.2-src.tar.xz) | [2020-08-03](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-08-03) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-08-03%2Frust-1.45.2-x86_64-unknown-linux-gnu.tar.xz) |
| 1.46.0 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.46.0-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.46.0-src.tar.xz) | [2020-08-27](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-08-27) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-08-27%2Frust-1.46.0-x86_64-unknown-linux-gnu.tar.xz) |
| 1.47.0 | [gz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.47.0-src.tar.gz) / [xz](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2Frustc-1.47.0-src.tar.xz) | [2020-10-08](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-10-08) / [x86](https://links.jianshu.com/go?to=https%3A%2F%2Fstatic.rust-lang.org%2Fdist%2F2020-10-08%2Frust-1.47.0-x86_64-unknown-linux-gnu.tar.xz) |

