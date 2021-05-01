[TOC]

# Ruby 代码风格指南

这是 Airbnb 的 Ruby 代码风格指南

指南灵感来自于 [Github 的指南](https://web.archive.org/web/20160410033955/https://github.com/styleguide/ruby) 和 [Bozhidar Batsov 的指南](https://github.com/bbatsov/ruby-style-guide)

Airbnb 也在维护 [JavaScript 风格指南](https://github.com/airbnb/javascript)

## 内容表 (Table of Contents)

1. [空格 (Whitespace)](https://github.com/1c7/ruby-airbnb#空格-whitespace) 1. [缩进 (Indentation)](https://github.com/1c7/ruby-airbnb#缩进-indentation) 1. [行内 (Inline)](https://github.com/1c7/ruby-airbnb#行内-inline) 1. [换行 (Newlines)](https://github.com/1c7/ruby-airbnb#换行-newlines)
2. [行宽 (Line Length)](https://github.com/1c7/ruby-airbnb#行宽-line-length)
3. [注释 (Commenting)](https://github.com/1c7/ruby-airbnb#注释-commenting) 1. [文件级/类级 注释 (File/class-level comments)](https://github.com/1c7/ruby-airbnb#文件类-级别的注释-fileclass-level-comments) 1. [函数注释 (Function comments)](https://github.com/1c7/ruby-airbnb#函数注释-function-comments) 1. [块级和行内注释 (Block and inline comments)](https://github.com/1c7/ruby-airbnb#块级和行内注释-block-and-inline-comments) 1. [标点符号, 拼写和语法 (Punctuation, spelling, and grammar)](https://github.com/1c7/ruby-airbnb#标点符号-拼写和语法-punctuation-spelling-and-grammar) 1. [待办注释 (TODO comments)](https://github.com/1c7/ruby-airbnb#待办注释-todo-comments) 1. [注释掉的代码 (Commented-out code)](https://github.com/1c7/ruby-airbnb#注释掉的代码-commented-out-code)
4. [方法 (Methods)](https://github.com/1c7/ruby-airbnb#方法-methods) 1. [方法定义 (Method definitions)](https://github.com/1c7/ruby-airbnb#方法定义-method-definitions) 1. [方法调用 (Method calls)](https://github.com/1c7/ruby-airbnb#方法调用-method-calls)
5. [条件表达式 (Conditional Expressions)](https://github.com/1c7/ruby-airbnb#条件表达式-conditional-expressions) 1. [关键字 (Conditional keywords)](https://github.com/1c7/ruby-airbnb#关键字-conditional-keywords) 1. [三元操作符 (Ternary operator)](https://github.com/1c7/ruby-airbnb#三元操作符-ternary-operator)
6. [语法 (Syntax)](https://github.com/1c7/ruby-airbnb#语法-syntax)
7. [命名 (Naming)](https://github.com/1c7/ruby-airbnb#命名-naming)
8. [类 (Classes)](https://github.com/1c7/ruby-airbnb#类-classes)
9. [异常 (Exceptions)](https://github.com/1c7/ruby-airbnb#异常-exceptions)
10. [集合 (Collections)](https://github.com/1c7/ruby-airbnb#集合-collections)
11. [字符串 (Strings)](https://github.com/1c7/ruby-airbnb#字符串-strings)
12. [正则表达式 (Regular Expressions)](https://github.com/1c7/ruby-airbnb#正则表达式-regular-expressions)
13. [百分比字面量 (Percent Literals)](https://github.com/1c7/ruby-airbnb#百分比字面量-percent-literals)
14. [Rails](https://github.com/1c7/ruby-airbnb#rails) 1. [范围 (Scopes)](https://github.com/1c7/ruby-airbnb#范围-scopes)
15. [保持一致 (Be Consistent)](https://github.com/1c7/ruby-airbnb#保持一致-be-consistent)

## 空格 (Whitespace)

### 缩进 (Indentation)

- 始终用 2 个空格做缩进。[[link](https://github.com/1c7/ruby-airbnb#default-indentation)]

- `when` 的缩进和 `case` 一致。 [[link](https://github.com/1c7/ruby-airbnb#indent-when-as-case)]

  ```
  case
  when song.name == 'Misty'
    puts 'Not again!'
  when song.duration > 120
    puts 'Too long!'
  when Time.now.hour > 21
    puts "It's too late"
  else
    song.play
  end
  
  kind = case year
         when 1850..1889 then 'Blues'
         when 1890..1909 then 'Ragtime'
         when 1910..1929 then 'New Orleans Jazz'
         when 1930..1939 then 'Swing'
         when 1940..1950 then 'Bebop'
         else 'Jazz'
         end
  ```

- 函数的参数要么全部在同一行，如果参数要分成多行，则每行一个参数， 相同缩进。[[link](https://github.com/1c7/ruby-airbnb#align-function-params)]

  ```
  # 错误
  def self.create_translation(phrase_id, phrase_key, target_locale,
                              value, user_id, do_xss_check, allow_verification)
    ...
  end
  
  # 正确
  def self.create_translation(phrase_id,
                              phrase_key,
                              target_locale,
                              value,
                              user_id,
                              do_xss_check,
                              allow_verification)
    ...
  end
  
  # 正确
  def self.create_translation(
    phrase_id,
    phrase_key,
    target_locale,
    value,
    user_id,
    do_xss_check,
    allow_verification
  )
    ...
  end
  ```

- 多行的布尔表达式，下一行缩进一下。[[link](https://github.com/1c7/ruby-airbnb#indent-multi-line-bool)]

  ```
  # 错误
  def is_eligible?(user)
    Trebuchet.current.launch?(ProgramEligibilityHelper::PROGRAM_TREBUCHET_FLAG) &&
    is_in_program?(user) &&
    program_not_expired
  end
  
  # 正确
  def is_eligible?(user)
    Trebuchet.current.launch?(ProgramEligibilityHelper::PROGRAM_TREBUCHET_FLAG) &&
      is_in_program?(user) &&
      program_not_expired
  end
  ```

### 行内 (Inline)

- 行末不要留空格。 [[link](https://github.com/1c7/ruby-airbnb#trailing-whitespace)]

- 写行内注释的时候，在代码和注释之间放 1 个空格。 [[link](https://github.com/1c7/ruby-airbnb#space-before-comments)]

  ```
  # 错误
  result = func(a, b)# we might want to change b to c
  
  # 正确
  result = func(a, b) # we might want to change b to c
  ```

- 操作符两边放一个空格；逗号，冒号，分号后面都放一个空格；左大括号 `{` 两边放空格，右大括号 `}` 左边放空格。 [[link](https://github.com/1c7/ruby-airbnb#spaces-operators)]

  ```
  sum = 1 + 2
  a, b = 1, 2
  1 > 2 ? true : false; puts 'Hi'
  [1, 2, 3].each { |e| puts e }
  ```

- 逗号前面永远不要放空格 [[link](https://github.com/1c7/ruby-airbnb#no-space-before-commas)]

  ```
  result = func(a, b)
  ```

- block 语法里，| | 内部的两边不应该带多余的空格，参数之间应该有1个空格，| | 后面应该有一个空格 [[link](https://github.com/1c7/ruby-airbnb#spaces-block-params")]

  ```
  # 错误
  {}.each { | x,  y |puts x }
  
  # 正确
  {}.each { |x, y| puts x }
  ```

- 感叹号和参数间不要留空格，下面是个正确的例子。[[link](https://github.com/1c7/ruby-airbnb#no-space-after-!)]

  ```
  !something
  ```

- `(`, `[` 后面不要有空格
  `]`, `)` 前面不要有空格 [[link](https://github.com/1c7/ruby-airbnb#no-spaces-braces)]

  ```
  some(arg).other
  [1, 2, 3].length
  ```

- 字符串插值时候忽略空格。[[link](https://github.com/1c7/ruby-airbnb#no-spaces-string-interpolation)]

  ```
  # 错误
  var = "This #{ foobar } is interpolated."
  
  # 正确
  var = "This #{foobar} is interpolated."
  ```

- 当表达范围时，不要写额外的空格。[[link](https://github.com/1c7/ruby-airbnb#no-spaces-range-literals)]

  ```
  # 错误
  (0 ... coll).each do |item|
  
  # 正确
  (0...coll).each do |item|
  ```

### 换行 (Newlines)

- if 条件保持相同缩进，方便识别哪些是条件，哪些是内容。 [[link](https://github.com/1c7/ruby-airbnb#multiline-if-newline)]

  ```
  if @reservation_alteration.checkin == @reservation.start_date &&
     @reservation_alteration.checkout == (@reservation.start_date + @reservation.nights)
  
    redirect_to_alteration @reservation_alteration
  end
  ```

- 条件语句，块，case 语句，等等东西后面换一行， 例子如下。[[link](https://github.com/1c7/ruby-airbnb#newline-after-conditional)]

  ```
  if robot.is_awesome?
    send_robot_present
  end
  
  robot.add_trait(:human_like_intelligence)
  ```

- 不同缩进的代码之间无需空行 (比如 class 或 module 和内容之间)。 [[link](https://github.com/1c7/ruby-airbnb#newline-different-indent)]

  ```
  # 错误
  class Foo
  
    def bar
      # body omitted
    end
  
  end
  
  # 正确
  class Foo
    def bar
      # body omitted
    end
  end
  ```

- 方法之间 1 个空行就好。[[link](https://github.com/1c7/ruby-airbnb#newline-between-methods)]

  ```
  def a
  end
  
  def b
  end
  ```

- 1 个空行隔开类似的逻辑。 [[link](https://github.com/1c7/ruby-airbnb#method-def-empty-lines)]

  ```
  def transformorize_car
    car = manufacture(options)
    t = transformer(robot, disguise)
  
    car.after_market_mod!
    t.transform(car)
    car.assign_cool_name!
  
    fleet.add(car)
    car
  end
  ```

- 文件末尾只放一个空行。 [[link](https://github.com/1c7/ruby-airbnb#trailing-newline)]

## 行宽 (Line Length)

- 把每一行控制在可读宽度内， 除非有特别理由, 每一行应小于 100 个字符 ([rationale](https://github.com/1c7/ruby-airbnb/blob/master/rationales.md#line-length)) [[link](https://github.com/1c7/ruby-airbnb#line-length)]

## 注释 (Commenting)

> 虽然写注释很痛苦, 但是对于保持代码可读非常重要
> 下面的规则描述了你应该怎么写注释, 以及写在哪里。
> 但是记住：注释虽然非常重要, 但是最好的注释是代码本身。
> 直观的变量名本身就是注释， 比起起一个奇怪的名字，然后用注释解释要好得多

> 当写注释时，为你的观众去写：下一个贡献者需要理解你的代码。
> 请慷慨一些 - 下一个可能就是你自己！

—[Google C++ 风格指南](https://google.github.io/styleguide/cppguide.html)

这一部分的指南从 Google [C++](https://google.github.io/styleguide/cppguide.html#Comments) 和 [Python](https://google.github.io/styleguide/pyguide.html#Comments) 风格指南那边借鉴了很多。

### 文件/类 级别的注释 (File/class-level comments)

每个类的定义都应该带些注释， 说明它是干什么的, 以及怎么用它。

如果文件里没有任何 class， 或者多于一个 class， 顶部应该有注释说明里面是什么。

```
# Automatic conversion of one locale to another where it is possible, like
# American to British English.
module Translation
  # Class for converting between text between similar locales.
  # Right now only conversion between American English -> British, Canadian,
  # Australian, New Zealand variations is provided.
  class PrimAndProper
    def initialize
      @converters = { :en => { :"en-AU" => AmericanToAustralian.new,
                               :"en-CA" => AmericanToCanadian.new,
                               :"en-GB" => AmericanToBritish.new,
                               :"en-NZ" => AmericanToKiwi.new,
                             } }
    end

  ...

  # Applies transforms to American English that are common to
  # variants of all other English colonies.
  class AmericanToColonial
    ...
  end

  # Converts American to British English.
  # In addition to general Colonial English variations, changes "apartment"
  # to "flat".
  class AmericanToBritish < AmericanToColonial
    ...
  end
```

所有文件，包括数据和配置文件，都应该有文件级别(file-level)的注释。

```
# List of American-to-British spelling variants.
#
# This list is made with
# lib/tasks/list_american_to_british_spelling_variants.rake.
#
# It contains words with general spelling variation patterns:
#   [trave]led/lled, [real]ize/ise, [flav]or/our, [cent]er/re, plus
# and these extras:
#   learned/learnt, practices/practises, airplane/aeroplane, ...

sectarianizes: sectarianises
neutralization: neutralisation
...
```

### 函数注释 (Function comments)

函数声明的前面都应该有注释，描述函数是做什么的，以及怎么用。
这些注释应该是 描述性的(descriptive) 比如 ("Opens the file")
而不是 命令式的(imperative) 比如 ("Open the file")；
注释描述这个函数，而不是告诉这个函数应该做什么。
总的来说，函数前面的注释并不负责解释函数是怎么做到它提供的功能的。
解释功能怎么实现的注释，应该零散的分布在函数内部的注释里。

每个函数都应该提到输入和输出是什么，除非碰到以下情况：

- 不是外部可见的函数 (not externally visible)
- 非常短 (very short)
- 很明显 (obvious)

你喜欢什么格式都行。在 Ruby 里两种常见的格式是 [TomDoc](http://tomdoc.org/) 和 [YARD](http://rubydoc.info/docs/yard/file/docs/GettingStarted.md)。
你也可以直接简洁明了的写出来，比如这样：

```
# Returns the fallback locales for the_locale.
# If opts[:exclude_default] is set, the default locale, which is otherwise
# always the last one in the returned list, will be excluded.
#
# For example:
#   fallbacks_for(:"pt-BR")
#     => [:"pt-BR", :pt, :en]
#   fallbacks_for(:"pt-BR", :exclude_default => true)
#     => [:"pt-BR", :pt]
def fallbacks_for(the_locale, opts = {})
  ...
end
```

### 块级和行内注释 (Block and inline comments)

这个部分有点小麻烦. 如果你下次 code review 需要解释这些代码的话， 你现在就应该写注释。
复杂的操作应该把注释写在前面。
单行的不太容易理解的代码, 注释应该写在代码后面。

```
def fallbacks_for(the_locale, opts = {})
  # dup() to produce an array that we can mutate.
  ret = @fallbacks[the_locale].dup

  # We make two assumptions here:
  # 1) There is only one default locale (that is, it has no less-specific
  #    children).
  # 2) The default locale is just a language. (Like :en, and not :"en-US".)
  if opts[:exclude_default] &&
      ret.last == default_locale &&
      ret.last != language_from_locale(the_locale)
    ret.pop
  end

  ret
end
```

在另一方面，永远不要在注释里描述代码。你要假设读代码的人对这门语言的了解比你知道的多。

相关的一个事儿：不要用块级注释，他们前面没有空格，不方便一眼认出来是注释。 [[link](https://github.com/1c7/ruby-airbnb#no-block-comments)]

```
# 错误
=begin
comment line
another comment line
=end

# 正确
# comment line
# another comment line
```

### 标点符号, 拼写和语法 (Punctuation, spelling, and grammar)

要注意注释里的标点符号, 拼写和语法；
注释写得好读起来也容易。

注释应该和叙述文 (narrative text) 一样易读 (readable)，
有着正确的大小写和标点符号。 在很多情况下。 完整的句子比句子碎片容易读得多。
短的注释， 比如跟在代码后面的， 可以不那么正式， 但风格应该一致。

虽然在提交代码时, 别人指出你在应该用分号的地方用了逗号, 这种小事蛮折磨人的.
但重要的是源代码应该保持高度的清晰和可读. 正确的标点符号, 拼写, 和语法非常重要.

### 待办注释 (TODO comments)

当代码是临时解决方案，够用但是并不完美时，用 TODO 注释。

TODO 应该全大写, 然后是写这个注释的人名, 用圆括号括起来, 冒号可写可不写。
然后后面是解释需要做什么，统一 TODO 注释样式的目的是方便搜索。
括号里的人名不代表这个人负责解决这个问题, 只是说这个人知道这里要解决什么问题.
每次你写 TODO 注释的时候加上你的名字。

```
  # 错误
  # TODO(RS): Use proper namespacing for this constant.

  # 错误
  # TODO(drumm3rz4lyfe): Use proper namespacing for this constant.

  # 正确
  # TODO(Ringo Starr): Use proper namespacing for this constant.
```

### 注释掉的代码 (Commented-out code)

- 注释掉的代码就不要留在代码库里了。 [[link](https://github.com/1c7/ruby-airbnb#commented-code)]

## 方法 (Methods)

### 方法定义 (Method definitions)

- 函数要接参数的时候就用括号, 不用参数时就不用写括号了。 下面是正确的例子.[[link](https://github.com/1c7/ruby-airbnb#method-def-parens)]

  ```
  def some_method
    # body omitted
  end
  
  def some_method_with_parameters(arg1, arg2)
    # body omitted
  end
  ```

- 不要用默认参数，用一个选项 hash 来做这个事。[[link](https://github.com/1c7/ruby-airbnb#no-default-args)]

  ```
  # 错误
  def obliterate(things, gently = true, except = [], at = Time.now)
    ...
  end
  
  # 正确
  def obliterate(things, options = {})
    default_options = {
      :gently => true, # obliterate with soft-delete
      :except => [], # skip obliterating these things
      :at => Time.now, # don't obliterate them until later
    }
    options.reverse_merge!(default_options)
  
    ...
  end
  ```

- 避免定义单行函数，虽然有些人喜欢这样写，但是这样容易引起一些奇怪的问题 [[link](https://github.com/1c7/ruby-airbnb#no-single-line-methods)]

  ```
  # 错误
  def too_much; something; something_else; end
  
  # 正确
  def some_method
    # body
  end
  ```

### 方法调用 (Method calls)

应该用 **圆括号** 的情况：

- 如果方法会返回值。 [[link](https://github.com/1c7/ruby-airbnb#returns-val-parens)]

  ```
  # 错误
  @current_user = User.find_by_id 1964192
  
  # 正确
  @current_user = User.find_by_id(1964192)
  ```

- 如果第一个参数需要圆括号。[[link](https://github.com/1c7/ruby-airbnb#first-arg-parens)]

  ```
  # 错误
  put! (x + y) % len, value
  
  # 正确
  put!((x + y) % len, value)
  ```

- 方法名和左括号之间永远不要放空格。[[link](https://github.com/1c7/ruby-airbnb#space-method-call)]

  ```
  # 错误
  f (3 + 2) + 1
  
  # 正确
  f(3 + 2) + 1
  ```

- 对于不用接收参数的方法， **忽略圆括号** 。[[link](https://github.com/1c7/ruby-airbnb#no-args-parens)]

  ```
  # 错误
  nil?()
  
  # 正确
  nil?
  ```

- 如果方法不返回值 (或者我们不关心返回值)，那么带不带括号都行。
  (如果参数会导致代码多于一行， 建议加个括号比较有可读性) [[link](https://github.com/1c7/ruby-airbnb#no-return-parens)]

  ```
  # okay
  render(:partial => 'foo')
  
  # okay
  render :partial => 'foo'
  ```

不论什么情况:

- 如果一个方法的最后一个参数接收 hash， 那么不需要 `{` `}` 。 [[link](https://github.com/1c7/ruby-airbnb#options-no-braces)]

  ```
  # 错误
  get '/v1/reservations', { :id => 54875 }
  
  # 正确
  get '/v1/reservations', :id => 54875
  ```

## 条件表达式 (Conditional Expressions)

### 关键字 (Conditional keywords)

- 永远不要把 `then` 和多行的 `if/unless` 搭配使用。 [[link](https://github.com/1c7/ruby-airbnb#multiline-if-then)]

  ```
  # 错误
  if some_condition then
    ...
  end
  
  # 正确
  if some_condition
    ...
  end
  ```

- `do` 不要和多行的 `while` 或 `until`搭配使用。[[link](https://github.com/1c7/ruby-airbnb#multiline-while-until)]

  ```
  # 错误
  while x > 5 do
    ...
  end
  
  until x > 5 do
    ...
  end
  
  # 正确
  while x > 5
    ...
  end
  
  until x > 5
    ...
  end
  ```

- `and`, `or`， 和`not` 关键词禁用。 因为不值得。 总是用 `&&`， `||`， 和 `!` 来代替。 [[link](https://github.com/1c7/ruby-airbnb#no-and-or)]

- 适合用 `if/unless` 的情况： 内容简单， 条件简单， 整个东西能塞进一行。
  不然的话， 不要用 `if/unless`。 [[link](https://github.com/1c7/ruby-airbnb#only-simple-if-unless)]

  ```
  # 错误 - 一行塞不下
  add_trebuchet_experiments_on_page(request_opts[:trebuchet_experiments_on_page]) if request_opts[:trebuchet_experiments_on_page] && !request_opts[:trebuchet_experiments_on_page].empty?
  
  # 还行
  if request_opts[:trebuchet_experiments_on_page] &&
       !request_opts[:trebuchet_experiments_on_page].empty?
  
    add_trebuchet_experiments_on_page(request_opts[:trebuchet_experiments_on_page])
  end
  
  # 错误 - 这个很复杂,需要写成多行,而且需要注释
  parts[i] = part.to_i(INTEGER_BASE) if !part.nil? && [0, 2, 3].include?(i)
  
  # 还行
  return if reconciled?
  ```

- 不要把 `unless` 和 `else` 搭配使用。[[link](https://github.com/1c7/ruby-airbnb#no-unless-with-else)]

  ```
  # 错误
  unless success?
    puts 'failure'
  else
    puts 'success'
  end
  
  # 正确
  if success?
    puts 'success'
  else
    puts 'failure'
  end
  ```

- 避免用多个条件的 `unless` 。[[link](https://github.com/1c7/ruby-airbnb#unless-with-multiple-conditions)]

  ```
    # 错误
    unless foo? && bar?
      ...
    end
  
    # 还行
    if !(foo? && bar?)
      ...
    end
  ```

- 条件语句 `if/unless/while` 不需要圆括号。 [[link](https://github.com/1c7/ruby-airbnb#parens-around-conditions)]

  ```
  # 错误
  if (x > 10)
    ...
  end
  
  # 正确
  if x > 10
    ...
  end
  ```

### 三元操作符 (Ternary operator)

- 避免使用三元操作符 (`?:`)，如果不用三元操作符会变得很啰嗦才用。 对于单行的条件, 用三元操作符(`?:`) 而不是 `if/then/else/end`.[[link](https://github.com/1c7/ruby-airbnb#avoid-complex-ternary)]

  ```
  # 错误
  result = if some_condition then something else something_else end
  
  # 正确
  result = some_condition ? something : something_else
  ```

- 不要嵌套使用三元操作符，换成 `if/else`.[[link](https://github.com/1c7/ruby-airbnb#no-nested-ternaries)]

  ```
  # 错误
  some_condition ? (nested_condition ? nested_something : nested_something_else) : something_else
  
  # 正确
  if some_condition
    nested_condition ? nested_something : nested_something_else
  else
    something_else
  end
  ```

- 避免多条件三元操作符。最好判断一个条件。 [[link](https://github.com/1c7/ruby-airbnb#single-condition-ternary)]

- 避免拆成多行的 `?:` (三元操作符), 用 `if/then/else/end` 就好了。 [[link](https://github.com/1c7/ruby-airbnb#no-multiline-ternaries)]

  ```
  # 错误
  some_really_long_condition_that_might_make_you_want_to_split_lines ?
    something : something_else
  
  # 正确
  if some_really_long_condition_that_might_make_you_want_to_split_lines
    something
  else
    something_else
  end
  ```

## 语法 (Syntax)

- 永远不要用 `for`，除非有非常特殊的理由。
  绝大部分情况都应该用 `each`。 `for` 是用 `each` 实现的(所以你间接加了一层)，
  但区别是 - `for` 不会有新 scope (不像 `each`) 里面定义的变量外面可见。[[link](https://github.com/1c7/ruby-airbnb#no-for)]

  ```
  arr = [1, 2, 3]
  
  # 错误
  for elem in arr do
    puts elem
  end
  
  # 正确
  arr.each { |elem| puts elem }
  ```

- 单行的情况下, 尽量用 `{...}` 而不是 `do...end`。
  多行的情况下避免用 `{...}`. 对于 "control flow" 和 "方法定义"(举例: 在 Rakefiles 和某些 DSLs 里) 总是用 `do...end`。
  方法连用(chaining)时 避免使用 `do...end` 。[[link](https://github.com/1c7/ruby-airbnb#single-line-blocks)]

  ```
  names = ["Bozhidar", "Steve", "Sarah"]
  
  # 正确
  names.each { |name| puts name }
  
  # 错误
  names.each do |name| puts name end
  
  # 正确
  names.each do |name|
    puts name
    puts 'yay!'
  end
  
  # 错误
  names.each { |name|
    puts name
    puts 'yay!'
  }
  
  # 正确
  names.select { |name| name.start_with?("S") }.map { |name| name.upcase }
  
  # 错误
  names.select do |name|
    name.start_with?("S")
  end.map { |name| name.upcase }
  ```

  有些人会说多行连用(chaining) 用 `{...}` 符号 其实也没那么难看, 但他们应该问问自己这代码真的可读吗, 而且 block 里的内容是否可以抽出来弄好看些.

- 尽可能用短的自赋值操作符 (self assignment operators)。[[link](https://github.com/1c7/ruby-airbnb#self-assignment)]

  ```
  # 错误
  x = x + y
  x = x * y
  x = x**y
  x = x / y
  x = x || y
  x = x && y
  
  # 正确
  x += y
  x *= y
  x **= y
  x /= y
  x ||= y
  x &&= y
  ```

- 避免用分号。 除非是单行 class 定义的情况下。 而且当使用分号时， 分号前面不应该有空格。[[link](https://github.com/1c7/ruby-airbnb#semicolons)]

  ```
  # 错误
  puts 'foobar'; # 多余的分号
  puts 'foo'; puts 'bar' # 两个表达式放到一行
  
  # 正确
  puts 'foobar'
  
  puts 'foo'
  puts 'bar'
  
  puts 'foo', 'bar' # this applies to puts in particular
  ```

- :: 的使用场景是引用常量，(比如 classes 和 modules 里的常量) 以及调用构造函数 (比如 Array() 或者 Nokogiri::HTML())。 普通方法调用就不要使用 :: 了。[[link](https://github.com/1c7/ruby-airbnb#colon-use)]

  ```
  # 错误
  SomeClass::some_method
  some_object::some_method
  
  # 正确
  SomeClass.some_method
  some_object.some_method
  SomeModule::SomeClass::SOME_CONST
  SomeModule::SomeClass()
  ```

- 尽量避免用 `return`。 [[link](https://github.com/1c7/ruby-airbnb#redundant-return)]

  ```
  # 错误
  def some_method(some_arr)
    return some_arr.size
  end
  
  # 正确
  def some_method(some_arr)
    some_arr.size
  end
  ```

- 条件语句里不要用返回值[[link](https://github.com/1c7/ruby-airbnb#assignment-in-conditionals)]

  ```
  # 错误 - shows intended use of assignment
  if (v = array.grep(/foo/))
    ...
  end
  
  # 错误
  if v = array.grep(/foo/)
    ...
  end
  
  # 正确
  v = array.grep(/foo/)
  if v
    ...
  end
  ```

- 请随意用 `||=` 来初始化变量。 [[link](https://github.com/1c7/ruby-airbnb#double-pipe-for-uninit)]

  ```
  # 把 name 赋值为 Bozhidar, 仅当 name 是 nil 或者 false 时
  name ||= 'Bozhidar'
  ```

- 不要用 `||=` 来初始化布尔变量。 (想象下如果值刚好是 `false` 会咋样.)[[link](https://github.com/1c7/ruby-airbnb#no-double-pipes-for-bools)]

  ```
  # 错误 - would set enabled to true even if it was false
  enabled ||= true
  
  # 正确
  enabled = true if enabled.nil?
  ```

- 当调用 lambdas 时，明确使用 `.call`。 [[link](https://github.com/1c7/ruby-airbnb#lambda-calls)]

  ```
  # 错误
  lambda.(x, y)
  
  # 正确
  lambda.call(x, y)
  ```

- 避免使用 Perl 风格的特殊变量名 ( 比如 `$0-9`, `$`, 等等. ). 因为看起来蛮神秘的. 建议只在单行里使用。建议用长名字， 比如 `$PROGRAM_NAME`.[[link](https://github.com/1c7/ruby-airbnb#no-cryptic-perl)]

- 当一个方法块只需要 1 个参数，而且方法体也只是读一个属性， 或者无参数的调用一样方法，这种情况下用 `&:` . [[link](https://github.com/1c7/ruby-airbnb#single-action-blocks)]

  ```
  # 错误
  bluths.map { |bluth| bluth.occupation }
  bluths.select { |bluth| bluth.blue_self? }
  
  # 正确
  bluths.map(&:occupation)
  bluths.select(&:blue_self?)
  ```

- 当调用当前实例的某个方法时， 尽量用 `some_method` 而不是 `self.some_method`。[[link](https://github.com/1c7/ruby-airbnb#redundant-self)]

  ```
  # 错误
  def end_date
    self.start_date + self.nights
  end
  
  # 正确
  def end_date
    start_date + nights
  end
  ```

  在下面 3 种常见情况里, 需要用, 而且应该用`self.`:

  1. 当定义一个类方法时: `def self.some_method`.
  2. 当调用一个赋值方法 (assignment method) 时,*左手边* 应该用 self, 比如当 self 是 ActiveRecord 模型然后你需要赋值一个属性: `self.guest = user`.
  3. 指向 (Referencing) 当前实例的类: `self.class`.

## 命名 (Naming)

- 用 蛇命名法 (`snake_case`) 来命名 methods 和 variables。 [[link](https://github.com/1c7/ruby-airbnb#snake-case)]

- 用 驼峰命名法(`CamelCase`) 命名 class 和 module。 (缩写词如 HTTP, RFC, XML 全部大写) [[link](https://github.com/1c7/ruby-airbnb#camel-case)]

- 用尖叫蛇命名法 (`SCREAMING_SNAKE_CASE`) 来命名常量。[[link](https://github.com/1c7/ruby-airbnb#screaming-snake-case)]

- 断定方法的名字 (predicate methods) (意思是那些返回布尔值的方法) 应该以问号结尾。 (比如 `Array#empty?`)。[[link](https://github.com/1c7/ruby-airbnb#predicate-method-names)]

- 有一定 "潜在危险" 的方法 (意思就是那些. 会修改 `self` 的方法, 或者原地修改参数的方法, 或者带有 `exit!` 的方法, 等等) 应该以感叹号结尾. 这种危险方法应该仅当同名的不危险方法存在之后, 才存在. ([More on this](http://dablog.rubypal.com/2007/8/15/bang-methods-or-danger-will-rubyist).) [[link](https://github.com/1c7/ruby-airbnb#bang-methods)]

- 把不用的变量名命名为 `_`. [[link](https://github.com/1c7/ruby-airbnb#throwaway-variables)]

  ```
  payment, _ = Payment.complete_paypal_payment!(params[:token],
                                                native_currency,
                                                created_at)
  ```

## 类 (Classes)

- 避免使用类变量(`@@`)， 因为在继承的时候它们会有 "淘气" 的行为。 [[link](https://github.com/1c7/ruby-airbnb#avoid-class-variables)]

  ```
  class Parent
    @@class_var = 'parent'
  
    def self.print_class_var
      puts @@class_var
    end
  end
  
  class Child < Parent
    @@class_var = 'child'
  end
  
  Parent.print_class_var # => 会输出"child"
  ```

  你可以看到在这个类的继承层级了，所有的类都共享一个类变量。 尽量使用实例变量而不是类变量。

- 用 `def self.method` 来定义单例方法(singleton methods). 这样在需要改类名的时候更方便. [[link](https://github.com/1c7/ruby-airbnb#singleton-methods)]

  ```
  class TestClass
    # 错误
    def TestClass.some_method
      ...
    end
  
    # 正确
    def self.some_other_method
      ...
    end
  ```

- 除非必要，避免写 `class << self`， 必要的情况比如 single accessors 和 aliased attributes。 [[link](https://github.com/1c7/ruby-airbnb#no-class-self)]

  ```
  class TestClass
    # 错误
    class << self
      def first_method
        ...
      end
  
      def second_method_etc
        ...
      end
    end
  
    # 正确
    class << self
      attr_accessor :per_page
      alias_method :nwo, :find_by_name_with_owner
    end
  
    def self.first_method
      ...
    end
  
    def self.second_method_etc
      ...
    end
  end
  ```

- `public`, `protected`, `private` 它们和方法定义保持相同缩进。 并且上下各留一个空行。[[link](https://github.com/1c7/ruby-airbnb#access-modifiers)]

  ```
  class SomeClass
    def public_method
      # ...
    end
  
    private
  
    def private_method
      # ...
    end
  end
  ```

## 异常 (Exceptions)

- 不要把异常用于控制流里 (flow of control) [[link](https://github.com/1c7/ruby-airbnb#exception-flow-control)]

  ```
  # 错误
  begin
    n / d
  rescue ZeroDivisionError
    puts "Cannot divide by 0!"
  end
  
  # 正确
  if d.zero?
    puts "Cannot divide by 0!"
  else
    n / d
  end
  ```

- 避免捕捉 `Exception` 这个大类的异常 [[link](https://github.com/1c7/ruby-airbnb#dont-rescue-exception)]

  ```
  # 错误
  begin
    # an exception occurs here
  rescue Exception
    # exception handling
  end
  
  # 正确
  begin
    # an exception occurs here
  rescue StandardError
    # exception handling
  end
  
  # 可以接受
  begin
    # an exception occurs here
  rescue
    # exception handling
  end
  ```

- 传 2 个参数调 raise 异常时不要明确指明`RuntimeError`。尽量用 error 子类这样比较清晰和明确。[[link](https://github.com/1c7/ruby-airbnb#redundant-exception)]

  ```
  # 错误
  raise RuntimeError, 'message'
  
  # 正确一点 - RuntimeError 是默认的
  raise 'message'
  
  # 最好
  class MyExplicitError < RuntimeError; end
  raise MyExplicitError
  ```

- 尽量将异常的类和讯息两个分开作为 `raise` 的参数，而不是提供异常的实例。[[link](https://github.com/1c7/ruby-airbnb#exception-class-messages)]

  ```
  # 错误
  raise SomeException.new('message')
  # 注意，提供异常的实例没办法做到 `raise SomeException.new('message'), backtrace`.
  
  # 正确
  raise SomeException, 'message'
  # 可以达到 `raise SomeException, 'message', backtrace`.
  ```

- 避免使用 rescue 的变异形式。 [[link](https://github.com/1c7/ruby-airbnb#rescue-as-modifier)]

  ```
  # 错误
  read_file rescue handle_error($!)
  
  # 正确
  begin
    read_file
  rescue Errno:ENOENT => ex
    handle_error(ex)
  end
  ```

## 集合 (Collections)

- 尽量用 `map` 而不是 `collect`。[[link](https://github.com/1c7/ruby-airbnb#map-over-collect)]

- 尽量用 `detect` 而不是 `find`。 `find` 容易和 ActiveRecord 的 `find` 搞混 - `detect` 则是明确的说明了 是要操作 Ruby 的集合, 而不是 ActiveRecord 对象。 [[link](https://github.com/1c7/ruby-airbnb#detect-over-find)]

- 尽量用 `reduce` 而不是 `inject`。 [[link](https://github.com/1c7/ruby-airbnb#reduce-over-inject)]

- 尽量用 `size`， 而不是 `length` 或者 `count`， 出于性能理由。[[link](https://github.com/1c7/ruby-airbnb#size-over-count)]

- 尽量用数组和 hash 字面量来创建， 而不是用 new。 除非你需要传参数。 [[link](https://github.com/1c7/ruby-airbnb#empty-collection-literals)]

  ```
  # 错误
  arr = Array.new
  hash = Hash.new
  
  # 正确
  arr = []
  hash = {}
  
  # 正确, 因为构造函数需要参数
  x = Hash.new { |h, k| h[k] = {} }
  ```

- 为了可读性倾向于用 `Array#join` 而不是 `Array#*`。 [[link](https://github.com/1c7/ruby-airbnb#array-join)]

  ```
  # 错误
  %w(one two three) * ', '
  # => 'one, two, three'
  
  # 正确
  %w(one two three).join(', ')
  # => 'one, two, three'
  ```

- 用 符号(symbols) 而不是 字符串(strings) 作为 hash keys。 [[link](https://github.com/1c7/ruby-airbnb#symbol-keys)]

  ```
  # 错误
  hash = { 'one' => 1, 'two' => 2, 'three' => 3 }
  
  # 正确
  hash = { :one => 1, :two => 2, :three => 3 }
  ```

- 如果可以的话, 用普通的 symbol 而不是字符串 symbol。[[link](https://github.com/1c7/ruby-airbnb#symbol-literals)]

  ```
  # 错误
  :"symbol"
  
  # 正确
  :symbol
  ```

- 用 `Hash#key?` 而不是 `Hash#has_key?` 用 `Hash#value?` 而不是 `Hash#has_value?`. 根据 Matz 的说法, 长一点的那种写法在考虑要废弃掉。 [[link](https://github.com/1c7/ruby-airbnb#deprecated-hash-methods")

  ```
  # 错误
  hash.has_key?(:test)
  hash.has_value?(value)
  
  # 正确
  hash.key?(:test)
  hash.value?(value)
  ```

- 用多行 hashes 使得代码更可读, 逗号放末尾。 [[link](https://github.com/1c7/ruby-airbnb#multiline-hashes)]

  ```
  hash = {
    :protocol => 'https',
    :only_path => false,
    :controller => :users,
    :action => :set_password,
    :redirect => @redirect_url,
    :secret => @secret,
  }
  ```

- 如果是多行数组，用逗号结尾。[[link](https://github.com/1c7/ruby-airbnb#array-trailing-comma)]

  ```
  # 正确
  array = [1, 2, 3]
  
  # 正确
  array = [
    "car",
    "bear",
    "plane",
    "zoo",
  ]
  ```

## 字符串 (Strings)

- 尽量使用字符串插值，而不是字符串拼接[[link](https://github.com/1c7/ruby-airbnb#string-interpolation)]

  ```
  # 错误
  email_with_name = user.name + ' <' + user.email + '>'
  
  # 正确
  email_with_name = "#{user.name} <#{user.email}>"
  ```

  另外，记住 Ruby 1.9 风格的字符串插值。比如说你要构造出缓存的 key 名：

  ```
  CACHE_KEY = '_store'
  
  cache.write(@user.id + CACHE_KEY)
  ```

  那么建议用字符串插值而不是字符串拼接:

  ```
  CACHE_KEY = '%d_store'
  
  cache.write(CACHE_KEY % @user.id)
  ```

- 在需要构建大数据块时，避免使用 `String#+`。
  而是用 `String#<<`. 它可以原位拼接字符串而且它总是快于 `String#+`, 这种用加号的语法会创建一堆新的字符串对象.[[link](https://github.com/1c7/ruby-airbnb#string-concatenation)]

  ```
  # 正确而且快
  html = ''
  html << '<h1>Page title</h1>'
  
  paragraphs.each do |paragraph|
    html << "<p>#{paragraph}</p>"
  end
  ```

- 对于多行字符串， 在行末使用 `\` ，而不是 `+` 或者 `<<` [[link](https://github.com/1c7/ruby-airbnb#multi-line-strings)]

  ```
  # 错误
  "Some string is really long and " +
    "spans multiple lines."
  
  "Some string is really long and " <<
    "spans multiple lines."
  
  # 正确
  "Some string is really long and " \
    "spans multiple lines."
  ```

## 正则表达式 (Regular Expressions)

- 避免使用 `$1-9` 因为可能难以辨认出是哪一个，取个名。 [[link](https://github.com/1c7/ruby-airbnb#regex-named-groups)]

  ```
  # 错误
  /(regexp)/ =~ string
  ...
  process $1
  
  # 正确
  /(?<meaningful_var>regexp)/ =~ string
  ...
  process meaningful_var
  ```

- 小心使用 `^` 和 `$` 因为它们匹配的是 行头/行末，而不是某个字符串的结尾. 如果你想匹配整个字符串, 用: `\A` 和 `\z`.[[link](https://github.com/1c7/ruby-airbnb#caret-and-dollar-regexp)]

  ```
  string = "some injection\nusername"
  string[/^username$/]   # matches
  string[/\Ausername\z/] # don't match
  ```

- 使用 `x` 修饰符在复杂的正则表达式上。 这使得它更可读, 并且你可以加有用的注释。只是要注意空格会被忽略。[[link](https://github.com/1c7/ruby-airbnb#comment-regexes)]

  ```
  regexp = %r{
    start         # some text
    \s            # white space char
    (group)       # first group
    (?:alt1|alt2) # some alternation
    end
  }x
  ```

## 百分比字面量 (Percent Literals)

- 统一用圆括号，不要用其他括号， 因为它的行为更接近于一个函数调用。[[link](https://github.com/1c7/ruby-airbnb#percent-literal-delimiters)]

  ```
  # 错误
  %w[date locale]
  %w{date locale}
  %w|date locale|
  
  # 正确
  %w(date locale)
  ```

- 随意用 `%w` [[link](https://github.com/1c7/ruby-airbnb#percent-w)]

  ```
  STATES = %w(draft open closed)
  ```

- 在一个单行字符串里需要 插值(interpolation) 和内嵌双引号时使用 `%()`。 对于多行字符串，建议用 heredocs 语法。[[link](https://github.com/1c7/ruby-airbnb#percent-parens)]

  ```
  # 错误 - 不需要字符串插值
  %(<div class="text">Some text</div>)
  # 直接 '<div class="text">Some text</div>' 就行了
  
  # 错误 - 无双引号
  %(This is #{quality} style)
  # 直接 "This is #{quality} style" 就行了
  
  # 错误 - 多行了
  %(<div>\n<span class="big">#{exclamation}</span>\n</div>)
  # 应该用 heredoc.
  
  # 正确 - 需要字符串插值, 有双引号, 单行.
  %(<tr><td class="name">#{name}</td>)
  ```

- 仅在需要匹配 *多于一个* '/'符号的时候使用 `%r`。[[link](https://github.com/1c7/ruby-airbnb#percent-r)]

  ```
  # 错误
  %r(\s+)
  
  # 依然不好
  %r(^/(.*)$)
  # should be /^\/(.*)$/
  
  # 正确
  %r(^/blog/2011/(.*)$)
  ```

- 避免使用 %x ，除非你要调用一个带引号的命令(非常少见的情况)。 [[link](https://github.com/1c7/ruby-airbnb#percent-x)]

  ```
  # 错误
  date = %x(date)
  
  # 正确
  date = `date`
  echo = %x(echo `date`)
  ```

## Rails

- 当调用 `render` 或 `redirect_to` 后需要马上"返回"时，把 `return` 放到下一行, 不要放到同一行。 [[link](https://github.com/1c7/ruby-airbnb#next-line-return)]

  ```
  # 错误
  render :text => 'Howdy' and return
  
  # 正确
  render :text => 'Howdy'
  return
  
  # still bad
  render :text => 'Howdy' and return if foo.present?
  
  # 正确
  if foo.present?
    render :text => 'Howdy'
    return
  end
  ```

### 范围 (Scopes)

- 当定义 ActiveRecord 的模型 scopes 时， 把内容用大括号包起来。 如果不包的话, 在载入这个 class 时就会被强迫连接数据库。 [[link](https://github.com/1c7/ruby-airbnb#scope-lambda)]

  ```
  # 错误
  scope :foo, where(:bar => 1)
  
  # 正确
  scope :foo, -> { where(:bar => 1) }
  ```

## 保持一致 (Be Consistent)

> 在你编辑某块代码时，看看周围的代码是什么风格。
> 如果它们在数学操作符两边都放了空格，那么你也应该这样做。
> 如果它们的代码注释用 # 井号包成了一个盒子, 那么你也应该这样做。

> 风格指南存在的意义是 让看代码时能关注 "代码说的是什么"。
> 而不是 "代码是怎么说这件事的"。这份整体风格指南就是帮助你做这件事的。
> 注意局部的风格同样重要。如果一个部分的代码和周围的代码很不一样。
> 别人读的时候思路可能会被打断。尽量避免这一点。

—[Google C++ Style Guide](https://google.github.io/styleguide/cppguide.html)

