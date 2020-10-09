# Erlang 学习笔记

## Erlang 为什么天然支持并发

副作用和并发是无法共存的。

1. Erlang 是函数式语言，函数式语言没有副作用，即不允许修改变量
2. Erlang 里，改变单个进程内部的状态是允许的，但一个进程改变另一个进程的状态则不行
3. Erlang 里，进程不共享任何数据。进程间只能以交换消息的方式进行交互

## Erlang 世界

```erlang
% person.erl

-module(person).
-export([init/1]).

init(Name) -> ok.

```

spawn 每次都会创建一个新的进程

```erlang
%world.erl
-module(world).
-export([start/0])

start() ->
    Joe = spawn(person, init, ["joe"]).
    % person:init("Joe")
    Alex = spawn(person, init, ["alex"]).
    % person:init("Alex")
    % Joe 和 Alex 是 pid
    ...
```

现在 Joe 想要发消息给 Alex

Pid ! Msg

```erlang
Alex ! {Joe, "Hope the dogs don't catch the rabbit"}

```

Alex 接受消息

```erlang
receive
    {From, Message} ->
    % From = Joe
    % Message = "Hope the dogs don't catch the rabbit"
end
```

## 编译运行 Erlang 的两种方法

```erlang
% hello.erl

-module(hello).
-export([start/0]).

start() ->
    io:format("Hello world ~n").
```

**使用 Erlang Shell 运行**

```
1> c(hello).
{ok, hello}

2> hello:start().
Hello world

3> halt().
$
```

**使用 Erlang compile 运行**

```
$ erlc hello.erl # 生成 hello.beam
$ erl -noshell -s hello start -s init stop
# 首先执行了 hello:start()
# 而后执行 init:stop() 终止 erlang 会话

Hello world
```

## Erlang file server

```erlang
-module(afile_server).
-export([start/0, start/1, loop/1]).

start() ->
    spawn(afile_server, loop, ["."]).


start(Dir) ->
    spawn(afile_server, loop, [Dir]).

% 收到 list_dir 就 ls Dir
% 收到 {get_file, File} 就 cat Dir/File
loop(Dir) ->
    receive
        {Client, list_dir} ->
            Client ! {self(), file:list_dir(Dir)};
        {Client, {get_file, File}} ->
            Full = filename:join(Dir, File),
            Client ! {self(), file:read_file(Full)}
    end,
    loop(Dir).

```

```erlang
-module(afile_client).
-export([ls/1, get_file/2]).

ls(Server) ->
    Server ! {self(), list_dir},
    receive
        {Server, {ok, FileList}} ->
            FileList;
        {Server, {error, Error}} ->
            Error
    end.

get_file(Server, File) ->
    Server ! {self(), {get_file, File}},
    receive
        {Server, Content} ->
            Content
    end.

```

## 函数的重载？

名字相同，但参数数量不同的函数，按照名字不相同的函数处理
使用 . 分割

```erlang
-export([add/2, add/3, sub/2])

add(X, Y) -> X + Y.

add(X, Y, Z) -> X + Y + Z.

sub(X, Y) -> X - Y.

```

名字相同，参数相同，但 pattern 不同的函数。
使用 ; 分割

```erlang
sort([]) -> [];
sort([Pivot|T]) ->
    sort([X || X <- T, X < Pivot]) ++
    [Pivot] ++
    sort([X || X <- T, X > Pivot]).
```

## shell 命令

q() 就是 init:stop() 在 shell 中的别名
b() bind 列出所有绑定变量
f() forget 忘记所有已绑定的变量
f(Var) 忘记这个变量
rf(Record) 忘记 record
pwd()
ls()
cd(Dir)

## 原子

对应 C 的全局常量

```c
#define OP_READ 1
#define OP_WRITE 2
...

#define SUCCESS 255


int ret = file_open(OP_READ, buff);
if (ret == SUCCESS) {...}
```

```erlang
Ret = file_open(op_read, buff);
if Ret == success ->
```

通常以小写字母开头，后结 \_ @ 。
monday, tuesday, joe@somehost, a_long_name 等
如果想以大写字母开头，或者使用特殊字符，需要用单引号 ''
'Monday' '+'

所以说 ' 和 " 不能互换使用

## 元组

```c
struct point {
    int x;
    int y;
} P

p.x = 10, p.y = 45
```

```erlang
p = {10, 45}
% 为了增加可读性，一般将原子作为元组的第一个元素

p = {point, 10, 45}
```

嵌套元组

```erlang
Person = {
    person,
    {name, joe},
    {height, 1.82},
    {footsize, 42}
}.
```

## 列表

```
Nums = [1,2,3].
[H|T] = Nums.
% H: 1
% T: [2,3]

[N1, N2 | T] = Nums.
% N1 1
% N2 2
% T [3]

Nums ++ 4
% [1, 2, 3, 4]
```

## 字符串

严格来说，Erlang 里没有字符串。
可以用 **整数列表** 或 **二进制** 表示字符串

定义整数列表时，如果里面的整数都可以打印，就会被转化为字符串

```erlang
1> [83, 117, 114, 112, 114, 105, 115, 101]
Suprise

2> [1, 117, 114, 112, 114, 105, 115, 101]
[1, 117, 114, 112, 114, 105, 115, 101] % 因为 ascii 码为数字1的字符串，不是一个**可打印的Latin字符**
```

如果不知道哪一个字符串的 ascii 码，可以用 \$ 符号，进行转换

Unicode

```erlang
1> X = "a \x{221e}b"
[97, 8734, 98] % 221e 不是一个可打印的Latin字符
2> io:format("~ts ~n", [X]). % 打印 Unicode
```

使用 io:format 打印 ascii 码

```erlang
io:format("~w~n", ["abc"]).
[97, 98, 99].
```

## term

Erlang 所有数据结构的统称

## 模块和函数

术语

```
-export([area/1]) % 1 称为函数的元数(arity)
```

参数数量相同，但 pattern 不同的**子句**，使用分号相隔

## 匿名函数 fun

```erlang
Double = fun(X) -> 2 * X end.

TempConvert = fun({c, C}) -> {f, 32 + C * 9/5};
        ({f, F}) -> {c, (F - 32) * 5 / 9}
        end.

Even = fun(X) -> (X rem 2) =:= 0 end.

list:map(Even, [1,3,4])
% erlang 约定俗成，将 List 作为函数的第二个参数
```

## 导入

```erlang
-import(lists, [map/2]).

现在，可以直接使用 map 而不是 lists:map

```

## lists 内置函数

lists:seq(1, 10) -> [1, 2, ..10]

## 关卡 Guard

```erlang
max(X, Y) when X > Y -> X;
max(X, Y) -> Y.

```

关卡序列只单一或一系列关卡 G1; G2; G3 ... Gn，只要其中有一个为 true，它的值就为 true. or
关卡由一系列关卡表达式组成 GuardExpr1, GuardExpr2 ... GuardExprN 只有所有关卡表达式为 true，表达式才为 true. and

合法的关卡表达式必须是无副作用的。
用户定义的函数不能作为关卡，应为无法保证没有副作用。

## 布尔表达式

and &
or |

短路布尔表达式
andalso &&
orelse ||

## 关卡判断函数

is_atom(X)
is_binary(X)
is_bitstring(X)
is_boolean(X)
is_recoad(X, Tag) X 是一个类型为 Tag 的记录
is_record(X, Tag, N) X 是一个类型为 Tag、大小为 N 的记录

## 关卡内置函数

abs(N)
tuple_size(T)
element(N, T) 元组的第 N 个元素
hd(L)
tl(L)

## case

```erlang
case Expression of
    Pattern1 [when Guard1] -> Expr_seq1;
    Pattern2 [when Guard2] -> Expr_seq2;
    ...
    _ -> % 没有任何 pattern 匹配时,会进入这个分支。 如果去掉这个分支，就会产生错误。
    ...
end
```

## if

```erlang
if
    Guard1 ->
        Expr_seq1;
    Guard2 ->
        Expr_seq2;
    ...
    true ->
        % 最后措施
end
```

## 记录

-record(Name, {
%% 以下两个键带有默认值
key1 = Default1,
key2 = Default2,
...
key3
})

注，record 不是一个 shell 命令，shell 里用 rr(read records)

```erl
% 创建
X1 = #todo{status=urgent, text="tix"}.
X2 = X1#todo{status=done}.


% 提取

#todo{who=W, text=Txt} = X2

X2#todo.who

```

% 记录其实就是元组

```
10>rf(todo)
ok

11>X2
{todo, done, joe, "Tix"}

```

## map 映射表

map 在系统内部是作为有序集合存储的

```erl
3> F2 = #{b => 2, a => 1}.
#{a => 1, b => 2}.

```

有两种基于现有 map 创建新 map 的方法
NewMap = OldMap#{K1 => V1, ... Kn => Vn}
K 可以是 OldMap 里没有的

NewMap = OldMap#{K1 := V1, ... Kn := Vn}
K 必须是 OldMap 里有的

所以 := 一般用于更新，对于拼写错误进行报错

使用 map 的最佳实践，在首次定义某个键时，使用 =>。在修改某个键时，使用 :=

模式匹配使用 :=

```erl
{a := A} = F2
```
