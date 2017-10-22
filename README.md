
---

https://swtch.com/~rsc/regexp/regexp1.html

---

> every regular expression has an equivalent NFA and vice versa.
> DFAs are equivalent in power to NFAs and regular expressions.

---

关于分支的遍历方式

一种是同时遍历多个分支。
感觉上就是一个广度优先的遍历，读取一个字符，然后队列里的状态机都跑一遍。
能继续前进的加入下个队列，死路一条的分支抛弃掉。

这种方式的有个优点，输入的字符串只要遍历一次，即遍历的复杂度为 O(n)。
但是队列里状态可能有很多。
比如 NFA 有 m 个节点，最糟的情况可能处理一个字符的时候，要测试全部 m 个状态。

这是 Thompson 在 1968 发表的算法……要五十年了。
NFA 处理下变成 DFA（其实就是将队列里的多个状态合并成了一个状态）。
这个遍历就变成更直观的状态机执行了。

---

另一种方式是回溯（backstracking）。（就是比较低效的那种

遇到分支的时候，先处理其中一个分支，不匹配再回头处理其他分支。
相当于深度优先的遍历了。
好处是递归实现比较容易。
不足之处在于，匹配失败时要回溯，此时字符串可能匹配了一部分了，但还是只能抛弃掉。
这样就做不到 O(n) 的时间复杂度了。

---

- alternation
- concatenation
- repetition operators
- group

- character classes
- escape sequences
- counted repetition
- submatch extraction
- unanchored matches
- non-greedy operators
- assertions
- backreferences
- backtracking with memoization
- character sets

---

> no one knows how to implement regular expressions with backreferences
> efficiently, though no one can prove that it's impossible either.

> A particularly clever implementation could combine the two (methods),
> resorting to backtracking only to accommodate the backreferences.

---
