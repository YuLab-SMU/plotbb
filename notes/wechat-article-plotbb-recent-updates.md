# 用 base 图也能“像 ggplot2 一样 + 叠加”：plotbb 最近更新了什么？

（面向：广泛 R 用户）

你可能已经会用 base graphics 画图：快、直接、自由。但一旦图变复杂，代码就容易变成“现场施工”：`par()` 改来改去、叠加顺序难控、加个标注还可能被坐标轴裁掉。

plotbb 的思路很简单：

- 把“画图”变成一个对象（`bbplot()`）
- 然后用 `+` 把图层、主题、标注一点点叠上去
- 最终仍然由 base graphics 渲染输出

最近这次更新，补齐了几类日常最常用、也最容易踩坑的能力。

---

## 一句话看懂：这次更新带来了什么

- 新增常用统计图层：箱线图 `bb_boxplot()`、直方图 `bb_hist()`
- 新增显著性标注：`bb_signif(comparisons = ...)`
- 新增预设主题：`bb_theme_bw()` 和 `bb_theme_minimal()`
- 叠加逻辑更稳：显著性标注会被纳入整体 y 范围计算，减少“画了但看不到”

【配图建议：一张对比图】左：传统 base 代码；右：plotbb 的 `bbplot() + layer` 链式写法。

---

## 1）常用统计图层补齐：箱线图、直方图

过去你可能更常用散点、线段这类几何对象。现在 `bb_boxplot()` 和 `bb_hist()` 也补上了，可以更自然地融入同一套“图层链”。

### 箱线图

```r
library(plotbb)

bbplot(mtcars, bb_aes(factor(cyl), mpg)) +
  bb_boxplot(fill = "grey90")
```

### 直方图

```r
bbplot(mtcars, bb_aes(mpg)) +
  bb_hist(col = "grey80", border = "white")
```

【配图建议：箱线图 + 直方图各一张】配一句：同一套语法，换图层即可。

---

## 2）显著性标注来了：bb_signif 支持 comparisons

很多人画箱线图的“最后一步”是显著性比较：两组/多组之间做检验，然后把结果标在图上。

plotbb 现在提供 `bb_signif()`，并支持用 `comparisons` 直接描述你要比较的组。

```r
bbplot(mtcars, bb_aes(factor(cyl), mpg)) +
  bb_boxplot(fill = "grey90") +
  bb_signif(comparisons = list(c("4", "6"), c("4", "8")))
```

这里有一个很真实、也很常见的痛点：显著性标注经常因为 y 轴范围不够而被裁掉，导致“我明明加了标注但看不到”。

这次更新之后，plotbb 会把显著性标注也纳入整体 y 范围计算，出图时自动留出空间，让结果更稳、更可见。

【配图建议：箱线图 + 显著性标注】最好放一个“以前会被裁掉、现在可见”的例子。

---

## 3）主题更像“开关”：bb_theme_bw 与 bb_theme_minimal

base 的 `par()` 很强，但当你想要一键切换整体风格时，手动改参数既繁琐也容易影响后续图。

plotbb 的 theme 更像是“对当前 plot 对象生效”的开关。这次新增了两个常用预设主题：

- `bb_theme_bw()`：偏论文风、对比清晰
- `bb_theme_minimal()`：更干净，适合展示和汇报

```r
p <- bbplot(mtcars, bb_aes(mpg, disp, col = factor(cyl))) +
  bb_point(pch = 19)

p + bb_theme_bw()
p + bb_theme_minimal()
```

---

## 4）更接近“可组合”的画图方式

这次更新之后，plotbb 的体验更接近你在 ggplot2 里习惯的工作流：

- 先把数据 + 映射变成一个 plot 对象
- 再把图层（boxplot/hist/signif…）、主题（bw/minimal…）、图例、分面逐步加上去
- 最终由 base graphics 渲染输出，但代码更易读、也更容易复用

如果你平时主要用 base，但又经常需要“多层叠加 + 统一风格 + 快速标注”，plotbb 这次的更新已经让它更适合进入日常工作流。

---

## 备选标题（10 个）

1. base R 画图终于能“加图层”了：plotbb 更新速览
2. 不换体系，也能做图层语法：plotbb 新增 boxplot/hist/signif
3. 显著性标注总是看不见？plotbb 现在会自动留出空间
4. bb_signif 上线：一行代码把 p 值标到 boxplot 上
5. 从 theme 到 layer：plotbb 把 base 图的可控性拉满了
6. 用 `+` 组装 base 图：plotbb 适合哪些人？
7. bb_theme_bw / minimal：给 base 图一键“清爽风”
8. 柱状图/箱线图/直方图：plotbb 的常用统计图层补齐了
9. 想保留 base 的自由，又想要语法化组合？试试 plotbb
10. 把“画图”拆成对象 + 图层：plotbb 的新用法示例

