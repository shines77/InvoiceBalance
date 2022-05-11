# InvoiceBalance

## Introduction [English]

A small routine about the prices and amounts of invoice goods balance.

## 介绍 [Chinese]

### 趣味编程题 -- 发票凑单

这是我的一个会计朋友实际工作中遇到的问题，大概很多会计都会遇到这样的问题。

有任意 `N` 种商品，每种商品单价不同，数量未知，如下所示：

|  商品  |     单价    | 数量 |
|:------:|:-----------:|-----|
| 商品 1 |  212.00 元  | 未知 |
| 商品 2 |  172.50 元  | 未知 |
| 商品 3 |  226.00 元  | 未知 |
| 总计   | xxxxxx.xx 元 |    |

要求你调整每种商品的数量以及单价，让总价等于一个指定的值 `TotalPrice`，
例如：120000.00 元。

每种商品的单价最大可调整的范围为 +/- `Fluctuation`，例如：+/- 2.00 元。

每种商品的最小数量为 1 （不能等于 0 ），最大数量不限。

现在要求你写一个程序，求出每种商品的单价和数量，使其总价刚好等于指定的 `TotalPrice` 。

### 输入

输入的 `Ini 配置文件` 范例：

```ini
[Setting]
# 发票总金额，单位: 元
TotalPrice = 120000.00
# 单价允许浮动范围，单位: 元
Fluctuation = 2.00

[Goods]
# 物品的价格, 没用到的可以留空
Price1 = 212.00
Price2 = 172.50
Price3 = 226.00
Price4 =
```

### 输出

输出范例：

```text
  商品        数量          单价            合计
---------------------------------------------------------------

   1          126         212.09         26723.34
   2          243         172.10         41820.30
   3          227         226.68         51456.36

  总计                                  120000.00
---------------------------------------------------------------
  误差                                       0.00
```
