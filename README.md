# просто моя реализация лямбда-исчисления.
## само собой, медленная
# синтаксис
Терм окружается скобками только если это набор нескольких термов ("Аппликация"). Иначе, скобки не ставятся. Также, L вместо лямбды.
Lxyz.T ~ Lx.Ly.Lz.T ~ Lx.(Ly.(Lz.(T)))
Также, L.yx.xy означает (Lyx.x)y.
В остальном, всё как обычно.
т.к. пока нет автопереименования переменных, выражения типа Lxx.(xx) могут существовать. Для разбора можно включить опцию, которая будет показывать, на сколько уровней вверх ссылается переменная:

 +-------+
 | +--+  |
 | |  |  |
 V V  |  |
Lxyx.(x'2x'4y'3)
  ^         |
  |         |
  +---------+
 
# основные выражения
I = Lx.x. Ia -> a
K = Lab.a ~ True
KI = Lab.b ~ False
M = Lx.(xx)
OMEGA = M M. OMEGA->OMEGA->OMEGA->OMEGA->...
C = Lxyz.(xzy) ~ not
# числа
Если представить лямбду N такую, что Nf ->   f^n (в смысле, n-кратное применение функции), то:
n = 0: Lfx.x (id)
n = 1: Lfx.fx
n = 2: Lfx.f(fx)
n = 3: Lfx.f(f(fx))
...
для чисел можно определить операцию следуюшего числа:
SUCC = Lwyx.(y(wyx))
И (господи) предидущего:
PRED = Lnfx.(n(Lgh.(h(gf)) h Lu.x I))
а также, сложения, умножения и возведения в степень:
PLUS = Lmnfx.(mf(nfx))
MULT = Lmnf.(m(nf))
POW = Lbe.(eb)
Кстати, забавно, что описание степень проще умножения, а умножение проще сложения.
и не помешает ещё проверка числа на натуральность. Конечно, отрицательных чисел нет, но есть же ноль:
ISZERO = Ln.(n(x False)True)
# логика
Помимо булевых значений и оператора "не" стоит добавить оператор "и" и "или"
AND = Lqp.(pqp)
AND = Lqp.(ppq)
IF = Lpab.(pab),   IF A B C ~ "A ? B : C"
# рекурсия
Y = Lg.(Lx.(g(xx))Lx.(g(xx)))
Данный терм обладает интересным свойством:
YQ = Q(YQ)
Зная это (и то, что бета-редукция происходит в порядке обхода в ширину), можно реализовать вычисление рекурсивных функций и циклов. Вот, например, факториал:
Lg.(Lx.(g(xx))Lx.(g(xx)))Lrn.(Lpab.(pab)(Ln.(nLxab.bLab.a)n)Lfx.(fx)(Lmnf.(m(nf))n(r(Lnfx.(nLgh.(h(gf))Lu.xLa.a)n))))
Если немного упростить, то:
G = L.rn(IF (ISZERO n) 1 (MULT n r (PRED n)))
FACT = Y G




