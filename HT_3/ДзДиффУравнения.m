(*сначала подгружу уравнения Навье-Стокса для вязкой несжимаемой \
жидкости из библиотеки Mathematica*)
(*Переменные u и v обозначают соответственно скорость течения по осям \
x и y, p - давление в жидкости*)
op = {
    Div[({{-mu, 0}, {0, -mu}}.Grad[u[x, y], {x, y}]), {x, y}] + 
     D[p[x, y], x],
    Div[({{-mu, 0}, {0, -mu}}.Grad[v[x, y], {x, y}]), {x, y}] + 
     D[p[x, y], y],
    D[u[x, y], x] + D[v[x, y], y]
    } /. mu -> 1;
pde = op == {0, 0, 0};


(*Создам расчетную область. Такая геометрия выбрана исходя из \
подобной задачи в курсе FlowVision, который мы изучаем в этом \
семестре*)
reg = RegionDifference[
   Rectangle[{0, 0}, {2, 1/2}],
   RegionUnion[
    Rectangle[{0.98, 0.}, {1.02, 0.15}],
    Rectangle[{0.98, 0.35}, {1.02, 0.5}]
    ]
   ];
(*Визуально оценю область:*)
RegionPlot[reg, AspectRatio -> Automatic]


(*Задам граничные условия задачи. Их 3 штуки:
Первое - задаю скорость потока на входе в трубу,
Второе - на стенках трубы задаю прилипание частиц,
Третье - условие свободного выхода жидкости - нулевое давление.
*)
bcs = {
   DirichletCondition[u[x, y] == 4*0.3*y*(0.5 - y)/(0.41)^2, x == 0.],
    DirichletCondition[{u[x, y] == 0., v[x, y] == 0.}, 0 < x < 2],
   DirichletCondition[p[x, y] == 0., x == 2]
   };


(*Все записанные условия использую в решателе ДУ и получаю \
интерполированные функции скорости и давления.
В параметрах функции задаю метод решения, порядок точности переменных \
и максимальный размер ячейки сетки (сетка создается автоматически на \
этом этапе)
*)
{xVel, yVel, pressure} = 
  NDSolveValue[{op == {0, 0, 0}, bcs}, {u, v, p}, {x, y} \[Element] 
    reg, Method -> {"FiniteElement", 
     "InterpolationOrder" -> {u -> 2, v -> 2, p -> 1}, 
     "MeshOptions" -> {"MaxCellMeasure" -> 0.0001}}];


(*ДУ решено, дальше только визуализация результатов!*)


(*График модуля скорости*)
DensityPlot[Sqrt[xVel[x, y]^2 + yVel[x, y]^2], {x, y} \[Element] reg, 
 AspectRatio -> Automatic, ColorFunction -> "TemperatureMap", 
 PlotLegends -> Automatic, PlotPoints -> 100]


(*График давления*)
DensityPlot[pressure[x, y], {x, y} \[Element] reg, 
 AspectRatio -> Automatic, ColorFunction -> "TemperatureMap", 
 PlotLegends -> Automatic, PlotPoints -> 100, Mesh -> 31, 
 MeshFunctions -> {#3 &}, MeshStyle -> {Black, Thickness[0.0008]}]


(*Векторное поле скоростей*)
Show[{
  RegionPlot[reg, AspectRatio -> Automatic],
  StreamPlot[{xVel[x, y], yVel[x, y]}, {x, y} \[Element] reg, 
   AspectRatio -> Automatic, StreamPoints -> Fine, 
   StreamStyle -> {Black, Thin}]
  }]
