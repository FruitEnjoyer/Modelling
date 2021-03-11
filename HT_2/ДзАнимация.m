(*Загружу модельку челнока из базы Wolfram*)
shuttle = 
  ExampleData[{"Geometry3D", "SpaceShuttle"}, "GraphicsComplex"];


(*Выделю в отдельные переменные элементы модельки*)
points = shuttle[[1]];
faces = shuttle[[2]];


(*Создам функции преобразования координат точек*)

(*func[] поворачивает крылья шаттла*)
func[point_, angle_] := If[point[[2]] > 1.56,
  RotationTransform[angle, {1, 0, 0}, {0, 1.56, -1.}][point],
  If[point[[2]] < -1.56,
   RotationTransform[angle, {1, 0, 0}, {0, -1.56, -1.}][point],
   point
   ]
  ]


(*это функция реализации эффекта сминания корпуса при ударе о стену. \
х - координата стены*)
collision[point_, x_] := 
 If[point[[1]] < x, {x + RandomReal[{0, 1}], 
   point[[2]] + RandomReal[{-1, 1}], 
   point[[3]] + RandomReal[{-1, 1}]}, point]


(*чисто графическая функция - создаются вспышки от взрыва челнока :3*)


flashes[point_, x_] := 
 If[point[[1]] < x, {Opacity[0.5], 
   RandomChoice[{Yellow, Red, Orange}], 
   Sphere[{x + RandomReal[{0, 1}], point[[2]] + RandomReal[{-1, 1}], 
     point[[3]] + RandomReal[{-1, 1}]}]}
  , Nothing]


(*Создаю массив из 3D-кадров для Rainbow_Shuttle. Это ~ формат vtk*)
shots = Table[
   With[{color = ColorData["Rainbow"]},
    Graphics3D[
     {EdgeForm[],
      shuttle /. 
       GraphicsComplex[v_, r__] :> 
        GraphicsComplex[Map[func[#, angle] &, points], faces, 
         VertexColors -> ({color[#[[2]] + 0.5]} & /@ v)]}
     , PlotRange -> {{-10, 10}, {-7, 7}, {-7, 7}}
     ]
    ]
   , {angle, -Pi/3, Pi/3, Pi/90}];


(*...и аналогичный массив для столкновения*)
frames = Table[Graphics3D[
    {GraphicsComplex[Map[collision[#, x] &, points], faces],
     InfinitePlane[{x, 0, 0}, {{0, 1, 0}, {0, 0, 1}}],
     Map[flashes[#, x] &, points]
     }
    , PlotRange -> {{-5 + x, 20 + x}, {-7, 7}, {-7, 7}}, Axes -> True]
   , {x, -10, 10, 0.1}];


(*Экспортирую данные...*)
Export["C:\\Users\\Руслан\\Desktop\\Hollywood.gif", frames]
Export["C:\\Users\\Руслан\\Desktop\\Rainbow_Shuttle.gif", shots]
