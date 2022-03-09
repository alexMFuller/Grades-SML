datatype grade = A | B | C | D | F;

datatype 'a tree = 
   Leaf |
   Node of 'a tree * 'a * 'a tree;

fun percent2grade x = 
  if x>= 90.0 then A:grade
  else if (x>= 80.0 andalso x<90.0) then B
  else if (x>= 70.0 andalso x<80.0) then C
  else if (x>= 60.0 andalso x<70.0) then D
  else F;

val test = percent2grade 85.0;

fun grade2point x =
  if x = A then 4.0
  else if x = B then 3.0
  else if x = C  then 2.0
  else if x = D then 1.0
  else 0.0;

val test2 = grade2point A;

fun gpa x = (foldr (op+) 0.0 (map(fn z => grade2point z) x) ) / Real.fromInt(length x);

val test3 = gpa [A, B, C, D, A, B, A];

fun gpaFromPercent x = gpa (map(fn z => percent2grade z) x);

val test4 = gpaFromPercent [67.0, 77.0, 84.0, 99.0];
