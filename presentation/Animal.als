/*
All clafers: 15 | Abstract: 1 | Concrete: 14 | References: 0
Constraints: 1
Goals: 0
Global scope: 24..*
All names unique: False
*/
open util/integer
pred show {}
run  show for 1 but 7 int, 9 c16_Torso, 36 c17_Leg, 36 c18_Feet, 4 c19_Cat, 9 c1_Animal, 24 c20_Whiskers, 3 c21_Rhino, 3 c22_Horn, 2 c23_Elephant, 2 c24_Trunk, 9 c2_Head, 18 c3_Eye, 18 c4_Ear, 9 c5_Mouth, 9 c6_Age

abstract sig c1_Animal
{ r_c2_Head : one c2_Head
, r_c6_Age : one c6_Age
, r_c16_Torso : one c16_Torso
, r_c17_Leg : set c17_Leg }
{ all disj x, y : this.@r_c6_Age | (x.@ref) != (y.@ref)
  4 <= #r_c17_Leg and #r_c17_Leg <= 4 }

sig c2_Head
{ r_c3_Eye : set c3_Eye
, r_c4_Ear : set c4_Ear
, r_c5_Mouth : one c5_Mouth }
{ one @r_c2_Head.this
  2 <= #r_c3_Eye and #r_c3_Eye <= 2
  2 <= #r_c4_Ear and #r_c4_Ear <= 2 }

sig c3_Eye
{}
{ one @r_c3_Eye.this }

sig c4_Ear
{}
{ one @r_c4_Ear.this }

sig c5_Mouth
{}
{ one @r_c5_Mouth.this }

sig c6_Age
{ ref : one Int }
{ one @r_c6_Age.this }

sig c16_Torso
{}
{ one @r_c16_Torso.this }

sig c17_Leg
{ r_c18_Feet : one c18_Feet }
{ one @r_c17_Leg.this }

sig c18_Feet
{}
{ one @r_c18_Feet.this }

fact { 4 <= #c19_Cat and #c19_Cat <= 4 }
sig c19_Cat extends c1_Animal
{ r_c20_Whiskers : set c20_Whiskers }
{ 6 <= #r_c20_Whiskers and #r_c20_Whiskers <= 6 }

sig c20_Whiskers
{}
{ one @r_c20_Whiskers.this }

fact { 3 <= #c21_Rhino and #c21_Rhino <= 3 }
sig c21_Rhino extends c1_Animal
{ r_c22_Horn : one c22_Horn }

sig c22_Horn
{}
{ one @r_c22_Horn.this }

fact { 2 <= #c23_Elephant and #c23_Elephant <= 2 }
sig c23_Elephant extends c1_Animal
{ r_c24_Trunk : one c24_Trunk }

sig c24_Trunk
{}
{ one @r_c24_Trunk.this }

