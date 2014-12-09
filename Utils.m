(* ::Package:: *)

BeginPackage["Utils`"]
Needs["Global`"];
Unprotect[Part, Set, Piecewise, Delete, Orderless, Print]; 
Orderless[pattern_] := Alternatives @@ Permutations[pattern]
Delete[s_String, p_] := StringReplace[s, 
   HoldPattern[p] -> ""]
HoldPattern[{___, key_ -> value_, ___}[[key_]]] := value; 
KeyExist[map_List, key_String] := 
   Length[Cases[map, HoldPattern[key -> _]]] > 0; 
HoldPattern[(hashMap_)[[key_]] = value_] := 
  Append[hashMap, key -> value]
Sprite[img_Image, x_:0, y_:0] := 
  Module[{size = ImageDimensions[img], w, h}, 
   w = size[[1]]; h = size[[2]]; {Texture[ImageData[img]], 
     Polygon[MatrixForm[{{x, y}, {w, y}, {w, h}, {x, h}}], 
      VertexTextureCoordinates -> MatrixForm[
        {{0, 0}, {1, 0}, {1, 1}, {0, 1}}]]}]
Unfix[expr_] := Sequence @@ expr; 
MakeCookies[str_String] := 
   (Module[{pair = StringSplit[#1, "="]}, 
      {"Domain" -> ".", "Path" -> "/", "Secure" -> "FALSE", 
       "Expires" -> DateString[{2020, 1, 1, 0, 0, 0}], 
       "Name" -> RemoveWhiteSpace[First[pair]], 
       "Value" -> Last[pair]}] & ) /@ StringSplit[str, 
     ";"]; 
Piecewise[{cond___, {else_, else | otherwise}}, 0] := 
  Piecewise[{cond}, else]
JSON[data_] := ExportString[data, "JSON"]; 
ConvertAS3Matrix[{a_, b_, c_, d_, tx_, ty_}] := 
  {{a, c, tx}, {b, d, -ty}, {0, 0, 1}}
FlattenLv1[list_] := Flatten[list, 1]
RemoveWhiteSpace[str_] := StringReplace[str, 
   {Whitespace -> ""}]
JSONCompress[jsonStr_String] := StringReplace[jsonStr, 
   {Whitespace -> "", n:NumberString :> 
     Piecewise[{{StringTake[n, 8], StringLength[n] > 8}, 
       {n, otherwise}}]}]
Zero := 0 | 0.; 
ResolveAffineMatrix[{{a_, Zero, x_}, 
      {Zero, d_, y_}, {_, _, _}}] := 
   {"rotation" -> 0, "translation" -> {x, y}, 
    "scale" -> {a, d}}; 
ResolveAffineMatrix[{{Zero, c_, x_}, 
      {b_, Zero, y_}, {_, _, _}}] := 
   {"rotation" -> Pi/2, "translation" -> {x, y}, 
    "scale" -> {-c, b}}; 
ResolveAffineMatrix[{{a_, c_, x_}, {b_, d_, y_}, 
      {_, _, _}}] := Module[{r, rotation, scale}, 
    r = Piecewise[{{Pi/2, b == 0}, {ArcCot[a/b], 
         otherwise}}]; rotation = Piecewise[
       {{r + Pi, a < 0}, {r + 2*Pi, b < 0}, 
        {r, otherwise}}]; scale = Piecewise[
       {{{a, d}, Cos[rotation] == 0}, 
        {{a/Cos[rotation], d/Cos[rotation]}, otherwise}}]; 
     {"rotation" -> rotation, "translation" -> {x, y}, 
      "scale" -> scale}]; 
ExportImages[dir_, imgs_, format_:"PNG"] := 
   imgs /. (index_ -> img_) :> Export[StringJoin[dir, "/", 
       index, ".png"], img, "PNG"]; 
ImportImages[dir_] := FileNames["*.png", dir] /. 
    png_String :> StringCases[png, 
       __~~"\\"~~name__~~".png" :> name -> Import[png]][[
      1]]; 
BrowseFile[title_String:None]:=SystemDialogInput["FileOpen",WindowTitle->title];
BrowseDirectory[title_String:None]:=SystemDialogInput["Directory",WindowTitle->title];

EndPackage[]
