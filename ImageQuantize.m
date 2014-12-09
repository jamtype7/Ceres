(* ::Package:: *)

BeginPackage["ImageQuantize`"]

<<"https://raw.githubusercontent.com/jamtype7/Ceres/master/ConfigUtils.m";
Needs["ConfigUtils`"];
Needs["Global`"]

pngQuant=RequireExternalTool["pngquant.exe"]; 
ImageAutoQuantize[file_String, q_:75, dit_:True, 
   override_:True] := Run[StringJoin[pngQuant, " --ext .png", 
    Piecewise[{{" --force", override}, {"", True}}], 
    " --verbose --quality=0-", IntegerString[q], 
    Piecewise[{{" --nofs ",  !dit}, {" ", True}}], "\"", 
    Piecewise[{{file, override}, {file, True}}], "\"", ""]]
QuantizeImageFolder[dir_, q_:75, dit_:True] := 
  (ImageAutoQuantize[#1, q, dit] & ) /@ FileNames["*.png", dir]

EndPackage[]



