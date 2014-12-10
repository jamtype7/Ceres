(* ::Package:: *)

BeginPackage["Animation`"]
<<"https://raw.githubusercontent.com/jamtype7/Ceres/master/Utils.m";
Needs["Utils`"];
Needs["Global`"];

FrameData := {"alpha" -> _?NumberQ, "filters" -> _List, "imageIndex" -> _String, "transform" -> _List}; 
NormalizeFrameData[frames_] := Module[{indexDic, defaultValues}, 
    indexDic = {"alpha" -> "a", "filters" -> "f", "imageIndex" -> "i", "transform" -> "t"}; 
     defaultValues = {("alpha" -> "a") :> "alpha" -> 1, ("filters" -> "f") :> "filters" -> {}}; 
     indexDic /. (frames /. {} -> Sequence[]) /. defaultValues /. ("transform" -> t_) :> 
         "transform" -> ConvertAS3Matrix[t] /. ("imageIndex" -> i_) :> "imageIndex" -> ToString[i] /. 
      {"alpha" -> 1, "filters" -> {}, "imageIndex" -> "i", "transform" -> ConvertAS3Matrix["t"]} :> {}]; 
Resize[frames_, images_, scale_] := Module[{localFrames, IndeiesOf, tolerance, threshold, resized, 
     resizedImages, resizedFrames, IndexOfImg, FramesOfImg, d1, d2, d3, FindMaxScale}, 
    tolerance = 0.1; threshold = 1/scale - tolerance; IndexOfImg[img_Image] := 
      Cases[images, (index_ -> img) :> index][[1]]; FramesOfImg[i_Image] := 
      Cases[frames, {___, "imageIndex" -> IndexOfImg[i], ___}, Infinity]; 
     FindMaxScale[s_] := {Max[s[[All,1]]], Max[s[[All,2]]]}; IndeiesOf[collection_] := 
      DeleteDuplicates[Cases[collection, (i_ -> _) :> i]]; 
     d1 = (FindMaxScale /@ #1 & )[(Cases[frames, {___, "imageIndex" -> #1, ___, "transform" -> t_, ___} :> 
            Abs[ResolveAffineMatrix[t][["scale"]]], Infinity] & ) /@ IndeiesOf[images] /. 
        {} -> Sequence[]]; d2 = MapThread[List, {images, d1}]; 
     d3 = Cases[d2, {_, {___, s_, ___}} /; s < threshold]; d3 = d3 /. Zero :> 1.*^-7; 
     Print["threshold:", threshold]; resized = 
      d3 /. {idx_ -> i_Image, s_} :> {i, s, (Piecewise[{{1, #1 > 1}, {#1, True}}] & ) /@ (s*scale), 
          s*scale} /. {i_Image, s_, s1_, s2_} :> Module[{newSize}, newSize = ImageDimensions[i]*s1; 
          newSize = newSize /. r_Real /; r < 1 :> 1; {i, ImageResize[i, newSize, Resampling -> "Nearest"], 
           ScalingTransform[((1/scale)*(s2/s1))/s][[1]]}]; 
     resizedImages = images /. (resized /. {o_Image, n_Image, t_} :> o -> n); resizedFrames = frames; 
     resized /. {o_Image, n_Image, t_} :> Module[{fo = FramesOfImg[o], r}, 
        r = MapThread[Rule, {fo, fo /. ("transform" -> ot_) :> "transform" -> ot . t}]; 
         resizedFrames = resizedFrames /. r]; {"frames" -> resizedFrames, "images" -> resizedImages}]; 
AnimationData := Orderless[{"className" -> _String, "frameLabels" -> _List, "frames" -> {{FrameData..}..}, 
     "scale" -> _?NumberQ}]; 
NormalizeAnimationData[animData_] := animData /. ("frames" -> frames_List) :> 
     "frames" -> NormalizeFrameData[HandleEmptyFrames[frames]]; 
NormalizeImages[images_] := Join[images, {"___blank" -> Image[{{{0, 0, 0, 0}}}, ColorSpace -> "RGB"]}]; 
HandleEmptyFrames[frames_] := Piecewise[{{frames /. {} :> {{"i" -> "___blank", "t" -> {1, 0, 0, 1, 0, 0}}}, 
      Length[Cases[frames, {}]] > 0}, {frames, otherwise}}]; 
HaveEmptyFrame[frames_] := Length[Cases[frames, HoldPattern["imageIndex" -> "___blank"], Infinity]] > 0; 
GetFrameLabelInfo[anim:AnimationData] := Module[{actionFramePattern, frameLabels, actionFrameLabels, 
    GetFrameIndex, normalFrameLabels}, 
   actionFramePattern = frameName_String /; StringMatchQ[frameName, "do_"~~__] -> _List; 
    frameLabels = GatherBy[anim[["frameLabels"]], MatchQ[#1, actionFramePattern] & ]; 
    actionFrameLabels = Piecewise[{{Last[frameLabels], Length[frameLabels] > 1}, {{}, otherwise}}]; 
    GetFrameIndex[info_] := info /. (_String -> {index_}) :> index; 
    normalFrameLabels = Sort[First[frameLabels], GetFrameIndex[#1] < GetFrameIndex[#2] & ]; 
    If[Length[normalFrameLabels] == 0, normalFrameLabels = {"defaultAnim" -> {1}}]; 
    normalFrameLabels = normalFrameLabels //. {x___, frameName_ -> {i_Integer}, 
         nextFrame:(_ -> {j_Integer}), z___} :> {x, frameName -> {i, j - 1}, nextFrame, z} /. 
      (lastFrame_ -> {i_Integer}) :> lastFrame -> {i, Length[anim[["frames"]]]}; 
    {"frameLabels" -> normalFrameLabels, "events" -> actionFrameLabels}]
SeprateAnimation[anim:AnimationData] := SeprateAnimation[anim, {}]; 
SeprateAnimation[anim:AnimationData, groups_List] := 
  Module[{frames, groupRange, newGroupRange, frameLabelInfo, frameLabels, newFrameLabels, 
    locGroups = groups}, frameLabelInfo = GetFrameLabelInfo[anim][["frameLabels"]]; 
    If[Length[locGroups] == 0, locGroups = Cases[frameLabelInfo, label_String :> {label}, Infinity]]; 
    frameLabels = anim[["frameLabels"]]; Print["frame label info: ", frameLabelInfo]; 
    groupRange = locGroups /. name_String :> frameLabelInfo[[name]]; 
    newGroupRange = groupRange /. {x___, {h_Integer, i_}, {j_, k_}, z___} :> 
        {x, {h, i}, {i + 1, k - j + i + 1}, z} /. l:{{_Integer, _}..} :> l - (l[[1,1]] - 1); 
    newFrameLabels = Module[{i = 0, g = Flatten[locGroups]}, newGroupRange /. 
       {j_Integer, _} :> (i++; g[[i]] -> j)]; 
    frames = Apply[Join, groupRange /. {begin_Integer, end_} :> anim[["frames"]][[begin ;; end]], {1}]; 
    MapThread[{"frames" -> #1, "frameLabels" -> #2, "className" -> 
        StringJoin[Riffle[Join[{anim[["className"]]}, Cases[locGroups, _String, Infinity]], "_"]], 
       "scale" -> anim[["scale"]]} & , {frames, newFrameLabels}]]

EndPackage[]
