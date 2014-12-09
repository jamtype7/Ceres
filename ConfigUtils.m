(* ::Package:: *)

BeginPackage["ConfigUtils`"]
<<"https://raw.githubusercontent.com/jamtype7/Ceres/master/Utils.m";
Needs["Utils`"];

RequireExternalTool[name_String] := 
   Module[{allConfig, configFilePath, LoadConfig, ExportConfig, 
     GetConfig, BrowseFile}, configFilePath = StringJoin[Directory[], 
       "\\_ExternalToolConfig.json"]; LoadConfig := 
      Piecewise[{{Import[configFilePath], FileExistsQ[configFilePath]}, 
        {{}, otherwise}}]; allConfig = LoadConfig; 
     ExportConfig[config_] := Export[configFilePath, config, "JSON"]; 
     BrowseFile := SystemDialogInput["FileOpen", 
       WindowTitle -> StringJoin["\:8bf7\:9009\:62e9", name]]; 
     If[ !KeyExist[allConfig, name], allConfig = allConfig[[name]] = 
        BrowseFile]; ExportConfig[allConfig]; allConfig[[name]]]; 
EndPackage[]
