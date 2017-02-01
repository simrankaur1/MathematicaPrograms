(* Wolfram Language Package *)

BeginPackage["TestCreater`"]
(* Exported symbols added here with SymbolName::usage *)
generateServiceRequests ::usage = "generateServiceRequests[] generate set of requests for given service"
customExport ::usage = "customExport[file_,expr_,JSON] exports service requests data to json file"
customImport ::usage = "customExport[file_,JSON] imports service requests data from json file"

Begin["`Private`"] (* Begin Private Context *)

flickrdata = {
   "ServiceName" -> "Flickr",
   "Requests" -> {
     {"Name" -> "ImageSearch", "RequiredParameters" -> {}, 
      "OptionalParameters" -> {"User" -> {"testwolfram13", 
          "testwolfram1"}, 
        "Keywords" -> {"sunset", "casino", "waterfall"}, 
        "DateTaken" -> {{DateObject[{2015, 1, 1}], 
           Today}, {DateObject[{2015, 4, 4}], Today}}, 
        "MaxItems" -> {2, 10, 15, 20}, 
        "Description" -> {"canyon", "water", "nature"}, 
        "DateUploaded" -> {{DateObject[{2015, 1, 1}], 
           Today}, {DateObject[{2015, 4, 4}], Today}}, 
        "Location" -> {Entity[
           "City", {"Miami", "Florida", "UnitedStates"}], 
          Entity["City", {"LasVegas", "Nevada", "UnitedStates"}]}, 
        "Elements" -> {"Data", "LinkedThumbnails", "Images"}, 
        "ImageSize" -> {"Small", "Medium", "Large"}, 
        "Format" -> {"Dataset"}}
      },
     {"Name" -> "AlbumImages", 
      "RequiredParameters" -> {"AlbumID" -> {"72157644874295829", 
          "72157626881403859", "72157621364913858"}}, 
      "OptionalParameters" ->{"MaxItems" -> {2, 10, 15, 20}, 
 		"Elements" -> {"Data", "Images", "LinkedThumbnails"}, 
 		"Format" -> {"Dataset"}, "ImageSize" -> {"Small", "Medium", "Large"}}
      }
     }
   };
   
$EXPRWRAPPER = stringifiedExpression;

(*helper functions*)
validExpressionAsJSONDataQ[expr_?AtomQ] := 
 MatchQ[expr, Null | _String | _Integer | _Real];
 
validExpressionAsJSONDataQ[expr_] := 
 MatchQ[expr, {___?validExpressionAsJSONDataQ}];
 
expressionToString[a : Null | _String | _Integer | _Real] := a;

expressionToString[exprList_List] := expressionToString /@ exprList;

expressionToString[expr_Rule] := ruleParse[expr];

expressionToString[expr : Except[_List]] := 
  ToString[$EXPRWRAPPER[expr], InputForm, 
   CharacterEncoding -> "ASCII"];
   
stringToExpression[str_String] := 
 With[{regex = 
    RegularExpression[ToString[$EXPRWRAPPER] <> "\\[.*\\]"]}, 
  Replace[str, 
   s_?(StringMatchQ[#, regex] &) :> (ToExpression[
       s] /. $EXPRWRAPPER -> Identity)]]
       
   
ruleParse[expr_Rule] :=
  (
   expr[[1]] -> 
    Replace[expr[[2]], 
     rhs : Except[_?validExpressionAsJSONDataQ] :> 
      expressionToString[rhs]]
   );
   
(*Define function generating parameters randomly*)
randomParamGen[rparams_,oparams_, count_Integer] := Module[{orandomkeys,orandomvalues,reqrandomkeys,reqrandomvalues},   	
  		orandomkeys = RandomSample[Keys[oparams], count];
  		orandomvalues = Map[RandomChoice[oparams[#]]&, orandomkeys];
  		If[Length[rparams]=!=0,
	  		reqrandomkeys = Keys[rparams];
	  		reqrandomvalues = Map[RandomChoice[rparams[#]]&, reqrandomkeys];
	  		(*finally join the required and optional params*)
	  		Normal@AssociationThread[Join[reqrandomkeys,orandomkeys], Join[reqrandomvalues,orandomvalues]]
	  		,
	  		Normal@AssociationThread[orandomkeys, orandomvalues]
  		]
  ];
  
randomParamGen[sname_String,request_] := Module[{rname,rparams,oparams,len,arg},
	rname = "Name"/.request;
	rparams = Association@"RequiredParameters"/.request;
	oparams = Association@"OptionalParameters"/.request;
	len = Length[oparams];
	{sname,rname,arg = randomParamGen[rparams,oparams,#]}&/@Range@len
];
 
generateServiceRequests[servicedata_List] := Module[{sname,requests,requestset={}},
	sname = "ServiceName"/.servicedata;
	requests = "Requests"/.servicedata;
	(*generating dynamic requests for a given service*)
	AppendTo[requestset,randomParamGen[sname,#]]&/@requests;
	Flatten[requestset,1]
];

 (*Need to see if today symbol can be holded at all!*)
generateServiceRequests[file_String] := Module[{servicedata,sname,requests,requestset={}},
	(*first import the data*)
	servicedata = customImport[file,"JSON"];
	sname = "ServiceName"/.servicedata;
	requests = "Requests"/.servicedata;
	(*generating dynamic requests for a given service*)
	AppendTo[requestset,randomParamGen[sname,#]]&/@requests;
	Flatten[requestset,1]
];

(*Custom export function  exports stringified mathematica expressions*)
customExport[file_, expr_, "JSON"] :=
  Composition[Export[file, #, "JSON"] &, Map[ruleParse[#] &, #] &][expr];
  
customImport[file_, "JSON"] := 
  Composition[Sort, 
    With[{regex = 
        RegularExpression[ToString[$EXPRWRAPPER] <> "\\[.*\\]"]}, 
      Map[#[[1]] -> 
         ReplaceAll[#[[2]], 
          rhs_String?(StringMatchQ[#, regex] &) :> 
           stringToExpression[rhs]] &, #]] &, Import[#, "JSON"] &][file];
  

End[] (* End Private Context *)

EndPackage[]