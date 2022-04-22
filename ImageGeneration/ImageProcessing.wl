(* ::Package:: *)

PrepareImage[image_,size_:500]:=ColorConvert[ImageResize[image,size],"HSB"]


CollorCorrelatedWithBrightnessAvg[BaseHue_:.6,HueRandomnessScale_:.25,SaturationRandomnessScale_:.05,LightnessRandomnessScale_:.2]:=Module[{L},
(*

BaseHue                  : The base hue that the image will be based on
HueRandomnessScale       : The hue is determined by the base hue and some randomness. This determines how much impact the randomness has. Additionaly the randomnes becomes more pronounced at higher lightnesses.
SaturationRandomnessScale: A number between 0 and 1 that determines how randomized the saturation is.
LightnessRandomnessScale : A number between 0 and 1 that determines how randomized the lightness is.

Returns a function that takes in a lightness and gives a color 
*)
	Function[{l}, 
        
	(* If the lightness is zero set it to a random number between 0 and .1 so the result is less plain*)
	If[l==0,
		L=RandomReal[]/10,
		L=l];
	(* Return the color determined by the given lightness *)
	Hue[
		{BaseHue+L*Random[]*HueRandomnessScale,
		(1-((1-SaturationRandomnessScale)L+SaturationRandomnessScale Random[])),
		(1-LightnessRandomnessScale)L+LightnessRandomnessScale Random[]}
		]
		]
]


ChoosePoints[image_]:=Module[{},
(*
image : The image to detect the edges and corners of

This function displays a small UI that lets you choose the threshold for corner detection and radius for edge detection. Drag these sliders untill there are only lines and points whhich you would like to be a part of the triangulation.
*)
Manipulate[
	edges = EdgeDetect[image,edgeRadius];
	corners = ImageCorners[image,MaxFeatures->maxFeatures];
	HighlightImage[ImageAdd[image,edges],corners],{{edgeRadius,2.7},0,10},{{maxFeatures,200},0,4000}]
]


GetFacePoints[image_]:=Module[{w,h,nosePoint,features,KeyFacePoints},
	{w,h}=ImageDimensions[image];
	(*Set the default nose poiunt as the center of the image*)
	nosePoint = {w/2,h/2};
	features = {"LeftEyebrowPoints","LeftEyePoints","MouthExternalPoints","MouthInternalPoints","NosePoints","OutlinePoints","RightEyebrowPoints","RightEyePoints"};
	
	(*Extract the face points*)
	KeyFacePoints = Flatten[Table[FacialFeatures[image,feature],{feature,features}],2];
	nosePoint = FacialFeatures[image,"NosePoints"][[1]][[4]];
	{KeyFacePoints,nosePoint}
]


GetCorners[image_]:=Module[{w,h},
	{w,h}=ImageDimensions[image];
	{{0,0},{w,0},{0,h},{w,h}}
]


removeAdjacentPoints[pts_,minSep_]:=Union[pts,SameTest->(Norm[#1-#2]<minSep*((Norm[#1- NosePoint])/(Norm[#1- {500,0}]))&)];


ColorImageMesh[image_,imageMesh_,colorMap_]:=Graphics[Table[{colorMap[ImageValue[image,Mean@@p][[1]]],p},{p,MeshPrimitives[imageMesh,2]}]]
