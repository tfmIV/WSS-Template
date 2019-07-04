(* Pictogram lib definitions *)
(* This is my first m file *)




distributer[img_Image, numberOfItems_, imageSpacing_, displayDimensions_List] := 
	Module[{
	lengthOfRows, 
	numberRows, 
	displayWidth = displayDimensions[[1]], 
	table, 
	row, 
	widthOfImage = ImageDimensions[img][[1]]
	},
	
	lengthOfRows = Floor[displayWidth/(widthOfImage + 2 * imageSpacing)];
	numberRows = Ceiling[numberOfItems/(lengthOfRows)];
	row = Table[img, lengthOfRows];
	table = Table[row, numberRows - 1];
	AppendTo[table, Table[img, numberOfItems - (numberRows - 1) * lengthOfRows]];
	Return[table]
	];

createRow[img_Image, numberItems_, separation_, displaySize_List, width_] :=
	Rasterize[
		Grid[
			distributer[
				ImageResize[img, width],
				numberItems,
				separation,
				displaySize
				],
			ItemSize->{0,0},
			Spacings->{0,0}
			]
		]

createTable[img_Image, separation_, displaySize_List, numbers_List, width_?NumericQ] := 
	Module[
		{imgList = createRow[img,#, separation, displaySize,width]&/@numbers}
		,
		
		TableForm[
			Transpose[{
				Map[
					Framed[Image[#, Magnification->1]]&,
					imgList
					]
				}],
			TableHeadings -> {
				{"a", "a", "a", "a"}
				, {"Pizza"}
				}
			]
	];
createTable[___] := Throw[$Failed]; 	

interpretWord[str_String] :=
	SemanticInterpretation[str]["Image"]

createImage[str_String]:=
	Module[
		{rawImage}
		,
		rawImage = interpretWord[str];
		Return[interpretWord[str]]
		];