(* Pictogram lib definitions *)

distributer[img_Image, numberOfItems_, imageSpacing_, displayWidth_] := 
	Module[{
	lengthOfRows, 
	numberRows, 
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

createRow[img_Image, numberItems_, separation_, displayWidth_, width_] :=
	Module[{resizedImage,imgGrid},
		If[
			width != "Automatic", 
			resizedImage = ImageResize[img, width], 
			resizedImage = img
		];
		imgGrid = 
			Grid[
				distributer[
					resizedImage,
					numberItems,
					separation,
					displayWidth
				],
			ItemSize->{0,0},
			Spacings->{0,0}
			];
			
		Rasterize[imgGrid]
	]

Pictogram[img_Image, numbers_List, opts:OptionsPattern[{"DisplayWidth"-> 500, "ImageWidth"->Automatic, "Separation"->0, "TableHeadings"->None,"Magnification"->1}]] := 
	Module[
		{imgAssociation,
		imgList, 
		magnifiedList, 
		separation = OptionValue["Separation"], 
		displayWidth = OptionValue["DisplayWidth"], 
		imgWidth = OptionValue["ImageWidth"],
		tableHeadings = OptionValue["TableHeadings"],
		mag = OptionValue["Magnification"]
		},
		imgAssociation = 
			Map[
				(createRow[
					img, #, separation, displayWidth, imgWidth
				]->#)&, numbers
			];
		imgList = Keys[imgAssociation];
		magnifiedList = 
				{Map[
					Tooltip[
						Framed[
							Image[
								#, Magnification->mag
							]
						]
						,Lookup[imgAssociation,#]
					]&,
					imgList
				]};
		
		TableForm[
			Transpose[
				magnifiedList
				],
			TableHeadings -> tableHeadings
		]
	];	

interpretWord[str_String] :=
	SemanticInterpretation[str]["Image"]

createImage[str_String]:=
	Module[
		{rawImage}
		,
		rawImage = interpretWord[str];
		Return[interpretWord[str]]
		];