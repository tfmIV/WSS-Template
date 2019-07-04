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
	Module[{resizedImage = ImageResize[img, width],imgGrid},
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

Pictogram[img_Image, numbers_List, opts:OptionsPattern[{"DisplayWidth"-> 500, "ImageWidth"->50, "Separation"->0, "TableHeadings"->None}]] := 
	Module[
		{imgAssociation,
		imgList, 
		magnifiedList, 
		separation = OptionValue["Separation"], 
		displayWidth = OptionValue["DisplayWidth"], 
		imgWidth = OptionValue["ImageWidth"],
		tableHeadings = OptionValue["TableHeadings"]
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
								#, Magnification->1
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