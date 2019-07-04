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
	
distributer[img_Graphics|img_Graphics3D, numberOfItems_, imageSpacing_, displayWidth_] := 
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
		imgGrid
	];
	
createRow[img_Graphics|img_Graphics3D, numberItems_, separation_, displayWidth_, width_] :=
	Module[{imgGrid},
		imgGrid = 
			GraphicsGrid[
				distributer[
					img,
					numberItems,
					separation,
					displayWidth
				]
			];	
		imgGrid
	];

Pictogram[img_Image|img_Graphics3D|img_Graphics, numbers_List, opts:
	OptionsPattern[{"DisplayWidth"->500, "ItemWidth"->Automatic, "Separation"->0, "TableHeadings"->None, "Magnification"->1, "TableDirections"->Column, "Background"->White}]] := 
	Module[
		{imgAssociationMagnified,
		imgList, 
		magnifiedList, 
		separation = OptionValue["Separation"], 
		displayWidth = OptionValue["DisplayWidth"], 
		imgWidth = OptionValue["ItemWidth"],
		tableHeadings = OptionValue["TableHeadings"],
		tableDirections = OptionValue["TableDirections"],
		mag = OptionValue["Magnification"],
		bg = OptionValue["Background"]
		},
		(*How do I clean this up...nested map?*)
		imgAssociationMagnified = 
			Map[
				Magnify[#,mag]&/@(createRow[
					img, #, separation, displayWidth, imgWidth
				]->#)&, numbers
			];
		imgList = Keys[imgAssociationMagnified];
		magnifiedList = 
				{Map[
					Tooltip[
						Framed[
							#
						]
						,Lookup[imgAssociationMagnified,#]
					]&,
					imgList
				]};
		
		Style[
			TableForm[
				Transpose[
					magnifiedList
					],
				TableHeadings -> tableHeadings,TableDirections->Column
			],Background->bg
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