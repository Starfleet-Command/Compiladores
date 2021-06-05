#Drac Compiler main driver class

#Rubén Sánchez             A01021759
#Juan Francisco Gortarez   A01021926
#Martín Alegría            A01022216

drac.exe: Driver.cs Scanner.cs Token.cs TokenCategory.cs Parser.cs \
	SyntaxError.cs SpecificNodes.cs Node.cs SemanticVisitor.cs SemanticError.cs \
	WatVisitor.cs CodePoints.cs

	mcs -out:drac.exe Driver.cs Scanner.cs Token.cs TokenCategory.cs Parser.cs \
	SyntaxError.cs SpecificNodes.cs Node.cs SemanticVisitor.cs SemanticError.cs \
	WatVisitor.cs CodePoints.cs

clean:

	rm -f drac.exe
