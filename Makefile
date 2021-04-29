#Drac Compiler main driver class

#Rubén Sánchez             A01021759
#Juan Francisco Gortarez   A01021926
#Martín Alegría            A01022216

drac.exe: Driver.cs Scanner.cs Token.cs TokenCategory.cs Parser.cs \
	SyntaxError.cs SpecificNodes.cs Node.cs

	mcs -out:drac.exe Driver.cs Scanner.cs Token.cs TokenCategory.cs Parser.cs \
	SyntaxError.cs SpecificNodes.cs Node.cs

clean:

	rm -f drac.exe
