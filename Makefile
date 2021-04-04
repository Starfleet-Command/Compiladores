#Drac Compiler main driver class

#Rubén Sánchez             A01021759
#Juan Francisco Gortarez   A01021926
#Martín Alegría            A01022216

drac.exe: Driver.cs Scanner.cs Token.cs TokenCategory.cs \
	SyntaxError.cs

	mcs -out:drac.exe Driver.cs Scanner.cs Token.cs TokenCategory.cs \
	SyntaxError.cs

clean:

	rm -f drac.exe
