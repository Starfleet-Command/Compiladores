/*
  Drac compiler - CodePoints class.

  Rubén Sánchez             A01021759
  Juan Francisco Gortarez   A01021926
  Martín Alegría            A01022216
*/

using System;
using System.Collections.Generic;

class CodePoints {
    public IList<int> AsCodePoints(String str) {
        var result = new List<int>(str.Length - 2);
        for (var i = 1; i < (str.Length-1); i++) {
            
            
            if(str[i]=='\\' && ((i+1)<str.Length)){
                
                switch(str[i+1]){
                    case 'n':
                        result.Add(10);
                        break;
                    case 'r':
                        result.Add(13);
                        break;
                     case 't':
                        result.Add(9);
                        break;
                     case '\\':
                        result.Add(92);
                        break;
                     case '\'':
                        result.Add(39);
                        break;
                     case '\"':
                        result.Add(34);
                        break;
                    case 'u':
                        var unicodeChr = str.Substring(i+2, 6);
                        int decValue = Convert.ToInt32(unicodeChr, 16);
                        result.Add(decValue);
                        i+=6;
                        break;
                }
                i++;
            }
            else{
                result.Add(char.ConvertToUtf32(str, i));
                if (char.IsHighSurrogate(str, i)) {
                    i++;
                }
            }
        }
        return result;
    }
}