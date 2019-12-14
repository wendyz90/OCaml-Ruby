open SmallCTypes


let tokenize input = 
 
  let rec token pos inputs =
    if pos >= String.length inputs then
      [EOF]
    else begin
      if (Str.string_match (Str.regexp "(") inputs pos) then
        Tok_LParen::(token (pos+1) inputs)
        
      else if (Str.string_match (Str.regexp ")") inputs pos) then
        Tok_RParen::(token (pos+1) inputs)
     
      else if (Str.string_match (Str.regexp "{") inputs pos) then
        Tok_LBrace::(token (pos+1) inputs)
 
      else if (Str.string_match (Str.regexp "}") inputs pos) then
        Tok_RBrace::(token (pos+1) inputs)
      
      else if (Str.string_match (Str.regexp "==") inputs pos) then
        Tok_Equal::(token (pos+2) inputs)
        
      else if (Str.string_match (Str.regexp "!=") inputs pos) then
        Tok_NotEqual::(token (pos+2) inputs)
        
      else if (Str.string_match (Str.regexp "=") inputs pos) then
        Tok_Assign::(token (pos+1) inputs)
      
      else if (Str.string_match (Str.regexp ">") inputs pos) then
        Tok_Greater::(token (pos+1) inputs)
      
      else if (Str.string_match (Str.regexp "<") inputs pos) then
        Tok_Less::(token (pos+1) inputs)  
        
      else if (Str.string_match (Str.regexp ">=") inputs pos) then
        Tok_GreaterEqual::(token (pos+2) inputs)  
      
      else if (Str.string_match (Str.regexp "<=") inputs pos) then
        Tok_LessEqual::(token (pos+2) inputs)
      
      else if (Str.string_match (Str.regexp "||") inputs pos) then
        Tok_Or::(token (pos+2) inputs)
      
      else if (Str.string_match (Str.regexp "&&") inputs pos) then
        Tok_And::(token (pos+2) inputs)    
        
      else if (Str.string_match (Str.regexp "!") inputs pos) then
        Tok_Not::(token (pos+1) inputs)
      
      else if (Str.string_match (Str.regexp ";") inputs pos) then
        Tok_Semi::(token (pos+1) inputs)     
      
      else if (Str.string_match (Str.regexp "int ") inputs pos) then
        Tok_Int_Type::(token (pos+4) inputs)         
      
      else if (Str.string_match (Str.regexp "bool ") inputs pos) then
        Tok_Bool_Type::(token (pos+5) inputs)    
 
      else if (Str.string_match (Str.regexp "printf ") inputs pos) then
        Tok_Print::(token (pos+7) inputs)  

      else if (Str.string_match (Str.regexp "printf(") inputs pos) then
        Tok_Print::(Tok_LParen::(token (pos+7) inputs))
        
      else if (Str.string_match (Str.regexp "main ") inputs pos) then
        Tok_Main::(token (pos+5) inputs)    
      else if (Str.string_match (Str.regexp "main(") inputs pos) then
        Tok_Main::(Tok_LParen::(token (pos+5) inputs))
        
      else if (Str.string_match (Str.regexp "if ") inputs pos) then
        Tok_If::(token (pos+3) inputs)           
      else if (Str.string_match (Str.regexp "if(") inputs pos) then
        Tok_If::(Tok_LParen::(token (pos+3) inputs))    
        
      else if (Str.string_match (Str.regexp "else") inputs pos) then
        Tok_Else::(token (pos+4) inputs)         
      else if (Str.string_match (Str.regexp "else(") inputs pos) then
        Tok_Else::(Tok_LParen::(token (pos+5) inputs))
        
      else if (Str.string_match (Str.regexp "while ") inputs pos) then
        Tok_While::(token (pos+6) inputs)           
      else if (Str.string_match (Str.regexp "while(") inputs pos) then
        Tok_While::(Tok_LParen::(token (pos+6) inputs))    
        
      else if (Str.string_match (Str.regexp "+") inputs pos) then
        Tok_Add::(token (pos+1) inputs)           
      
      else if (Str.string_match (Str.regexp "-") inputs pos) then
        Tok_Sub::(token (pos+1) inputs)
 
      else if (Str.string_match (Str.regexp "*") inputs pos) then
        Tok_Mult::(token (pos+1) inputs)           
 
      else if (Str.string_match (Str.regexp "/") inputs pos) then
        Tok_Div::(token (pos+1) inputs)
 
      else if (Str.string_match (Str.regexp "\\^") inputs pos) then
        Tok_Pow::(token (pos+1) inputs)           
      
      else if (Str.string_match (Str.regexp "true ") inputs pos) then
        Tok_Bool(true)::(token (pos+5) inputs)    
        
      else if (Str.string_match (Str.regexp "false ") inputs pos) then
        Tok_Bool(false)::(token (pos+6) inputs)      
 
      else if (Str.string_match (Str.regexp "[-]?[0-9]+") inputs pos) then
        let temp = Str.matched_string inputs in
        let new_pos = Str.match_end () in
        (Tok_Int(int_of_string temp))::(token new_pos inputs)           
 
      else if (Str.string_match  (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") inputs pos) then
        let temp = Str.matched_string inputs in
        let new_pos = Str.match_end () in
        (Tok_ID temp)::(token new_pos inputs)
      else if (Str.string_match (Str.regexp "[ \t\n]") inputs pos) then
      token (Str.match_end ()) inputs    
      else
        raise (InvalidInputException "invalid input")
     end
  in      
  
        
  token 0 input
  ;;