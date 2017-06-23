procedure Range_Checks is

   subtype My_Type is Natural range 0 .. 20;

   n1, n2, n : My_Type;

   c : My_Type := -5; -- illegal init

   function Add (A, B: My_Type) return My_Type is
      ret : My_Type := A+B; -- BUG: not being checked
      ret2 : My_Type;
   begin
      
      ret2 := A+B+c; -- OK
      return ret;
      --return A+B+c; -- BUG: no assert being generated!
   end Add;

begin
   n1 := 10;
   n2 := n1 + n1 + n1; -- OK: assert being generated
   n2 := n2 + 1; -- OK: assert being generated
   n := Add (n1, n2);
end Range_Checks;
