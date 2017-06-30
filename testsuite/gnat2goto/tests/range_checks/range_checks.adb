procedure Range_Checks is

   subtype My_Type is Natural range 0 .. 20;

   n1, n2, n : My_Type;

   c : My_Type := -5; -- OK: illegal init => ASSERT FALSE

   function Add (A, B: My_Type; off : Boolean) return My_Type is
      ret  : My_Type := A + B; -- OK
      ret2 : My_Type;
   begin      
      --pragma Assert (A <= 20);
      ret2 := A + B + c; -- OK
      if off then
         ret2 := 5;
      end if;
      return c + c + c; -- OK
   end Add;

begin
   n1 := 10; -- RHS is seen as Natural. Why?
   n2 := n1 + n1 + n1; -- OK
   n2 := n2 + 1; -- OK
   n := Add (n1, n2, False);
end Range_Checks;
