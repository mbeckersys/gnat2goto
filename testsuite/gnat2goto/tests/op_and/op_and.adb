procedure Op_And is
   z, a, b : Boolean;
   n : Integer := Integer'Last;
begin
   b := True;
   a := False;
   z := a and b;

   if z then
      n := n + 1;
   end if;
end Op_And;
