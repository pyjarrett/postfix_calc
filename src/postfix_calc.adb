with Ada.Text_IO;

with Machines;
with Scanners;

procedure Postfix_Calc with SPARK_Mode => Off is
   M : Machines.Machine;
   S : Scanners.Scanner;
begin
   Machines.Push (M, 20.0);
   Machines.Push (M, 30.0);
   Ada.Text_IO.Put_Line (Machines.Status (M)'Image);

   loop
      declare
         Input : constant String := Ada.Text_IO.Get_Line;
         use type Scanners.Small_Int;
      begin
         exit when Input = "exit" or else Input = "quit";
         Scanners.Load_Input (S, Input);

         while Scanners.Remaining (S) > 0 loop
            Scanners.Next (S);
         end loop;
      end;
   end loop;
end Postfix_Calc;
