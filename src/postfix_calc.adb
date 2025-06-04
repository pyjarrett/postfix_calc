with Ada.Text_IO;

with Machines;

procedure Postfix_Calc with SPARK_Mode => Off is
   M : Machines.Machine;
begin
   Machines.Push (M, 20.0);
   Machines.Push (M, 30.0);
   Ada.Text_IO.Put_Line (Machines.Status (M)'Image);
end Postfix_Calc;
