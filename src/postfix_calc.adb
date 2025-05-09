with Calc;
with Ada.Text_IO;

procedure Postfix_Calc is
   package AIO renames Ada.Text_IO;
   Process_Result : Calc.Machine_Status;
begin
   Process_Result := Calc.Process ("1 2 +");
   AIO.Put_Line (Process_Result'Image);
end Postfix_Calc;
