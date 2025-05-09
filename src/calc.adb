with Ada.Text_IO;

package body Calc is
   Max_Tape_Length : constant := 512;

   type Tape is limited record
      Contents : String (1 .. Max_Tape_Length);
      Start    : Positive;
      Cursor   : Positive;
      Length   : Positive;
   end record;

   type Machine_Op is (Add, Sub, Mul, Div, Print);

   type Machine is limited record
      Status : Machine_Status;
      Input  : Tape;
   end record;

   type Cell_Kind is (Cell_Value, Cell_Op, Cell_Empty);

   type Cell (Kind : Cell_Kind := Cell_Empty) is record
      case Kind is
         when Cell_Value =>
            Value : Float;

         when Cell_Op =>
            Op : Machine_Op;

         when Cell_Empty =>
            null;
      end case;
   end record;
   type Cell_Array is array (Positive range <>) of Cell;

   Max_Stack_Size : constant := 256;
   type Stack is limited record
      Contents : Cell_Array (1 .. Max_Stack_Size);
   end record;

   ---

   function Make_Tape (Contents : String) return Tape is
   begin
      return T : Tape do
         T.Contents (1 .. Contents'Length) := Contents;
         T.Start := 1;
         T.Cursor := 1;
         T.Length := Contents'Length;
      end return;
   end Make_Tape;

   function Is_At_End (A_Tape : Tape) return Boolean is
   begin
      return A_Tape.Cursor = A_Tape.Contents'Length;
   end Is_At_End;

   procedure Next (A_Tape : in out Tape) is
   begin
      if not Is_At_End (A_Tape) then
         A_Tape.Cursor := A_Tape.Cursor + 1;
      end if;
   end Next;

   function Take (A_Tape : in out Tape) return String is
   begin
      return Result : String := A_Tape.Contents (A_Tape.Start .. A_Tape.Cursor)
      do
         A_Tape.Start := A_Tape.Cursor;
      end return;
   end Take;

   ---

   function Process_Tape
     (A_Tape : in out Tape; A_Stack : in out Stack) return Machine_Status is
   begin
      pragma Unreferenced (A_Stack);
      while not Is_At_End (A_Tape) loop
         Next (A_Tape);
      end loop;
      return Ok;
   end Process_Tape;

   function Process (Contents : String) return Machine_Status is
      A_Tape : Tape := Make_Tape (Contents);
   begin
      Ada.Text_IO.Put_Line ("Processing: " & Contents);

      while not Is_At_End (A_Tape) loop
         Next (A_Tape);
      end loop;

      return Ok;
   end Process;
end Calc;
