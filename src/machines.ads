with Interfaces;

package Machines
  with SPARK_Mode => On, Always_Terminates
is
   type Machine_Status is
     (Ok, Stack_Overflow, Stack_Underflow, Value_Out_Of_Bounds);

   -- Value definitions
   type Value is new Interfaces.IEEE_Float_64;
   Max_Value : constant := 1.0e5;
   Min_Value : constant := -1.0e5;
   Max_Sum   : constant := 2 * Max_Value;
   subtype Bounded_Value is Value range Min_Value .. Max_Value;

   -- Stack definitions
   Max_Stack_Size : constant := 1024;
   type Element_Count is new Integer range 0 .. Max_Stack_Size;
   subtype Stack_Index is Element_Count range 1 .. Max_Stack_Size;
   type Machine_Stack is array (Stack_Index) of Value;

   type Machine_Op is
     (Add, Subtract, Multiply, Divide, Dupe, Print, Dump_Stack, Error, Reset);

   type Machine is record
      Status : Machine_Status := Ok;
      Stack  : Machine_Stack;
      Top    : Element_Count := 0;
   end record;

   function Status (Self : Machine) return Machine_Status
   is (Self.Status)
   with Global => null, Depends => (Status'Result => Self);

   function Is_Running (Self : Machine) return Boolean
   is (Self.Status = Ok)
   with Global => null, Depends => (Is_Running'Result => Self);

   function Is_Stopped (Self : Machine) return Boolean
   is (Status (Self) /= Ok)
   with Global => null;

   function Stack_Size (Self : Machine) return Element_Count
   is (Self.Top)
   with Global => null;

   function Is_Stack_Empty (Self : Machine) return Boolean
   is (Self.Top = 0)
   with Global => null;

   function Is_Stack_Full (Self : Machine) return Boolean
   is (Stack_Size (Self) = Max_Stack_Size)
   with
     Global => null,
     Post   => Is_Stack_Full'Result = (Self.Top = Max_Stack_Size);

   function Peek (Self : Machine) return Value
   is (Self.Stack (Stack_Index (Self.Top)))
   with
     Global  => null,
     Depends => (Peek'Result => Self),
     Pre     => not Is_Stack_Empty (Self) and then Is_Running (Self);

   procedure Push (Self : in out Machine; Element : Value)
   with
     Global         => null,
     Depends        => (Self => +Element),
     Pre            => Self.Status = Ok,
     Contract_Cases =>
       (Is_Stack_Full (Self) => Status (Self) = Stack_Overflow,
        others               =>
          Is_Running (Self)
          and then not Is_Stack_Empty (Self)
          and then Self.Top = Self'Old.Top + 1
          and then Stack_Size (Self) = Stack_Size (Self'Old) + 1
          and then Peek (Self) = Element);

   procedure Pop (Self : in out Machine; Element : in out Value)
   with
     Global         => null,
     Pre            => Is_Running (Self),
     Contract_Cases =>
       (Stack_Size (Self) = 0 => Status (Self) = Stack_Underflow,
        others                =>
          Is_Running (Self)
          and then Stack_Size (Self) = Stack_Size (Self'Old) - 1
          and then Peek (Self'Old) = Element);

   procedure Execute (Self : in out Machine; Op : Machine_Op)
   with
     Contract_Cases =>
       (Is_Stopped (Self) and then Op /= Reset =>
          Is_Stopped (Self) and then Stack_Size (Self) = Stack_Size (Self'Old),
        Is_Stopped (Self) and then Op = Reset  =>
          Is_Running (Self) and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                                 => true);

   procedure Op_Add (Self : in out Machine)
   with
     Global         => null,
     Pre            => Is_Running (Self),
     Contract_Cases =>
       (Stack_Size (Self) < 2 =>
          Status (Self) = Stack_Underflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                =>
          ((Stack_Size (Self) = Stack_Size (Self'Old) - 1)
           or else (Self.Status = Value_Out_Of_Bounds)));

   procedure Op_Subtract (Self : in out Machine)
   with
     Global         => null,
     Pre            => Is_Running (Self),
     Contract_Cases =>
       (Stack_Size (Self) < 2 =>
          Status (Self) = Stack_Underflow
          and then Stack_Size (Self) = Stack_Size (Self'Old),
        others                =>
          ((Stack_Size (Self) = Stack_Size (Self'Old) - 1)
           or else (Self.Status = Value_Out_Of_Bounds)));

   function To_Machine_Op (Input : String) return Machine_Op
   with Global => null;
end Machines;
