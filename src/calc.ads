package Calc is

   type Machine_Status is (Ok, Overflow, Underflow);

   function Process (Contents : String) return Machine_Status;

end Calc;
