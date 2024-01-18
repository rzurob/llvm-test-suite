!*********************************************************************
!***********************************************************************

      !!! No Parentheses: verify that grammar requires parentheses when
      !!! suffix is present.
      !!! Set 1: function,entry with result suffix.

      !! RESULT(x) on function and function-entry with parens:
      function f1 () result(f1r2)  ! ok
         f1r2 = 1
      entry f1e () result(f1er2)  ! ok
         f1er2 = 2
      end function

      !! RESULT(x) on function and function-entry without parens:
      function f2 result(f2r2)  ! err
         f2r2 = 1
      entry f2e result(f2er2)  ! err
         f2er2 = 2
      end function
