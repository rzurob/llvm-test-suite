!*********************************************************************
!***********************************************************************

      !!! No Parentheses: verify that grammar requires parentheses for
      !!!   functions, even if there is no suffix, and doesn't require
      !!!   them for subroutines when there is no suffix.
      !!! Set 4: subroutine,function,entry with no suffix.

      !! RESULT(x) on function and function-entry with parens:
      function f1 ()  ! ok function with parens
         f1r2 = 1

      entry f1e1 ()  ! ok entry with parens
         f1er2 = 2
      entry f1e2  ! ok, entry w/out parens, even though it's within a function.
         f2er2 = 2
      end function

      !! RESULT(x) on function and function-entry without parens:
      function f2  ! err: function w/out parens
         f2r2 = 1
      end function

      subroutine s1 ()  ! ok: subroutine with parens
      end subroutine

      subroutine s2  ! ok: subroutine w/out parens
      end subroutine
