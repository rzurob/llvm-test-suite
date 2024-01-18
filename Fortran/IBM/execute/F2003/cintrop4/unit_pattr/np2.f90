!*********************************************************************
!***********************************************************************

      !!! No Parentheses: verify that grammar requires parentheses when
      !!! suffix is present.
      !!! Set 2: function,entry with bind(c) suffix.

      !! BIND(C) on function and function-entry with parens:
      function f1 () bind(C)  ! ok: parens between function and bind-no-name
         f1 = 1
      entry f1e () bind(C)  ! ok: parens btw fn-entry and bind-no-name
         f1e = 2
      end function

      function f2 () bind(C,name='f2B')  ! ok: parens btw fn & bind-with-NAME=
         f2 = 1
      entry f2e () bind(C,name='f2eB')  ! ok: parens btw fn-entry & bind-NAME=
         f2e = 2
      end function

      !! BIND(C) on function and function-entry without parens:
      function f3 bind(C)  ! err: no-parens btw function and bind-no-name
         f3 = 1
      entry f3e bind(C)  ! err: no-parens btw fn-entry and bind-no-name
         f3e = 2
      end function

      function f4 bind(C,name='f4B')  ! err: no-parens btw fn and bind-NAME=
         f4 = 1
      entry f4e bind(C,name='f4eB')  ! err: no-parens btw fn-entry & bind-NAME=
         f4e = 2
      end function
