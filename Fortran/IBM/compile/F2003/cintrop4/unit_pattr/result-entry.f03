!*********************************************************************
!***********************************************************************

      function f1 ()
         f1 = 1
         return
      entry f1e () result(res0)  ! illegal: result on entry
         res0 = 2
      end function

      function f1 ()
         f1 = 1
         return
      entry f1e () bind(C)  ! ok: one bind
         f1e = 2
      end function
      function f1 ()
         f1 = 1
         return
      entry f1e () bind(C,name='f1B')  ! ok: one bind-w/-name
         f1e = 2
      end function

      function f1 ()
         f1 = 1
         return
      entry f1e () result(res0) bind(C)  ! illegal: result on entry
         res0 = 2
      end function
      function f1 ()
         f1 = 1
         return
      entry f1e () result(res0) bind(C,name='f1B')  ! illegal: result on entry
         res0 = 2
      end function

      function f1 ()
         f1 = 1
         return
      entry f1e () bind(C) result(res0)  ! illegal: result on entry
         res0 = 2
      end function
      function f1 ()
         f1 = 1
         return
      entry f1e () bind(C,name='f1B') result(res0)  ! illegal: result on entry
         res0 = 2
      end function

      function f1 ()
         f1 = 1
         return
      entry f1e () bind(C) bind(C)  ! illegal: two no-name binds
         f1e = 2
      end function
      function f1 ()
         f1 = 1
         return
      entry f1e () bind(C,name='f1B') bind(C,name='f1B')  ! ill: two same binds
         f1e = 2
      end function
      function f1 ()
         f1 = 1
         return
      entry f1e () bind(C,name='f1B') bind(C,name='f1C')  ! ill: two diff binds
         f1e = 2
      end function
