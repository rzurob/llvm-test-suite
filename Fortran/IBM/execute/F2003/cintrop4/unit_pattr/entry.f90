!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/vfloop.sh
! %COMPOPTS: -qdebug=intmsg -c
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************

      subroutine s1
      entry s1e bind(c)  ! err: sub-entry w/out parens
        return
      end subroutine

      subroutine s2
      entry s2e () bind(c)  ! ok: sub-entry w/ parens and 0 args
        return
      end subroutine

      subroutine s3
      entry s3e (arg1) bind(c)  ! ok: sub-entry w/ parens and 1 arg
        return
      end subroutine

      subroutine s4
      entry s4e (arg1, arg2) bind(c)  ! ok: sub-entry w/ parens and 2 args
        return
      end subroutine

      function f1()
         f1 = 1
         return
      entry f1e bind(C)  ! err: fn-entry w/out parens
         f1e = 2
      end function

      function f2()
         f2 = 1
         return
      entry f2e () bind(C)  ! ok: fn-entry w/ parens and 0 args
         f2e = 2
      end function

      function f3()
         f3 = 1
         return
      entry f3e (arg1) bind(C)  ! ok: fn-entry w/ parens and 1 args
         f3e = 2
      end function

      function f4()
         f4 = 1
         return
      entry f4e (arg1, arg2) bind(C)  ! ok: fn-entry w/ parens and 2 args
         f4e = 2
      end function

      program p1  ! don't complain when -c not supplied
      end program
