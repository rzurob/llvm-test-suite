!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/vfloop.sh
! %COMPOPTS: -qdebug=intmsg
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************

      !?? maybe I should compress this -- gen the arg variants w/ a .tcopt file
      !?? likewise for sub.f

      !!! BIND(C) w/out NAME=
      function f1 BIND(C)  ! err: fn w/out parens
        f1 = 0.0
        return
      end function

      function f2 () BIND(C)  ! ok: fn w/ parens and 0 args, no NAME=
        f2 = 0.0
        return
      end function

      function f3 (a) BIND(C)  ! ok: fn w/ parens and 1 arg, no NAME=
        f3 = 0.0
        return
      end function

      function f4 (a, b) BIND(C)  ! ok: fn w/ parens and 2 args, no NAME=
        f4 = 0.0
        return
      end function

      !!! BIND(C) w/ NAME= and non-default name
      function fn1 BIND(C, name='F1')  ! err: fn w/out parens
        fn1 = 0.0
        return
      end function

      function fn2 () BIND(C, name='F2')  ! ok: fn w/ parens and 0 args, NAME=non-default
        fn2 = 0.0
        return
      end function

      function fn3 (a) BIND(C, name='F3')  ! ok: fn w/ parens and 1 arg, NAME=non-default
        fn3 = 0.0
        return
      end function

      function fn4 (a, b) BIND(C, name='F4')  ! ok: fn w/ parens and 2 args, NAME=non-default
        fn4 = 0.0
        return
      end function

      !!! BIND(C) w/ NAME= and default name
      function fd1 BIND(C, name='fd1')  ! err: fn w/out parens
        fd1 = 0.0
        return
      end function

      function fd2 () BIND(C, name='fd2')  ! ok: fn w/ parens and 0 args, NAME=default
        fd2 = 0.0
        return
      end function

      function fd3 (a) BIND(C, name='fd3')  ! ok: fn w/ parens and 1 arg, NAME=default
        fd3 = 0.0
        return
      end function

      function fd4 (a, b) BIND(C, name='fd4')  ! ok: fn w/ parens and 2 args, NAME=default
        fd4 = 0.0
        return
      end function
