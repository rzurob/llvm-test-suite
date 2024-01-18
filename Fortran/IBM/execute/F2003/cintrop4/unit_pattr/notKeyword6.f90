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

      !!! `bind' as name of procedure itself and `(c)' as name of
      !!! procedure argument in procedure declaration.

      function f1()
      entry bind(c)  ! ok: is fn-entry name -- not BIND-stm
        bind = c - 1.0
      end

      program p1
        C = 7.0
        print*, 'rv =', bind(C)  ! ok: is fnname -- not BIND-stm
      end
