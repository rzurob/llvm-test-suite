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
      !!! For missing `call', same error as for
      !!! missing `call' to ordinary procedure.

      subroutine s0(c)
        print*, 'arg c =', c
      end

      subroutine s1()
      entry bind(c)  ! ok: is sub-entry name -- not BIND-stm
        print*, 'arg c =', c
      end

      program p1
        C = 7.0
        s0(C)  ! err: missing `call' for subroutine call
        bind(C)  ! err: missing `call' for subroutine call
      end
