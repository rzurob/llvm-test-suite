!*********************************************************************
!***********************************************************************

      !!! `bind' as name of procedure itself and `(c)' as name of
      !!! procedure argument in procedure declaration.
      !!! For missing `call', same error as for
      !!! missing `call' to ordinary procedure.

      subroutine s0(c)
        print*, 'arg c =', c
      end

      subroutine bind(c)  ! ok: is subname -- not BIND-stm
        print*, 'arg c =', c
      end

      program p1
        C = 7.0
        s0(C)  ! err: missing `call' for subroutine call
        bind(C)  ! err: missing `call' for subroutine call
      end
