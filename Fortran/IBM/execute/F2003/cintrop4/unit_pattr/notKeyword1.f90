!*********************************************************************
!***********************************************************************

      !!! `bind' as name of procedure itself and `(c)' as name of
      !!! procedure argument in procedure declaration.

      subroutine bind(c)  ! ok: is subname -- not BIND-stm
        print*, 'arg c =', c
      end

      program p1
        C = 7.0
        call bind(C)  ! ok: is subname -- not BIND-stm
      end
