!*********************************************************************
!***********************************************************************

      !!! `bind' as name of procedure itself and `(c)' as name of
      !!! procedure argument in procedure declaration.

      function bind(c)  ! ok: is fnname -- not BIND-stm
        bind = c - 1.0
      end

      program p1
        C = 7.0
        print*, 'rv =', bind(C)  ! ok: is fnname -- not BIND-stm
      end