!*********************************************************************
!***********************************************************************

      !!! `bind' as name of procedure itself and `(c)' as name of
      !!! procedure argument in procedure declaration.
      !!! For missing expression context, same error as for
      !!! missing expression context to ordinary procedure.

      function f0(c)
        print*, 'arg c =', c
      end

      function f1()
      entry bind(c)  ! ok: is fn-entry name -- not BIND-stm
        bind = c - 1.0
      end

      program p1
        C = 7.0
        f0(C)  ! err: missing expression context for function call
        bind(C)  ! err: missing expression context for function call
      end