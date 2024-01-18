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

      !! dummy function
      FUNCTION f0 () BIND(C)
        f0 = 3.0
      END FUNCTION


      !! user of dummy procedure argument
      SUBROUTINE s1 (dummyProc)
        INTERFACE
          FUNCTION dummyProc () BIND(C)
          END FUNCTION
        END INTERFACE

        rv = 1.0
        PRINT*, rv

        rv = dummyProc ()
        PRINT*, rv

      END SUBROUTINE


      !! supplier of dummy procedure argument
      program m0
        INTERFACE
          FUNCTION f0 () BIND(C)
          END FUNCTION
        END INTERFACE

        CALL s1 (f0)
      end
