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

      !! dummy subroutine
      SUBROUTINE s0 () BIND(C)
        print*, 'hello'
      END SUBROUTINE

      FUNCTION f0 () BIND(C)
        f0 = 1.0
      END FUNCTION


      !! user of dummy SUBROUTINE argument
      SUBROUTINE rcv1 (dummyProc)
        INTERFACE
          SUBROUTINE dummyProc () BIND(C, name='s0_b')
          END SUBROUTINE
        END INTERFACE

        call dummyProc
      END SUBROUTINE

      !! user of dummy FUNCTION argument
      SUBROUTINE rcv2 (dummyProc)
        INTERFACE
          FUNCTION dummyProc () BIND(C, name='f0_b')
          END FUNCTION
        END INTERFACE

        d = dummyProc()
        print *, 'dummy proc returned: ', d
      END SUBROUTINE


      !! supplier of dummy procedure argument
      program m0
        INTERFACE
          SUBROUTINE s0 () BIND(C)
          END SUBROUTINE

          FUNCTION f0 () BIND(C)
          END FUNCTION
        END INTERFACE

        CALL rcv1 (s0)
        CALL rcv2 (f0)
      end
