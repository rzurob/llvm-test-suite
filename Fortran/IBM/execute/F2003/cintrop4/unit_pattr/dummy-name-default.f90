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

      !!! dummy procedure with BIND(C) attribute
      !
      !!! Procedures to be supplied as dummy args.

      SUBROUTINE s0 () BIND(C)
        print*, 'hello'
      END SUBROUTINE

      FUNCTION f0 () BIND(C)
        f0 = 1.0
      END FUNCTION

      ! This procedure should never be invoked.  If it is, xlf has a bug.
      SUBROUTINE dummyproc ()
        print*, 'I am dummyproc.  I should not have been called.'
      END SUBROUTINE

      !!! Users of dummy procedure arguments.

      !! user of dummy SUBROUTINE argument
      SUBROUTINE rcv1 (dummyProc)
        INTERFACE
          SUBROUTINE dummyProc () BIND(C)
          END SUBROUTINE
        END INTERFACE

        call dummyProc
      END SUBROUTINE

      !! user of dummy FUNCTION argument
      SUBROUTINE rcv2 (dummyProc)
        INTERFACE
          FUNCTION dummyProc () BIND(C)
          END FUNCTION
        END INTERFACE

        d = dummyProc()
        print *, 'dummy proc returned: ', d
      END SUBROUTINE

      !!! Driver -- supplier of dummy procedure argument.
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
