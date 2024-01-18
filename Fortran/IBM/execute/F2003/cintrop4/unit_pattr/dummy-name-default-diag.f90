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

      !!! dummy procedure with explicit but default binding label.
      !
      ! ?? xlf should issue an error message (because explicit binding
      ! binding labels are disallowed on dummy procedures) but doesn't.
      ! Nevertheless, the program works correctly.

      !!! Procedures to be supplied as dummy args.

      SUBROUTINE s0 () BIND(C)
        print*, 'hello'
      END SUBROUTINE

      FUNCTION f0 () BIND(C)
        f0 = 1.0
      END FUNCTION

      !! Procedure which might be incorrectly invoked, if the explicit
      !! binding label confuses xlf.
      ! This procedure should never be invoked.  If it is, xlf has a bug.
      SUBROUTINE dummyproc ()
        print*, 'I am dummyproc.  I should not have been called.'
      END SUBROUTINE


      !!! Users of dummy procedure arguments.

      !! user of dummy SUBROUTINE argument
      SUBROUTINE rcv1 (dummyProc)
        INTERFACE
          SUBROUTINE dummyProc () BIND(C, name='dummyproc')  ! err: binding label disallowed on dummy proc
          END SUBROUTINE
        END INTERFACE

        call dummyProc
      END SUBROUTINE

      !! user of dummy FUNCTION argument
      SUBROUTINE rcv2 (dummyProc)
        INTERFACE
          FUNCTION dummyProc () BIND(C, name='dummyproc')  ! err: binding label disallowed on dummy proc
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
