!*********************************************************************
!***********************************************************************

      !! dummy subroutine
      SUBROUTINE s0 () BIND(C)
        print*, 'hello'
      END SUBROUTINE

      !! user of dummy procedure argument
      SUBROUTINE s1 (dummyProc)
        INTERFACE
          SUBROUTINE dummyProc () BIND(C)
          END SUBROUTINE
        END INTERFACE

        call dummyProc
      END SUBROUTINE

      !! supplier of dummy procedure argument
      program m0
        INTERFACE
          SUBROUTINE s0 () BIND(C)
          END SUBROUTINE
        END INTERFACE

        CALL s1 (s0)
      end
