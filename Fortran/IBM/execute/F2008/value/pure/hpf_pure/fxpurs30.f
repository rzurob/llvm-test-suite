@process free(f90)
      program fxpurs30
!*********************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct 08, 1995
!*
!*  PRIMARY FUNCTIONS TESTED   : PURE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Sanity test case 30:  Pure subroutine that
!*                               calls a pure function
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS            : ??
!*  STATUS                     :
!*
!*  STRUCTURE                  : Main program
!*  EXECUTABLE                 : Yes
!*
!*  INPUTS                     : None
!*  OUTPUTS                    : None
!*
!*  SETUP REQUIREMENTS         : N/A
!*  DEPENDENCIES               : None
!*
!*  REQUIRED COMPILER OPTIONS  : None
!*
!*  NORMAL COMPLETION          : Return code = 0
!*  ABNORMAL COMPLETION        : Return code ^= 0
!*
!*  RUN TIME ESTIMATE          : <60 SECS
!*
!*  ASSUMPTIONS                : None
!*
!*  CONDITIONS TESTED          :
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/08/95   MS     -Initial Version
!*  12/01/10   GB     -Copy from $(tsrcdir)hpf_pure/*.f per feature 384867
!*                     changing intent(in) to value for non arrays dummy args
!*
!* ===================================================================
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

      real*4 res (10)

!hpf$ processors P(4)
!hpf$ distribute (BLOCK) onto P :: res

      interface
        pure subroutine getanum (x, r)
          real*4, value :: x
          real*4, intent (out) :: r
        end subroutine getanum
      end interface

!hpf$ independent
      do i=1,10
        call getanum (i * 1.2, res (i))
      end do
      write (6, fmt='(5f9.2)') res
      end program

      pure subroutine getanum (x, r)
        real*4, value :: x
        real*4, intent (out) :: r
        interface
          pure real*4 function numbar (x)
            real*4, value :: x
          end function numbar
        end interface

        r = numbar (x * 3)
      end subroutine getanum

      pure real*4 function numbar (x)
        real*4, value :: x
        numbar = x + 2.1
      end function numbar
