@process free(f90)
      program fxpurs20
!*********************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct 08, 1995
!*
!*  PRIMARY FUNCTIONS TESTED   : PURE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Sanity test case 20:  Pure function that
!*                               calls a pure subroutine
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
        pure real*4 function getanum (x)
          real*4, value :: x
        end function getanum
      end interface

      forall (i=1:10)
        res (i) = getanum (i * 1.2)
      end forall
      write (6, fmt='(5f9.2)') res
      end program

      pure real*4 function getanum (x)
        real*4, value :: x
        interface
          pure subroutine numbar (x, r)
            real*4, value :: x
            real*4, intent (out) :: r
          end subroutine numbar
        end interface

        call numbar (x * 3, getanum)
      end function getanum

      pure subroutine numbar (x, r)
        real*4, value :: x
        real*4, intent (out) :: r
        r = x + 2.1
      end subroutine numbar
