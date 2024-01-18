!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 20, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Diagnostic: KIND of the result
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      interface

        subroutine s4(arg)
          integer(kind=4) :: arg
        end subroutine
        subroutine s8(arg)
          integer(kind=8) :: arg
        end subroutine
        subroutine s8arr(arg)
          integer(kind=8) :: arg(3)
        end subroutine
        subroutine s(arg)
          integer :: arg
        end subroutine

      end interface

      integer, save :: coarr(10)[10,10,*]

      call s4(ucobound(coarr,3,8))
      call s8(lcobound(coarr,3,4))
      call s8arr(ucobound(coarr,kind=4))
      call s(ubound(coarr,1)) ! this should be fine

      end
