!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 17, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This diagnostic test, makes sure that the intrinsic
!*                               is not allowed as an argument to a function.
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      intrinsic is_iostat_end
      intrinsic is_iostat_eor

      write(*,*) caller(-2, is_iostat_end) ! should be flagged, since this intrin
                                           ! is not allowed as actual argument

      write(*,*) caller(-2, is_iostat_eor) ! should be flagged, since this intrin
                                           ! is not allowed as actual argument

      contains

      logical function caller(a, func)

        integer :: a

        interface
           logical function func(i)
           integer, intent(in) :: i
           end function func
        end interface

      caller = func(a)

      end function caller
      end
