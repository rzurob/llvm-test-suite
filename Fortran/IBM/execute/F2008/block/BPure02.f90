!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-12-06
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : temporary in block in pure procedure
!*  ADAPTED FROM               : -
!*
!*  DESCRIPTION
!*
!*  block in pure procedure - assign result to temporary before returning value
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BPure02
    implicit none
    integer :: ivar
    ivar = ifun(3)
    print *, ivar, ifun(4)
    if (ivar /= 45) stop 2
    if (ifun(4) /= 46) stop 3

  contains

    pure integer function ifun(a)
      integer, intent(in) :: a
      integer :: tmp
      block
        integer, parameter :: adams = 42
        tmp = a + adams
      end block
      ifun = tmp
    end function ifun

end program BPure02
