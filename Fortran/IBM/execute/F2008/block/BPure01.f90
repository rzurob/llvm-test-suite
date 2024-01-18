!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-12-06
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : block in pure procedure
!*  ADAPTED FROM               : -
!*
!*  DESCRIPTION
!*
!*  block in pure procedure
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BPure01
    implicit none
    integer :: ivar
    ivar = ifun(3)
    print *, ivar, ifun(4)
    if (ivar /= 45) error stop 2
    if (ifun(4) /= 46) error stop 3

  contains

    pure integer function ifun(a)
      integer, intent(in) :: a
      block
        integer, parameter :: adams = 42
        ifun = a + adams
      end block
    end function ifun

end program BPure01
