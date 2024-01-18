!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-11-15
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : host association diagnostic
!*  ADAPTED FROM               : BHost01
!*
!*  DESCRIPTION
!*
!*  An inner procedure invoked from within a block can access host-associated
!*  vars, but not any defined in the block. This is a syntax error if implicit
!*  none is specified and no such variable exists in the host.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BHost01d
  implicit none
  block
    character(3) :: b
    b = 'xyz'
    print *, 'block: ', b
    call inner (b)
  end block
contains
  subroutine inner(v)
    character(*) :: v
    print *, 'inner: character:', v
    print *, b ! not available
  end subroutine inner
end program BHost01d
