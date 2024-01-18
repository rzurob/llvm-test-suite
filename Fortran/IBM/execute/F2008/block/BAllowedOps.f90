!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : BAllowedOps
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-12-14
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : allowed operations
!*  ADAPTED FROM               : -
!*
!*  DESCRIPTION
!*
!*  Make sure that the correct operations are applied within a block after
!*  variables are redefined: that no syntax errors are produced, and that the
!*  correct arithmetic operations are applied.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BAllowedOps
  implicit none
  integer :: v1
  character(2) :: v2
  real    :: v3
  complex :: v4
  logical :: v5

  v1 = 1234
  v2 = 'xy'
  v3 = 5.1
  v4 = (1.1,2.2)
  v5 = .true.

  block
    integer :: v2
    character(2) :: v3
    character(1) :: c
    real    :: v4
    complex :: v5
    logical :: v1

    v2 = 54321
    v3 = 'ab'
    v4 = 1e-17
    v5 = cmplx(9.1,10.1)
    v1 = .false.

    v2 = v2 + 121212
    c  = 'a'
    v3 = c // 'b'
    v4 = 1 / v4
    v5 = v5 * v5
    v1 = v1 .neqv. .false.

    print *, v1, v2, v3, v4, v5

  end block

  v1 = v1 + 121212
  v2 = v2 // 'b'
  v3 = 1 / v3
  v4 = v4 * v4
  v5 = v5 .neqv. .false.

  print *, v1, v2, v3, v4, v5

end program BAllowedOps
