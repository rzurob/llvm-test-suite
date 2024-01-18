!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/value/array/diag/valuearray_d101.f
!*
!*  PROGRAMMER                 : Cezar Lutac 
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing restrictions on the extensions to the VALUE attribute
!*									- assumed size arrays
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
implicit none

integer*4 :: i(10)
i = (/1,2,3,4,5,6,7,8,9,0/)

call sub1_int(i)

contains

subroutine sub1_int(arg)
    integer*4 :: arg(*)
	value arg
end subroutine

end      
