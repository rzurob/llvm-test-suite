!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/value/array/diag/valuearray_d102.f
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing restrictions on the extensions to the VALUE attribute
!*									- assumed rank arrays
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
implicit none

integer*4 :: i(2,2)
i = reshape ((/1,2,3,4/), (/2,2/))

call sub1_int(i)

contains

subroutine sub1_int(arg)
    integer*4 :: arg(..)
	value arg
end subroutine

end
