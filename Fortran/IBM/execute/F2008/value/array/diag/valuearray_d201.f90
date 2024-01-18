!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/value/array/diag/valuearray_d201.f
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing restrictions on the extensions to the VALUE attribute
!*									- POINTER attribute
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
implicit none

integer*4, POINTER :: i(:)

call sub1_int(i)

contains

subroutine sub1_int(arg)
    integer*4, POINTER :: arg(:)
	value arg
end subroutine

end
