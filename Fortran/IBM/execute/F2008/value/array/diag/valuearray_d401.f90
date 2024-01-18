!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/value/array/diag/valuearray_d401.f
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing restrictions on the extensions to the VALUE attribute
!*									- array constructor syntax error
!*										call sub(/i/) vs call sub((/i/))
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

implicit none

call sub1_int  (/ 100/)

contains

subroutine sub1_int(arg)
    integer*4, value :: arg(:)
end subroutine

end
