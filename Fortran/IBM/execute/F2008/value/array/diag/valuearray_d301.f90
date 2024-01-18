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
!*									- using VALUE attribute in a BIND(c) routine
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

use, intrinsic :: iso_c_binding

implicit none

interface
	function func(arg) BIND(c)
		use, intrinsic :: iso_c_binding
		implicit none
		integer(c_int), value :: arg(10)
		integer(c_int) func
	end function func
end interface


end
