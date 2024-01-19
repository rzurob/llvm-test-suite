!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan 9, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Insure integer overflow is handled
	implicit none
	integer :: ios

	print *, is_iostat_end(i=70000) !bigger than integer*2
	print *, is_iostat_end(i=4594967295) !bigger than integer*4
	print *, is_iostat_end(i=19446744073709551615) !bigger than integer*8
	print *, is_iostat_eor(i=70000) !bigger than integer*2
	print *, is_iostat_eor(i=4594967295) !bigger than integer*4
	print *, is_iostat_eor(i=19446744073709551615) !bigger than integer*8

	end
