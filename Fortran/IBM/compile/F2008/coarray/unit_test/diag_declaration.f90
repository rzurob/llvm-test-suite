!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : December 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test Coarray declarations get flagged properly with CAF off.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer :: arr[10]
	integer, save :: caf[*]
end
