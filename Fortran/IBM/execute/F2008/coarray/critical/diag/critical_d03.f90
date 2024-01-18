!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : January 2011
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test Critical constructs must have unique tags.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	a: critical
	end critical a

a: 	critical
	end critical a

end


subroutine sub1()

	a: critical
	end critical a

end subroutine
