!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : October 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test the codimension attribute and statement
!*                               in situations of invalid syntax.
!*                                   a) Improper bracket syntax
!*                                   b) Mispelling
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, save :: cafa, cafb
	integer, save, codimension :: caf2[*]		!ERROR
	integer, save, codimension(*) :: caf3		!ERROR
	integer, save, codim[*] :: caf4			!ERROR

	codimension[*] cafa				!ERROR
	codim cafb[*]					!ERROR
end


subroutine sub1()
	integer, save, codimension[*] :: caf5
	integer, save :: caf6[1,*]

	codimension caf5[*]				!ERROR
	codimension caf6[2,2,*]				!ERROR
end subroutine
