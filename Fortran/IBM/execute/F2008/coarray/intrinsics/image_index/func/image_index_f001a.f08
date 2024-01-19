!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : August 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test image_index returns 0 when the
!*                               2nd arg exceeds the corank.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, save :: caf[*]
	integer :: x

	print *, image_index(caf, [0])
        print *, image_index(caf, [1])
        print *, image_index(caf, [2])
        print *, image_index(caf, [3])
        print *, image_index(caf, [4])
        print *, image_index(caf, [5])
        print *, image_index(caf, [6])
	call sub0()

	x = 10
	call sub1(image_index(caf, [x]))

contains

	subroutine sub0()
		integer, save :: caf[5:*]

		print *, ""
		print *, image_index(caf, [4])
		print *, image_index(caf, [5])
		print *, image_index(caf, [6])
	end subroutine

	subroutine sub1(x)
                integer :: x
                print *, x
	end subroutine
end
