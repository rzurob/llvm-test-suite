!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : August 2010
!*  ORIGIN                     : AIX Compiler Development,
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
	integer, save :: caf2[2,*], caf3[1,2,*], caf4[1,1,3,1:*]
	integer, save :: caf5[1,1,1,1,*], caf6[1,-1:-1,0:0,1:1,1,0:*]

!#### Corank 2
	print *, image_index(caf2, [3,1])
	print *, image_index(caf2, [1,4])
	print *, image_index(caf2, [4,4])


!#### Corank 3
	print *, image_index(caf3, (/2,1,1/))
	print *, image_index(caf3, (/1,3,1/))
	print *, image_index(caf3, (/1,2,4/))


!#### Corank 4
        print *, image_index(caf4, [2,1,1,1])
        print *, image_index(caf4, [1,3,1,1])
        print *, image_index(caf4, [1,1,4,1])
	print *, image_index(caf4, [1,1,2,3])


!#### Corank 5
        print *, image_index(caf5, [2,1,1,1,1])
        print *, image_index(caf5, [1,2,1,1,1])
        print *, image_index(caf5, [1,1,5,1,1])
        print *, image_index(caf5, [1,1,1,9,1])
	print *, image_index(caf5, [1,1,1,1,20])


!#### Corank 6
        print *, image_index(caf6, [2,1,1,1,1,1])
        print *, image_index(caf6, [1,-2,1,1,1,1])
        print *, image_index(caf6, [1,0,1,1,1,1])
        print *, image_index(caf6, [1,1,15,1,1,1])
        print *, image_index(caf6, [1,1,1,1,1,1])
        print *, image_index(caf6, [1,1,1,0,1,1])
        print *, image_index(caf6, [1,1,1,1,-1,1])
	print *, image_index(caf6, [1,1,1,1,1,6])

	sync all
end
