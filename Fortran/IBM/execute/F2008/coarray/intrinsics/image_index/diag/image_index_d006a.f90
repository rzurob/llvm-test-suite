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
!*  DESCRIPTION                : Invalid number of arguments for image_index.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, save :: caf[*]
	integer :: x, y, z
	integer :: w(1)

!#### 0 Args
	print *, image_index()

!#### 1 Args
	print *, image_index(caf)

!#### 3 Args
	print *, image_index(caf, x, y)
	print *, image_index(caf, w, w)
	print *, image_index(z, x, y)

!#### 4 Args
	print *, image_index(w, x, y, z)

end
