!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-target is the associate name where the selector has pointer attr,
!*   acutally, the associate name is associated with the target of the pointer.
!*   and has TARGET attribute
!* - check if the compiler complains about it (feature 294341)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

   program main
        class(*), pointer :: ptr(:)
        class(*), pointer :: tar(:)

        allocate(tar(10), source= (/(i, i=1,10) /))

        select type(y=>tar)
            type is (integer)
                ptr(-10:-1) => y
            class default
                stop 2
        end select

	if ( .not. associated(ptr,tar)) error stop 3
	if ( lbound(ptr,1) /= -10 ) error stop 4
	if ( ubound(ptr,1) /= -1 ) error stop 5

        select type(x=>ptr)
	    type is (integer)
		if ( any( x .ne. (/(i,i=1,10)/))) error stop 6
	    class default
	        stop 21
	end select

   end program
