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
!* - data-pointer is func name, which is arg of transpose, type character(:)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	character(4), target, allocatable :: ch4(:)
	character(:), allocatable :: ch(:,:)

	allocate(ch4(24), source = (/ (repeat(char(i),4), i=65,88) /))

	ch = transpose(func(ch4))

	if ( any( shape(ch) .ne. (/6,4/) )) error stop 21

	do i = 1, 6
	   print *, ch(i, :)
	enddo

	contains
            function func(ch)
	        character(4), target, allocatable :: ch(:)
	        character(:), pointer :: func(:,:)

	        if ( .not. allocated(ch) ) error stop 3

	        func(3:6,2:7) => ch

	        if ( .not. associated(func)) error stop 5
		if ( any( lbound(func) .ne. (/3,2/) )) error stop 13
		if ( any( ubound(func) .ne. (/6,7/) )) error stop 13

	    end function

end program
