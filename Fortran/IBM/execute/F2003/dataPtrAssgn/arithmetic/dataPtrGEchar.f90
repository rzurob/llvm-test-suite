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
!* - data-ptr, a dummy arg of module proc, has save attribute
!* - test operator .ge.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    logical, allocatable :: ll(:)

    contains
        function test(arg)
	    character(:), save, pointer :: ptr(:)
	    character(len=1), optional, target :: arg(:)
	    character(:), pointer :: test(:)

            if ( present(arg) ) then

                ptr(-16:-1) => arg

                if ( .not. associated(ptr, arg)) error stop 2
                if (lbound(ptr,1) /= -16 ) error stop 3
                if (ubound(ptr,1) /= -1 ) error stop 4

   		test(2:) => ptr(ubound(ptr,1):lbound(ptr,1):-1)

                if ( .not. associated(test, ptr(ubound(ptr,1):lbound(ptr,1):-1))) error stop 5
                if (lbound(test,1) /= 2 ) error stop 6
                if (ubound(test,1) /= 17 ) error stop 7
	    else
		test(3:) => ptr(::2)
                if ( .not. associated(test, ptr(::2))) error stop 11
                if (lbound(test,1) /= 3 ) error stop 12
                if (ubound(test,1) /= 10 ) error stop 13
                ll = test .ge. (/ 'A','B', 'E', 'F', 'J', 'M', 'M', 'H' /)

  	    endif

        end function

end module

program main

    use m

    print *, test( (/ (achar(i,1), i=65,80 ) /) )
    print *, test()
    print *, ll

 End program

