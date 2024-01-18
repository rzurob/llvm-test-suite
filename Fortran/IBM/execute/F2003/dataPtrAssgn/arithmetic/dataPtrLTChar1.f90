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
!* - data-tar has save attribute of character(:)
!* - data-ptr is of type class(*)
!* - test operator '<'
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    class(*), pointer :: ptr(:)
end module

program main

    use m
    interface foo
	subroutine sub1(arg)
	    integer, optional :: arg
	    character(:), allocatable, save :: ch(:)
	end subroutine
    end interface

    call foo(10)

    if ( .not. associated(ptr)) error stop 11
    if ( lbound(ptr, 1) /= 1 ) error stop 12
    if ( ubound(ptr, 1) /= 10 ) error stop 13

    select type (ptr)
 	type is (character(*))
	    ptr = ptr( size(ptr):1:-1)
	class default
	    stop 15
    end select

    call foo()

    if ( .not. associated(ptr)) error stop 21
    if ( lbound(ptr, 1) /= 2 ) error stop 22
    if ( ubound(ptr, 1) /= 6 ) error stop 23

    select type (ptr)
 	type is (character(*))
	    print *, ptr
            print *, ptr < (/ 'kk', 'ii', 'gg', 'ee', 'cc' /)
	class default
	    stop 25
    end select

end program

	subroutine sub1(arg)
	    use m, only : ptr
	    integer, optional :: arg

            integer, save :: count = 0

	    character(:), target, allocatable, save :: ch(:)

	    if ( .not. allocated(ch)) then
	        if ( present(arg)) then
		allocate(ch(arg), source = (/ &
                    (repeat(achar(i),2)//' ', i=97,97+arg-1 ) /) )
	        endif
	    endif

	    if ( allocated(ch)) then
                if ( count == 0 ) then
	 	    if ( present(arg)) then
		        ptr(1:arg) => ch
		    else
	  	        ptr => ch
		    endif

	 	    count = count + 1
	        else
		    print *, ch
		    ptr(len_trim(ch(1)):) => ch(::2)
	        endif
	    endif

	end subroutine
