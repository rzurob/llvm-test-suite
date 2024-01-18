!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrGTChar1.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-ptr is a module object of type class(*)
!* - data-target is renamed local name of a module name
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module A
    class(*), target,  allocatable :: ch(:)
    class(*), pointer :: ptr(:)
end module

program main
    use A

    character(8), target :: tar(2) = (/ "IBM XLF ", 'ibm xlf ' /)

    allocate(ch(2), source = (/ 'compiler', 'COMPILER' /) )

    ptr(2:3) => tar

    if ( .not. associated(ptr, tar)) stop 1
    if ( lbound(ptr, 1) /= 2 ) stop 3
    if ( ubound(ptr, 1) /= 3 ) stop 5

    select type (ptr)
	type is (character(*))
    	    print *, ptr
	class default
	    stop 8
    end select

    call sub

    if ( .not. associated(ptr, ch(2:1:-1))) stop 11
    if ( lbound(ptr, 1) /=  -1 ) stop 13
    if ( ubound(ptr, 1) /= 0 ) stop 15

    select type (ptr)
	type is (character(*))
    	    print *, ptr
	    print *, ptr >= (/'COMPILER', 'COMPILER' /)
	class default
	    stop 8
    end select

    contains
        subroutine sub
            use A, tar => ch

            ptr(-1:) => tar(2:1:-1)
            if ( .not. associated(ptr, tar(2:1:-1))) stop 21
            if ( lbound(ptr, 1) /= -1 ) stop 23
            if ( ubound(ptr, 1) /= 0 ) stop 25

            select type (ptr)
	        type is (character(*))
    	            print *, ptr
	        class default
	            stop 8
            end select

        end subroutine
end program
