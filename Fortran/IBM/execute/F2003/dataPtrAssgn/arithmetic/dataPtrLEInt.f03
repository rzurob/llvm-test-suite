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
!* - data-target is a generic name of type-bound procedure
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type base
	class(*), pointer :: p(:,:)

	contains
	    procedure, nopass, private :: print_int
	    procedure, nopass, public :: print_char
	    generic, public :: output => print_int, print_char
    end type

    contains
	function print_int(arg)
	    byte, pointer :: print_int(:)
	    integer*1 :: arg(:)

	    allocate(print_int(size(arg)), source = arg)
	end function
	function print_char(arg)
	    character(len=1), pointer :: print_char(:)
	    character :: arg(:)

	    allocate(print_char(size(arg)), source = arg)
	end function
end module

program main

    use m

    type(base) :: b1

    b1%p(2:3,1:2) => b1%output( [ -3_1,-20_1,0_1,-23_1] )

    if ( .not. associated(b1%p)) error stop 1

    select type(x => b1%p)
	type is (byte)
            if ( any (lbound(x) .ne. (/2,1/))) error stop 2
            if ( any (ubound(x) .ne. (/3,2/))) error stop 3
	    if ( all(x .le. 0)) print *, x
    	type is (character(*))
	    print *, x
	class default
	   stop 5
    end select

end program
