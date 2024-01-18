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
!* - data-pointer of type character(*), dummy arg;
!* - data-target of type character(3)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

    character(:), pointer :: p(:)
    character(3), target, allocatable :: tar(:)

    call sub(tar)

    if ( .not. associated(p)) stop 21
    if ( lbound(p,1) /= 2 ) stop 23
    if ( ubound(p,1) /= 4 ) stop 25

    print *, (/ (p(i), i=2,4) /)

    p = (/ 'IBM', 'XLF', 'XLC' /)

    print *, unpack(p, (/ (/(.true., i=1,3)/), (/(.false., i=1,7) /) /), tar )

    contains
        subroutine sub(a)
 	   character(*), target, allocatable :: a(:)
	   allocate(a(10), source =(/ (repeat(char(i),3), i=65,74) /) )

	   if ( .not. allocated(a)) stop 1

	   p(2:4) => a(::3)

	end subroutine
end
