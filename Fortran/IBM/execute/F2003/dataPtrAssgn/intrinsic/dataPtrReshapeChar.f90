!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrReshapeChar.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - date-pointer/target are components of same DT
!* - data-ptr of type character(*), data-tar of type character(:)
!* - data ptr assignment appears in external proc
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

	type base
	    character(len =1), pointer :: p1(:)
	    character(:), pointer :: p2(:)
	end type

	interface foo

	    subroutine sub(a1, a2)
		character(*), pointer :: a1(:)
		character(:), pointer :: a2(:)
	    end subroutine
	end interface

end module

    program main

        use m

	type(base) :: dt
	character(:), allocatable :: tar(:,:)

	allocate(dt%p2(20), source = (/ (achar(i), i=65,84) /) )

	call foo (dt%p1, dt%p2)

	if ( .not. associated(dt%p1, dt%p2(::2))) stop 5
	if ( lbound(dt%p1,1) /= 20) stop 7
	if ( ubound(dt%p1,1) /= 29) stop 9

	print *, dt%p1

	tar =  reshape(dt%p1, (/ 5, 2/))

	print *, (/ ( tar(i, :), i=1,5)/)

    end program

	    subroutine sub(a1, a2)
		character(*), pointer :: a1(:)
		character(:), pointer :: a2(:)

		if ( .not. associated(a2)) stop 11

		a1(ubound(a2,1):) => a2(::2)

	    end subroutine
