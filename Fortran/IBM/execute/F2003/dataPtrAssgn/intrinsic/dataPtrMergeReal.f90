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
!* - data pointer assignment appears at if/else/endif block
!* - data-pointer is the component of an external proc dumm arg, arg of merge
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

        type base
            real, pointer :: p(:,:)
        end type

        real, target :: tar(20) = (/ ( real(i),i=1,20) /)

        interface foo
            subroutine sub(a)
                import base
                type(base) :: a
            end subroutine
        end interface

end module

    program main

        use m
        type(base), allocatable :: a

        allocate(a)
        a%p(1:4,1:5) => tar
	if ( .not. associated(a%p) ) error stop 2
        if ( any (lbound(a%p) .ne. (/1,1/) ) ) error stop 3
        if ( any (ubound(a%p) .ne. (/4,5/) ) ) error stop 5

        call sub(a)
	if ( .not. associated(a%p) ) error stop 12
        if ( any (lbound(a%p) .ne. (/-1,0/) ) ) error stop 13
        if ( any (ubound(a%p) .ne. (/1,3/) ) ) error stop 15

	call foo(a)
	if ( .not. associated(a%p) ) error stop 25
        if ( any (lbound(a%p) .ne. (/2,3/) ) ) error stop 35
        if ( any (ubound(a%p) .ne. (/4,6/) ) ) error stop 45

	write(*, '(4f14.8)') a%p
	write(*, '(4f14.8)') merge(a%p, a%p, .true.)

    end program

  	    subroutine sub(a)
                use m, only : base, tar
                type(base) :: a

                if ( any(shape(a%p) .eq. (/4,5/)) ) then
                    a%p(-1:1,0:3) => tar
                else
                    a%p(2:,3:) => a%p(1:-1:-1,3:0:-1)
                endif
            end subroutine

