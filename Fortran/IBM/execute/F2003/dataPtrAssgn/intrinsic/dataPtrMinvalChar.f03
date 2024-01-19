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
!* - data-pointer is component of DT base, of type class(*)
!* - data_target is component of Child of DT base, of type character(:)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

   module m
        type base
            class(*), pointer :: p(:)
        end type

        type, extends(base) :: child
            character(:), allocatable :: ch(:)
        end type

    end module

    program main

        use m

        class(base), target, allocatable :: val

        !allocate(child::val)
	allocate(val, source = child(null(), null()))

        select type (x => val)
            type is (child)
                allocate(x%ch(len('COMPILER')), source = &
                    (/ (repeat(achar(i),2), i=65,72) /) )

		x%ch = x%ch(8:1:-1)

                x%p(1:) => x%ch

                if ( .not. associated(x%p, x%ch)) error stop 12

		select type ( y => x%p)
		    type is (character(*))

			y = y(8:1:-1)

                	if ( any ( lbound(x%p) .ne. (/1/) )) error stop 22
	                if ( any ( ubound(x%p) .ne. (/8/) )) error stop 32

			print *, (/ (y(i), i=1,8)/)
			print *, Minval(y)

		    class default
			stop 42
		end select

            class default
                stop 52
        end select

    end program
