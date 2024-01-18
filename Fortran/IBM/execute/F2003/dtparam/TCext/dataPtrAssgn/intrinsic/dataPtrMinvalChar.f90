! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrMinvalChar.f
! opt variations: -qck -qnok -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrMinvalChar.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : Aug 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION
!*
!* - data-pointer is component of DT base, of type class(*)
!* - data_target is component of Child of DT base, of type character(:)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

   module m
        type base(k1,n1)    ! (4,20)
            integer, kind :: k1
            integer, len  :: n1
            class(*), pointer :: p(:)
        end type

        type, extends(base) :: child(k2,n2)    ! (4,20,4,20)
            integer, kind :: k2
            integer, len  :: n2
            character(:), allocatable :: ch(:)
        end type

    end module

    program main

        use m

        class(base(4,:)), target, allocatable :: val

        !allocate(child::val)
	allocate(val, source = child(4,20,4,20)(null(), null()))

        select type (x => val)
            type is (child(4,*,4,*))
                allocate(x%ch(len('COMPILER')), source = &
                    (/ (repeat(achar(i),2), i=65,72) /) )

		x%ch = x%ch(8:1:-1)

                x%p(1:) => x%ch

                if ( .not. associated(x%p, x%ch)) stop 12

		select type ( y => x%p)
		    type is (character(*))

			y = y(8:1:-1) 

                	if ( any ( lbound(x%p) .ne. (/1/) )) stop 22
	                if ( any ( ubound(x%p) .ne. (/8/) )) stop 32

			print *, (/ (y(i), i=1,8)/)
			print *, Minval(y) 

		    class default
			stop 42
		end select

            class default
                stop 52
        end select

    end program
