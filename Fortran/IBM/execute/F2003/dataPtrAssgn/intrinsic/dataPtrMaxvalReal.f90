!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrMaxvalReal.f 
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
!* - data-pointer of type class(*) , a component of DT
!* - data-target is type real*8, real*4, real*16, components of the DT
!* - lb is intrinsic dot_product & min
!* - defect 326008
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base
        class(*), pointer :: p(:) 
	real*4 :: t4(10)
	real*8, allocatable :: t8(:)
	real*16 :: t16(10)
    end type

end module

program main
    use m

	type(base), target, allocatable :: b

	allocate(b)

	b%t4 = (/(real(i,4),i=1,10)/) 
	b%p(2:6) => b%t4(::2)

	b%t4 = b%t4 
        if ( .not. associated(b%p)) stop 5 
        if ( any ( lbound(b%p) .ne. (/2/) )) stop 7 
        if ( any ( ubound(b%p) .ne. (/6/) )) stop 9 

        select type(x => b%p)
            type is (real)
		write(*, '(5f8.3)')  (/ (x(i), i=2,6  )/) 
		!write(*, '(5f8.3)') x 
		write(*, '(5f8.3)') maxval(x) 
            class default
                stop 11 
        end select

	allocate(b%t8(5), source=(/ (real(i,8), i=1,5) /))

	b%p => b%t8
	b%p(min(int(maxval(b%t4)),1):) => b%p

        if ( .not. associated(b%p)) stop 15 
        if ( any ( lbound(b%p) .ne. (/1/) )) stop 17 
        if ( any ( ubound(b%p) .ne. (/5/) )) stop 19 

        select type(x => b%p)
            type is (real*8)
		write(*, '(5f10.6)') x 
		write(*, '(5f10.6)') maxval(x) 
            class default
                stop 21 
        end select

	b%t16 = (/(real(i,16),i=10,1,-1)/) 

	b%p => b%t16(::2)

	b%p(dot_product((/1,2/),(/2,3/)):) => b%p

        if ( .not. associated(b%p)) stop 25 
        if ( any ( lbound(b%p) .ne. (/8/) )) stop 27 
        if ( any ( ubound(b%p) .ne. (/12/) )) stop 29 

        select type(x => b%p)
            type is (real*16)
		write(*, '(5f15.10)') x 
		write(*, '(5f15.10)') maxval(x) 
            class default
                stop 35 
        end select

    end program
