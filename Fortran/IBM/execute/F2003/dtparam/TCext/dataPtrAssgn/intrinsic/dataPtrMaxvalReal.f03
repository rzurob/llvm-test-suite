! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrMaxvalReal.f
! opt variations: -qnol -qnodeferredlp

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
!* - data-pointer of type class(*) , a component of DT
!* - data-target is type real*8, real*4, real*16, components of the DT
!* - lb is intrinsic dot_product & min
!* - defect 326008
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(n1,k1,k2,k3)    ! (20,4,8,16)
	integer, kind         :: k1,k2,k3
	integer, len          :: n1
        class(*), pointer :: p(:)
	real(k1)              :: t4(10)
	real(k2), allocatable :: t8(:)
	real(k3)              :: t16(10)
    end type

end module

program main
    use m

	type(base(:,4,8,16)), target, allocatable :: b

	allocate(base(20,4,8,16) :: b)

	b%t4 = (/(real(i,4),i=1,10)/)
	b%p(2:6) => b%t4(::2)

	b%t4 = b%t4
        if ( .not. associated(b%p)) error stop 5
        if ( any ( lbound(b%p) .ne. (/2/) )) error stop 7
        if ( any ( ubound(b%p) .ne. (/6/) )) error stop 9

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

        if ( .not. associated(b%p)) error stop 15
        if ( any ( lbound(b%p) .ne. (/1/) )) error stop 17
        if ( any ( ubound(b%p) .ne. (/5/) )) error stop 19

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

        if ( .not. associated(b%p)) error stop 25
        if ( any ( lbound(b%p) .ne. (/8/) )) error stop 27
        if ( any ( ubound(b%p) .ne. (/12/) )) error stop 29

        select type(x => b%p)
            type is (real*16)
		write(*, '(5f15.10)') x
		write(*, '(5f15.10)') maxval(x)
            class default
                stop 35
        end select

    end program