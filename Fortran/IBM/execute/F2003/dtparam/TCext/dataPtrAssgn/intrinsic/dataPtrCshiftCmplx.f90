! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrCshiftCmplx.f
! opt variations: -qnol

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
!* - data-targets are module vars with protected/private attributes
!* - data-pointers are args to module procedure
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m

        complex(4), target, private :: c8_t(25)
        complex(8), target, allocatable, protected :: c6_t(:)

	type base(n1,k1)    ! (20,16)
            integer, kind            :: k1
            integer, len             :: n1
            complex(k1), allocatable :: c3_t(:)
	end type

	type(base(20,16)), protected, target :: bT

        contains
            subroutine init()

		c8_t = (/(cmplx(i-1,i+1,4), i=1,25 ) /)

		allocate(c6_t(5),source = (/(cmplx(i,-i,8),i=-5,-1 )/) )

		allocate(bT%c3_t(10), source= (/ (cmplx(-i,-i,16), i=1,10)/))

            end subroutine

	    subroutine associate(a,b,c)
        	complex*8, pointer :: a(:)
        	complex*16, pointer :: b(:)
        	complex(16), pointer :: c(:)


		a(kind(c):) => c8_t(1:25:2)
		b(ubound(a,1):) => c6_t(5:1:-1)
		c(int(real(c8_t(1)),1):4_4) => bT%c3_t(2:10:2)

	    end subroutine

end module

program main
        use m

        complex*8, pointer :: c8_p(:)
        complex*16, pointer :: c6_p(:)
        complex(16), pointer :: c3_p(:)

	call init

	call associate(c8_p, c6_p, c3_p)

	if ( .not. associated(c8_p)) stop 12
	if ( .not. associated(c6_p)) stop 14
	if ( .not. associated(c3_p)) stop 16

	print *, lbound(c8_p), ubound(c8_p)
	print *, lbound(c6_p), ubound(c6_p)
	print *, lbound(c3_p), ubound(c3_p)


	write (*, '("(",f10.6,", ", f10.6, ")")') c8_p
	write (*, '("(",f10.6,", ", f10.6, ")")') c6_p
	write (*, '("(",f10.6,", ", f10.6, ")")') c3_p

	print *, "calling to cshift ..."

	write (*, '("(",f10.6,", ", f10.6, ")")') cshift(c8_p, -1)
	write (*, '("(",f10.6,", ", f10.6, ")")') cshift(c6_p,0)
	write (*, '("(",f10.6,", ", f10.6, ")")') cshift(c3_p, 1)

    end program
