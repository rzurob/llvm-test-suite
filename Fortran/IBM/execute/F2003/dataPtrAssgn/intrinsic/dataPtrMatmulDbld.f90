!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrMatmulCmplx.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!* - double precsion/complex pointers variable as args of matmul
!* - pointer of type double precision, target of type real*8
!* - pointer of type double complex, target of type complex*16
!* - lb of data-pointer is pointer of integer*1
!* - data-pointers initialized by data statement
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type A
        double complex, pointer :: p(:,:)
    end type

    type B
        double precision, pointer :: p(:)
    end type
end module

    program main

        use m

        type(B) :: b1
        type(A) :: a1

	real*8, pointer :: r8P(:)
	complex*16, target :: c16P(2) = (/(2.0D+01,1.0D+01), &
		(1.0D+01,2.0D+01) /)
	integer*1, pointer :: intP

        DATA b1%p, a1%p / null(), null() /

	if (associated(b1%p)) stop 1
	if (associated(a1%p)) stop 2

	! double complex pointer, complex*16 target
	a1%p(0:1,1:1) => c16P
	if ( .not. associated(a1%p) ) stop 5
	if ( any ( lbound(a1%p) .ne. (/ 0, 1/) )) stop 7
	if ( any ( ubound(a1%p) .ne. (/ 1, 1/) )) stop 9

	write(*, '(2f20.15)') matmul(a1%p, a1%p)

	! double precision pointer, real*8 target
	allocate(r8p(10), source=(/(real(i*2.0,8),i=1,10 )/))
	allocate(intP, source=3_1)

	b1%p(intP:) => r8P(2:5)

	if ( .not. associated(b1%p)) stop 12
	if ( lbound(b1%p,1) /= 3 ) stop 15
	if ( ubound(b1%p,1) /= 6 ) stop 18

	write(*, '(f14.8)') matmul(b1%p,reshape((/r8P(1),r8p(6:8)/), (/4,1/)))

    end program
