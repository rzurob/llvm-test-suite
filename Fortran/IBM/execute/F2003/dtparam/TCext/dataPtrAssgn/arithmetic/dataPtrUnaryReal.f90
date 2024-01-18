! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrUnaryReal.f
! opt variations: -qnok -ql

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
!* - a pointer object of type class(*) is common block object, associated with
!*      a sequence derived-type object
!* - data-pointer is a real*8 type pointer component of the sequence DT
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(k1)    ! (4)
        integer, kind :: k1
	sequence
	double precision, pointer :: p(:)
    end type
end module

    program main

	use m
	real*8, target :: tar(20)
	type(base(4)), pointer :: tmp_p(:)

	!type(base), pointer :: bp(:)
	class(*), pointer :: bp(:)

	common /comm1/ tar, bp

	allocate( tmp_p(1))

	tar = (/ ( real(i*2.0,8), i= 1, 20 ) /)

	tmp_p(1)%p(kind(2_8):) => tar(::2)

	bp(0:0) => tmp_P

	if ( .not. associated(bp, tmp_p)) stop 2
	if ( any(lbound(bp,1) .ne. (/0/))) stop 4
	if ( any(lbound(bp,1) .ne. (/0/))) stop 6

	call sub

 End program

 subroutine sub
	use m , only : base
	real*8, target ::  rtar(20)

	class(*), pointer :: p(:)
	!type(base), pointer :: p(:)

	type(base(4)),  pointer :: tmp_p(:)
	logical precision_r4

	common /comm1/ rtar, p

	tmp_p(2:) => p

	if ( .not. associated(tmp_p, p)) stop 12
	if ( any(lbound(tmp_p,1) .ne. (/2/))) stop 14
	if ( any(lbound(tmp_p,1) .ne. (/2/))) stop 16

	if ( .not. associated(tmp_p(2)%p, rtar(::2))) stop 29
	if ( any(lbound(tmp_p(2)%p,1) .ne. (/8/))) stop 31
	if ( any(ubound(tmp_p(2)%p,1) .ne. (/ 17/))) stop 33
	if ( .not. precision_r4( -tmp_p(2)%p,(/(real(-i*2.0,8),i=1,20,2)/))) stop 37

 end subroutine
