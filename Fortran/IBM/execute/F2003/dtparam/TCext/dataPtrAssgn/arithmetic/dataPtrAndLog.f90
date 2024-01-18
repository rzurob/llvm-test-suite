! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrAndLog.f
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
!* - data-ptr of a component of type logical of a sequence derived-type
!* - the derived-type & data-tar are common-block-objects
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

     type base(n1,k1)    ! (20,4)
	integer, kind        :: k1
	integer, len         :: n1
	sequence
	logical(k1), pointer :: p(:)
     end type

end module

    program main

	use m

	type(base(20,4)) :: b
	logical, target :: tar(10)
	common /comm1/ tar, b

	call sub

	if ( .not. associated(b%p, tar(10:1:-2))) error stop 29
	if ( lbound(b%p,1) /= 2 ) error stop 31
	if ( ubound(b%p,1) /= 6 ) error stop 31

	print *, b%p
	print *, b%p .and. (/.true., .false.,.true.,.true.,.false. /)

 End program

 subroutine  sub
	use m, only : base
	logical, target :: ltar(10)
	type(base(20,4)) :: b1

	common /comm1/ ltar, b1

	ltar = (/ ( mod(i,3)==0, i=1,10 ) /)

	b1%p(2:) => ltar(10:1:-2)

 end subroutine


