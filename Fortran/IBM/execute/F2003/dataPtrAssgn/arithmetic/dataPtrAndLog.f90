!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrAndLog.f 
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
!* - data-ptr of a component of type logical of a sequence derived-type
!* - the derived-type & data-tar are common-block-objects 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

     type base
	sequence
	logical, pointer :: p(:)
     end type

end module 

    program main

	use m

	type(base) :: b
	logical, target :: tar(10)
	common /comm1/ tar, b 

	call sub

	if ( .not. associated(b%p, tar(10:1:-2))) stop 29 
	if ( lbound(b%p,1) /= 2 ) stop 31
	if ( ubound(b%p,1) /= 6 ) stop 31

	print *, b%p
	print *, b%p .and. (/.true., .false.,.true.,.true.,.false. /) 

 End program

 subroutine  sub
	use m, only : base	
	logical, target :: ltar(10)
	type(base) :: b1

	common /comm1/ ltar, b1 

	ltar = (/ ( mod(i,3)==0, i=1,10 ) /)	

	b1%p(2:) => ltar(10:1:-2)

 end subroutine


