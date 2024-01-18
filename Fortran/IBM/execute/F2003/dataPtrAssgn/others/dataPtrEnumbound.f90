!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrEnumbound.f 
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
!* - lb/ub of data-ptr are constants defined by enumerator
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
        implicit integer(z,n,t,l)
        enum, bind(c)
             enumerator :: zero, two =2
	     enumerator :: nine = two + 7, last = max(nine, 11)
        end enum 

        integer, pointer :: ptr(:,:)
	integer, allocatable, target :: tar(:)

	allocate(tar(100))

	tar = (/ [zero, 1, two], (/(i,i=3,99) /) /) 

	ptr(zero:nine, two:last) => tar

        if ( .not. associated(ptr)) stop 11
        if ( any(lbound(ptr) .ne. (/0, 2/))) stop 13
        if ( any(ubound(ptr) .ne. (/9, 11/))) stop 15

	if ( any(ptr .ne. reshape((/(i,i=0,99)/),(/10,10/)))) stop 17

    end program

