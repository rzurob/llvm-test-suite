!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrAllocChar.f 
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
!* - data-pointer is of type character(:), self reference 
!* - the part of data-pointer is redefined by another part of data-pointer 
!*  
!234567890123456789012345678901234567890123456789012345678901234567890
 
program main

        character(:), pointer  ::  ch(:)

        allocate(ch(5), source=(/'IBm', '123', 'XLc', '456', 'xLF' /))

        print *, ch

        ch => ch(5:1:-1)

	if ( .not. associated(ch)) stop 9 
	if ( lbound(ch,1) /= 1 ) stop 19 
	if ( ubound(ch,1) /= 5 ) stop 29 

        ch(3:5) = ch(1:3)

        print *, ch

end program
