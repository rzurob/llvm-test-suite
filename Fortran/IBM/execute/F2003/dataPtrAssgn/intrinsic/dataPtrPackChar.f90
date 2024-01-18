!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrPackChar.f 
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
!* - data-pointer of type character(*), target of type char(:), 
!* - data-pointer as arg of merge 
!* - data-pointer is redefined by self reference in module procedure 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    character(2), pointer :: chP(:)
    character(:), allocatable, target :: chT(:)

    contains
        subroutine sub(ch)
            character(*), pointer :: ch(:)

	    if ( .not. associated(ch)) stop 30
	    ch = ch(ubound(ch,1):lbound(ch,1):-1)
        end subroutine
end module

    program main

        use m
	
	allocate(chT(26), source = (/ (repeat(achar(i+64),2), i= 1,26)  /) )  

	chP(ichar('F'):ichar('W')) => chT

	if ( .not. associated(chP)) stop 5 
	if ( lbound(chP,1) /= 70) stop 7
	if ( ubound(chP,1) /= 87) stop 9

	print *, chP

        call sub(chP)

	print *, pack(chP, (/(mod(i,2) == 1,i=1,18)/)) 

    end program
