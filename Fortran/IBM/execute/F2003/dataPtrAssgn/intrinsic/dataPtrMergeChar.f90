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
!* - data-pointer of type character(:), target of type char(2),
!* - data-pointer as arg of merge
!* - data-target is redefined by self reference in module procedure
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    character(:), pointer :: chP(:)
    character(2), allocatable, target :: chT(:)

    contains
        subroutine sub(ch)
            character(*), allocatable :: ch(:)

	    if ( .not. allocated(ch)) error stop 30
	    ch = ch(ubound(ch,1):lbound(ch,1):-1)
        end subroutine
end module

    program main

        use m

	allocate(chT(26), source = (/ (repeat(achar(i+64),2), i= 1,26)  /) )

	chP(size(chT):) => chT(::2)

	if ( .not. associated(chP)) error stop 5
	if ( lbound(chP,1) /= 26) error stop 7
	if ( ubound(chP,1) /= 38) error stop 9

	print *, chP

        call sub(chT)
	print *, chP

	print *, merge(chP, "->", (/(mod(i,2) == 1,i=1,13) /))

    end program
