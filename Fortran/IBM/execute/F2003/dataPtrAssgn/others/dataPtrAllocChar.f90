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
!* - data-pointer is of type character(:), self reference
!* - the part of data-pointer is redefined by another part of data-pointer
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

        character(:), pointer  ::  ch(:)

        allocate(ch(5), source=(/'IBm', '123', 'XLc', '456', 'xLF' /))

        print *, ch

        ch => ch(5:1:-1)

	if ( .not. associated(ch)) error stop 9
	if ( lbound(ch,1) /= 1 ) error stop 19
	if ( ubound(ch,1) /= 5 ) error stop 29

        ch(3:5) = ch(1:3)

        print *, ch

end program
