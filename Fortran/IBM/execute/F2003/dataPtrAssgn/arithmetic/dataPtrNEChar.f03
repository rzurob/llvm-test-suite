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
!* - data-ptr of type character(3), a component of a derived-type
!* - data-target is allocatable component of same derived-type, type char
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 Program main
    type try
        character(3), pointer :: ptr(:)
        character(3), allocatable :: char(:)
    end type

    type(try), target :: t1

    t1%char = (/ 'abc','def','ghi','jkl','lmn','opq','rst' /)

    print *, t1%char

    t1%ptr(1:) => t1%char

    if ( .not. associated(t1%ptr, t1%char)) error stop 12
    if ( lbound(t1%ptr,1) /= 1 ) error stop 15
    if ( ubound(t1%ptr,1) /= 7 ) error stop 18
    print *,  t1%ptr == t1%char
    print *, t1%ptr

 End program