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
!* - a derived-type object as dummy arg with value attr; => appears in the dummy proc
!* - data-ptr & data-tar are components of the derivedtype object
!* -
!234567890123456789012345678901234567890123456789012345678901234567890

module m


    type try
        character(:), pointer :: ptr(:)
        character(:), allocatable :: char(:)
    end type


    contains
        subroutine sub(a)
            type(try), target, value :: a

            print *, a%char
            print *, a%ptr

            a%ptr(3:) => a%char
            a%char = (/ '123', '456','789','012','345','678','910' /)

    	    if ( .not. associated(a%ptr, a%char)) error stop 12
    	    if ( lbound(a%ptr,1) /= 3 ) error stop 15
    	    if ( ubound(a%ptr,1) /= 9 ) error stop 18

            print *, a%char
            print *, a%ptr
        end subroutine

end module

program main

    use m
    type(try), target :: t1

    t1%char = (/ 'abc','def','ghi','jkl','lmn','opq','rst' /)

    t1%ptr(2:) => t1%char

    if ( .not. associated(t1%ptr, t1%char)) error stop 2
    if ( lbound(t1%ptr,1) /= 2 ) error stop 5
    if ( ubound(t1%ptr,1) /= 8 ) error stop 8

    call sub(t1)

    if ( .not. associated(t1%ptr, t1%char)) error stop 22
    if ( lbound(t1%ptr,1) /= 2 ) error stop 25
    if ( ubound(t1%ptr,1) /= 8 ) error stop 28
    print *, t1%char
    print *, t1%ptr

 End program
