! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrNE1Char.f
! opt variations: -qnock -qnok -qnol

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrNE1Char.f
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


    type try(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        character(:), pointer :: ptr(:)
        character(:), allocatable :: char(:)
    end type


    contains
        subroutine sub(a)
            type(try(4,20)), target, value :: a

            print *, a%char
            print *, a%ptr

            a%ptr(3:) => a%char
            a%char = (/ '123', '456','789','012','345','678','910' /)

    	    if ( .not. associated(a%ptr, a%char)) stop 12
    	    if ( lbound(a%ptr,1) /= 3 ) stop 15
    	    if ( ubound(a%ptr,1) /= 9 ) stop 18

            print *, a%char
            print *, a%ptr
        end subroutine

end module

program main

    use m
    type(try(4,20)), target :: t1

    t1%char = (/ 'abc','def','ghi','jkl','lmn','opq','rst' /)

    t1%ptr(2:) => t1%char

    if ( .not. associated(t1%ptr, t1%char)) stop 2
    if ( lbound(t1%ptr,1) /= 2 ) stop 5
    if ( ubound(t1%ptr,1) /= 8 ) stop 8

    call sub(t1)

    if ( .not. associated(t1%ptr, t1%char)) stop 22
    if ( lbound(t1%ptr,1) /= 2 ) stop 25
    if ( ubound(t1%ptr,1) /= 8 ) stop 28
    print *, t1%char
    print *, t1%ptr

 End program

