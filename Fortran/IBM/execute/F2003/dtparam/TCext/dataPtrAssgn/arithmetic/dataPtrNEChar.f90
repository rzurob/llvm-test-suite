! GB DTP extension using:
! ftcx_dtp -qk -qreuse=none /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrNEChar.f
! opt variations: -qck -qnok -qreuse=self

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrNEChar.f
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
    type try(k1,n1,n2)    ! (4,3,3)
        integer, kind              :: k1
        integer, len               :: n1,n2
        character(n1), pointer     :: ptr(:)
        character(n2), allocatable :: char(:)
    end type

    type(try(4,3,3)), target :: t1

    t1%char = (/ 'abc','def','ghi','jkl','lmn','opq','rst' /)

    print *, t1%char

    t1%ptr(1:) => t1%char

    if ( .not. associated(t1%ptr, t1%char)) stop 12
    if ( lbound(t1%ptr,1) /= 1 ) stop 15
    if ( ubound(t1%ptr,1) /= 7 ) stop 18
    print *,  t1%ptr == t1%char
    print *, t1%ptr

 End program
