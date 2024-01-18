! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrConcat.f
! opt variations: -qnock -qnok -qnol

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
!* - data-ptr & data-tar are components of same derived-type, of type char(:)
!* - pointer-assignment applied in module proc; the results verified in main proc
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m


    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        character(:), pointer :: ptr(:)
        character(:), allocatable :: char(:)
    end type


    contains
        subroutine sub(a)
            type(base(4,*)), target :: a

            a%ptr(3:) => a%char

	    a%char(1:3) = a%char(7:5:-1)
        end subroutine

end module

program main

    use m
    type(base(4,20)), target :: t1

    t1%char = (/ 'abc','def','ghi','jkl','lmn','opq','rst' /)

    print *, t1%char

    call sub(t1)

    if  ( .not. associated(t1%ptr, t1%char)) stop 1
    if ( lbound(t1%ptr, 1) /= 3 ) stop 3
    if ( ubound(t1%ptr, 1) /= 9 ) stop 5

    print *, t1%ptr
    print *, t1%ptr // "---"

 End program

