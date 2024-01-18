! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/OO_poly/intrinsics/typeQuery/definedOperation004.f
! opt variations: -qck -qnol -qdeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: definedOperation004.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 11/10/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : Use defined operation to change the
!*    dynamic types.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(n2)    ! (20,4,10)
        integer, len  :: n2
        character(n2) :: c
    end type
end module

program definedOperation004
use m
    interface operator(+)
        function bPlusB2c(a, b)
        use m
            class(Base(*,4)), intent(in) :: a(:), b(size(a))
            type(Child(20,4,10)) :: bPlusB2c(size(a))
        end function
    end interface

    class(*), allocatable :: u1(:)
    type(Base(20,4)) :: b1(5)
    class(Child(20,4,10)), allocatable :: c1(:)
    b1%i = 1
    allocate(c1(size(b1)))
    c1%i = 2
    c1%c = "abc"
    allocate(u1(size(b1)), SOURCE=(b1+c1))

    if(.NOT. extends_type_of(u1, b1)) error stop 1_4
    if(.NOT. extends_type_of(u1, c1)) error stop 2_4
    if(.NOT. extends_type_of(u1, Base(20,4)(1))) error stop 3_4

    if(same_type_as(u1, b1)) error stop 4_4
    if(.NOT. same_type_as(u1, c1)) error stop 5_4
    if(.NOT. same_type_as(u1, Child(20,4,10)(1, "a"))) error stop 6_4

    select type(name=>u1)
        type is (Child(*,4,*))
            if(name(1)%i /= 3) error stop 7_4
            if(name(5)%c /= "bPlusB2c") error stop 8_4
        class default
            error stop 9_4
    end select
end

function bPlusB2c(a, b)
use m
    class(Base(*,4)), intent(in) :: a(:), b(size(a))
    type(Child(20,4,10)) :: bPlusB2c(size(a))
    bPlusB2c%i = a%i + b%i
    bPlusB2c%c = "bPlusB2c"
end function
