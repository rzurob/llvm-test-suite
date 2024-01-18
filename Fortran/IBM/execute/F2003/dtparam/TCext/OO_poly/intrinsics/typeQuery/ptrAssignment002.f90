! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/intrinsics/typeQuery/ptrAssignment002.f
! opt variations: -qck -qnok -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ptrAssignment002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/27/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : Use data pointer assignment to change
!*    the dynamic type. Unlimited polymorphic pointer.
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
        integer(k1)   :: i = 1
    end type

    type, extends(Base) :: Child(n2)    ! (20,4,10)
        integer, len  :: n2
        character(n2) :: c = "abc"
    end type

    type Container(k2,n3)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n3
        class(*), pointer :: b => null()
    end type
end module

program ptrAssignment002
use m
    type(Container(4,20)) :: x, y
    type(Base(20,4)), target :: b1
    type(Child(20,4,10)), target :: c1
    integer, target :: i1
    integer, target :: i2

    if(.NOT. extends_type_of(y%b, x%b)) error stop 1_4
    if(.NOT. extends_type_of(x%b, y%b)) error stop 2_4
    if(same_type_as(y%b, x%b)) error stop 3_4

    x%b => b1
    y%b => c1

    if(.NOT. extends_type_of(y%b, x%b)) error stop 4_4
    if(extends_type_of(x%b, y%b)) error stop 5_4
    if(same_type_as(y%b, x%b)) error stop 6_4

    y = x

    if(.NOT. extends_type_of(y%b, x%b)) error stop 7_4
    if(.NOT. extends_type_of(x%b, y%b)) error stop 8_4
    if(.NOT. same_type_as(y%b, x%b)) error stop 9_4

    x%b => i1
    y%b => i2

    if(extends_type_of(y%b, x%b)) error stop 10_4
    if(extends_type_of(x%b, y%b)) error stop 11_4
    if(.NOT. same_type_as(y%b, x%b)) error stop 12_4
end
