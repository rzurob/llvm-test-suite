! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv /tstdev/OO_poly/intrinsics/typeQuery/ptrAssignment003.f
! opt variations: -qck -ql -qdefaultpv

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ptrAssignment003.f
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
!*    the dynamic type. Polymorphic array pointer.
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: i = 1
    end type

    type, extends(Base) :: Child(n1)    ! (4,10)
        integer, len  :: n1
        character(n1) :: c = "abc"
    end type

    type Container(k2)    ! (4)
        integer, kind            :: k2
        class(Base(k2)), pointer :: b(:,:) => null()
    end type
end module

program ptrAssignment003
use m
    type(Container(4)) :: x, y
    type(Base(4)), target :: b1(2,3)
    type(Child(4,10)), target :: c1(8,5)

    if(.NOT. extends_type_of(y%b, x%b)) error stop 1_4
    if(.NOT. extends_type_of(x%b, y%b)) error stop 2_4
    if(.NOT. same_type_as(y%b, x%b)) error stop 3_4

    x%b => b1
    y%b => c1

    if(.NOT. extends_type_of(y%b, x%b)) error stop 4_4
    if(extends_type_of(x%b, y%b)) error stop 5_4
    if(same_type_as(y%b, x%b)) error stop 6_4

    y = x

    if(.NOT. extends_type_of(y%b, x%b)) error stop 7_4
    if(.NOT. extends_type_of(x%b, y%b)) error stop 8_4
    if(.NOT. same_type_as(y%b, x%b)) error stop 9_4
end
