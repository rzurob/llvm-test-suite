! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/03/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : MOLD or A is a structure component,
!*    which may be a scalar or an array, non-poly or poly or unlimited
!*    poly. The object containing the component is a scalar.
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
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: BaseOne
        integer i
    end type

    type, extends(AbstractParent) :: Base
        class(AbstractParent), pointer :: ap => null()
        class(*), allocatable :: u1
    end type

    type, extends(Base) :: Child
        type(Base) :: b(5,8)
        class(Base), pointer :: b1(:) => null()
        class(BaseOne), allocatable :: b2(:,:)
    end type
end module

program structureComponent001
use m
    type(Child) :: arg1
    class(AbstractParent), pointer :: arg2 => null()
    class(*), allocatable :: arg3
    type(Base) :: arg4

    if(.NOT. extends_type_of(arg1, arg4%ap)) error stop 1_4
    if(.NOT. extends_type_of(arg1%base, arg2)) error stop 2_4
    if(.NOT. extends_type_of(arg1%b, arg3)) error stop 3_4
    if(.NOT. extends_type_of(arg1%b1, arg4%u1)) error stop 4_4
    if(.NOT. extends_type_of(arg1%b2, arg4%ap)) error stop 5_4
    if(extends_type_of(arg4%u1, arg1%b1)) error stop 6_4

    if(.NOT. same_type_as(arg1%base, arg1%b)) error stop 7_4
    if(.NOT. same_type_as(arg1%b1, arg4)) error stop 8_4
    if(.NOT. same_type_as(arg2, arg4%ap)) error stop 9_4
    if(same_type_as(arg3, arg4%u1)) error stop 10_4

    allocate(BaseOne::arg4%ap)
    allocate(integer::arg4%u1)
    allocate(Child::arg1%b1(5))
    allocate(arg1%b2(3,6))
    allocate(Child::arg2)
    allocate(integer::arg3)

    if(.NOT. extends_type_of(arg2, arg1%b)) error stop 11_4
    if(.NOT. extends_type_of(arg1%b1, arg1%base)) error stop 12_4
    if(.NOT. extends_type_of(arg1%b2, arg4%ap)) error stop 13_4
    if(extends_type_of(arg3, arg4%u1)) error stop 14_4

    if(.NOT. same_type_as(arg1%base, arg1%b)) error stop 15_4
    if(.NOT. same_type_as(arg1%b1, arg2)) error stop 16_4
    if(.NOT. same_type_as(arg1%b2, arg4%ap)) error stop 17_4
    if(.NOT. same_type_as(arg3, arg4%u1)) error stop 18_4
end
