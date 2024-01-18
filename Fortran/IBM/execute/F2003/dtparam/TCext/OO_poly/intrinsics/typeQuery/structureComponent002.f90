! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/intrinsics/typeQuery/structureComponent002.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent002.f
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
!*    poly. The object containing the component is an array.
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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: BaseOne    ! (4)
        integer(k1) i
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        type(BaseOne(k1)) :: b1(2,3)
    end type

    type, extends(Base) :: Child    ! (4)
        type(Base(k1)) :: b2(5,8)
    end type
end module

program structureComponent002
use m
    class(Base(4)), allocatable :: arg1(:,:)
    class(Child(4)), pointer :: arg2(:)
    class(AbstractParent(4)), pointer :: arg3(:,:)

    allocate(Child(4)::arg1(3,5))
    allocate(arg2(5))
    allocate(Child(4)::arg3(4,8))

    if(.NOT. extends_type_of(arg1(:,:), arg2(4)%b2)) error stop 1_4
    if(.NOT. extends_type_of(arg1(:,:5)%b1(1,1), arg2(2)%b1)) error stop 2_4
    if(.NOT. extends_type_of(arg1(:,:5), arg2(2:)%base)) error stop 3_4
    if(.NOT. extends_type_of(arg3(1:,:5), arg2(:2)%base)) error stop 4_4

    if(same_type_as(arg1(:,:), arg2(4)%b2)) error stop 5_4
    if(.NOT. same_type_as(arg1(:,:5)%b1(1,1), arg2(2)%b1)) error stop 6_4
    if(.NOT. same_type_as(arg2(:)%b2(1,1), arg2(2:)%base)) error stop 7_4
    if(same_type_as(arg3(1:,:5), arg2(:2)%base)) error stop 8_4
end
