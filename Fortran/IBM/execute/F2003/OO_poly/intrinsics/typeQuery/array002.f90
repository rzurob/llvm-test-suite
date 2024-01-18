! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: array002.f
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
!*  DESCRIPTION                : MOLD and A are multi dimensional whole
!*    array, explicit-shape or deferred-shape; non-poly, poly, or
!*    unlimited poly. Declared type can be abstract.
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

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type
end module

program array002
use m
    type(Base) :: mold1(5,2)
    class(AbstractParent), pointer :: mold2(:,:) => null()
    class(*), allocatable :: mold3(:,:,:)

    type(Child) :: b1(10,5,3)
    class(Base), allocatable :: b2(:,:,:)
    class(*), pointer :: b3(:,:) => null()

    if(.NOT. extends_type_of(b1, mold1)) error stop 1_4
    if(.NOT. extends_type_of(b1, mold2)) error stop 2_4
    if(.NOT. extends_type_of(b1, mold3)) error stop 3_4
    if(.NOT. extends_type_of(b2, mold1)) error stop 4_4
    if(.NOT. extends_type_of(b2, mold2)) error stop 5_4
    if(.NOT. extends_type_of(b2, mold3)) error stop 6_4
    if(extends_type_of(b3, mold1)) error stop 7_4
    if(extends_type_of(b3, mold2)) error stop 8_4
    if(.NOT. extends_type_of(b3, mold3)) error stop 9_4

    if(same_type_as(b1, mold1)) error stop 10_4
    if(same_type_as(b1, mold2)) error stop 11_4
    if(same_type_as(b1, mold3)) error stop 12_4
    if(.NOT. same_type_as(b2, mold1)) error stop 13_4
    if(same_type_as(b2, mold2)) error stop 14_4
    if(same_type_as(b2, mold3)) error stop 15_4
    if(same_type_as(b3, mold1)) error stop 16_4
    if(same_type_as(b3, mold2)) error stop 17_4
    if(same_type_as(b3, mold3)) error stop 18_4

    allocate(Child::mold2(5,6))
    allocate(Base::mold3(3,5,8))
    allocate(Base::b2(2,3,10))
    allocate(Child::b3(6,9))

    if(.NOT. extends_type_of(b1, mold1)) error stop 19_4
    if(.NOT. extends_type_of(b1, mold2)) error stop 20_4
    if(.NOT. extends_type_of(b1, mold3)) error stop 21_4
    if(.NOT. extends_type_of(b2, mold1)) error stop 22_4
    if(extends_type_of(b2, mold2)) error stop 23_4
    if(.NOT. extends_type_of(b2, mold3)) error stop 24_4
    if(.NOT. extends_type_of(b3, mold1)) error stop 25_4
    if(.NOT. extends_type_of(b3, mold2)) error stop 26_4
    if(.NOT. extends_type_of(b3, mold3)) error stop 27_4

    if(same_type_as(b1, mold1)) error stop 28_4
    if(.NOT. same_type_as(b1, mold2)) error stop 29_4
    if(same_type_as(b1, mold3)) error stop 30_4
    if(.NOT. same_type_as(b2, mold1)) error stop 31_4
    if(same_type_as(b2, mold2)) error stop 32_4
    if(.NOT. same_type_as(b2, mold3)) error stop 33_4
    if(same_type_as(b3, mold1)) error stop 34_4
    if(.NOT. same_type_as(b3, mold2)) error stop 35_4
    if(same_type_as(b3, mold3)) error stop 36_4
end
