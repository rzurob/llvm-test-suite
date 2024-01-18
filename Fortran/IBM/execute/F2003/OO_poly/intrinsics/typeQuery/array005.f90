! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: array005.f
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
!*  DATE                       : 11/03/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : MOLD or A is array element or array
!*    section, explicit-shape or deferred-shape; non-poly, poly, or
!*    unlimited poly. Array sections can be empty. Section subscript
!*    can be subscript-triplet or vector-subscript. Declared type can
!*    be abstract.
!*      Besides, either MOLD or A is unlimited poly and is either a
!*    diassociated pointer or an unallocated allocatable. This is
!*    mainly to test extends_type_of.
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

program array005
use m
    class(*), allocatable :: mold0(:,:,:)
    class(*), pointer :: b0(:,:) => null()

    type(Base) :: arg1(10,5,3)
    class(AbstractParent), allocatable :: arg2(:)
    class(*), pointer :: arg3(:,:,:) => null()

    !----- second argument is unlimited poly and is unallocated allocatable

    allocate(Base::arg2(8))
    allocate(Child::arg3(6,7,9))

    if(.NOT. extends_type_of(arg1(2:5,1:5,1:1), mold0)) error stop 1_4
    if(.NOT. extends_type_of(arg1(5:3,1:5:-2,3:1), mold0)) error stop 2_4
    if(.NOT. extends_type_of(arg2(2:), mold0)) error stop 3_4
    if(.NOT. extends_type_of(arg3(:,:,:), mold0)) error stop 4_4

    if(same_type_as(arg1(2:5,1:5,1:1), mold0)) error stop 5_4
    if(same_type_as(arg1(5:3,1:5:-2,3:1), mold0)) error stop 6_4
    if(same_type_as(arg2(2:), mold0)) error stop 7_4
    if(same_type_as(arg3(:,:,:), mold0)) error stop 8_4

    deallocate(arg3)
    allocate(integer::arg3(6,7,9))

    if(.NOT. extends_type_of(arg3(2:5,1:5,1:1), mold0)) error stop 9_4
    if(.NOT. extends_type_of(arg3(5:3,1:5:-2,9:3), mold0)) error stop 10_4
    if(.NOT. extends_type_of(arg3(:,:,:), mold0)) error stop 11_4

    if(same_type_as(arg3(2:5,1:5,1:1), mold0)) error stop 12_4
    if(same_type_as(arg3(5:3,1:5:-2,9:3), mold0)) error stop 13_4
    if(same_type_as(arg3(:,:,:), mold0)) error stop 14_4

    !----- first argument is unlimited poly and is disassociated pointer

    if(extends_type_of(b0, arg1(2:5,1:5,1:1))) error stop 15_4
    if(extends_type_of(b0, arg1(5:3,1:5:-2,9:3))) error stop 16_4
    if(extends_type_of(b0, arg2(2:))) error stop 17_4
    if(extends_type_of(b0, arg3(:,:,:))) error stop 18_4

    if(same_type_as(b0, arg1(2:5,1:5,1:1))) error stop 19_4
    if(same_type_as(b0, arg1(5:3,1:5:-2,8:3))) error stop 20_4
    if(same_type_as(b0, arg2(2:))) error stop 21_4
    if(same_type_as(b0, arg3(:,:,:))) error stop 22_4

    deallocate(arg3)
    allocate(Child::arg3(6,7,9))

    if(extends_type_of(b0, arg3(2:5,1:5,1:1))) error stop 23_4
    if(extends_type_of(b0, arg3(5:3,1:5:-2,9:3))) error stop 24_4
    if(extends_type_of(b0, arg3(:,:,:))) error stop 25_4

    if(same_type_as(b0, arg3(2:5,1:5,1:1))) error stop 26_4
    if(same_type_as(b0, arg3(5:3,1:5:-2,9:3))) error stop 27_4
    if(same_type_as(b0, arg3(:,:,:))) error stop 28_4
end
