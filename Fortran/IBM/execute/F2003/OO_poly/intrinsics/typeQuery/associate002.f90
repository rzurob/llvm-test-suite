! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate002.f
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
!*  DATE                       : 10/25/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : Query type inside an associate construct.
!*    Selector is array.
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
        type(Base) :: b(3,5)
    end type
end module

program associate002
use m
    class(*), pointer :: i(:,:) => null()
    class(*), pointer :: ap1(:) => null()
    class(AbstractParent), allocatable :: ap2(:,:,:)
    type(Child) :: c

    allocate(Base::ap1(5))
    allocate(Child::ap2(2,3,7))
    allocate(integer::i(6,4))

    associate(name1=>ap1, name2=>ap2(1:,:,3:5), name3=>i(3,3), name4=>c%b)
        if(.NOT. extends_type_of(name2, name1)) error stop 1_4
        if(.NOT. extends_type_of(name2, ap1(:3))) error stop 2_4
        if(.NOT. extends_type_of(name2, name4)) error stop 3_4
        if(.NOT. extends_type_of(name4(20:10,:), name1(1:))) error stop 4_4
        if(same_type_as(name2, name1)) error stop 5_4
        if(same_type_as(name2, ap1(1:3))) error stop 6_4
        if(.NOT. same_type_as(name1, name4)) error stop 7_4
        if(.NOT. same_type_as(name4(20:10,:), name1(1:))) error stop 8_4
    end associate
end
