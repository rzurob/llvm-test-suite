! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/31/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    MATRIX is a dummy argument. Dummy argument is a pointer or
!*  allocatable, and non-poly.
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
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program argAssociation004
use m
    type(Base), pointer :: b1(:,:)
    type(Base), allocatable :: b2(:,:)

    allocate(b1(3,4), SOURCE=reshape((/(Base(i),i=1,12)/),(/3,4/)))
    allocate(b2(2,3), SOURCE=reshape((/(Base(i),i=3,8)/),(/2,3/)))

    call sub1(b1, b2)

    contains

    subroutine sub1(arg1, arg2)
        type(Base), pointer :: arg1(:,:)
        type(Base), allocatable :: arg2(:,:)

        print *, transpose(arg1)
        print *, transpose(arg2)
    end subroutine
end