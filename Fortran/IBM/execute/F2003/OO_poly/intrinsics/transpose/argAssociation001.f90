! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/31/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    MATRIX is a dummy argument. Dummy argument is non-pointer,
!*  non-allocatable, and non-poly.
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

program argAssociation001
use m
    type(Base) :: b(3,4)
    class(Base), pointer :: c(:,:)

    b = reshape((/(Base(i),i=1,12)/), (/3,4/))
    allocate(c(4,2), SOURCE=reshape((/(Child(i,i-1),i=2,9)/), (/4,2/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        type(Base) :: arg1(3, 4)
        type(Base), CONTIGUOUS :: arg2(:,:)

        print *, transpose(arg1)
        print *, transpose(arg2)
    end subroutine
end
