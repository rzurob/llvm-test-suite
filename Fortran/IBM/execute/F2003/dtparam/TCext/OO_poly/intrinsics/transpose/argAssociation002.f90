! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/transpose/argAssociation002.f
! opt variations: -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/31/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    MATRIX is a dummy argument. Dummy argument is non-pointer,
!*  non-allocatable, and poly.
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
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program argAssociation002
use m
    type(Base(4)) :: b1(3,4)
    type(Child(4)) :: c1(2,3)
    class(Base(4)), pointer :: b2(:,:)
    class(Child(4)), allocatable :: c2(:,:)

    b1 = reshape((/(Base(4)(i),i=1,12)/), (/3,4/))
    c1 = reshape((/(Child(4)(i,i-1),i=2,7)/), (/2,3/))

    allocate(b2(4,2), SOURCE=reshape((/(Child(4)(i,i+1),i=2,9)/), (/4,2/)))
    allocate(c2(3,3), SOURCE=reshape((/(Child(4)(i,i),i=-9,-1)/), (/3,3/)))

    call sub1(b1, c1, b2, c2)

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        class(Base(4)) :: arg1(3, 4)
        class(Base(4)) :: arg2(:,:)
        class(Base(4)) :: arg3(4, 2)
        class(Base(4)) :: arg4(:,:)

        select type(name1=>transpose(arg1))
            type is (Base(4))
                print *, name1
            class default
                error stop 1_4
        end select

        select type(name1=>transpose(arg2))
            type is (Child(4))
                print *, name1
                if(ubound(name1, DIM=1) .NE. 3) error stop 2_4
                if(ubound(name1, DIM=2) .NE. 2) error stop 3_4
            class default
                error stop 4_4
        end select

        select type(name1=>transpose(arg3))
            type is (Child(4))
                print *, name1
            class default
                error stop 5_4
        end select

        select type(name1=>transpose(arg4))
            type is (Child(4))
                print *, name1
                if(ubound(name1, DIM=1) .NE. 3) error stop 6_4
                if(ubound(name1, DIM=2) .NE. 3) error stop 7_4
            class default
                error stop 8_4
        end select
    end subroutine
end
