! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/transpose/argAssociation005.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/31/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    MATRIX is a dummy argument. Dummy argument is a pointer or
!*  allocatable, and poly.
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
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type
end module

program argAssociation005
use m
    class(Base(:,4)), pointer :: b1(:,:)
    class(*), allocatable :: b2(:,:)

    allocate(b1(3,4), SOURCE=reshape((/(Child(20,4)(i,i),i=1,12)/),(/3,4/)))
    allocate(b2(2,3), SOURCE=reshape((/(Base(20,4)(i),i=3,8)/),(/2,3/)))

    call sub1(b1, b2)

    contains

    subroutine sub1(arg1, arg2)
        class(Base(:,4)), pointer :: arg1(:,:)
        class(*), allocatable :: arg2(:,:)

        select type(name1=>transpose(arg1))
            type is (Child(*,4))
                print *, name1
                if(size(name1) .NE. 12) error stop 1_4
                if(ubound(name1, DIM=1) .NE. 4) error stop 2_4
                if(ubound(name1, DIM=2) .NE. 3) error stop 3_4
            class default
                error stop 4_4
        end select

        select type(name1=>transpose(arg2))
            type is (Base(*,4))
                print *, name1
                if(size(name1) .NE. 6) error stop 5_4
                if(ubound(name1, DIM=1) .NE. 3) error stop 6_4
                if(ubound(name1, DIM=2) .NE. 2) error stop 7_4
            class default
                error stop 8_4
        end select
    end subroutine
end
