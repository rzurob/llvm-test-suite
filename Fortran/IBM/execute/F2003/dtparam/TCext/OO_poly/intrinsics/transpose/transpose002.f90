! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/transpose/transpose002.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/30/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : MATRIX is poly.
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

program transpose002
use m
    class(Base(:,4)), allocatable :: b1(:,:)
    class(Base(:,4)), pointer :: b2(:,:)

    allocate(b1(2,4), SOURCE=reshape((/(Base(20,4)(i),i=1,8)/), (/2,4/)))
    allocate(b2(3,3), SOURCE=reshape((/(Child(20,4)(i,i-1),i=11,19)/), &
     (/3,3/)))

    select type(name1=>transpose(b1))
        type is (Base(*,4))
            print *, name1
        class default
            error stop 1_4
    end select

    select type(name1=>transpose(b2))
        type is (Child(*,4))
            print *, name1
        class default
            error stop 2_4
    end select
end
