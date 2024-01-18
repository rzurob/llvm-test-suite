! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/transpose/argumentKeyword001.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

!***********************************************************************
!* =====================================================================
!* DATE                       : 12/30/2004
!* PRIMARY FUNCTIONS TESTED   : transpose
!* DESCRIPTION                :
!*   Actual arguments are specified using argument keywords.
!* =====================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 03/29/05
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) Removed TRUN header.
!*                              2) Change test case: poly entity cannot
!*                                 be processed by regular IO.
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(n2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      j
    end type
end module

program argumentKeyword001
use m
    class(Base(:,4)), pointer :: b2(:,:)

    allocate(b2(2,4), SOURCE=reshape((/(Base(20,4)(i),i=1,8)/),(/2,4/)))

    print *, transpose(MATRIX=reshape((/(Base(20,4)(i),i=1,8)/),(/2,4/)))

    select type(name1=>transpose(MATRIX=b2))
        type is (Base(*,4))
            print *, name1
        class default
            error stop 1_4
    end select
end
