! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp /tstdev/OO_poly/point_assgn/fpAssgn003d2.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/06/2005
!*
!*  DESCRIPTION                : data pointer assignment (for associated()
!                               intrinsic TARGET must be of a type that is
!                               allowed in a data pointer assignment statement)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn003d2
    type A(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type B(n2,k2)    ! (20,4)
        integer, kind  :: k2
        integer, len   :: n2
        type(A(n2,k2))    a1
    end type

    integer, pointer :: i_ptr
    type(A(:,4)), pointer :: a_ptr

    type (A(20,4)), target :: aa
    type (B(20,4)), target :: bb

    i_ptr => aa%i

    a_ptr => bb%a1

    print *, associated (i_ptr, aa), associated (a_ptr, bb) !<-- illegal call

end

