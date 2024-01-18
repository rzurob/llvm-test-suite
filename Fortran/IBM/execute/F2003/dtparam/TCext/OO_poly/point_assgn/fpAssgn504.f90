! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/point_assgn/fpAssgn504.f
! opt variations: -qnok -ql

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/25/2005
!*
!*  DESCRIPTION                : data pointer assignment (data pointer
!                               initializations via DATA statement)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn504
    type base(k1)    ! (4)
        integer, kind :: k1
        class(*), pointer :: data(:,:)
        class(*), pointer :: data2(:)
    end type

    type (base(4)) b1, b2

    data b1%data, b1%data2 /null(), null()/

    data b2 /base(4)(null(), null())/

    if (associated (b1%data)) error stop 1_4
    if (associated (b1%data2)) error stop 2_4

    if (associated (b2%data)) error stop 3_4
    if (associated (b2%data2)) error stop 4_4
end
