!#######################################################################
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
    type base
        class(*), pointer :: data(:,:)
        class(*), pointer :: data2(:)
    end type

    type (base) b1, b2

    data b1%data, b1%data2 /null(), null()/

    data b2 /base(null(), null())/

    if (associated (b1%data)) error stop 1_4
    if (associated (b1%data2)) error stop 2_4

    if (associated (b2%data)) error stop 3_4
    if (associated (b2%data2)) error stop 4_4
end
