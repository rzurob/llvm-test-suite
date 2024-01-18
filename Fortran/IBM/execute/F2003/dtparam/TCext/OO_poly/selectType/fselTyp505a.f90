! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/selectType/fselTyp505a.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type (work around of the nested select
!                               type problem in fselTyp505.f)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fselTyp505a
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: data
    end type

    logical precision_r8
    type (base(4,20)) :: b1, b2

    allocate (b1%data, source=100)

    b2 = b1

    if (.not. allocated (b2%data)) error stop 1_4

    select type (x => b2%data)
        type is (integer)
            if (x /= 100) error stop 2_4

            deallocate (b1%data)
            allocate (b1%data, source=1.0_8)
    end select

    b2 = b1

    select type (x => b2%data)
        type is (real(8))
            if (.not. precision_r8(x, 1.0_8)) error stop 3_4
    end select
end
