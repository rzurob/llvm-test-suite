! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (intrinsic types used as source-expr
!                               for allocating unlimited poly pointer array)
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

program falloc006a7
    class(*), pointer :: x1(:)

    logical precision_x6, precision_r4

    allocate (x1(0:1), source=(/(1.4d0, 2.1d0), (.5d0, .1d1)/))

    select type (x1)
        type is (complex(8))
            if (.not. precision_x6 (x1(0), (1.4d0, 2.1d0))) error stop 1_4

            if (.not. precision_x6 (x1(1), (.5d0, .1d1))) error stop 2_4
        class default
            error stop 3_4
    end select

    deallocate (x1)

    allocate (x1(3), source=((/1.2e0, -.4e0, -1.56e3/)))

    select type (y=> x1(::3))
        type is (real(4))
            if (size(y) /= 1) error stop 4_4

            if (.not. precision_r4(y(1), 1.2e0)) error stop 5_4
        class default
            error stop 7_4
    end select
end
