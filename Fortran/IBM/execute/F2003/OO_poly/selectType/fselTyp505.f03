! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type (nested select type and intrinsic
!                               assignment interacted)
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

program fselTyp505
    type base
        class(*), allocatable :: data
    end type

    type (base) :: b1, b2

    allocate (b1%data, source=100)

    b2 = b1

    if (.not. allocated (b2%data)) error stop 1_4

    select type (x => b2%data)
        type is (integer)
            if (x /= 100) error stop 2_4

            x = 2 * x

            b1 = b2

            select type (y => b1%data)
                type is (integer)
                    if (y /= 200) error stop 3_4
            end select
    end select
    end