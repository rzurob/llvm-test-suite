! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (unlimited
!                               poly-allocatable component in structure
!                               constructor)
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

program fconstr033
    class (*), allocatable :: x1

    type base
        class (*), allocatable :: data
    end type

    associate (x => base(null()), y => base(x1))
        if (allocated (x%data) .or. allocated (y%data)) error stop 1_4
    end associate


    allocate (x1, source=100_8)

    associate (x => base (x1))
        if (.not. allocated (x%data)) error stop 2_4

        select type (y => x%data)
            type is (integer(8))
                if (y /= 100) error stop 3_4
            class default
                error stop 4_4
        end select
    end associate
end