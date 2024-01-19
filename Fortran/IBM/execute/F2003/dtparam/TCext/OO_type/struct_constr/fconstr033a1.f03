! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr033a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (unlimited
!                               poly-allocatable scalar component; use of
!                               character type and logical type as data-source)
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

program fconstr033a1
    class (*), allocatable :: x1, x2

    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class (*), allocatable :: data
    end type

    allocate (x1, source='xlftest 101')
    allocate (x2, source=(10>1))

    !! test the structure constructors
    associate (y1 => base(4,20)(x1), y2 => base(4,20)(x2))
        if ((.not. allocated (y1%data)) .or. (.not. allocated (y2%data))) &
                    error stop 1_4

        !! test y1%data first
        select type (z1 => y1%data)
            type is (character(*))
                print *, z1
            class default
                print *, 'wrong'
        end select

        !! then y2%data
        select type (z2 => y2%data)
            type is (logical)
                print *, z2
            class default
                print *, 'wrong'
        end select
    end associate
end
