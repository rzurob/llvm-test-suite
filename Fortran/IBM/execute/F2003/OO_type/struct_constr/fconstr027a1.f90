! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
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

module m
    type base
        integer*4 :: id
        real*4 :: value
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    type (child), allocatable :: c1_m, c2_m(:)
    type (base), allocatable :: b1_m

    contains

        subroutine initializeModuleData

            if (allocated (c1_m)) deallocate (c1_m)
            if (allocated (c2_m)) deallocate (c2_m)
            if (allocated (b1_m)) deallocate (b1_m)

            allocate (c1_m, c2_m(5), b1_m)

            b1_m = base (id = 10, value = 1.0)

            c1_m = child (1, 1.0, 'c1_m')

            c2_m = (/(child (id=i, value = i*1.0, name=''), i=2,6)/)
        end subroutine

end module


program fconstr027a1
use m

    type(base), allocatable :: b1(:)
    type (child), allocatable :: c1, c2(:)

    allocate (b1(10), c1, c2(5))

    b1 = base (-1, -1.0)

    c1 = child (base = base (100, 1.0), name = 'c1')

    c2 = (/(child (base = base (id = i, value = 1.0), name = 'c2'), i=100,104)/)

    call initializeModuleData

    print *, c2_m%id, c2%id

end

