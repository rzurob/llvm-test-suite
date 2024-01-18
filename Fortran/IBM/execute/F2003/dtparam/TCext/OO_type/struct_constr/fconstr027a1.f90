! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr027a1.f
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
    type base(k1,k2)    ! (4,4)
        integer, kind :: k1,k2
        integer(k1)   :: id
        real(k2)      :: value
    end type

    type, extends(base) :: child(k3,n1)    ! (4,4,1,20)
        integer, kind             :: k3
        integer, len              :: n1
        character(kind=k3,len=n1) :: name
    end type

    type (child(4,4,1,20)), allocatable :: c1_m, c2_m(:)
    type (base(4,4)), allocatable :: b1_m

    contains

        subroutine initializeModuleData

            if (allocated (c1_m)) deallocate (c1_m)
            if (allocated (c2_m)) deallocate (c2_m)
            if (allocated (b1_m)) deallocate (b1_m)

            allocate (c1_m, c2_m(5), b1_m)

            b1_m = base(4,4) (id = 10, value = 1.0)

            c1_m = child(4,4,1,20) (1, 1.0, 'c1_m')

            c2_m = (/(child(4,4,1,20) (id=i, value = i*1.0, name=''), i=2,6)/)
        end subroutine

end module


program fconstr027a1
use m

    type(base(4,4)), allocatable :: b1(:)
    type (child(4,4,1,20)), allocatable :: c1, c2(:)

    allocate (b1(10), c1, c2(5))

    b1 = base(4,4) (-1, -1.0)

    c1 = child(4,4,1,20) (base = base(4,4) (100, 1.0), name = 'c1')

    c2 = (/(child(4,4,1,20) (base = base(4,4) (id = i, value = 1.0), name = 'c2'), i=100,104)/)

    call initializeModuleData

    print *, c2_m%id, c2%id

end

