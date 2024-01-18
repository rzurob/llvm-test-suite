! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr027a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/22/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (data assignment for
!*                               allocatable objects)
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

            c2_m = (/(child(4,4,1,20) (id=i, value = i*1.0, name='c2_m'), i=2,6)/)
        end subroutine

end module


program fconstr027a
use m

    type(base(4,4)), allocatable :: b1(:)

    type (child(4,4,1,20)), allocatable :: c1, c2(:)

    integer i, j
    logical precision_r4

    allocate (b1(10), c1, c2(5))

    b1 = base(4,4) (-1, -1.0)

    c1 = child(4,4,1,20) (base = base(4,4) (100, 1.0), name = 'c1')

    c2 = (/(child(4,4,1,20) (base = base(4,4) (id = i, value = 1.0), name = 'c2'), i=100,104)/)

    call initializeModuleData

    !validate the variables

    do j = 1, 10
        if ((b1(j)%id /= -1) .or. (b1(j)%value /= -1)) error stop 1_4
    end do

    if ((c1%id /= 100) .or. (c1%value /= 1.0) .or. (c1%name /= 'c1')) &
                    error stop 2_4

    do j = 5, 1, -1
        if ((c2(j)%id /= 99+j) .or. (c2(j)%value /= 1.0) .or. &
                    (c2(j)%name /= 'c2')) error stop 3_4
    end do

    if ((b1_m%id /= 10) .or. (b1_m%value /= 1.0)) error stop 4_4

    if ((c1_m%id /= 1) .or. (c1_m%value /= 1.0) .or. (c1_m%name /= 'c1_m')) &
                    error stop 5_4

    do j = 1, 5
        if ((c2_m(j)%id /= j+1) .or. (.not. precision_r4(c2_m(j)%value, &
            (j+1)*1.0)) .or. (c2_m(j)%name /= 'c2_m')) error stop 6_4
    end do
end
