! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr035.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (allocation status
!*                               is the same as the allocatable entity in expr)
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

    type base(k1)    ! (4)
        integer, kind            :: k1
        integer(k1), allocatable :: flag (:)
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

end module

program fconstr035
use m
    integer*4, allocatable :: blankA(:)

    type (child(4,1,20)) :: c1
    type (base(4)) :: b1

    c1 = child(4,1,20) (base = base(4)(flag = blankA), name = 'c1')
    b1 = base(4) (blankA)


    if (allocated (b1%flag)) error stop 1_4
    if (allocated (c1%flag)) error stop 2_4

    allocate (blankA(3))

    blankA = (/2, 3, 1/)

    b1 = base(4) (flag = blankA)
    if (.not. allocated (b1%flag) .or. (size(b1%flag) /= 3)) error stop 3_4

    c1 = child(4,1,20) (base = base(4)(flag=blankA), name = 'c1')

    if ((.not. allocated (c1%flag)) .or. (c1%name /= 'c1')) error stop 4_4

    if (size (c1%flag) /= 3) error stop 5_4


    if ((c1%flag(1) /= 2) .or. (c1%flag(2) /= 3) .or. (c1%flag(3) /= 1)) error stop 6_4
end
