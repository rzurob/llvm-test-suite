! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr031.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 12, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (keyword for allocatable
!*                               component in the constructor)
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
        integer(k1), allocatable :: value(:)
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    integer*4, allocatable :: i1_m(:)

    integer*4 :: i2_m (10:11) = (/2, 4/)

    type (base(4)) :: b1_m, b2_m, b3_m
    type (base(4)) :: b4_m = base(4) (value = null())

    type (child(4,1,20)) :: c1_m

    contains

    subroutine initializeModuleData ()
        allocate (i1_m(2))

        i1_m = (/10, 100/)

        b1_m = base(4) (value = (/1, 2, 3/))
        b2_m = base(4) (value = null())
        b3_m = base(4) (value = i1_m)

        c1_m = child(4,1,20) (name = 'c1_m', value = i2_m)
    end subroutine
end module

program fconstr031
use m

    type (base(4)) :: b1 = base(4) (value = null())

    type (child(4,1,20)) :: c1 = child(4,1,20) (name = 'c1', value = null())
    type (child(4,1,20)) :: c2, c3


    c2 = child(4,1,20) (value = i1_m, name = 'c2')    !i1_m unallocated yet

    call initializeModuleData

    c3 = child(4,1,20) (i1_m, name = 'c3')


    !validate all variables
    if (allocated (b1%value)) error stop 1_4

    if (allocated(c1%value) .or. (c1%name /= 'c1')) error stop 2_4

    if (allocated(c2%value) .or. (c2%name /= 'c2')) error stop 3_4

    if ((size(c3%value) /= 2) .or. (c3%name /= 'c3')) error stop 4_4
    if ((c3%value(1) /= 10) .or. (c3%value(2) /= 100)) error stop 5_4

    ! validate the module variables
    if (allocated (b2_m%value)) error stop 6_4

    if ((lbound(b1_m%value, 1) /= 1) .or. (ubound(b1_m%value, 1) /= 3)) error stop 7_4

    do i = 2, 6, 2
        if (b1_m%value (i/2) /= i/2) error stop 8_4
    end do

    if (size(b3_m%value) /= 2) error stop 9_4

    if ((b3_m%value(1) /= 10) .or. (b3_m%value(2) /= 100)) error stop 10_4

    if ((size(c1_m%value) /= 2) .or. (c1_m%name /= 'c1_m')) error stop 11_4

    if ((lbound (c1_m%value, 1) /= 10) .or. (ubound(c1_m%value, 1) /= 11)) error stop 12_4

    do i = 1, 2
        if (c1_m%value(i+9) /= 2*i) error stop 13_4
    end do

end
