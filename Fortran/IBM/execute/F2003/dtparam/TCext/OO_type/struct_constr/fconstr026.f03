! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr026.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/22/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (pointer component
!*                               initialization, pointer of intrinsic type)
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
        integer, kind        :: k1,k2
        integer(k1), pointer :: id
        real(k2)             :: value
    end type

    type, extends(base) :: child(k3,n1)    ! (4,4,1,20)
        integer, kind             :: k3
        integer, len              :: n1
        character(kind=k3,len=n1) :: name
    end type

    type, extends(child) :: thirdGeneration(k4)    ! (4,4,1,20,1)
        integer, kind :: k4
        logical(k4)   :: isSet
    end type

    integer*4, target :: x_m = 10

    type (base(4,4)) :: b1_m = base(4,4) (null(), 1.0)
    type (child(4,4,1,20)) :: c1_m = child(4,4,1,20) (name = 'c1_m', base = base(4,4) &
                    (id = null(), value = 2.0))

    type (thirdGeneration(4,4,1,20,1)) :: t1_m = thirdGeneration(4,4,1,20,1) (isSet = .true., &
         child = child(4,4,1,20) (base = base(4,4) (null(), value = 3.0), name = 't1_m'))


    contains

    subroutine updateModuleData
        b1_m = base(4,4) (value = 1.0, id = x_m)

        c1_m = child(4,4,1,20) (x_m, value = 1.0, name = 'c1_m')

        t1_m = thirdGeneration(4,4,1,20,1) (name = 't1_m', base = base(4,4) (x_m, 1.0), &
                isSet = (x_m == 10))
    end subroutine
end module


program fconstr026
use m

    integer*4, target :: x = 100
    type (base(4,4)) :: b1
    type (child(4,4,1,20)) :: c1, c2
    type (thirdGeneration(4,4,1,20,1)) :: t1

    b1 = base(4,4) (id = null(), value = 3)
    c1 = child(4,4,1,20) (name = 'c1', base = base(4,4)(id = x, value = 4.0))
    t1 = thirdGeneration(4,4,1,20,1) (name = 't1', isSet = t1_m%isSet, base = b1_m)

    c2 = child(4,4,1,20) (name = 'c2', base = base(4,4)(x, 5.0))

    ! validate all objects

    if (associated(b1_m%id) .or. (b1_m%value /= 1.0)) error stop 1_4

    if (associated(c1_m%id) .or. (c1_m%value /= 2.0) .or. &
        (c1_m%name /= 'c1_m'))  error stop 2_4

    if (associated(t1_m%id) .or. (t1_m%value /= 3.0) .or. &
        (t1_m%name /= 't1_m') .or. (.not. t1_m%isSet)) error stop 3_4

    if (associated(b1%id) .or. (b1%value /= 3.0)) error stop 4_4

    if ((c1%id /= 100) .or. (c1%value /= 4.0) .or. (c1%name /= 'c1')) &
            error stop 5_4

    if ((.not. associated (c2%id, x)) .or. (c2%value /= 5.0) .or. &
            (c2%name /= 'c2')) error stop 6_4

    if (associated(t1%id) .or. (t1%value /= 1.0) .or. (t1%name /='t1') .or. &
        (.not. t1%isSet)) error stop 7_4


    call updateModuleData

    ! validate the module data again
    if ((b1_m%id /= 10) .or. (b1_m%value /= 1.0)) error stop 8_4

    if ((.not. associated(c1_m%id, x_m)) .or. (c1_m%value /= 1.0) .or. &
            (c1_m%name /= 'c1_m')) error stop 9_4

    if ((.not. associated(t1_m%id, x_m)) .or. (t1_m%value /= 1.0) .or. &
            (t1_m%name /= 't1_m') .or. (.not. t1_m%isSet)) error stop 10_4
end
