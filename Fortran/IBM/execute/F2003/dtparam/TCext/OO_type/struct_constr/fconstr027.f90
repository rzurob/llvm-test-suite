! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr027.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/22/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (use heap memory)
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

    type, extends(child) :: thirdGeneration(k4)    ! (4,4,1,20,1)
        integer, kind :: k4
        logical(k4)   :: isSet
    end type

    type (child(4,4,1,20)), pointer :: c1_m => null()
    type (thirdGeneration(4,4,1,20,1)), pointer :: t1_m(:) => null()

    contains

        subroutine initializeModuleData
            if (associated (c1_m)) nullify (c1_m) !assume no allocation
            if (associated (t1_m)) nullify (t1_m) !assume no allocation

            allocate (c1_m, t1_m(2))
            c1_m = child(4,4,1,20) (1, 1.0, 'c1_m')
            t1_m(1) = thirdGeneration(4,4,1,20,1)(id=2, value=2.0, name='t1_m', isSet=.true.)
            t1_m(2) = thirdGeneration(4,4,1,20,1)(child = child(4,4,1,20)(3, 3.0, name='t1_m2'), &
                    isSet = (associated (t1_m)))
        end subroutine

        subroutine cleanModuleData
            deallocate (c1_m, t1_m)
        end subroutine
end module


program fconstr027
use m

    type (thirdGeneration(4,4,1,20,1)), pointer :: t1

    allocate (t1)

    t1 = thirdGeneration(4,4,1,20,1) (3, value = 3.0, name = 't1', isSet=.true.)

    if (t1%id /= 3) error stop 1_4

    if ((t1%id /= t1%child%id) .or. (t1%id /= t1%base%id) &
        .or. (t1%id /= t1%child%base%id)) error stop 2_4

    if (t1%value /= 3.0) error stop 3_4

    if ((t1%value /= t1%child%value) .or. (t1%value /= t1%base%value) &
        .or. (t1%value /= t1%child%base%value)) error stop 4_4

    if (t1%name /= 't1') error stop 5_4

    if (t1%name /= t1%child%name) error stop 6_4

    if (.not. t1%isSet) error stop 7_4

    ! initialize module objects
    call initializeModuleData

    if (c1_m%id /= 1) error stop 8_4
    if (c1_m%id /= c1_m%base%id) error stop 9_4

    if (c1_m%name /= 'c1_m') error stop 10_4

    if (t1_m(1)%id /= 2) error stop 11_4

    if ((t1_m(1)%id /= t1_m(1)%child%id) .or. (t1_m(1)%id /= t1_m(1)%base%id) &
        .or. (t1_m(1)%id /= t1_m(1)%child%base%id)) error stop 12_4


    if (t1_m(1)%value /= 2.0) error stop 13_4

    if ((t1_m(1)%value /= t1_m(1)%child%value) .or. (t1_m(1)%value /= t1_m(1)%base%value) &
        .or. (t1_m(1)%value /= t1_m(1)%child%base%value)) error stop 14_4

    if (t1_m(1)%name /= 't1_m') error stop 15_4

    if (t1_m(1)%name /= t1_m(1)%child%name) error stop 16_4

    if (.not. t1_m(1)%isSet) error stop 17_4

    if ((t1_m(2)%id /= 3) .or. (t1_m(2)%name /= 't1_m2') .or. &
            (t1_m(2)%value /= 3.0))  error stop 18_4

    call cleanModuleData

    deallocate (t1)

end
