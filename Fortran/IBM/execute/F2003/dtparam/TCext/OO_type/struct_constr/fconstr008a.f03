! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr008a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 12, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (three generations and
!*                               the previous two generations are both empty
!*                               types)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(base) :: child(k2,n2)    ! (4,20,4,20)
        integer, kind :: k2
        integer, len  :: n2
    end type

    type, extends (child) :: thirdGeneration(k3,k4,n3)    ! (4,20,4,20,4,1,20)
        integer, kind             :: k3,k4
        integer, len              :: n3
        integer(k3)               :: id
        character(kind=k4,len=n3) :: name
    end type

    type (thirdGeneration(4,20,4,20,4,1,20)) :: t1_m = thirdGeneration(4,20,4,20,4,1,20) (1, 't1_m')
    type (thirdGeneration(4,20,4,20,4,1,20)) :: t2_m = thirdGeneration(4,20,4,20,4,1,20) (name = 't2_m', id = 2)

    type (thirdGeneration(4,20,4,20,4,1,20)) :: t3_m = thirdGeneration(4,20,4,20,4,1,20) (base = base(4,20)(), id = 10, &
            name = 't3_m')

    type (thirdGeneration(4,20,4,20,4,1,20)) :: t4_m = thirdGeneration(4,20,4,20,4,1,20) (child = child(4,20,4,20)(), id = 20, &
            name = 't4_m')

end module

program fconstr008a
use m

    type (base(4,20)) :: b1 = base(4,20)()
    type (child(4,20,4,20)) :: c1 = child(4,20,4,20)()
    type (thirdGeneration(4,20,4,20,4,1,20)) :: t1 = thirdGeneration(4,20,4,20,4,1,20) (3, name = 't1')
    type (thirdGeneration(4,20,4,20,4,1,20)) :: t2 = thirdGeneration(4,20,4,20,4,1,20) (name = 't2', id = 4)

    type (thirdGeneration(4,20,4,20,4,1,20)) :: t3 = thirdGeneration(4,20,4,20,4,1,20) (base = base(4,20)(), name = &
            't3', id = 30)

    type (thirdGeneration(4,20,4,20,4,1,20)) :: t4 = thirdGeneration(4,20,4,20,4,1,20) (child = child(4,20,4,20)(), id = &
            40, name = 't4')


    ! validate all the data entities
    if (t1_m%id /= 1) error stop 1_4
    if (t1_m%name /= 't1_m') error stop 2_4

    if (t2_m%id /= 2) error stop 3_4
    if (t2_m%name /= 't2_m') error stop 4_4

    if (t1%id /= 3) error stop 5_4
    if (t1%name /= 't1') error stop 6_4

    if (t2%id /= 4) error stop 7_4
    if (t2%name /= 't2') error stop 8_4

    if (t3_m%id /= 10) error stop 9_4
    if (t3_m%name /= 't3_m') error stop 10_4

    if (t4_m%id /= 20) error stop 11_4
    if (t4_m%name /= 't4_m') error stop 12_4

    if (t3%id /= 30) error stop 13_4
    if (t3%name /= 't3') error stop 14_4

    if (t4%id /= 40) error stop 15_4
    if (t4%name /= 't4') error stop 16_4

    print *, b1, c1     ! nothing prints out
end
