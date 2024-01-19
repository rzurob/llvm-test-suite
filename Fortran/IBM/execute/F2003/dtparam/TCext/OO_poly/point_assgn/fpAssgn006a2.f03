! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qnodeferredlp /tstdev/OO_poly/point_assgn/fpAssgn006a2.f
! opt variations: -qck -ql -qdefaultpv -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (occurred for pointer
!*                               component during the intrinsic assignment;
!*                               non-poly pointer assigned to entities of
!*                               different dynamic types)
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
    type dataType(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
        contains

        procedure, pass :: idVal => dataTypeID
    end type

    type, extends(dataType) :: moduleData(n1)    ! (4,20)
        integer, len  :: n1
        character(n1) :: name

        contains

        procedure, pass :: idVal => moduleDataID
    end type

    contains

    integer*4 function dataTypeID (d)
        class (dataType(4)), intent(in) :: d
        dataTypeID = d%id
    end function

    integer*4 function moduleDataID (d)
        class (moduleData(4,*)), intent(in) :: d

        moduleDataID = 1000000 + d%id
    end function
end module

module m1
use m

    type base(k2)    ! (4)
        integer, kind               :: k2
        type(dataType(k2)), pointer :: data => null()
    end type

end module

program fpAssgn006a2
use m1
    type(base(4)) :: b1, b2

    class(dataType(4)), pointer :: d_ptr

    class (moduleData(4,20)), allocatable, target :: md_alloc
    type(moduleData(4,20)), target :: md1
    type(moduleData(4,20)), pointer :: md_ptr

    md1 = moduleData(4,20)(1, 'md1')

    ! use md1's parent component
    b1 = base(4) (md1%dataType)

    if (b1%data%idVal() /= 1) error stop 1_4

    md1%id = 10
    d_ptr => md1

    ! dynamic type of target is different from pointer's type
    b1%data => d_ptr

    if ((b1%data%idVal() /= 10) .or. (d_ptr%idVal() /= 1000010)) error stop 2_4

    allocate (md_ptr, md_alloc)
    md_ptr = moduleData(4,20)(2, 'md_ptr')

    d_ptr => md_ptr

    if (d_ptr%idVal() /= 1000002) error stop 3_4

    ! again dynamic type of target is different from pointer
    b2 = base(4) (d_ptr)

    !! intrinsic assignment
    b1 = b2

    if ((b1%data%idVal() /= 2) .or. (b2%data%idVal() /= 2)) error stop 4_4


    !! this update will be available to all associated pointers
    md_ptr = moduleData(4,20)(3, 'md_ptr')

    if (d_ptr%idVal() /= 1000003) error stop 5_4
    if ((b1%data%idVal() /= 3) .or. (b2%data%idVal() /= 3)) error stop 6_4

    deallocate (md_ptr)

    md_alloc%id = 4

    !! dynamic type of d_ptr is moduleData again
    d_ptr => md_alloc

    if ((d_ptr%idVal()/=1000004) .or. (md_alloc%idVal() /= 1000004)) error stop 7_4

    b2 = base(4) (d_ptr)
    b1 = b2

    if ((b1%data%idVal() /= 4) .or. (b2%data%idVal() /= 4)) error stop 8_4
end
