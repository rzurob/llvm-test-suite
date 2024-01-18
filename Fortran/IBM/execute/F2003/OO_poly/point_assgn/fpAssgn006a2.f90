!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn006a2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
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
    type dataType
        integer*4 :: id
        contains

        procedure, pass :: idVal => dataTypeID
    end type

    type, extends(dataType) :: moduleData
        character*20 :: name

        contains

        procedure, pass :: idVal => moduleDataID
    end type

    contains

    integer*4 function dataTypeID (d)
        class (dataType), intent(in) :: d
        dataTypeID = d%id
    end function

    integer*4 function moduleDataID (d)
        class (moduleData), intent(in) :: d

        moduleDataID = 1000000 + d%id
    end function
end module

module m1
use m

    type base
        type (dataType), pointer :: data => null()
    end type

end module

program fpAssgn006a2
use m1
    type(base) :: b1, b2

    class(dataType), pointer :: d_ptr

    class (moduleData), allocatable, target :: md_alloc
    type(moduleData), target :: md1
    type(moduleData), pointer :: md_ptr

    md1 = moduleData(1, 'md1')

    ! use md1's parent component
    b1 = base (md1%dataType)

    if (b1%data%idVal() /= 1) error stop 1_4

    md1%id = 10
    d_ptr => md1

    ! dynamic type of target is different from pointer's type
    b1%data => d_ptr

    if ((b1%data%idVal() /= 10) .or. (d_ptr%idVal() /= 1000010)) error stop 2_4

    allocate (md_ptr, md_alloc)
    md_ptr = moduleData(2, 'md_ptr')

    d_ptr => md_ptr

    if (d_ptr%idVal() /= 1000002) error stop 3_4

    ! again dynamic type of target is different from pointer
    b2 = base (d_ptr)

    !! intrinsic assignment
    b1 = b2

    if ((b1%data%idVal() /= 2) .or. (b2%data%idVal() /= 2)) error stop 4_4


    !! this update will be available to all associated pointers
    md_ptr = moduleData(3, 'md_ptr')

    if (d_ptr%idVal() /= 1000003) error stop 5_4
    if ((b1%data%idVal() /= 3) .or. (b2%data%idVal() /= 3)) error stop 6_4

    deallocate (md_ptr)

    md_alloc%id = 4

    !! dynamic type of d_ptr is moduleData again
    d_ptr => md_alloc

    if ((d_ptr%idVal()/=1000004) .or. (md_alloc%idVal() /= 1000004)) error stop 7_4

    b2 = base (d_ptr)
    b1 = b2

    if ((b1%data%idVal() /= 4) .or. (b2%data%idVal() /= 4)) error stop 8_4
end
