!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr030.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 1/8/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (unlimited poly-
!*                               pointer type, scalar pointer component)
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
    type container
        class(*), pointer :: data => null()
    end type

    type (container), save :: c1_m = container(null())
end module

module m1
    type dataType
    end type
end module

program fconstr030

use m
use m1

    type, extends(dataType) :: childData
        integer*4 :: id
    end type

    integer*4, target :: i1 = 10
    real*4, target :: r1 = 1.0

    type (childData), pointer :: cd1_ptr => null()
    type (childData), target :: cd1 = childData(10)

    type (dataType), target :: d1 = dataType ()
    class (dataType), pointer :: d1_ptr => null()

    complex, pointer :: cx1 => null()

    type (container) :: c1 = container(null())

    if (associated(c1%data)) error stop 1_4


    c1 = container(i1)
    if (.not. associated(c1%data, i1)) error stop 2_4


    c1 = container(data = r1)
    if (.not. associated(c1%data, r1)) error stop 3_4


    c1 = container(data = cd1)
    if (.not. associated(c1%data, cd1)) error stop 4_4


    c1 = container(cd1_ptr)
    if (associated(c1%data)) error stop 5_4

    c1 = container(d1)
    if (.not. associated(c1%data)) error stop 6_4


    d1_ptr => cd1
    c1 = container(data = d1_ptr)
    if (.not. associated(c1%data, cd1)) error stop 7_4

    allocate (cx1)

    cx1 = (1.0, 1.0)

    c1 = container(cx1)
    if (.not. associated(c1%data, cx1)) error stop 8_4

    deallocate (cx1)
end
