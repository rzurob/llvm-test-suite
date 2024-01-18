! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr030a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr030a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 1/9/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (unlimited poly-pointer
!*                               array component)
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
    type container(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class (*), pointer :: data(:) => null()
    end type

    type(container(4,20)), save :: c1_m
end module

module m1
    type dataType(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
    end type
end module

use m
use m1

    type, extends (dataType) :: mainData(k3)    ! (4,20,4)
        integer, kind :: k3
        integer(k3)   :: id
    end type

    type (dataType(4,20)), target :: d1(6)

    type (mainData(4,20,4)), target :: md1(10)


    class (dataType(4,20)), pointer :: d_ptr(:)
    class (dataType(4,20)), target, allocatable :: d_allo(:)

    integer*4, target :: iarray(20)
    class(*), pointer :: x_ptr(:) => null()

    type (container(4,20)) :: c1

    ! initialize all variables
    d1 = (/(dataType(4,20)(), i=11,16)/)

    md1 = (/(mainData(4,20,4)(2*i), i=1,10)/)

    d_ptr => md1

    allocate (d_allo(3))


    c1 = container(4,20) (data = d1)

    c1_m = container(4,20) (data = md1)

    if (.not. associated (c1%data)) error stop 1_4
    if (size (c1%data) /= 6) error stop 2_4

    if (.not. associated (c1_m%data, md1)) error stop 3_4
    if ((lbound (c1_m%data, 1) /= 1) .or. (ubound (c1_m%data, 1) /= 10)) &
                error stop 4_4

    c1 = container(4,20) (d_ptr)
    c1_m = container(4,20) (data = d_allo)

    if (.not. associated (c1%data, md1)) error stop 5_4
    if (size(c1%data) /= 10) error stop 6_4

    if (.not. associated (c1_m%data)) error stop 7_4
    if (size(c1_m%data) /= 3) error stop 8_4

    c1 = container(4,20) (data = x_ptr)

    if (associated(c1%data)) error stop 9_4

    x_ptr => iarray

    c1 = container(4,20) (x_ptr)

    if (.not. associated (c1%data, iarray)) error stop 10_4
    if (size(c1%data) /= 20) error stop 11_4


    c1 = container(4,20) (iarray)

    if (.not. associated (c1%data, iarray)) error stop 12_4
    if (size(c1%data) /= 20) error stop 13_4
end
