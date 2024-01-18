!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE TITLE            :
!*                                                                     
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 1/9/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
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
    type container
        class (*), pointer :: data(:) => null()
    end type

    type(container), save :: c1_m
end module

module m1
    type dataType
    end type
end module

use m
use m1

    type, extends (dataType) :: mainData
        integer*4 :: id
    end type

    type (dataType), target :: d1(6)

    type (mainData), target :: md1(10)


    class (dataType), pointer :: d_ptr(:)
    class (dataType), target, allocatable :: d_allo(:)

    integer*4, target :: iarray(20)
    class(*), pointer :: x_ptr(:) => null()

    type (container) :: c1

    ! initialize all variables
    d1 = (/(dataType(), i=11,16)/)

    md1 = (/(mainData(2*i), i=1,10)/)

    d_ptr => md1

    allocate (d_allo(3))


    c1 = container (data = d1)

    c1_m = container (data = md1)

    if (.not. associated (c1%data)) error stop 1_4
    if (size (c1%data) /= 6) error stop 2_4

    if (.not. associated (c1_m%data, md1)) error stop 3_4
    if ((lbound (c1_m%data, 1) /= 1) .or. (ubound (c1_m%data, 1) /= 10)) &
                error stop 4_4

    c1 = container (d_ptr)
    c1_m = container (data = d_allo)

    if (.not. associated (c1%data, md1)) error stop 5_4
    if (size(c1%data) /= 10) error stop 6_4

    if (.not. associated (c1_m%data)) error stop 7_4
    if (size(c1_m%data) /= 3) error stop 8_4

    c1 = container (data = x_ptr)

    if (associated(c1%data)) error stop 9_4

    x_ptr => iarray

    c1 = container (x_ptr)

    if (.not. associated (c1%data, iarray)) error stop 10_4
    if (size(c1%data) /= 20) error stop 11_4


    c1 = container (iarray)

    if (.not. associated (c1%data, iarray)) error stop 12_4
    if (size(c1%data) /= 20) error stop 13_4
end
