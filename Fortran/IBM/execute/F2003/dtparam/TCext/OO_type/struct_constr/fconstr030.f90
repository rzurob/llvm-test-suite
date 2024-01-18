! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr030.f
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
! %GROUP: fconstr030.f
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
!*  DATE                       : 1/8/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
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
    type container(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), pointer :: data => null()
    end type

    type (container(4,20)), save :: c1_m = container(4,20)(null())
end module

module m1
    type dataType(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
    end type
end module

program fconstr030

use m
use m1

    type, extends(dataType) :: childData(k3)    ! (4,20,4)
        integer, kind :: k3
        integer(k3)   :: id
    end type

    integer*4, target :: i1 = 10
    real*4, target :: r1 = 1.0

    type (childData(4,20,4)), pointer :: cd1_ptr => null()
    type (childData(4,20,4)), target :: cd1 = childData(4,20,4)(10)

    type (dataType(4,20)), target :: d1 = dataType(4,20) ()
    class (dataType(4,20)), pointer :: d1_ptr => null()

    complex, pointer :: cx1 => null()

    type (container(4,20)) :: c1 = container(4,20)(null())

    if (associated(c1%data)) error stop 1_4


    c1 = container(4,20)(i1)
    if (.not. associated(c1%data, i1)) error stop 2_4


    c1 = container(4,20)(data = r1)
    if (.not. associated(c1%data, r1)) error stop 3_4


    c1 = container(4,20)(data = cd1)
    if (.not. associated(c1%data, cd1)) error stop 4_4


    c1 = container(4,20)(cd1_ptr)
    if (associated(c1%data)) error stop 5_4

    c1 = container(4,20)(d1)
    if (.not. associated(c1%data)) error stop 6_4


    d1_ptr => cd1
    c1 = container(4,20)(data = d1_ptr)
    if (.not. associated(c1%data, cd1)) error stop 7_4

    allocate (cx1)

    cx1 = (1.0, 1.0)

    c1 = container(4,20)(cx1)
    if (.not. associated(c1%data, cx1)) error stop 8_4

    deallocate (cx1)
end
