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
! %GROUP: fpAssgn006a1.f
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
!*  DATE                       : 02/03/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : pointer assignment (pointer assignment for
!*                               pointer component may also take
!*                               place by execution of a derived type intrinsic
!*                               assignment statement; use unlimited
!*                               poly-pointer as the component)
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
    type base
    end type

    type, extends(base) :: child
        integer*4 :: id = 1
    end type
end module

module m1
    type container
        class(*), pointer :: data => null()
    end type
end module

program fpAssgn006a1
use m
use m1

    type (container) :: co, co1

    real*4, target :: f1
    type (base), target :: b1
    type (child), target :: c1
    class (base), pointer :: b_ptr

    f1 = 10.0
    b1 = base()
    c1 = child (100)

    b_ptr => null()

    co%data => b_ptr
    co1 = co

    if (associated (co1%data) .or. associated (co%data)) error stop 1_4

    co%data => f1
    co1 = co

    if (.not. associated (co1%data, f1)) error stop 2_4

    co%data => b1
    co1 = co

    if (.not. associated (co1%data)) error stop 3_4

    if (associated (co1%data, b1)) error stop 10_4

    b_ptr => c1

    co%data => b_ptr
    co1 = co

    if (.not. associated (co1%data, c1)) error stop 4_4

end
