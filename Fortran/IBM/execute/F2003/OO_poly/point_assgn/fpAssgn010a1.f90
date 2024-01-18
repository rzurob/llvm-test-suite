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
! %GROUP: fpAssgn010a1.f
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
!*  DESCRIPTION                : pointer assignment (assignment causes pointers
!*                               to be disassociated; non-poly pointer as LHS)
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
        integer id
    end type
end module

program fpAssgn010a1
use m
    type (base), pointer :: b_ptr1
    class (base), pointer :: b_ptr2
    type (child) :: c_ptr1, c1
    pointer c_ptr1
    target c1
    class (child), pointer :: c_ptr2

    b_ptr1 => c1%base

    if (.not. associated (b_ptr1)) error stop 1_4

    c_ptr1 => c1
    b_ptr2 => c1

    allocate (c_ptr2)

    deallocate (c_ptr2)

    c_ptr1 => c_ptr2        ! c_ptr1 diassociated

    if (associated (c_ptr1) .or. associated(c_ptr2)) error stop 2_4

    c_ptr2 => c1

    nullify (c_ptr2)
    b_ptr2 => c_ptr2        ! b_ptr2 dissociated

    if (associated (b_ptr2) .or. associated (c_ptr2)) error stop 3_4

    b_ptr1 => b_ptr2        ! b_ptr1 dissociated

    if (associated (b_ptr1)) error stop 4_4
end
