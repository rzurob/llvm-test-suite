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
! %GROUP: fconstr500a1.f
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
!*  DATE                       : 04/29/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : structure constructor (data-pointer-assignment
!*                               for the pointer components; poly-pointer
!*                               components to poly-targets)
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
        integer*4 :: id
    end type

    type container
        class (*), pointer :: data1 => null()
        class (*), pointer :: data2(:) => null()
    end type
end module

program  fconstr500a1
use m
    class (*), allocatable, target :: x1 (:)
    class (*), pointer :: x2 => null ()

    type (container) :: co1
    type (base), target :: b1

    allocate (integer*4 :: x1(2:11))

    x2 => b1

    co1 = container (x2, data2 = x1)

    if (.not. associated (co1%data1, b1)) error stop 1_4

    if (.not. associated (co1%data2, x1)) error stop 2_4

    if (size (co1%data2) /= 10) error stop 3_4

    if ((lbound(co1%data2,1) /= 2) .or. (ubound(co1%data2,1) /= 11)) error stop 4_4
end
