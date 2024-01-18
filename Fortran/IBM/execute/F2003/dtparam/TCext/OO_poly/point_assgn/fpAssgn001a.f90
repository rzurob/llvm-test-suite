! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/point_assgn/fpAssgn001a.f
! opt variations: -ql

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
! %GROUP: fpAssgn001a.f
! %VERIFY: fpAssgn001a.out:fpAssgn001a.vf
! %STDIN:
! %STDOUT: fpAssgn001a.out
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
!*  DATE                       : 01/15/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : pointer assignment (unlimited polymorphic
!*                               pointer array assignment; test size(),
!*                               lbound() and ubound() intrinsics)
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

program fpAssgn001a
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: i1
    end type

    type, extends (base) :: child    ! (4)

    end type

    class (*), pointer :: x(:), x1(:)

    class (base(4)), pointer :: b_ptr(:)

    integer*4, target :: i(0:10)
    type (base(4)), target :: b1 (1:10)
    type (child(4)), target :: b2 (-10:1)

    x => i

    if ((size(x) /= 11) .or. (lbound(x, 1) /= 0) .or. &
        (ubound(x, 1) /= 10)) error stop 1_4

    x => b1

    if ((size(x) /= 10) .or. (lbound(x,1) /= 1) .or. (ubound(x,1) /= 10)) error stop 2_4

    x => b2

    if ((size(x) /= 12) .or. (lbound(x,1) /=-10) .or. (ubound(x,1) /=1)) error stop 3_4

    b_ptr => b2

    print *, size(b_ptr), lbound(b_ptr), ubound(b_ptr)
    x => b_ptr

    if ((size(x) /= 12) .or. (lbound(x,1) /=-10) .or. (ubound(x,1) /=1)) error stop 4_4


    call test1 (b_ptr, -10, 1)

    call test1 (b1, 1, 10)

    contains

    !This subroutine test the (l/u)bound() and size()
    subroutine test1 (a, lb, ub)
        class (base(4)), intent(in), target :: a(0:)
        integer*4, intent(in) :: lb, ub

        class (*), pointer :: x1(:)

        x1 => a

        if ((lbound(x1,1) /= 0) .or. (ubound(x1,1) /= ub-lb)) error stop 10_4

        if (size(x1) /= ub-lb+1) error stop 11_4
    end subroutine
end
