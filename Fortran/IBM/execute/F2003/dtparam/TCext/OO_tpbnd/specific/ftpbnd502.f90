! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd502.f
! opt variations: -qnok -ql -qreuse=none

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
! %GROUP: ftpbnd502.f
! %VERIFY: ftpbnd502.out:ftpbnd502.vf
! %STDIN:
! %STDOUT: ftpbnd502.out
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
!*  DATE                       : 03/10/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : specific type bound (PASS binding referenced by
!*                               array elements)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        contains

        procedure, pass :: print => printBase
    end type

    type, extends(base) :: child    ! (4)
        integer(k1) :: id

        contains

        procedure, pass :: print => printChild
    end type

    class (base(4)), pointer :: b_ptr

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b
        print *, 'base'
    end subroutine

    subroutine printChild (b)
        class (child(4)), intent(in) :: b
        print *, 'child', b%id
    end subroutine
end module

program ftpbnd502
use m

    type(child(4)), allocatable, target :: c1(:)

    allocate (c1(10))

    c1 = (/(child(4)(10*i), i=2, 11)/)

    do i=1,10
        b_ptr => c1(i)

        call c1(i)%print
        call b_ptr%print
    end do

end
