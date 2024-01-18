! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_tpbnd/specific/ftpbnd503a3.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=base

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
! %GROUP: ftpbnd503a3.f
! %VERIFY: ftpbnd503a3.out:ftpbnd503a3.vf
! %STDIN:
! %STDOUT: ftpbnd503a3.out
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
!*  DATE                       : 03/12/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : specific type bound (inherited binding called
!*                               in the overriding binding)
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
    type base(n1,k1)    ! (20,4)
        integer, kind        :: k1
        integer, len         :: n1
        integer(k1), private :: id = 10

        contains

        procedure, non_overridable :: getID => baseID
        procedure :: print => printBase
    end type

    contains

    integer*4 function baseID (b)
        class (base(*,4)), intent(in) :: b

        baseID = b%id
    end function

    subroutine printBase (c)
        class (base(*,4)), intent(in) :: c

        print *, c%id
    end subroutine
end module

module m1
use m, only : base
    type, extends(base) :: child(k2,n2)    ! (20,4,4,20)
        integer, kind :: k2
        integer, len  :: n2
        character(n2) :: name = ''

        contains

        procedure :: print => printChild
    end type

    class (base(:,4)), pointer :: b_ptr => null()
    contains

    subroutine printChild (c)
        class (child(*,4,4,*)), intent(in) :: c

        print *, c%getID(), c%name
    end subroutine
end module

use m1
    type(child(20,4,4,20)), target :: c1
    class (child(:,4,4,:)), pointer :: c_ptr

    c1 = child(20,4,4,20) (name = 'c1')

    call c1%print
    call c1%base%print

    if (c1%getID() /= 10) error stop 1_4

    c_ptr => c1
    b_ptr => c_ptr

    call b_ptr%print

    call c_ptr%print

    call c_ptr%base%print
end
