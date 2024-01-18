! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr043a.f
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
! %GROUP: fconstr043a.f
! %VERIFY: fconstr043a.out:fconstr043a.vf
! %STDIN:
! %STDOUT: fconstr043a.out
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
!*  DATE                       : 03/18/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (non-poly pointer
!*                               component used to assign to compatible types;
!*                               use type bound to verify)
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
    type base(k1,n1)    ! (4,20)
        integer, kind              :: k1
        integer, len               :: n1
        type(base(k1,n1)), pointer :: next => null()

        contains

        procedure :: print => printBase
    end type

    contains

    subroutine printBase (b)
        class (base(4,*)), intent(in) :: b

        type (base(4,20)), pointer :: iterator
        integer*4 :: totCount

        totCount = 0
        iterator => b%next

        do while (associated (iterator))
            totCount = totCount + 1

            iterator => iterator%next
        end do

        if (totCount == 0) then
            print *, 'there is no data in this list'
        else
            print *, totCount, 'node(s) in the list'
        end if
    end subroutine
end module

module m1
use m
    type, extends(base) :: child(k2)    ! (4,20,4)
        integer, kind :: k2
        integer(k2)   :: id

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printChild (b)
        class (child(4,*,4)), intent(in) :: b

        call b%base%print

        print *, 'id = ', b%id
    end subroutine
end module

program fconstr043a
use m1
    class (base(4,20)), pointer :: b_ptr

    type (base(4,20)) :: b1
    type (child(4,20,4)), target :: c1

    c1 = child(4,20,4) (id = 10)

    b_ptr => c1

    b1 = base(4,20) (b_ptr)

    call b1%print

    call b1%next%print

    call b_ptr%print

    b1 = base(4,20) (c1%base)

    call b1%print

    b1 = base(4,20) (c1%next)

    call b1%print
end
