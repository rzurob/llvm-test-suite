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
! %GROUP: fconstr015a1.f
! %VERIFY: fconstr015a1.out:fconstr015a1.vf
! %STDIN:
! %STDOUT: fconstr015a1.out
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
!*  DATE                       : 07/12/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : structure constructor (generic name used in
!                               places of structure constructor for private
!                               component; also user defined assignment)
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
    !! private component data prevent this type to have structure constructor
    !out of the module
    type base
        integer*4, pointer, private :: data (:) => null()

        contains

        procedure :: print => printBase
        final :: freeData
    end type

    interface base
        module procedure createBase
    end interface

    interface assignment(=)
        module procedure base2Base
    end interface

    contains

    subroutine base2Base (b1, b2)
        type (base), intent(out) :: b1
        type (base), intent(in) :: b2

        if (associated (b2%data)) then
            allocate (b1%data (lbound(b2%data,1):ubound(b2%data,1)), &
                source=b2%data)
        end if
    end subroutine

    type (base) function createBase (data)
        integer*4, intent(in) :: data(2:)

        allocate (createBase%data(size(data)), source=data)

        if (lbound (createBase%data, 1) /= 1) error stop 10_4
    end function


    subroutine printBase (b)
        class (base), intent(in) :: b

        if (associated (b%data)) then
            write (*, *) b%data
        end if
    end subroutine

    subroutine freeData (b)
        type (base), intent(inout) :: b

        if (associated (b%data)) then
            print *, 'deallocating data'
            deallocate (b%data)
        end if
    end subroutine
end module


program fconstr015a1
use m
    integer*4 :: i1 (3)
    type (base) :: b1

    integer*4, allocatable :: i2(:)

    i1 = (/3,2,1/)

    b1 = base(i1)

    call b1%print

    allocate (i2(-1:1), source=(/-1,0,1/))

    b1 = base (i2)

    call b1%print
end
