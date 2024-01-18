! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/decimaledit/defaultIO/dcmlCharExprRW004.f
! opt variations: -qck -qnol -qnodeferredlp

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 06/07/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Use the type bound procedures for the IO
!                               operation; still test the DECIMAL= in the data
!                               transfer statement.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    character(*), parameter :: decimalModeConst(2) = (/'POINT', 'COMMA'/)

    type base(n1,k1)    ! (10,8)
        integer, kind          :: k1
        integer, len           :: n1
        character(n1), private :: decimalMode = decimalModeConst(1)

        real(k1)               :: data(10)

        contains

        procedure :: setDecMode
        procedure :: getDecMode
        procedure :: print => printBase
    end type

    contains

    function getDecMode (b)
        class(base(*,8)), intent(in) :: b
        character(b%decimalMode%len) getDecMode

        getDecMode = b%decimalMode
    end function

    subroutine setDecMode (b, mode)
        class(base(*,8)), intent(inout) :: b
        character(*), intent(in) :: mode

        b%decimalMode = mode
    end subroutine

    subroutine printBase (b, unit)
        class(base(*,8)), intent(in) :: b
        integer, intent(in) :: unit

        write(unit, '(10EN21.7)', decimal=b%getDecMode()) b%data
    end subroutine
end module


module m1
use m
    type, extends(base) :: child(n2,k2)    ! (10,8,20,4)
        integer, kind :: k2
        integer, len  :: n2
        complex(k2)   :: cx(5)

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printChild (b, unit)
        class(child(*,8,*,4)), intent(in) :: b
        integer, intent(in) :: unit

        complex(4) :: cx(5)

        namelist /nml1/ cx

        cx = b%cx

        call b%base%print (unit)

        write (unit, nml1, decimal=b%getDecMode())
    end subroutine
end module


program dcmlCharExprRW004
use m1
    class(base(:,8)), allocatable :: b1, b2

    open (1, file='dcmlCharExprRW004.out1', decimal='POINT')
    open (2, file='dcmlCharExprRW004.out2', decimal=decimalModeConst(2))

    allocate (base(20,8) :: b1)
    allocate (b2, source=child(10,8,20,4)(data=(/(i*10, i=1, 10)/), &
            cx=(/(cmplx(i,1),i=1,5)/)))

    b1%data = (/(i+10, i=1, 10)/)

    call b2%setDecMode (decimalModeConst(2))

    call b1%print (2)
    call b2%print (1)

    close(1)
    close(2)
end
