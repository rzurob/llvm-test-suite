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
!*  DATE                       : 06/14/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test the use of scalar character variable in
!                               INQUIRE statement that uses file name instead of
!                               unit number; test across multiple scoping units.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    character(10), parameter :: fileName = 'test101'

    contains

    subroutine openFile (unit, fileName, decMode)
        integer, intent(in) :: unit
        character(*), intent(in) :: fileName, decMode

        open (unit, file=fileName, decimal=decMode, form='formatted')
    end subroutine

    subroutine inquireDecimalMode (fileName, decMode)
        character(*), intent(in) :: fileName
        character(*), intent(out) :: decMode

        inquire (file=fileName, decimal=decMode)
    end subroutine
end module

program dcmlCharVarInquir001a
use m
    character(:), allocatable :: c1(:)

    allocate (character(5) :: c1(5))

    call inquireDecimalMode (fileName, c1(1))

    call openFile (10, fileName//'   ', 'COMMA ')

    call inquireDecimalMode (fileName, c1(2)(:))

    call openFile (10, fileName//'again', 'COMMA')

    call inquireDecimalMode (fileName, c1(3)(1:5))

    call openFile (1, fileName=fileName, decMode='POINT')

    call inquireDecimalMode (fileName=fileName//'   ', decMode=c1(4))

    call openFile (1, fileName=fileName, decMode='COMMA')

    call inquireDecimalMode (fileName=fileName//'   ', decMode=c1(5))

    if (c1(1) /= 'UNDEF') error stop 1_4
    if (c1(2) /= 'COMMA') error stop 2_4
    if (c1(3) /= 'UNDEF') error stop 3_4
    if (c1(4) /= 'POINT') error stop 4_4
    if (c1(5) /= 'COMMA') error stop 5_4
end
