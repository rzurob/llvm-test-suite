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
! %GROUP: fmisc001.f
! %VERIFY: fmisc001.out:fmisc001.vf
! %STDIN:
! %STDOUT: fmisc001.out
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
!*  DATE                       : 05/26/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : miscellaneous items; mostly from defect (defect
!                               283645)
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
        integer id
    end type

    type bigger
        type (base) :: b1
        character*20 :: name
    end type
end module

program fmisc001
use m
    interface createData
        function createBase (i)
        use m
            type (base) createBase
            integer*4, intent(in) :: i
        end function

        function createBigger (i, c)
        use m
            type (bigger) createBigger
            integer*4, intent(in) :: i
            character(*), intent(in) :: c
        end function
    end interface

    print *, createData (10, 'test')
end

function createBase (i)
use m
    type (base) createBase
    integer*4, intent(in) :: i

    createBase%id = i
end function

function createBigger (i, c)
use m
    type (bigger) createBigger
    integer*4, intent(in) :: i
    character(*), intent(in) :: c

    createBigger%b1%id = i
    createBigger%name = c
end function

