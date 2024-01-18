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
! %GROUP: fArg521.f
! %VERIFY: fArg521.out:fArg521.vf
! %STDIN:
! %STDOUT: fArg521.out
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
!*  DATE                       : 06/09/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : array constructor (generic-name in array
!                               constructor)
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

    type, extends(base) :: child
        character*20 :: name ='default'
    end type

    interface makeData
        function makeBase (id, n)
        import base
            integer*4, intent(in) :: id, n
            type (base) :: makeBase(n)
        end function

        function makeChildData (id, name, n)
        import child
            integer*4, intent(in) :: id, n
            character(*), intent(in) :: name

            type (child) :: makeChildData (n)
        end function
    end interface
end module

program fArg521
use m
    type(base) :: b1 (10)
    type (child) :: c1 (5)

    b1 = (/makeData(1,3), makeData(10, 6), base(20)/)

    print *, b1

    c1 = (/child(30), makeData(100, 'temp2-4', 3), child(2, 'temp5')/)

    print *, c1
end

type (base) function makeBase (id, n)
use m, only: base
    integer*4, intent(in) :: id, n
    dimension makeBase(n)

    makeBase%id = id
end function

type (child) function makeChildData (id, name, n)
use m, only:child
    integer*4, intent(in) :: id, n
    character(*), intent(in) :: name
    dimension makeChildData (n)

    makeChildData%id = id
    makeChildData%name = name
end function
