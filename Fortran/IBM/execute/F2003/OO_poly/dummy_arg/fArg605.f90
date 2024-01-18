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
! %GROUP: fArg605.f
! %VERIFY: fArg605.out:fArg605.vf
! %STDIN:
! %STDOUT: fArg605.out
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
!*  DATE                       : 12/15/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (some intrinsic function
!                               return values used as the actual arg to be
!                               associated with the unlimited poly dummy
!                               assumed-shape array)
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

    type, extends (base) :: child
        character(16) :: name
    end type

    contains

    subroutine printX (x)
        class (*), intent(in) :: x(:)

        print *, 'bounds: ', lbound(x), ubound(x)
        select type (x)
            type is (base)
                print *, x
            type is (child)
                print *, x
            type is (integer)
                print *, x
            class default
                print *, 'other data type'
        end select
    end subroutine
end module

program fArg605
use m
    integer i1(10)
    complex, pointer :: cx (:)

    class (base), allocatable :: b1(:)

    !! int() for real
    call printX (int((/1.0, 3.0/)))


    !! int() for complex
    allocate (cx(0:1), source=(/(-1.2, 2.0), (3.1, -1.5)/))
    call printX (int(cx))

    !! integer array
    i1 = (/(i, i=1,10)/)

    call printX (i1(8::2))

    !! derived types
    allocate (b1(3), source=(/child(1, 'xlftest1'), child(2, 'xlftest2'), &
                    child(3, 'xlftest3')/))

    call printX (transfer(b1, b1, 3))
end
