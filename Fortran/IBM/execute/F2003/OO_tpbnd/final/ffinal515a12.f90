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
! %GROUP: ffinal515a12.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
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
!*  DATE                       : 04/27/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : final sub (temps created by structure
!*                               constructor in FORALL statement)
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
        integer*4 :: id = -1

        contains

        final :: finalizeBase
        procedure :: isDefault => isBaseWithDefault
    end type

    type, extends (base) :: child
        character*20 :: name = 'no-name'

        contains

        final :: finalizeChild
        procedure :: isDefault => isChildWithDefault
    end type

    interface operator(==)
        pure logical function baseEqual (b1, b2)
        import base
            class (base), intent(in) :: b1
            type (base), intent(in) :: b2
        end function

        pure logical function childEqual (c1, c2)
        import child
            class (child), intent(in) :: c1
            type (child), intent(in) :: c2
        end function
    end interface

    contains

    pure subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        b%id = -1
    end subroutine

    pure subroutine finalizeChild (c)
        type (child), intent(inout) :: c

        c%name = 'no-name'
    end subroutine

    pure logical function isBaseWithDefault (b)
        class (base), intent(in) :: b

        isBaseWithDefault = (b%id == -1)
    end function

    pure logical function isChildWithDefault (b)
        class (child), intent(in) :: b

        isChildWithDefault = (b%base%isDefault() .and. (b%name == 'no-name'))
    end function
end module

pure logical function baseEqual (b1, b2)
use m, only: base
    class (base), intent(in) :: b1
    type (base), intent(in) :: b2

    baseEqual = (b1%id == b2%id)
end function

pure logical function childEqual (c1, c2)
use m, only: child, base, operator(==)
    class (child), intent(in) :: c1
    type (child), intent(in) :: c2

    childEqual = ((c1%base == c2%base) .and. (c1%name == c2%name))
end function

program ffinal515a12
use m
    type (child) :: c1 (10)

    print *, 'begin'

    forall (i=1:10, c1(i)%isDefault())  c1(i)%id = i

    print *, 'second forall'

    !! Note that there may be only one temp in this forall statement
    forall (i=1:3, c1(i) == child (1))  c1(i)%name = 'test'

    if (any (c1%id /= (/1,2,3,4,5,6,7,8,9,10/))) error stop 1_4

    if (c1(1)%name /= 'test') error stop 2_4

    if (any (c1(2:)%name /= 'no-name')) error stop 3_4
end
