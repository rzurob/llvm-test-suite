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
!*  DATE                       : 06/15/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final sub (finalization of function results in
!                               specification expression)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, private :: length

        contains

        procedure :: computeLength
        final :: finalizeBase
    end type

    interface base
            module procedure createBaseObj
    end interface

    contains

    pure type(base) function createBaseObj (len)
        integer,intent(in) :: len

        createBaseObj%length = len
    end function

    pure integer function computeLength(b, s)
        class(base), intent(in) :: b
        character(*), intent(in) :: s

        computeLength = len(s)
    end function

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal516a2
use m
    print *, 'start main'

    call test1

    print *, 'calling test2'

    call test2 (base(100))

    print *, 'end'

    contains

    subroutine test2 (b1)
        type(base), intent(in) :: b1
        character(b1%computeLength('again test')) s2

        print *, 'starting test2'

        if (len(s2) /= len('again test')) error stop 2_4
        print *, 'leaving test2'
    end subroutine
end

subroutine test1
use m
    character(computeLength(base(10), "test1")) s1

    print *, 'start test1'

    if (len(s1) /= len('test1')) error stop 1_4

    print *, 'end of test1'
end subroutine
