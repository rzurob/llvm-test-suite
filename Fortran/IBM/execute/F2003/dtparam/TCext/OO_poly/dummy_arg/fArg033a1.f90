! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg033a1.f
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
! %GROUP: fArg033a1.f
! %VERIFY: fArg033a1.out:fArg033a1.vf
! %STDIN:
! %STDOUT: fArg033a1.out
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
!*  DATE                       : 06/14/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (actual-arg changed during
!                               the execution of the procedure)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = -1

        contains

        procedure :: print => printBase
        procedure :: assgnID => assignID2Base
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    class (base(4)), pointer :: b1_m

    type (child(4,1,20)), target :: c1 = child(4,1,20) (1, 'c1')

    contains

    subroutine assignID2Base (b, id)
        class (base(4)), intent(out) :: b
        integer*4, intent(in) :: id

        b%id = b%id
    end subroutine

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine test1 (b)
        class (base(4)), target, intent(inout) :: b

        b1_m%id = 10

        call b%print
    end subroutine
end module


program fArg033a1
use m
    class (base(4)), pointer :: b1

    allocate (b1, source=child(4,1,20) (1, 'b1'))

    b1_m => c1

    call test1 (b1_m)

    call test2 (b1)

    call b1%print

    call b1_m%print

    deallocate (b1)

    contains

    subroutine test2 (b)
        class (base(4)), target, intent(out) :: b

        call b1%assgnID (100)

        call b%print
    end subroutine
end
