! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg013a8.f
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
! %GROUP: fArg013a8.f
! %VERIFY: fArg013a8.out:fArg013a8.vf
! %STDIN:
! %STDOUT: fArg013a8.out
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
!*  DESCRIPTION                : argument association (TARGET attribute on
!                               passed-object dummy-arg)
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
        integer(k1)   :: id

        contains

        procedure :: associate => associateBase
        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    class (base(4)), pointer :: global

    contains

    subroutine associateBase (b, b_ptr)
        class (base(4)), target, intent(in) :: b
        class (base(4)), pointer, intent(out) :: b_ptr

        b_ptr => b
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine
end module

program fArg013a8
use m
    type (base(4)), target :: b1
    type (child(4,1,20)), target :: c1

    class (base(4)), pointer :: main1, main2

    b1%id = 1

    c1%id = 2
    c1%name = 'c1'

    main1 => b1

    call main1%associate (global)

    if (.not. associated (global, b1)) error stop 1_4

    call global%print

    call c1%associate (main2)

    if (.not. associated (main2, c1)) error stop 2_4

    call main2%print

    call main2%associate (main1)

    if (.not. associated (main1, c1)) error stop 3_4

    call main1%print
end
