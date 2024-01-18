! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg013a.f
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
! %GROUP: fArg013a.f
! %VERIFY: fArg013a.out:fArg013a.vf
! %STDIN:
! %STDOUT: fArg013a.out
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
!*  DATE                       : 05/13/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (test the TARGET attribute
!                               where the pointer associations were tested
!                               before and after the procedure calls)
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

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    class (base(4)), pointer :: b_ptr, b_ptr2

    contains

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine assgnb1Tob2 (b1, b2)
        class (base(4)), pointer, intent(out) :: b1
        class (base(4)), target, intent(in) :: b2

        b1 => b2
    end subroutine
end module

program fArg013a
use m
    class (base(4)), pointer :: b1

    type (child(4,1,20)), target :: c1

    allocate (b1, source=child(4,1,20) (10, 'b1'))

    c1 = child(4,1,20) (20, 'c1')

    call assgnb1Tob2 (b_ptr, b1)

    call assgnb1Tob2 (b_ptr2, c1)

    if (.not. associated (b_ptr, b1)) error stop 1_4

    if (.not. associated (b_ptr2, c1)) error stop 2_4

    call b_ptr%print

    call b_ptr2%print

    deallocate (b1)

    call assgnb1Tob2 (b_ptr, b_ptr2)

    if (.not. associated (b_ptr, c1)) error stop 3_4
end
