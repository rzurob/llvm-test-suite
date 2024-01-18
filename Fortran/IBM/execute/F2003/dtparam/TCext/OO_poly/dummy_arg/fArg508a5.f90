! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg508a5.f
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
! %GROUP: fArg508a5.f
! %VERIFY: fArg508a5.out:fArg508a5.vf
! %STDIN:
! %STDOUT: fArg508a5.out
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
!*  DATE                       : 12/09/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) attribute
!                               will default-initialize actual-arg; implict
!                               interface for non-poly dummy-arg of
!                               explicit-shape; actual args are poly-array)
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
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    class (base(4)), allocatable :: b1_m(:)

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module


program fArg508a5
use m
    class (base(4)), pointer :: b_ptr(:)

    allocate (b_ptr(0:10), source=child(4,1,20) (100, 'b_ptr'))

    allocate (b1_m(11), source=child(4,1,20)(200,'b1_m'))

    call reset (b_ptr(::2))
    call reset (b1_m)

    do i = 1, 3
        call b_ptr(2*i-2)%print
        call b_ptr(2*i-1)%print

        call b1_m(i)%print
    end do

    print *, 'second test'

    call b1_m(4)%print
    call b_ptr(6)%print
    call b_ptr(7)%print

    deallocate (b_ptr, b1_m)
end

subroutine reset (b)
use m
    type (base(4)), intent(out) :: b(3)
end subroutine
