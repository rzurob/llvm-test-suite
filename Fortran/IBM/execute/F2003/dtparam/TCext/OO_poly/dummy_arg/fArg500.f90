! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg500.f
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
! %GROUP: fArg500.f
! %VERIFY: fArg500.out:fArg500.vf
! %STDIN:
! %STDOUT: fArg500.out
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
!*  DATE                       : 04/26/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : dummy-arg (poly dummy-arg; a simple application
!                                for poly-pointer dummy-arg)
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

        procedure, pass :: print => printBase
    end type

    contains

    subroutine printBase (b)
        class (base(4)) :: b

        print *, 'id = ', b%id
    end subroutine

    !! this can be viewed as a general utility for family base
    subroutine printBaseFamily (b)
        class (base(4)), pointer :: b

        if (associated (b))     call b%print
    end subroutine
end module

module m1
use m
    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure, pass :: print => printChild
    end type

    contains

    subroutine printChild (b)
        class (child(4,1,*)) :: b

        call printHeader

        print *, b%id, b%name
    end subroutine

    subroutine printHeader
        print *, 'Child type: '
    end subroutine
end module

module m2
use m
    type, extends(base) :: child2(k3)    ! (4,2)
        integer, kind :: k3
        integer(k3)   :: flag

        contains

        procedure, pass :: print => printChild2
    end type

    contains

    subroutine printChild2 (b)
        class (child2(4,2)) :: b

        print *, 'id = ', b%id, '; flag = ', b%flag
    end subroutine
end module

module m3
use m
    type container(k4)    ! (4)
        integer, kind            :: k4
        class(base(k4)), pointer :: data => null()
    end type
end module

program fpAssgn500
use m1
use m2
use m3

    type (container(4)) :: co(4)

    type (base(4)), target :: b1
    type (child(4,1,20)), target :: c1
    type (child2(4,2)), target :: c2

    class (base(4)), pointer :: b_ptr => null()

    b1 = base(4) (1)
    c1 = child(4,1,20) (2, 'c1')
    c2 = child2(4,2) (3, 1)

    co = (/container(4)(b1), container(4)(c1), container(4)(c2), container(4)(b_ptr)/)

    do i = 1, 4
        call printBaseFamily (co(i)%data)
    end do

    allocate (b_ptr, source=child2(4,2) (flag=10, id = 100))

    co(4) = container(4)(data = b_ptr)

    call printBaseFamily (co(4)%data)
end
