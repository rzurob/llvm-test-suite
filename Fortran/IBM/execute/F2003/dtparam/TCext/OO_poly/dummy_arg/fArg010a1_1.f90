! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg010a1_1.f
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
! %GROUP: fArg010a1_1.f
! %VERIFY: fArg010a1_1.out:fArg010a1_1.vf
! %STDIN:
! %STDOUT: fArg010a1_1.out
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
!*  DATE                       : 06/29/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (VALUE attribute for
!                               dummy-arg with pointer component; reallocate
!                               pointer component without affecting the original
!                               pointer)
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

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    type container(k3)    ! (4)
        integer, kind            :: k3
        class(base(k3)), pointer :: data => null()
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine allocateTemp (c)
        type (container(4)), value :: c

        print *, 'entering allocateTemp'

        if (associated (c%data))   call c%data%print

        allocate (child(4,1,20):: c%data)

        call c%data%print

        print *, 'leaving allocateTemp'
    end subroutine
end module

program fArg010a1_1
use m
    class (container(4)), allocatable :: c1 (:)

    allocate (c1 (3))

    allocate (c1(1)%data)

    call allocateTemp (c1(1))

    call c1(1)%data%print

    deallocate (c1(1)%data)
    deallocate (c1)

    allocate (c1(2))

    call allocateTemp (c1(2))

    if (associated (c1(2)%data)) error stop 1_4

    deallocate (c1)
end

