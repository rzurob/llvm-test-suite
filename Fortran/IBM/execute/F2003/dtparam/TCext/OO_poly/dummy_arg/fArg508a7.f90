! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg508a7.f
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
! %GROUP: fArg508a7.f
! %VERIFY: fArg508a7.out:fArg508a7.vf
! %STDIN:
! %STDOUT: fArg508a7.out
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
!*  DATE                       : 12/10/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) for
!                               explicit-shape unlimited-poly dummy array;
!                               actual-args are the array element or entire
!                               array)
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

    type (child(4,1,20)), save :: c1_m (3:5)

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine reset (b)
        class (*), intent(out) :: b(2)
    end subroutine
end module


program fArg508a7
use m
    type (base(4)) :: b1(3)

    class (base(4)), pointer :: b_ptr(:)

    b1%id = (/1,2,3/)

    c1_m%id = (/3,4,5/)
    c1_m%name = (/'c1_m_3', 'c1_m_4', 'c1_m_5'/)

    allocate (b_ptr(0:2), source=child(4,1,20) (100, 'b_ptr'))

    allocate (b1_m(3), source=child(4,1,20)(200,'b1_m'))

    call reset (b1(2))          !<-- b1(2), b1(3)
    call reset (c1_m(3))        !<-- c1_m(3), c1_m(4)
    call reset (b_ptr)          !<-- b_ptr (0, 1)
    call reset (b1_m(2:))        !<-- b1_m(2, 3)

    do i = 1, 3
        call b1(i)%print

        call c1_m(2+i)%print

        call b_ptr(i-1)%print

        call b1_m(i)%print
    end do

    deallocate (b_ptr)
end
