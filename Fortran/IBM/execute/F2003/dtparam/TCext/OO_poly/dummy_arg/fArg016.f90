! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg016.f
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
! %GROUP: fArg016.f
! %VERIFY: fArg016.out:fArg016.vf
! %STDIN:
! %STDOUT: fArg016.out
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
!*  DATE                       : 05/20/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (deferred-shape array)
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

    class (base(4)), pointer :: b_ptr (:)
    class (base(4)), allocatable :: b_alloc (:)

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine test1 (b)
        class (base(4)), pointer, intent(inout) :: b(:)

        if (associated (b)) then

            print *, 'test1: bounds:', lbound (b,1), ':', ubound (b,1)

            do i = lbound (b,1), lbound (b,1)+size(b)-1
                call b(i)%print
            end do

            deallocate (b)

            allocate (b(3:5))

            b%id = (/3,4,5/)
        end if
    end subroutine

    subroutine test2 (b)
        class (base(4)), allocatable, intent(inout) :: b(:)
        type (child(4,1,20)), save :: c1(6) = (/child(4,1,20) (1, 'c1_1'), child(4,1,20)(2, 'c1_2'), &
            child(4,1,20)(3,'c1_3'), child(4,1,20)(4,'c1_4'), child(4,1,20)(5,'c1_5'), child(4,1,20)(6,'c1_6')/)

        if (allocated (b)) then
            print *, 'test2: bounds:', lbound (b,1), ':', ubound(b,1)

            do i = lbound (b,1), lbound (b,1)+size(b)-1
                call b(i)%print
            end do

            deallocate (b)

            allocate (b(10:15), source=c1)
        end if
    end subroutine
end module

program fArg016
use m
    type (child(4,1,20)) :: c1(2) = (/child(4,1,20)(2, 'c1_2'), child(4,1,20)(3, 'c1_3')/)

    !! test the pointer array
    allocate (b_ptr(2:3), source=c1)

    call test1 (b_ptr)

    print *, 'bounds: from', lbound (b_ptr,1), 'to', ubound(b_ptr, 1)

    do i = lbound (b_ptr,1), lbound (b_ptr,1) + size(b_ptr)-1
        call b_ptr(i)%print
    end do

    !! test the allocatable array
    allocate (b_alloc(3:5))

    b_alloc%id = (/13, 14, 15/)

    call test2 (b_alloc)

    print *, 'bounds: from', lbound (b_alloc,1), 'to', ubound(b_alloc,1)

    do i = lbound (b_alloc,1), lbound (b_alloc,1) + size(b_alloc)-1
        call b_alloc(i)%print
    end do
end
