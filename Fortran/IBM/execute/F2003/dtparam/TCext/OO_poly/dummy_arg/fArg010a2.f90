! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg010a2.f
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
! %GROUP: fArg010a2.f
! %VERIFY: fArg010a2.out:fArg010a2.vf
! %STDIN:
! %STDOUT: fArg010a2.out
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
!*  DATE                       : 05/10/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (VALUE attribute; changes
!*                               made through pointer component will take
!*                               effect)
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

        procedure, non_overridable :: addID => addID2Base
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    type (base(4)), target :: b1 = base(4) (10)
    type (child(4,1,20)), target :: c1 = child(4,1,20) (20, 'c1')

    type (child(4,1,20)), target :: c2 (2:4)
    type (base(4)), target :: b2 (3:7)

    contains

    subroutine addID2Base (b, val)
        class (base(4)), intent(inout) :: b
        integer*4, intent(in) :: val

        b%id = b%id + val
    end subroutine

    subroutine initializeB2C2
        b2 = (/(base(4)(i), i=3,7)/)

        c2 = (/(child(4,1,20) (i, 'c2'), i=2,4)/)
    end subroutine
end module

module m1
use m
    type container(k3)    ! (4)
        integer, kind            :: k3
        class(base(k3)), pointer :: data => null()
    end type

    type container1(k4)    ! (4)
        integer, kind            :: k4
        class(base(k4)), pointer :: data(:) => null()
    end type
end module

program fArg010a2
use m1
    type (container(4)) :: co
    type (container1(4)) :: co1

    class (base(4)), allocatable, target :: b3, b4(:)


    call initializeB2C2

    call associateScalar (co, c1)

    call associateScalar (co, b1)

    call associateArray1 (co1, b2)

    call associateArray1 (co1, c2)


    ! validate that b1,b2,c1 and c2 are all modified

    if (b1%id /= 11) error stop 1_4

    if ((c1%id /= 21) .or. (c1%name /= 'c1')) error stop 2_4

    do i = 3, 7
        if (b2(i)%id /= i+2) error stop 3_4
    end do


    do i = 2, 4
        if (c2(i)%id /= i+2) error stop 4_4
    end do

    !! work with b3, b4

    allocate (b3, source = child(4,1,20) (50, 'b3'))
    allocate (b4(10), source=base(4)(100))


    call associateScalar (co, b3)

    call associateArray1 (co1, b4)

    if (b3%id /= 51) error stop 5_4

    if (any (b4%id /= 102)) error stop 6_4

    contains

    subroutine associateScalar (c, b)
        type (container(4)), value :: c
        class (base(4)), target, intent(inout) :: b

        c%data => b

        call c%data%addID (1)
    end subroutine

    subroutine associateArray1 (c, b)
        type (container1(4)), value :: c
        class (base(4)), target, intent(inout) :: b(:)

        c%data => b

        print *, lbound (c%data), ubound (c%data)

        do i = 1, size (c%data)
            call c%data(i)%addID (2)
        end do
    end subroutine
end
