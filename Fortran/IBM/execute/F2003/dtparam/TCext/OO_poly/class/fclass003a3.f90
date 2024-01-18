! GB DTP extension using:
! ftcx_dtp -qck -qk -qnol -qnodefaultpv /tstdev/OO_poly/class/fclass003a3.f
! opt variations: -qnock -qnok -ql -qdefaultpv

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
! %GROUP: fclass003a3.f
! %VERIFY: fclass003a3.out:fclass003a3.vf
! %STDIN:
! %STDOUT: fclass003a3.out
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
!*  DATE                       : 01/24/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : CLASS keyword (defined assignment involving
!                               poly-pointer array components and finalization)
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
    type base(k1)    ! (8)
        integer, kind :: k1
        integer(k1)      id

        contains

        final :: finalizeBase
        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n1)    ! (8,1,18)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        final :: finalizeChild
        procedure :: print => printChild
    end type

    type container(k3,k4)    ! (4,8)
        integer, kind            :: k3,k4
        class(base(k4)), pointer :: data(:)

        contains

        final :: finalizeData
    end type


    interface assignment (=)
        subroutine co2Assgn2co1 (co1, co2)
        import container
            class (container(4,8)), intent(out) :: co1
            class (container(4,8)), intent(in) :: co2
        end subroutine
    end interface


    contains

    subroutine finalizeBase (b)
        type (base(8)), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child(8,1,*)), intent(inout) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeData (d)
        type (container(4,8)), intent(inout) :: d

        if (associated (d%data)) then
            print *, 'deallocating data'
            deallocate (d%data)
        end if
    end subroutine

    subroutine printBase (b)
        class (base(8)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(8,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module


subroutine co2Assgn2co1 (co1, co2)
use m, only: container, base
    class (container(4,8)), intent(out) :: co1
    class (container(4,8)), intent(in) :: co2

    if (associated (co2%data)) then
        allocate (co1%data(size(co2%data)), source= &
            co2%data(ubound(co2%data,1):lbound(co2%data,1):-1))
    end if
end subroutine

program fclass003a3
use m
    class (base(8)), pointer :: b1(:)
    class (container(4,8)), allocatable :: co1, co2

    allocate (b1(2), source=(/child(8,1,18)(1, 'b1_1'), child(8,1,18)(2, 'b1_2')/))

    allocate (co1, co2)

    co1%data => null()

    co2%data => b1

    !! test 1: user defined assignment
    print *, 'test 1'
    co1 = co2

    if (.not. associated (co1%data)) error stop 1_4
    if (associated (co1%data, co2%data)) error stop 2_4
    if (size (co1%data) /= 2) error stop 3_4

    call co1%data(1)%print
    call co1%data(2)%print


    !! test 2: still defined assignment
    print *, 'test 2'

    deallocate (co2%data)

    allocate (co2%data(1), source=base(8)(100))

    co1 = co2

    if (.not. associated (co1%data)) error stop 4_4
    if (associated (co1%data, co2%data)) error stop 5_4
    if (size (co1%data) /= 1) error stop 6_4

    call co1%data(1)%print

    print *, 'end'
end
