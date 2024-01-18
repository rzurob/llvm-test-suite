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
! %GROUP: fclass003a2.f
! %VERIFY: fclass003a2.out:fclass003a2.vf
! %STDIN:
! %STDOUT: fclass003a2.out
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
!*  DESCRIPTION                : CLASS keyword (type with scalar pointer
!*                              components in defined assignment; use arrays for
!                               the assignment)
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
    type base
        integer(4) id

        contains

        final :: finalizeBase
        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character(20) name

        contains

        final :: finalizeChild
        procedure :: print => printChild
    end type

    type container
        class (base), pointer :: data => null()

        contains

        final :: finalizeData
    end type

    interface assignment (=)
        subroutine co2Toco1 (co1, co2)
        import container
            class (container), intent(out) :: co1
            class (container), intent(in) :: co2
        end subroutine
    end interface

    contains

    subroutine finalizeData (d)
        type (container), intent(inout) :: d

        if (associated (d%data)) then
            print *, 'deallocating data'
            deallocate (d%data)
        end if
    end subroutine

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fclass003a1
use m
    class (base), pointer :: b1, b2
    type (container), allocatable :: co1(:), co2(:)

    allocate (b1, source=child(1, 'b1'))
    allocate (b2, source=child(2, 'b2'))

    allocate (co1(2), co2(2))

    co2(1)%data => b1
    co2(2)%data => b2

    print *, 'test1'

    !! the defined assignment will NOT be invoked
    co1 = co2

    if (.not. associated (co1(1)%data, co2(1)%data)) error stop 1_4
    if (.not. associated (co1(2)%data, co2(2)%data)) error stop 2_4

    call co1(1)%data%print
    call co1(2)%data%print

    print *, 'test2'

    !! defined assignment will NOT be invoked here
    co1 = (/container(null()), container(null())/)

    if (associated (co1(1)%data)) error stop 3_4
    if (associated (co1(2)%data)) error stop 4_4
    print *, 'end'
end

subroutine co2Toco1 (co1, co2)
use m, only : base, container
    class (container), intent(out) :: co1
    class (container), intent(in) :: co2

    if (associated (co2%data)) then
        allocate (co1%data, source=co2%data)
    end if
end subroutine
