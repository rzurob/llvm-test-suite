! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/OO_poly/class/fclass003a1.f
! opt variations: -qnock -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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
! %GROUP: fclass003a1.f
! %VERIFY: fclass003a1.out:fclass003a1.vf
! %STDIN:
! %STDOUT: fclass003a1.out
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
!*                              components in defined assignment)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id

        contains

        final :: finalizeBase
        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1)    name

        contains

        final :: finalizeChild
        procedure :: print => printChild
    end type

    type container(k3,n2)    ! (4,20)
        integer, kind              :: k3
        integer, len               :: n2
        class(base(:,k3)), pointer :: data

        contains

        final :: finalizeData
    end type

    interface assignment (=)
        subroutine co2Toco1 (co1, co2)
        import container
            class (container(4,*)), intent(out) :: co1
            class (container(4,*)), intent(in) :: co2
        end subroutine
    end interface

    contains

    subroutine finalizeData (d)
        type (container(4,*)), intent(inout) :: d

        if (associated (d%data)) then
            print *, 'deallocating data'
            deallocate (d%data)
        end if
    end subroutine

    subroutine finalizeBase (b)
        type (base(*,4)), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child(*,4,1)), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4,1)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fclass003a1
use m
    class (base(:,4)), pointer :: b1
    class (container(4,:)), allocatable :: co1

    allocate (b1, source=child(20,4,1)(1, 'b1'))
    allocate (container(4,20) :: co1)

    nullify (co1%data)

    print *, 'test1'
    co1 = container(4,20) (b1)

    if (.not. associated (co1%data)) error stop 2_4

    call co1%data%print

    allocate (b1, source=base(20,4)(100))


    print *, 'test2'

    co1 = container(4,20) (b1)

    if (.not. associated (co1%data)) error stop 3_4

    call co1%data%print

    print *, 'end'
end

subroutine co2Toco1 (co1, co2)
use m, only : base, container
    class (container(4,*)), intent(out) :: co1
    class (container(4,*)), intent(in) :: co2

    if (associated (co2%data)) then
        allocate (co1%data, source=co2%data)
    end if
end subroutine
