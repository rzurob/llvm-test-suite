! GB DTP extension using:
! ftcx_dtp -qck -qnol -qnodeferredlp /tstdev/OO_poly/point_assgn/fpAssgn013a3.f
! opt variations: -qnock -ql -qdeferredlp

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn013a3.f
! %VERIFY: fpAssgn013a3.out:fpAssgn013a3.vf
! %STDIN:
! %STDOUT: fpAssgn013a3.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/31/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (module array data used
!*                               as TARGETS; test size and type-bound; module
!*                               data are polymorphic)
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
        integer(k1)   :: id = 0

        contains

        procedure :: print => printBase
        procedure, non_overridable :: addID => addID2Base
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = ''

        contains

        procedure :: print => printChild
        procedure, non_overridable :: catName => addStr2Name
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

    subroutine addID2Base (b, i)
        class (base(4)), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = b%id + i
    end subroutine

    subroutine addStr2Name (c, ch)
        class (child(4,1,*)), intent(inout) :: c
        character(*), intent(in) :: ch

        c%name = trim(c%name)//ch
    end subroutine
end module

module data1
use m
    class (child(4,1,20)), allocatable, target :: c1_m(:)
    class (base(4)), pointer :: b1_m (:) => null()
end module

program fpAssgn013a3
use data1

    class (base(4)), pointer :: b(:)

    call intializeModuleData

    call printData

    b => c1_m

    if (size (b) /= 10) error stop 1_4


    do i = 2, 11
        call b(i-1)%addID (10)
        call c1_m(i-1)%catName ('_d')
    end do

    b => b1_m

    if (size(b) /= 5) error stop 2_4

    do i = 1, 5
        call b(i)%addID (10)
    end do

    call printData

    deallocate (b, c1_m)
end


subroutine intializeModuleData
use data1
    allocate (c1_m(10), b1_m(5))

    do i = 1, 10
        call c1_m(i)%addID (i)
        call c1_m(i)%catName ('c1_m_'//char(ichar('0')+i-1))
    end do

    do i = 1, 5
        call b1_m(i)%addID (i+10)
    end do
end subroutine


subroutine printData
use data1
    do i = lbound(c1_m, 1), ubound(c1_m, 1)
        call c1_m(i)%print
    end do

    do i = lbound(b1_m, 1), ubound(b1_m, 1)
        call b1_m(i)%print
    end do
end subroutine
