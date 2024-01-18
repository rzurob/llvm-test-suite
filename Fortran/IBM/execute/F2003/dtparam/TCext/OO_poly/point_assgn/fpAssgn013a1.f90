! GB DTP extension using:
! ftcx_dtp -qnol -qnodeferredlp /tstdev/OO_poly/point_assgn/fpAssgn013a1.f
! opt variations: -qck -ql -qdeferredlp

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn013a1.f
! %VERIFY: fpAssgn013a1.out:fpAssgn013a1.vf
! %STDIN:
! %STDOUT: fpAssgn013a1.out
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
!*  DESCRIPTION                : data pointer assignment (module data used as
!*                               targets; module data are pointer and
!*                               allocatables)
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
        procedure, non_overridable :: setID => setBaseID
    end type

    type, extends(base) :: child(n1)    ! (4,20)
        integer, len  :: n1
        character(n1) :: name = ''

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
        class (child(4,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    !! this procedure uses INTENT(OUT) with consequences of reseting the input
    !! data's values as there are default initializations for all components
    subroutine setBaseID (b, i)
        class (base(4)), intent(out) :: b
        integer*4, intent(in) :: i

        b%id = i
    end subroutine

    subroutine addStr2Name (c, ch)
        class (child(4,*)), intent(inout) :: c
        character(*), intent(in) :: ch

        c%name = trim(c%name)//ch
    end subroutine
end module

module data1
use m
    type (child(4,20)), target, allocatable :: c1_m
    type (base(4)), pointer :: b1_m => null()
end module

program fpAssgn013a1
use data1

    class (base(4)), pointer :: b

    call intializeModuleData

    b => c1_m

    call b%setID (10)
    call c1_m%catName ('01')

    b => b1_m

    call b%setID (20)

    call printData

    deallocate (b1_m)
end


subroutine intializeModuleData
use data1
    allocate (b1_m, c1_m)

    c1_m = child(4,20) (1, name = 'c1_m')

    b1_m%id = 10
end subroutine


subroutine printData
use data1
    call c1_m%print

    call b1_m%print
end subroutine
