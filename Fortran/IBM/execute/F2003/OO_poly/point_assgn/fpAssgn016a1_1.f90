!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn016a1_1.f
! %VERIFY: fpAssgn016a1_1.out:fpAssgn016a1_1.vf
! %STDIN:
! %STDOUT: fpAssgn016a1_1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (in FORALL construct;
!*                               use intrinsic assignment to apply pointer
!*                               assignment for structure component)
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
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    interface makeData
        pure function makeData (id, name)
        import base, child
            class (base), pointer :: makeData
            integer*4, intent(in) :: id
            character(*), intent(in), optional :: name
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

pure function makeData (id, name)
use m, only : base, child
    class (base), pointer :: makeData
    integer*4, intent(in) :: id
    character(*), intent(in), optional :: name

    if (present (name)) then
        allocate (makeData, source=child(id,name))
    else
        allocate (makeData, source=base(id))
    end if
end function

module m1
use m
    type container
        class (base), pointer :: data => null()
    end type

    type (container), save :: co1(10)
end module

program fpAssgn016a1_1
use m1


    forall (i=1:10, (mod (i,2) == 0))
        co1(i) = container (makeData (i, 'child_'//char(ichar ('0')+i-1)))
    end forall

    forall (i=1:10, (mod (i,2) /= 0))
        co1(i) = container (makeData (i))
    end forall

    do i =1, 10
        if (.not. associated (co1(i)%data)) call zzrc(int(i,4_4))

        call co1(i)%data%print
        deallocate (co1(i)%data)
    end do
end
