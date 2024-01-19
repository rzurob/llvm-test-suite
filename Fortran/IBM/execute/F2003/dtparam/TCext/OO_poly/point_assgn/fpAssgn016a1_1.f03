! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv /tstdev/OO_poly/point_assgn/fpAssgn016a1_1.f
! opt variations: -qck -ql -qdefaultpv

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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(n1)    ! (4,20)
        integer, len  :: n1
        character(n1) :: name

        contains

        procedure :: print => printChild
    end type

    interface makeData
        pure function makeData (id, name)
        import base, child
            class (base(4)), pointer :: makeData
            integer*4, intent(in) :: id
            character(*), intent(in), optional :: name
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

pure function makeData (id, name)
use m, only : base, child
    class (base(4)), pointer :: makeData
    integer*4, intent(in) :: id
    character(*), intent(in), optional :: name

    if (present (name)) then
        allocate (makeData, source=child(4,20)(id,name))
    else
        allocate (makeData, source=base(4)(id))
    end if
end function

module m1
use m
    type container(k2)    ! (4)
        integer, kind            :: k2
        class(base(k2)), pointer :: data => null()
    end type

    type (container(4)), save :: co1(10)
end module

program fpAssgn016a1_1
use m1


    forall (i=1:10, (mod (i,2) == 0))
        co1(i) = container(4) (makeData (i, 'child_'//char(ichar ('0')+i-1)))
    end forall

    forall (i=1:10, (mod (i,2) /= 0))
        co1(i) = container(4) (makeData (i))
    end forall

    do i =1, 10
        if (.not. associated (co1(i)%data)) call zzrc(int(i,4_4))

        call co1(i)%data%print
        deallocate (co1(i)%data)
    end do
end
