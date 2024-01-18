! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeBound002.f
! %VERIFY: typeBound002.out:typeBound002.vf
! %STDIN:
! %STDOUT: typeBound002.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/31/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Cross testing type bound.
!*    Poly
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type, abstract :: AbstractParent
        contains
        procedure :: transposeIt
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    subroutine transposeIt(this, a)
        class(AbstractParent), intent(in) :: this
        class(AbstractParent), intent(in) :: a(:,:)
        associate (name1=>transpose(a))
            select type(name1)
                type is (Base)
                    print *, "Base", name1
                type is (Child)
                    print *, "Child", name1
                class default
                    error stop 1_4
            end select

            if(size(name1) .NE. 12) error stop 2_4
            if(ubound(name1, DIM=1) .NE. 4) error stop 3_4
            if(ubound(name1, DIM=2) .NE. 3) error stop 4_4
        end associate
    end subroutine
end module

program typeBound002
use m
    class(AbstractParent), pointer :: ap1
    class(AbstractParent), allocatable :: ap2(:,:)

    allocate(Base::ap1)
    allocate(ap2(3,4), SOURCE=reshape((/(Base(i),i=1,12)/), (/3,4/)))
    call ap1%transposeIt(ap2)

    deallocate(ap1, ap2)
    allocate(Child::ap1)
    allocate(ap2(3,4), SOURCE=reshape((/(Child(i,i+1),i=1,12)/),(/3,4/)))
    call ap1%transposeIt(ap2)
end
