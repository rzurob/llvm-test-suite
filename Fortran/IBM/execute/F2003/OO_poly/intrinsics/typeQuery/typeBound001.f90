! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeBound001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/27/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : Call the intrinsic inquiry functions
!*    inside the type bound procedures.
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

        procedure :: extendsTypeOf
        procedure :: sameTypeAs
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type

    contains

    logical function extendsTypeOf(this, a)
        class(AbstractParent), intent(in) :: this
        class(AbstractParent), intent(in) :: a
        extendsTypeOf = extends_type_of(this, a)
    end function

    logical function sameTypeAs(this, a)
        class(AbstractParent), intent(in) :: this
        class(AbstractParent), intent(in) :: a
        sameTypeAs = same_type_as(this, a)
    end function
end module

program typeBound001
use m
    type(Base) :: b1
    type(Child) :: c1
    class(AbstractParent), pointer :: ap1 => null()
    class(AbstractParent), allocatable :: ap2

    allocate(Base::ap1)
    allocate(Child::ap2)

    if(b1%extendsTypeOf(c1)) error stop 1_4
    if(.NOT. b1%extendsTypeOf(ap1)) error stop 2_4
    if(b1%extendsTypeOf(ap2)) error stop 3_4
    if(.NOT. c1%extendsTypeOf(ap1)) error stop 4_4
    if(.NOT. c1%extendsTypeOf(ap2)) error stop 5_4
    if(ap1%extendsTypeOf(ap2)) error stop 6_4

    if(b1%sameTypeAs(c1)) error stop 7_4
    if(.NOT. b1%sameTypeAs(ap1)) error stop 8_4
    if(b1%sameTypeAs(ap2)) error stop 9_4
    if(c1%sameTypeAs(ap1)) error stop 10_4
    if(.NOT. c1%sameTypeAs(ap2)) error stop 11_4
    if(ap1%sameTypeAs(ap2)) error stop 12_4
end
