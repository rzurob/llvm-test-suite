! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/typeQuery/typeBound002.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeBound002.f
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
!*    inside the type bound procedures. Unlimited polymorphic.
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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure :: extendsTypeOf
        procedure :: sameTypeAs
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child(n2)    ! (4,20,10)
        integer, len  :: n2
        character(n2) :: c
    end type

    contains

    logical function extendsTypeOf(this, a)
        class(AbstractParent(4,*)), intent(in) :: this
        class(*), allocatable, intent(in) :: a
        extendsTypeOf = extends_type_of(this, a)
    end function

    logical function sameTypeAs(this, a)
        class(AbstractParent(4,*)), intent(in) :: this
        class(*), allocatable, intent(in) :: a
        sameTypeAs = same_type_as(this, a)
    end function
end module

program typeBound002
use m
    type(Base(4,20)) :: b1
    type(Child(4,20,10)) :: c1
    class(AbstractParent(4,:)), pointer :: ap1 => null()
    class(AbstractParent(4,:)), allocatable :: ap2
    class(*), allocatable :: u1

    allocate(Base(4,20)::ap1)
    allocate(Child(4,20,10)::ap2)

    if(.NOT. b1%extendsTypeOf(u1)) error stop 1_4
    if(.NOT. c1%extendsTypeOf(u1)) error stop 2_4
    if(.NOT. ap1%extendsTypeOf(u1)) error stop 3_4
    if(.NOT. ap2%extendsTypeOf(u1)) error stop 4_4

    if(b1%sameTypeAs(u1)) error stop 5_4
    if(c1%sameTypeAs(u1)) error stop 6_4
    if(ap1%sameTypeAs(u1)) error stop 7_4
    if(ap2%sameTypeAs(u1)) error stop 8_4

    allocate(Child(4,20,10)::u1)

    if(b1%extendsTypeOf(u1)) error stop 9_4
    if(.NOT. c1%extendsTypeOf(u1)) error stop 10_4
    if(ap1%extendsTypeOf(u1)) error stop 11_4
    if(.NOT. ap2%extendsTypeOf(u1)) error stop 12_4

    if(b1%sameTypeAs(u1)) error stop 13_4
    if(.NOT. c1%sameTypeAs(u1)) error stop 14_4
    if(ap1%sameTypeAs(u1)) error stop 15_4
    if(.NOT. ap2%sameTypeAs(u1)) error stop 16_4

    deallocate(u1)
    allocate(integer::u1)

    if(b1%extendsTypeOf(u1)) error stop 17_4
    if(c1%extendsTypeOf(u1)) error stop 18_4
    if(ap1%extendsTypeOf(u1)) error stop 19_4
    if(ap2%extendsTypeOf(u1)) error stop 20_4

    if(b1%sameTypeAs(u1)) error stop 21_4
    if(c1%sameTypeAs(u1)) error stop 22_4
    if(ap1%sameTypeAs(u1)) error stop 23_4
    if(ap2%sameTypeAs(u1)) error stop 24_4
end
