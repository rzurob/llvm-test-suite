! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/typeQuery/typeBound001.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=base

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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 10/27/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure :: extendsTypeOf
        procedure :: sameTypeAs
    end type

    type, extends(AbstractParent) :: Base(n2,k2)    ! (4,20,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      i
    end type

    type, extends(Base) :: Child(k3,n3)    ! (4,20,20,4,4,10)
        integer, kind :: k3
        integer, len  :: n3
        character(n3) :: c
    end type

    contains

    logical function extendsTypeOf(this, a)
        class(AbstractParent(4,*)), intent(in) :: this
        class(AbstractParent(4,*)), intent(in) :: a
        extendsTypeOf = extends_type_of(this, a)
    end function

    logical function sameTypeAs(this, a)
        class(AbstractParent(4,*)), intent(in) :: this
        class(AbstractParent(4,*)), intent(in) :: a
        sameTypeAs = same_type_as(this, a)
    end function
end module

program typeBound001
use m
    type(Base(4,20,20,4)) :: b1
    type(Child(4,20,20,4,4,10)) :: c1
    class(AbstractParent(4,:)), pointer :: ap1 => null()
    class(AbstractParent(4,:)), allocatable :: ap2

    allocate(Base(4,20,20,4)::ap1)
    allocate(Child(4,20,20,4,4,10)::ap2)

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
