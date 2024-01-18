! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/transpose/typeBound002.f
! opt variations: -qnok -ql -qreuse=none

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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/31/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
        contains
        procedure :: transposeIt
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    contains

    subroutine transposeIt(this, a)
        class(AbstractParent(4)), intent(in) :: this
        class(AbstractParent(4)), intent(in) :: a(:,:)
        associate (name1=>transpose(a))
            select type(name1)
                type is (Base(4))
                    print *, "Base", name1
                type is (Child(4))
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
    class(AbstractParent(4)), pointer :: ap1
    class(AbstractParent(4)), allocatable :: ap2(:,:)

    allocate(Base(4)::ap1)
    allocate(ap2(3,4), SOURCE=reshape((/(Base(4)(i),i=1,12)/), (/3,4/)))
    call ap1%transposeIt(ap2)

    deallocate(ap1, ap2)
    allocate(Child(4)::ap1)
    allocate(ap2(3,4), SOURCE=reshape((/(Child(4)(i,i+1),i=1,12)/),(/3,4/)))
    call ap1%transposeIt(ap2)
end
