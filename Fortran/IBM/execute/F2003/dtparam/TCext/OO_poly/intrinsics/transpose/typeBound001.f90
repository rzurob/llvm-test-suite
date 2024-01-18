! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/intrinsics/transpose/typeBound001.f
! opt variations: -qnok -qnol -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeBound001.f
! %VERIFY: typeBound001.out:typeBound001.vf
! %STDIN:
! %STDOUT: typeBound001.out
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
!*    Non-poly
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
        procedure :: transposeIt
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type

    contains

    subroutine transposeIt(this, a)
        class(AbstractParent(4,*)), intent(in) :: this
        class(AbstractParent(4,*)), intent(in) :: a(:,:)
        associate (name1=>transpose(a))
            select type(name1)
                type is (Base(4,*))
                    print *, "Base", name1
                type is (Child(4,*))
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

program typeBound001
use m
    type(Base(4,20)) :: b1
    type(Child(4,20)) :: c1

    type(Base(4,20)) :: b2(3,4)
    type(Child(4,20)) :: c2(3,4)

    b2 = reshape((/(Base(4,20)(i),i=1,12)/), (/3,4/))
    c2 = reshape((/(Child(4,20)(i,i+1),i=4,15)/), (/3,4/))

    call b1%transposeIt(b2)
    call c1%transposeIt(c2)
end
