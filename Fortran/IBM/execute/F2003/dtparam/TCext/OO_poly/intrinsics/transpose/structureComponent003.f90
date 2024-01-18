! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/intrinsics/transpose/structureComponent003.f
! opt variations: -qnok -qnol -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent003.f
! %VERIFY: structureComponent003.out:structureComponent003.vf
! %STDIN:
! %STDOUT: structureComponent003.out
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
!*    MATRIX is a structure component, which is unlimited poly
!*  array. The object containing the component is a scalar.
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
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
        integer(k1) j
    end type

    type, extends(Base) :: Child    ! (4,20)
        class(*), allocatable :: b2(:,:)
    end type
end module

program structureComponent003
use m
    type(Base(4,20)) :: b1(20)
    type(Child(4,20)) :: c1

    b1 = (/ (Base(4,20)(i,i*2), i=1,20) /)

    allocate(c1%b2(4,6), SOURCE=reshape(b1, (/4,6/), &
     (/Base(4,20)(-1,-2),Base(4,20)(-3,-4)/), (/2,1/)))

    select type(name1=>transpose(c1%b2))
        type is (Base(4,*))
            print *, name1
            if(size(name1) .NE. 24) error stop 1_4
            if(ubound(name1, DIM=1) .NE. 6) error stop 2_4
            if(ubound(name1, DIM=2) .NE. 4) error stop 3_4
        class default
            error stop 4_4
    end select
end
