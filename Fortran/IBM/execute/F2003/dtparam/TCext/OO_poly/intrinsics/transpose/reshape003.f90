! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/transpose/reshape003.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape003.f
! %VERIFY: reshape003.out:reshape003.vf
! %STDIN:
! %STDOUT: reshape003.out
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
!*    MATRIX is function return of reshape. Poly and unlimited poly.
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
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type
end module

program reshape003
use m
    class(AbstractParent(4,:)), pointer :: c1(:)
    class(*), pointer :: b1(:)

    allocate(c1(5), SOURCE=(/(Child(4,20)(i,i-1),i=101,105)/))
    allocate(b1(30), SOURCE=(/(Base(4,20)(i),i=1,30)/))

    select type(name1=>transpose(reshape(c1, (/2,4/), &
     (/Child(4,20)(-1,-2)/), (/2,1/))))
        type is (Child(4,*))
            print *, name1
            if(size(name1) .NE. 8) error stop 1_4
            if(ubound(name1, DIM=1) .NE. 4) error stop 2_4
            if(ubound(name1, DIM=2) .NE. 2) error stop 3_4
        class default
            error stop 4_4
    end select

    select type(name1=>transpose(reshape(b1, (/5,3/))))
        type is (Base(4,*))
            print *, name1
            if(size(name1) .NE. 15) error stop 5_4
            if(ubound(name1, DIM=1) .NE. 3) error stop 6_4
            if(ubound(name1, DIM=2) .NE. 5) error stop 7_4
        class default
            error stop 8_4
    end select
end
