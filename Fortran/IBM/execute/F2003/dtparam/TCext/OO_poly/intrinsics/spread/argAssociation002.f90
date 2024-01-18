! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/spread/argAssociation002.f
! opt variations: -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation002.f
! %VERIFY: argAssociation002.out:argAssociation002.vf
! %STDIN:
! %STDOUT: argAssociation002.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 01/20/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    SOURCE is a dummy argument. Dummy argument is non-pointer,
!*  non-allocatable, non-poly, and is array.
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program argAssociation002
use m
    type(Base(4)) :: b(10)
    class(Base(4)), pointer :: c(:,:)
    class(Base(4)), pointer :: b1(:,:,:)

    b = (/ (Base(4)(i),i=1,10) /)

    allocate(c(3,2), SOURCE=reshape((/(Child(4)(j=-i,i=i),i=1,6)/),(/3,2/)))

    allocate(b1(2,2,3), SOURCE=reshape((/(Base(4)(i),i=2,13)/),(/2,2,3/)))

    call sub1(b, c, b1)

    contains

    subroutine sub1(arg1, arg2, arg3)
        type(Base(4)) :: arg1(10)
        type(Base(4)) :: arg2(:,:)
        type(Base(4)) :: arg3(3,*)

        print *, spread(arg1, 2, 2)
        print *, spread(arg2, 3, 2)

        associate(name1=>spread(arg3(:,:4), 3, 2))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        end associate
    end subroutine
end
