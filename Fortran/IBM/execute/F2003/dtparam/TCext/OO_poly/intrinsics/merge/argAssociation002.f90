! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/merge/argAssociation002.f
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
!*  DATE                       : 01/25/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    TSOURCE or FSOURCE is a dummy argument. Dummy argument is
!*  non-pointer, non-allocatable, poly, and is array.
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

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        class(Base(4)) :: arg1(10)
        class(Base(4)) :: arg2(:,:)
        class(Base(4)) :: arg3(:)
        class(Base(4)) :: arg4(2,3)
        logical :: m1(10)
        m1 = (/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE., &
         .FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./)

        select type(name1=>merge(arg1, arg3, m1))
            type is (Base(4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 1_4
        end select

        select type(name1=>merge(arg2, arg4, reshape(m1,(/2,3/))))
            type is (Child(4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 2_4
        end select
    end subroutine
end module

program argAssociation002
use m
    type(Base(4)) :: b1(10)
    type(Child(4)) :: c1(2,3)
    class(Base(4)), pointer :: b2(:)
    class(Child(4)), allocatable :: c2(:,:)

    b1 = (/ (Base(4)(i),i=1,10) /)
    c1 = reshape((/(Child(4)(i,i+1),i=5,15,2)/),(/2,3/))
    allocate(b2(10), SOURCE=(/(Base(4)(i),i=2,11)/))
    allocate(c2(2,3), SOURCE=reshape((/(Child(4)(j=i-1,i=i), &
     i=12,17)/), (/2,3/)))

    call sub1(b1, c1, b2, c2)
end
