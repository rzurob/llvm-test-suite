! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/merge/argAssociation001.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation001.f
! %VERIFY: argAssociation001.out:argAssociation001.vf
! %STDIN:
! %STDOUT: argAssociation001.out
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
!*  non-pointer, non-allocatable, poly, and is scalar.
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
        class(Base(4)) :: arg1
        class(Base(4)) :: arg2
        class(Base(4)) :: arg3
        class(Base(4)) :: arg4

        select type(name1=>merge(arg1, Base(4)(10), .TRUE.))
            type is (Base(4))
                print *, name1
            class default
                error stop 1_4
        end select

        select type(name1=>merge(arg2, arg3, .FALSE.))
            type is (Child(4))
                print *, name1
            class default
                error stop 2_4
        end select

        select type(name1=>merge(arg3, arg4, .FALSE.))
            type is (Child(4))
                print *, name1
            class default
                error stop 3_4
        end select
    end subroutine
end module

program argAssociation001
use m
    type(Base(4)) :: b1
    type(Child(4)) :: c1
    class(Base(4)), pointer :: b2
    class(Child(4)), allocatable :: c2

    b1 = Base(4)(10)
    c1 = Child(4)(7, 8)
    allocate(b2, SOURCE=Child(4)(3,4))
    allocate(c2, SOURCE=Child(4)(j=5,i=6))

    call sub1(b1, c1, b2, c2)
end
