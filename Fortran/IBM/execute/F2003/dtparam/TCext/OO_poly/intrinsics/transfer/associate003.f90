! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/transfer/associate003.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate003.f
! %VERIFY: associate003.out:associate003.vf
! %STDIN:
! %STDOUT: associate003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/21/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    SOURCE and/or MOLD are associate names.
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

program associate003
use m
    class(AbstractParent(4,:)), pointer :: ap1(:) => null()

    allocate(ap1(20), SOURCE=(/ (Base(4,20)(i),i=1,20) /))

    associate(name1=>ap1)
        select type (name2=>transfer(Child(4,20)(8,9), name1))
            type is (Base(4,*))
                print *, name2
            class default
                error stop 1_4
        end select
    end associate

    associate(name1=>ap1(3:7), name2=>ap1(10:))
        select type (name3=>transfer(name1, name2))
            type is (Base(4,*))
                print *, name3
            class default
                error stop 2_4
        end select
    end associate

    associate(name1=>ap1(15))
        select type (name2=>transfer(Child(4,20)(8,9), name1))
            type is (Base(4,*))
                print *, name2
            class default
                error stop 3_4
        end select
    end associate
end
