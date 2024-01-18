! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/spread/associate001.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate001.f
! %VERIFY: associate001.out:associate001.vf
! %STDIN:
! %STDOUT: associate001.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 01/06/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    SOURCE is an associate name.
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

program associate001
use m
    class(AbstractParent(4,:)), pointer :: ap1(:,:) => null()
    type(Child(4,20)) :: c1(5)

    allocate(ap1(4,5), SOURCE=reshape((/(Child(4,20)(i,i),i=1,20)/), (/4,5/)))

    associate(name1=>ap1(3,:))
        select type(name2=>spread(name1,2,3))
            type is (Child(4,*))
                print *, name2
                print *, size(name2)
                print *, shape(name2)
            class default
                error stop 1_4
        end select
    end associate

    select type(name1=>ap1(2:4,2:3))
        type is (Child(4,*))
            print *, spread(name1,3,4)
            print *, size(spread(name1,3,4))
            print *, shape(spread(name1,3,4))
        class default
            error stop 2_4
    end select
end
