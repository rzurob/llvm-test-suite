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
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program associate001
use m
    class(AbstractParent), pointer :: ap1(:,:) => null()
    type(Child) :: c1(5)

    allocate(ap1(4,5), SOURCE=reshape((/(Child(i,i),i=1,20)/), (/4,5/)))

    associate(name1=>ap1(3,:))
        select type(name2=>spread(name1,2,3))
            type is (Child)
                print *, name2
                print *, size(name2)
                print *, shape(name2)
            class default
                error stop 1_4
        end select
    end associate

    select type(name1=>ap1(2:4,2:3))
        type is (Child)
            print *, spread(name1,3,4)
            print *, size(spread(name1,3,4))
            print *, shape(spread(name1,3,4))
        class default
            error stop 2_4
    end select
end
