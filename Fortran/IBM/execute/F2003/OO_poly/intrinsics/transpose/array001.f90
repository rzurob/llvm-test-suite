! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: array001.f
! %VERIFY: array001.out:array001.vf
! %STDIN:
! %STDOUT: array001.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/30/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    MATRIX is array section
!*    Non-poly, poly, and unlimited poly
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
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program array001
use m
    type(Base) :: b1(2,2,2,2)
    class(Base), pointer :: b2(:,:)
    class(*), allocatable :: b3(:,:,:)

    b1 = reshape((/(Base(i),i=1,16)/), (/2,2,2,2/))

    allocate(b2(4,4), SOURCE=reshape((/(Child(i,i+1),i=5,20)/),(/4,4/)))
    allocate(b3(2,5,4), SOURCE=reshape((/(Base(i),i=11,50)/),(/2,5,4/)))

    print *, transpose(b1(2,:,:2,1))

    select type(name1=>transpose(b2(2:3,2:)))
        type is (Child)
            print *, name1
        class default
            error stop 1_4
    end select

    select type(name1=>transpose(b3(2:4:3,3,2:)))
        type is (Base)
            print *, name1
        class default
            error stop 2_4
    end select
end
