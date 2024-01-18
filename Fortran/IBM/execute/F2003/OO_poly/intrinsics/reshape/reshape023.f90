! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape023.f
! %VERIFY: reshape023.out:reshape023.vf
! %STDIN:
! %STDOUT: reshape023.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/05/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    SOURCE is non-poly
!*    Assigned data entity is poly
!*    PAD and ORDER are not specified
!*    SOURCE is rank three
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

program reshape023
use m
    type(Child) :: c1(20)
    type(Child) :: c2(3,4,2)
    class(Base), pointer :: b2(:,:) => null()

    c1 = (/ (Child(i,i+1), i=1,20) /)
    c2 = reshape(c1, (/3,4,2/), (/Child(-1,1),Child(-2,2)/), (/2,3,1/))

    allocate(b2(3,5), SOURCE=reshape(c2, (/3,5/)))

    print *, c1
    print *, c2

    select type (b2)
        type is (Child)
            print *, b2
        class default
            error stop 1_4
    end select
end
