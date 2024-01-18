! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape004.f
! %VERIFY: reshape004.out:reshape004.vf
! %STDIN:
! %STDOUT: reshape004.out
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
!*    Function return of transfer is the SOURCE of reshape. Poly and
!*  unlimited poly.
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

program reshape004
use m
    class(*), allocatable :: c1(:)
    class(AbstractParent), allocatable :: b1(:)

    allocate(c1(5), SOURCE=(/(Child(i,i-1),i=101,105)/))
    allocate(b1(30), SOURCE=(/(Base(i),i=1,30)/))

    select type(name1=>reshape(transfer(c1, b1, 10), &
     (/2,6/), (/Base(-8)/), (/1,2/)))
        type is (Base)
            print *, name1
            if(.NOT. same_type_as(name1, Base(1))) error stop 1_4
            if(size(name1) .NE. 12) error stop 2_4
        class default
            error stop 3_4
    end select
end
