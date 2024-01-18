! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent003.f
! %VERIFY: structureComponent003.out:structureComponent003.vf
! %STDIN:
! %STDOUT: structureComponent003.out
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
!*    SOURCE or MOLD is a structure component, which is poly
!*  array. The object containing the component is a scalar.
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
        integer j
    end type

    type, extends(Base) :: Child
        class(*), allocatable :: b2(:,:)
    end type

    type Base1
        class(*), pointer :: j(:,:)
    end type
end module

program structureComponent003
use m
    type(Base) :: b1(20)
    type(Child) :: c1
    type(Base1) :: b2

    b1 = (/ (Base(i,i*2), i=1,20) /)

    allocate(c1%b2(5,5), SOURCE=reshape(b1, (/5,5/), &
     (/Base(-1,-2),Base(-3,-4)/), (/2,1/)))

    allocate(integer::b2%j(2,2))

    select type(name1=>transfer(c1%b2, b2%j, 20))
        type is (integer)
            print *, name1
            if(size(name1) .NE. 20) error stop 1_4
        class default
            error stop 2_4
    end select
end
