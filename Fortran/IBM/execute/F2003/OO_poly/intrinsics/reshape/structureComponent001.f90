! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent001.f
! %VERIFY: structureComponent001.out:structureComponent001.vf
! %STDIN:
! %STDOUT: structureComponent001.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/06/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : SOURCE is a structure component, which
!*    is non-poly array. The object containing the component is a scalar.
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
        type(Base) :: b1(20)
        type(Base) :: b2(5,5)
    end type
end module

program structureComponent001
use m
    class(Base), allocatable :: b0(:,:,:)
    type(Child) :: c1

    c1%b1 = (/ (Base(i), i=1,20) /)

    c1%b2 = reshape(c1%b1, (/5,5/), (/Base(-1),Base(-2)/), (/2,1/))

    allocate(b0(3,2,4), SOURCE=reshape(c1%b2, (/3,2,4/), &
     (/Base(-3)/), (/3,2,1/)))

    print *, c1%b1
    print *, c1%b2

    select type (b0)
        type is (Base)
            print *, b0
        class default
            error stop 1_4
    end select
end
