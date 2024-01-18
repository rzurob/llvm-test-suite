! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate002.f
! %VERIFY: associate002.out:associate002.vf
! %STDIN:
! %STDOUT: associate002.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/31/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    MATRIX is an associate name.
!*    Selector is a polymorphic array section.
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

program associate002
use m
    class(AbstractParent), allocatable :: ap1(:,:,:,:)

    allocate(ap1(2,3,4,2), SOURCE=reshape((/(Base(i),i=1,48)/), &
     (/2,3,4,2/)))

    associate(name1=>ap1(2,:,2:3,1))
        select type (name2=>transpose(name1))
            type is (Base)
                print *, name2
                if(size(name2) .NE. 6) error stop 1_4
                if(ubound(name2, DIM=1) .NE. 2) error stop 2_4
                if(ubound(name2, DIM=2) .NE. 3) error stop 3_4
            class default
                error stop 4_4
        end select
    end associate
end
