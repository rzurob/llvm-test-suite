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
!*  DATE                       : 12/31/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    MATRIX is an associate name.
!*    Selector is an unlimited polymorphic array section, where the
!*  whole array may be a structure component.
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

    type Base1
        type(Base) :: b(4,6,3)
    end type
end module

program associate003
use m
    class(*), allocatable :: u1(:,:,:,:)
    class(Base1), pointer :: b1

    allocate(u1(2,3,4,2), SOURCE=reshape((/(Base(i),i=1,48)/), &
     (/2,3,4,2/)))
    allocate(b1, SOURCE=Base1(reshape((/(Base(i),i=-1,-72,-1)/), &
     (/4,6,3/))))

    associate(name1=>u1(2,:,2:3,1), name2=>b1%b(2:3,4,:))
        select type (name3=>transpose(name1))
            type is (Base)
                print *, name3
                if(size(name3) .NE. 6) error stop 1_4
                if(ubound(name3, DIM=1) .NE. 2) error stop 2_4
                if(ubound(name3, DIM=2) .NE. 3) error stop 3_4
            class default
                error stop 4_4
        end select

        associate(name4=>transpose(name2))
            print *, name4
            if(size(name4) .NE. 6) error stop 5_4
            if(ubound(name4, DIM=1) .NE. 3) error stop 6_4
            if(ubound(name4, DIM=2) .NE. 2) error stop 7_4
        end associate
    end associate
end
