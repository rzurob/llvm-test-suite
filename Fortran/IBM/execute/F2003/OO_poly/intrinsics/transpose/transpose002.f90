! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transpose002.f
! %VERIFY: transpose002.out:transpose002.vf
! %STDIN:
! %STDOUT: transpose002.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/30/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : MATRIX is poly.
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

program transpose002
use m
    class(Base), allocatable :: b1(:,:)
    class(Base), pointer :: b2(:,:)

    allocate(b1(2,4), SOURCE=reshape((/(Base(i),i=1,8)/), (/2,4/)))
    allocate(b2(3,3), SOURCE=reshape((/(Child(i,i-1),i=11,19)/), &
     (/3,3/)))

    select type(name1=>transpose(b1))
        type is (Base)
            print *, name1
        class default
            error stop 1_4
    end select

    select type(name1=>transpose(b2))
        type is (Child)
            print *, name1
        class default
            error stop 2_4
    end select
end
