! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: seqAssociation002.f
! %VERIFY: seqAssociation002.out:seqAssociation002.vf
! %STDIN:
! %STDOUT: seqAssociation002.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/29/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD of TRANSFER is a dummy argument. Dummy argument
!*  is an explicit-shape or assumed-size array. Actual argument is
!*  sequence associated with dummy argument. Actual argument is an
!*  array section. Poly.
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

    type Base1
        integer j
        type(Base) :: k
        integer m
    end type
end module

program seqAssociation002
use m
    class(Base1), allocatable :: b1(:,:)

    allocate(b1(4,3), SOURCE=reshape((/(Base1(i, Base(i+1), i+2), &
     i=3,14)/), (/4,3/)))

    call sub1(b1(:3,2:))

    contains

    subroutine sub1(arg1)
        class(Base1) :: arg1(:,:)

        select type(name1=>transfer(arg1(2:,:), arg1))
            type is (Base1)
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 1_4
        end select

        associate(name1=>transfer(arg1, Child(1,1), 8))
            if(.NOT. same_type_as(name1, Child(1,1))) error stop 2_4
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        end associate
    end subroutine
end
