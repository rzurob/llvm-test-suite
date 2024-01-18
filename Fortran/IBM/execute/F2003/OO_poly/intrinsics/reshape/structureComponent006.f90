! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent006.f
! %VERIFY: structureComponent006.out:structureComponent006.vf
! %STDIN:
! %STDOUT: structureComponent006.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/06/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is a structure component, which
!*    is a scalar. The object containing the component is an array and
!*    is unlimited poly.
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
        class(*), pointer :: b2
    end type
end module

program structureComponent006
use m
    class(Base), allocatable :: b0(:,:,:)
    type(Child) :: c1(4,5)
    type(Child) :: c2

    c1%i = reshape((/(i,i=101,120)/), (/4,5/))
    do i=1,4
        do j=1,5
            allocate(c1(i,j)%b2, SOURCE=Base(i+j))
        end do
    end do

    c2%i = -1
    allocate(c2%b2, SOURCE=Base(-2))

    allocate(b0(3,2,4), SOURCE=reshape(c1, (/3,2,4/), &
     (/c2/), (/3,2,1/)))

    print *, c1%i
    do i=1,5
        do j=1,4
            select type (name1=>c1(j,i)%b2)
                type is (Base)
                    print *, name1
                class default
                    error stop 1_4
            end select
        end do
    end do

    select type (b0)
        type is (Child)
            print *, b0%i
            do i=1,4
                do j=1,2
                    do k=1,3
                        select type (name1=>b0(k,j,i)%b2)
                            type is (Base)
                                print *, name1
                            class default
                                error stop 2_4
                        end select
                    end do
                end do
            end do
        class default
            error stop 3_4
    end select
end
