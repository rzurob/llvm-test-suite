! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (assumed-size arrays in
!                               select type construct)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8) :: id
    end type

    type, extends(base) :: child
        character(15) :: name
        logical :: isSet
    end type

    contains

    subroutine test1 (a, b)
        class (base), intent(in) :: a(*), b(2,*)

        select type (a)
            type is (base)
                print *, a(1), a(2)
            type is (child)
                print *, a(2), a(3)
            class default
                error stop 1_4
        end select

        select type (b)
            type is (base)
                print *, b(1,1), b(2,1)
            type is (child)
                print *, b(2,1), b(1,2)
            class default
                error stop 2_4
        end select
    end subroutine
end module

program fArg014a3
use m
    class (base), allocatable :: b1(:,:), b2(:,:,:)
    character(2) :: c1(4) = (/'01', '02', '03', '04'/)

    allocate (b1(0:1,2), source=reshape((/(child(i, name='xlftest_'//c1(i), &
                isSet=mod(i,2)==0), i=1,4)/), (/2,2/)))

    allocate (b2(2,2,0:1))

    b2%id = reshape ((/(j, j=1,8)/), (/2,2,2/))

    call test1 (b1, b2)
end
