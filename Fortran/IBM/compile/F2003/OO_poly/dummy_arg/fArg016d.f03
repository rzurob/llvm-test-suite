! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (deferred-shape array
!*                               shall not have bounds info in the declaration)
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
        integer*4 :: id = -1
    end type

    type, extends (base) :: child
        character*20 :: name = 'default'
    end type

    contains

    subroutine test2 (b)
        class (base), allocatable, intent(inout) :: b(23:)
        type (child), save :: c1(6) = (/child (1, 'c1_1'), child(2, 'c1_2'), &
            child(3,'c1_3'), child(4,'c1_4'), child(5,'c1_5'), child(6,'c1_6')/)

        if (.not. allocated (b)) then
            allocate (b(10:15), source=c1)
        end if
    end subroutine
end module

program fArg016d
end