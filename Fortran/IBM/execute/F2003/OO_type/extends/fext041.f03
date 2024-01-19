! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 11, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : define a linked-list for unlimited type object
!*                               use derived types
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
    type objectNode
        class (*), pointer :: valuePtr => null()
        type (objectNode), pointer :: nextNode => null()
    end type

end module

module m1
    type base
        integer*4 :: id

        contains

        procedure, nopass :: print => basePrint
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure, nopass :: print => childPrint
    end type

    type (base), target :: b1_m = base (1)
    type (child), target :: c1_m = child (2, 'c1_m')

    contains

        subroutine basePrint ()
            print *, 'base type'
        end subroutine

        subroutine childPrint ()
            print *, 'child type'
        end subroutine
end module

program fext041
    use m
    use m1

    type (base), target :: b1 = base(3)
    type (child), target :: c1 = child (4, 'c1')
    type (child), target :: c2 = child (5, 'c2')

    type (objectNode), target :: list
    type (objectNode), pointer :: iterator

    type (objectNode), pointer :: currentNode

    list%valuePtr => b1_m

    ! add one more node
    allocate (currentNode)

    currentNode%valuePtr => c1_m
    list%nextNode => currentNode

    ! add another node
    allocate (currentNode%nextNode)

    currentNode => currentNode%nextNode
    currentNode%valuePtr => b1

    ! yet another node
    allocate (currentNode%nextNode)
    currentNode => currentNode%nextNode
    currentNode%valuePtr => c1

    ! add another node for c2
    allocate (currentNode%nextNode)
    currentNode => currentNode%nextNode
    currentNode%valuePtr => c2

    ! iterate through the list and prints out the results
    iterator => list

    do while (associated (iterator))
        if (.not. associated (iterator%valuePtr)) error stop 1_4

        select type ( x => iterator%valuePtr)
            class is (base)

                call x%print()
        end select

        iterator => iterator%nextNode
    end do
end program
