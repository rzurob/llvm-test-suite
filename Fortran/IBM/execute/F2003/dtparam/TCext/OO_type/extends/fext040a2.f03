! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : define a linked-list for unlimited type object
!*                               use derived types.
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
    type objectNode(k1)
        integer, kind :: k1
        class (*), pointer :: valuePtr => null()
        type (objectNode(k1)), pointer :: nextNode => null()
    end type

end module

module m1
    type point(k2)
        integer, kind :: k2
        real(k2) :: x, y
    end type

    type, extends (point) :: point3D(k3)
        integer, kind :: k3
        real(k3) :: z
    end type

    type, extends (point3D) :: colorPoint3D(k4)
        integer, kind :: k4
        integer(k4) :: color = 1
    end type

    type (colorPoint3D(4,4,1)), save, target :: c1 = colorPoint3D(4,4,1)(0.0, 0.0, 0.0)
    type (point3D(4,4)), target :: i1 = point3D(4,4)(0.0, 0.0, 0.0)
end module

program fext040a2
    use m
    use m1

    type (point(4)), target :: r1 = point (4)(1.0, 0.0)

    type, extends (point) :: colorPoint(k5)
        integer, kind :: k5
        integer(k5) :: color = 1
    end type

    type (colorPoint(4,1)), target :: l1 = colorPoint (4,1)(1.0, 1.0, 2)

    type (colorPoint3D(4,4,1)), target :: c2 = colorPoint3D (4,4,1)(0.0, 10.0, 0.0, color=3)

    type (objectNode(4)), target :: list
    type (objectNode(4)), pointer :: iterator

    type (objectNode(4)), pointer :: currentNode

    integer*4 :: totalNodes = 0

    list%valuePtr => i1

    ! add one more node
    allocate (currentNode)

    currentNode%valuePtr => r1
    list%nextNode => currentNode

    ! add another node
    allocate (currentNode%nextNode)

    currentNode => currentNode%nextNode
    currentNode%valuePtr => l1

    ! yet another node
    allocate (currentNode%nextNode)
    currentNode => currentNode%nextNode
    currentNode%valuePtr => c1

    ! add another node for c2
    allocate (currentNode%nextNode)
    currentNode => currentNode%nextNode
    currentNode%valuePtr => c2

    ! iterate through the list and check for the association status for each
    ! node
    iterator => list

    do while (associated (iterator))
        totalNodes = totalNodes + 1
        iterator => iterator%nextNode
    end do

    if (totalNodes /= 5) error stop 1_4

    iterator => list

    if (.not. associated (iterator%valuePtr, i1)) error stop 2_4

    iterator => iterator%nextNode

    if (.not. associated (iterator%valuePtr, r1)) error stop 3_4

    iterator => iterator%nextNode

    if (.not. associated (iterator%valuePtr, l1)) error stop 4_4

    iterator => iterator%nextNode

    if (.not. associated (iterator%valuePtr, c1)) error stop 5_4

    iterator => iterator%nextNode

    if (.not. associated (iterator%valuePtr, c2)) error stop 6_4

end program
