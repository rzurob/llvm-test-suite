! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 11, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : define a linked-list for polymorphic types
!*                               use point <- point3D
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
    type point(k1)
        integer, kind :: k1
        real(k1) :: x, y

        contains

        procedure, nopass :: print => pointPrint
    end type

    type, extends(point) :: point3D(k2)
        integer, kind :: k2
        real(k2) :: z

        contains

        procedure, nopass :: print => point3DPrint
    end type

    type (point(4)), target :: p1_m = point (4)(0.0, 0.0)
    type (point3D(4,4)), target :: p3D1_m = point3D (4,4)(1.0, 1.0, 1.0)

    contains

    subroutine pointPrint ()
        print *, 'point'
    end subroutine

    subroutine point3DPrint ()
        print *, 'point3D'
    end subroutine

end module

module m1
use m
    type objectNode(k3)
        integer, kind :: k3
        class (point(k3)), pointer :: valuePtr => null()
        type (objectNode(k3)), pointer :: nextNode => null()
    end type

end module

program fext040a
    use m1

    type (point(4)), target :: p1 = point (4)(1.0, 1.0)
    type (point(4)), target :: p2 = point (4)(0.0, 1.0)

    type (point3D(4,4)), target :: p3D1 = point3D (4,4)(0.0, 0.0, 0.0)

    type (objectNode(4)), target :: list
    type (objectNode(4)), pointer :: iterator

    type (objectNode(4)), pointer :: currentNode

    list%valuePtr => p1_m

    ! add one more node
    allocate (currentNode)

    currentNode%valuePtr => p3D1_m
    list%nextNode => currentNode

    ! add another node
    allocate (currentNode%nextNode)

    currentNode => currentNode%nextNode
    currentNode%valuePtr => p1

    ! yet another node
    allocate (currentNode%nextNode)
    currentNode => currentNode%nextNode
    currentNode%valuePtr => p2

    ! add another node
    allocate (currentNode%nextNode)
    currentNode => currentNode%nextNode
    currentNode%valuePtr => p3D1

    ! iterate through the list and prints out the results
    iterator => list

    do while (associated (iterator))
        call iterator%valuePtr%print()
        print *, iterator%valuePtr%x, iterator%valuePtr%y
        iterator => iterator%nextNode
    end do
end program
