!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext040a.f
! %VERIFY:
! %STDIN:
! %STDOUT: fext040a.out
! %EXECARGS:
! %POSTCMD: spiff fext040a.out $TR_SRC/fext040a.vf
! %END
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
    type point
        real*4 :: x, y

        contains

        procedure, nopass :: print => pointPrint
    end type

    type, extends(point) :: point3D
        real*4 :: z

        contains

        procedure, nopass :: print => point3DPrint
    end type

    type (point), target :: p1_m = point (0.0, 0.0)
    type (point3D), target :: p3D1_m = point3D (1.0, 1.0, 1.0)

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
    type objectNode
        class (point), pointer :: valuePtr => null()
        type (objectNode), pointer :: nextNode => null()
    end type

end module

program fext040a
    use m1

    type (point), target :: p1 = point (1.0, 1.0)
    type (point), target :: p2 = point (0.0, 1.0)

    type (point3D), target :: p3D1 = point3D (0.0, 0.0, 0.0)

    type (objectNode), target :: list
    type (objectNode), pointer :: iterator

    type (objectNode), pointer :: currentNode

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
