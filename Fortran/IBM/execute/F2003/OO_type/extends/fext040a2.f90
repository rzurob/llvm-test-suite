!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext040a2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE TITLE            :
!*                                                                     
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 01/10/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
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
    type objectNode
        class (*), pointer :: valuePtr => null()
        type (objectNode), pointer :: nextNode => null()
    end type

end module

module m1
    type point
        real*4 :: x, y
    end type

    type, extends (point) :: point3D
        real*4 :: z
    end type

    type, extends (point3D) :: colorPoint3D
        integer*1 :: color = 1
    end type

    type (colorPoint3D), save, target :: c1 = colorPoint3D(0.0, 0.0, 0.0)
    type (point3D), target :: i1 = point3D(0.0, 0.0, 0.0)
end module

program fext040a2
    use m
    use m1

    type (point), target :: r1 = point (1.0, 0.0)

    type, extends (point) :: colorPoint
        integer*1 :: color = 1
    end type

    type (colorPoint), target :: l1 = colorPoint (1.0, 1.0, 2)

    type (colorPoint3D), target :: c2 = colorPoint3D (0.0, 10.0, 0.0, color=3)

    type (objectNode), target :: list
    type (objectNode), pointer :: iterator

    type (objectNode), pointer :: currentNode

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
