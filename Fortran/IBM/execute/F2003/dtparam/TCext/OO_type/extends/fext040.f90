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
! %GROUP: fext040.f
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
!*  DATE                       : Nov. 11, 2003
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
!*                               use intrinsic data type only
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
    type objectNode(k)
        integer, kind :: k
        class (*), pointer :: valuePtr => null()
        type (objectNode(k)), pointer :: nextNode => null()
    end type

end module

program fext040
    use m

    integer*4, target :: i1 = 10
    real*4, target :: r1 = 1.0
    logical*1, target :: l1 = .true.
    character(20), target :: c1 = 'This is a test'
    complex (4), target :: c2 = (-1.0, 1.0)

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
