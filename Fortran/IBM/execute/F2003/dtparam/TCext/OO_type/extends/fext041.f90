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
! %GROUP: fext041.f
! %VERIFY: fext041.out:fext041.vf
! %STDIN:
! %STDOUT: fext041.out
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
    type objectNode(k1)
        integer, kind :: k1
        class (*), pointer :: valuePtr => null()
        type (objectNode(k1)), pointer :: nextNode => null()
    end type

end module

module m1
    type base(k2)
        integer, kind :: k2
        integer(k2) :: id

        contains

        procedure, nopass :: print => basePrint
    end type

    type, extends(base) :: child(n)
        integer, len :: n
        character(n) :: name

        contains

        procedure, nopass :: print => childPrint
    end type

    type (base(4)), target :: b1_m = base (4)(1)
    type (child(4,20)), target :: c1_m = child (4,20)(2, 'c1_m')

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

    type (base(4)), target :: b1 = base (4)(3)
    type (child(4,20)), target :: c1 = child (4,20)(4, 'c1')
    type (child(4,20)), target :: c2 = child (4,20)(5, 'c2')

    type (objectNode(4)), target :: list
    type (objectNode(4)), pointer :: iterator

    type (objectNode(4)), pointer :: currentNode

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
            class is (base(4))

                call x%print()
        end select

        iterator => iterator%nextNode
    end do
end program
