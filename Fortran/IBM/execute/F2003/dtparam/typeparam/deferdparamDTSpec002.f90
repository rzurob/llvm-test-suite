!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 01/10/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters in
!                               declaration-type-spec: used in components and
!                               entities.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type namedNode (l)
        integer, len :: l

        character(l) :: name
        type(namedNode(:)), pointer :: next => null()
    end type
end module

program deferdparamDTSpec002
use m
    type (namedNode(10)), target :: n1 (10)

    type (namedNode(:)), pointer :: list, iterator

    n1%name = (/('n1:'//char(ichar('0')+i), i=0, 9)/)

    allocate (namedNode(20) :: list)

    list%name = 'head'

    allocate (namedNode(30) :: list%next)
    list%next%name= 'node 1'

    list%next%next => n1(8)

    list%next%next%next => n1(10)

    !! print out the names
    iterator => list

    do while (associated(iterator))
        print *, iterator%name
        iterator => iterator%next
    end do
end
