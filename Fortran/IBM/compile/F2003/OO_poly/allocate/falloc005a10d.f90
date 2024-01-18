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
!*  DATE                       : 01/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : allocate statement (allocate-object has to be
!                               type-compatible with source-expr)
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
        integer(4) :: id
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    interface makeData
        function produceBaseObj (id)
            import base
            type (base) produceBaseObj
            integer(4), intent(in) :: id
        end function

        function produceChildObj (id, name)
            import child
            type (child) produceChildObj
            integer(4), intent(in) :: id
            character(*), intent(in) :: name
        end function
    end interface
end module

program falloc005a10d
use m
    type (base), allocatable :: b1, b2(:)

    class (child), pointer :: c1

    !! the following 3 allocate statements will all fail as type-compatibilties
    !between source-exprs and allocate-objects

    allocate (b1, source=makeData (10, 'b1'))

    allocate (b2(3:5), source=makeData (20, 'b2_array_of_3'))

    allocate (c1, source=makeData (30))

end

type (base) function produceBaseObj (id)
use m, only: base
    integer(4), intent(in) :: id

    produceBaseObj%id = id
end function


type (child) function produceChildObj (id, name)
use m, only: child
    integer(4), intent(in) :: id
    character(*), intent(in) :: name

    produceChildObj%id = id
    produceChildObj%name = name
end function
