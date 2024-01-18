! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (subobject of a data with PROTECTED
!*                               or PARAMETER attributes still has that
!*                               attribute)
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
        integer*4 :: id

        contains

        procedure :: assgnID => assignIDVal
        procedure, nopass :: assgnBase
    end type

    type, extends (base) :: child
        character*20 :: name
    end type

    type container
        class (base), pointer :: data => null()
    end type

    type (child), protected :: c1 = child (1, 'c1_protected')
    type (container), protected, save :: co1

    type (child), parameter :: c2 = child (2, 'c2_const')

    contains

    subroutine assgnBase (b, id)
        class (base), pointer, intent(out) :: b
        integer*4, intent(in) :: id

        allocate (b, source=base(id))
    end subroutine

    subroutine assignIDVal (b, id)
        class (base), intent(inout) :: b
        integer*4, intent(in) :: id

        b%id = id
    end subroutine
end module

program ftpbnd510d0
use m
    class (base), pointer :: b

    call c1%base%assgnID (10)   !<-- this is illegal

    call c2%base%assgnID (20)   !<-- this is illegal

    co1%data => b       !<-- this is illegal

    call co1%data%assgnBase (co1%data, 15)  !<-- this is illegal
end
