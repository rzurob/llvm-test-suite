! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*                               If a nonpointer dummy arg of procedure has
!*                               intent(out) or intent(inout) attribute, the
!*                               actual argument shall be definable.
!*                               FROM is intent(inout) argument
!*                               TO is intent(out) argument.
!*                               function return is not definable
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type T
    end type

    class(T), allocatable :: TO, FROM

    call move_alloc(func(), TO)

    call move_alloc(FROM, func())

    contains
        function func()
          class(T), allocatable :: func
          allocate(T::func)
        end function
    end
