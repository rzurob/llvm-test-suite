! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : intrinsic function NULL (interact with
!                               same_type_as)
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

program fnull501
    type base
        integer id
    end type

    type, extends(base) :: child
        character(16) :: name
    end type

    class (base), pointer :: b1

    allocate (child :: b1)

    if (same_type_as (b1, null(b1))) error stop 1_4

    if (.not. same_type_as (base(10), null(b1))) error stop 2_4

    end