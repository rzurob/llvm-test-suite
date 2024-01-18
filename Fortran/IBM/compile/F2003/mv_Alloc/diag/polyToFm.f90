! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : If FROM is poly, TO must be poly.
!*                               But TO must be type compatible with FROM
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      type A
      end type

      type, extends(A) :: B
      end type

      class(B), allocatable :: TO
      class(A), allocatable :: FROM

      call move_alloc(FROM, TO )

      end
