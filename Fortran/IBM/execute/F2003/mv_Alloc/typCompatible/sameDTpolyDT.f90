! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TO is of poly DT same as FROM
!*                               FROM is a non-poly DT
!*                               component type is same as type defined
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      type A
          class(A), pointer :: self => null()
      end type

      class(A), allocatable, target ::  a1
      type(A), allocatable ::  a2

      allocate(a2, source = A(a1) )

      call move_alloc(a2, a1)

      if ( allocated(a2) ) error stop 21
      if ( .not. allocated(a1) ) error stop 23
      end
