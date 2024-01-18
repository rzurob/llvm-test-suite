! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of logical*8
!*                               use implicit to specify logical*8
!*                               @process intsize(8)
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      @process intsize(8)

      implicit logical ( k-l )

      allocatable  k1
      logical*8, allocatable :: l1

      allocate(k1, source = logical( .TrUE. .or. .False., 8) )

      call move_alloc(k1, l1)

      if ( allocated(k1) ) stop 21
      if ( .not. allocated(l1) ) stop 22
      if ( l1 .neqv. .true. ) stop 31

      end
