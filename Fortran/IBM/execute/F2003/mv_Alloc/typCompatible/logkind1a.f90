! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of logical*1
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

          logical*1, allocatable :: l2
          logical*1, allocatable :: l1

          allocate(l2, source = logical(lgt('max', 'min') , 1))

          call move_alloc(l2, l1)

          if ( allocated(l2) ) stop 21
          if ( .not. allocated(l1) ) stop 22
          if ( l1 .neqv. .false. ) stop 23

      end

