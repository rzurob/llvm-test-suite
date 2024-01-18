! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of type  logical
!*                               TO is of type integer
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

            logical, allocatable :: l1
            integer, allocatable :: i1

            allocate(i1, source = 10 )
            allocate(l1, source = .true.)


            call move_alloc(l1, i1)

            if ( .not. allocated(i1) ) stop 21
            if ( allocated(l1) ) stop 31
            if ( i1 /= 1 ) stop 41
            end

      end
