! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of deferred character type
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


      character(:), allocatable :: ch1, ch2
      allocate(ch1, source = 'get the length of char')
      call move_alloc(ch1, ch2)

      if ( allocated(ch1)) stop 11
      if ( .not. allocated(ch2) ) stop 13

      if ( len(ch2) /= len('get the length of char')  ) stop 21
      if ( ch2 /= 'get the length of char' ) stop 23
      end
