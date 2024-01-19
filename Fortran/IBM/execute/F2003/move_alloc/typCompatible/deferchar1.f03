! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of deferred character type
!*                               TO is of unlimit poly
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


      character(:), allocatable :: ch1
      class(*), allocatable :: ch2

      allocate(character(8) :: ch2)

      allocate(ch1, source = 'get the length of char')
      call move_alloc(ch1, ch2)

      if ( .not. allocated(ch2)) error stop 11
      if ( allocated(ch1) ) error stop 13

      select type (ch2)
         type is (character(*) )
            if ( len(ch2) /= len('get the length of char')  ) error stop 21
            if ( ch2 /= 'get the length of char' ) error stop 23
         class default
            stop 33
     end select

      end
