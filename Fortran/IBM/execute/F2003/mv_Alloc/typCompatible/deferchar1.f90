! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : deferchar1.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
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

      if ( .not. allocated(ch2)) stop 11
      if ( allocated(ch1) ) stop 13

      select type (ch2)
         type is (character(*) )
            if ( len(ch2) /= len('get the length of char')  ) stop 21
            if ( ch2 /= 'get the length of char' ) stop 23
         class default
            stop 33
     end select

      end
