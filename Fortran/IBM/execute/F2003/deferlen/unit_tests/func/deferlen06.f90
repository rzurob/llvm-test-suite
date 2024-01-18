!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Testing the ASSOCIATE related
!*                               with characters with deferred length
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none

      character(:), pointer     :: char1
      character*7, target       :: char2

      char1 => char2
      char2 = 'student'

      associate (item => char1 )
         if(item .ne. 'student') then
            error stop 1
         endif

         if (len(item) .ne. 7) then
            error stop 2
         endif

        char1 = 'name'

         if(item .ne. 'name')then
            error stop 3
         endif

         if (len(item) .ne. 7) then
            error stop 4
         endif

      end associate

      end
