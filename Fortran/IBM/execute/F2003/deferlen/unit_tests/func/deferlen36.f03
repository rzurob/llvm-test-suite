!*  ===================================================================
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

      character(:), allocatable :: char(:, :)
      allocate (character(10)::char(4, 5))

      char = 'student'
      char(2,4) = 'professor'
      char(4,5) = 'soldier'

      associate (item => char )
         if (item(1,2) /= 'student') error stop 1
         if (item(2,4) /= 'profess') error stop 2
         if (item(4,5) /= 'soldier')   error stop 3

         item(1,2) = 'enginerr'
         if (item(1,2) /= 'enginer') error stop 4

         item(2,4) = 'soldier'
         if(char(2,4) /= 'soldier') error stop 5
      end associate

      deallocate (char)
      end

