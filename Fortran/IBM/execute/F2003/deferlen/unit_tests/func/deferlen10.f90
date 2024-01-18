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

      character(:), pointer     :: char1
      character*7, target       :: char2

      char2 = "student"
      char1 => char2

      associate (item1 => char1)
         if (item1 /= 'student') error stop 1
      end associate

      associate (item1 => (char1))
         if (item1 /= 'student') error stop 2
      end associate

      associate (item1 => (char1//'name'))
         if (item1 /= 'studentname') error stop 3
      end associate

      associate (item1 => (char1//char2//char1))
         if (item1 /= 'studentstudentstudent') error stop 4
      end associate

      end
