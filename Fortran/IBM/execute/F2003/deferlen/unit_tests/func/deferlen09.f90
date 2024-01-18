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

      character(:), allocatable :: char
      allocate (character(10)::char)

      char(:) = 'student'

      associate (item => char )

         if (len(item) .ne. 10)  error stop 1
         if(item .ne. 'student') error stop 2

         char(:) = 'professor'
         if(item /= 'professor') error stop 3

         item = '1234567'
         if (char /= '1234567') error stop 4

      end associate

      deallocate (char)
      end
