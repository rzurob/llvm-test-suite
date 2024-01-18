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

      character(:),  allocatable :: char(:,:)
      character(:),  pointer     :: pchar(:,:)
      character(10), target      :: tchar(5,5)

      pchar => tchar
      allocate (char(5,5), source = tchar)

      tchar = 'student'
      tchar(2,4) = 'professor'
      tchar(4,5) = 'soldier'

      char = tchar

      associate (item => char(1,4)//pchar(4,5) )
         if (item /= 'student   soldier') error stop 1

         tchar(4,5) = 'engineer'
         if (item /= 'student   soldier') error stop 2

      end associate

      deallocate(char)
      end

