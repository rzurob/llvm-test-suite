!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the allocatable attributes on
!*                               characters with deferred length acting
!*                               as dummy argument
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
      character(:), allocatable ::char(:)
      call sub(char)
      deallocate(char)
      contains
      subroutine sub(char)
         implicit none
         character(:), allocatable :: char(:)
         character(10) :: result
         allocate (character(5)::char(8))
         char = 'ABCD'
         result = char(1)//char(8)
         if (result /= 'ABCDABCD') error stop 1
      end subroutine
      end

