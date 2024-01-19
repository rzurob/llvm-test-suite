!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the allocatable attributes on
!*                               characters with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      call sub(5)
      contains
      subroutine sub(a)
         implicit none
         integer a
         character(:), allocatable :: char
         allocate (character(len=a)::char)
         char(:) = '12345678'
         associate (item => char)
            if (len(item) .ne. 5) error stop 1
            if (item /= '12345')  error stop 2
         end associate
      end subroutine
      end
