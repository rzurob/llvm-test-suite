!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Deferred Character Length
!*
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  DRIVER STANZA              : xlf90/95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Testing the ASSOCIATE related 
!*                               with characters with deferred length
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
     
implicit none

      character(:), pointer     :: char1
      character(7), allocatable :: char2
      character*4, target       :: char3
      character*12              :: res
    
      char3 = "name"
      char1 => char3
      allocate(character(7)::char2)
      char2 = 'student'
      res = 'student name'
   
      associate (item => char2//' '//char1 )
         if(item .ne. res)then
            error stop 1
         endif
      end associate

      deallocate (char2)
      
      end
