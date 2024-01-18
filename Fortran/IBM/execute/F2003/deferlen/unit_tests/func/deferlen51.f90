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
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the SELECT type with character 
!*                               variables with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
     
      implicit none

      character(:), pointer :: pchar
      character(4), target  :: tchar
      class(*), pointer     :: uptr
    
      tchar = 'abcd'
      pchar => tchar
      uptr  => pchar

      select type (ulocalp => uptr)
      type is(character(*))
         if (ulocalp /= 'abcd') error stop 1
      class default
         error stop 2
      end select
      
      end
