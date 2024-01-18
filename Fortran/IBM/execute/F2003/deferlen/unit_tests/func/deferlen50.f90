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
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the character variables with
!*                               deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
      character*20, target :: ch_const='          ABCDEFGHIJ'
      character(:) :: ch_var
      allocatable  :: ch_var
      character(:) :: pchar
      pointer      :: pchar

      allocate (character(10)::ch_var)
      pchar => ch_const

      ch_var = '1234567890'

      if (pchar /= '          ABCDEFGHIJ') error stop 1
      if (ch_var /= '1234567890') error stop 2 
      deallocate(ch_var)

      end
