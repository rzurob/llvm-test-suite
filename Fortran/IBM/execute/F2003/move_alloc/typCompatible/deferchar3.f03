! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of deferred character type
!*                               FROM is allocated by instrinisc assignment
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


  character(:), allocatable :: ch1, ch2
  ch1 =  'IBM' // 'Compiler'

  call move_alloc(ch1, ch2)

  if ( .not. allocated(ch2) ) error stop 21
  if ( allocated(ch1) ) error stop 31
  if ( ch2 /= 'IBMCompiler' ) error stop 41

  end
