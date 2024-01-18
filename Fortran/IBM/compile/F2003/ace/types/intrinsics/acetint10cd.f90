!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint10cd
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-08-24
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Intrinsic type specifier with incorrect KIND in assignment (CHARACTER)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : intrinsic type, incorrect kind, CHARACTER
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Verify that intrinsic type specifiers enforce correct KIND values.
!*  We look at the correctness of the value and that of the dynamic type and
!*  kind of the constructed array in a separate test.
!*  Here we verify that CHARACTER is correctly handled.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint10cd

  implicit none

  character :: charr(3)

  charr = (/character(kind=-1):: 'a', 'b', 'c' /)
  charr = (/character(kind=2):: 'aa','bb','cc'/)
  charr = (/character(kind='1'):: 1,2,3/)
  charr = (/character(kind=1.1):: 'aa','bb','cc'/)
  charr = (/character(kind=(1.1,2.2)):: 'aa','bb','cc'/)
  charr = (/character(kind=.true.):: 'aa','bb','cc'/)
  charr = (/character(kind=.false.):: 'aa','bb','cc'/)
  charr = (/character(kind=*):: 'aa','bb','cc'/)

  charr = (/character(len='1'):: 1,2,3/)
  charr = (/character(len=1.1):: 'aa','bb','cc'/)
  charr = (/character(len=(1.1,2.2)):: 'aa','bb','cc'/)
  charr = (/character(len=.true.):: 'aa','bb','cc'/)
  charr = (/character(len=.false.):: 'aa','bb','cc'/)
  charr = (/character(len=*):: 'aa','bb','cc'/)
  charr = (/character(len=:):: 'aa','bb','cc'/)

  charr = (/character('1'):: 1,2,3/)
  charr = (/character(1.1):: 'aa','bb','cc'/)
  charr = (/character((1.1,2.2)):: 'aa','bb','cc'/)
  charr = (/character(.true.):: 'aa','bb','cc'/)
  charr = (/character(.false.):: 'aa','bb','cc'/)
  charr = (/character(*):: 'aa','bb','cc'/)
  charr = (/character(:):: 'aa','bb','cc'/)

  charr = (/character(len=*,kind=1):: 'a', 'b', 'c' /)
  charr = (/character(len=:,kind=1):: 'a', 'b', 'c' /)
  charr = (/character(kind=1,len=*):: 'a', 'b', 'c' /)
  charr = (/character(kind=1,len=:):: 'a', 'b', 'c' /)

  charr = (/character(len=1,kind=2):: 'a', 'b', 'c' /)
  charr = (/character(kind=2,len=1):: 'a', 'b', 'c' /)

end program acetint10cd
