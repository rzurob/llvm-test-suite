!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acesynt02d
!*
!*  DATE                       : 2006-07-04
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Use of square brackets replacing (//)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : syntax, square brackets, array constructor
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  "(/" and "/)" can be rewritten as "[" and "]" in F2003 array  constructors,
!*  but not in FORMAT statements.  Verify that this has not been overgeneralised.
!*
!*  The test is successful if the right syntax errors are produced.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

program acesynt04d
       integer i0, iarr(2)
       i0 = 88
       iarr = (/i0,10/)!       iarr = [i0,10]
       write(6,300)
       write(6,310) i0
       write(6,320) iarr
       write(6,330) iarr

300    format[]
310    format[i0]
320    format[(i0)]
330    format([(i0,i0=1,1)])

end program acesynt04d
