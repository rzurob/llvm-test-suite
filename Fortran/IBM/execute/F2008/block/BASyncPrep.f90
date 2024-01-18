!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : BASync
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2010-12-30
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : var declared in containing scope marked as async in block
!*
!*  DESCRIPTION
!*
!*  Create the data file to be read in by the main test program, BASync.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BASyncPrep

  implicit none
  integer, parameter :: DATA_WIDTH = 4
  integer, parameter :: ASIZE = 1000
  integer, parameter :: OUT_UNIT = 10
  integer(DATA_WIDTH) :: d1obvious(ASIZE), i0obvious, i1obvious, i

  open(OUT_UNIT, file="BASync.in", status="UNKNOWN", access="DIRECT", recl=(ASIZE+1)*DATA_WIDTH)
  i0obvious = 101
  i1obvious = 666 ! the devil is in the details!
  d1obvious = [(i0obvious+i,i=1,ASIZE/2), (i1obvious+i,i=1,ASIZE/2)]
  write(OUT_UNIT, rec=1) i0obvious, d1obvious(1:ASIZE/2)
  write(OUT_UNIT, rec=2) i1obvious, d1obvious(ASIZE/2+1:ASIZE)
  i0obvious = 1234567
  d1obvious(1:ASIZE/2) = [(i0obvious+i,i=1,ASIZE/2)]
  write(OUT_UNIT, rec=3) i0obvious, d1obvious(1:ASIZE/2)
  close(unit=OUT_UNIT)

end program BASyncPrep
