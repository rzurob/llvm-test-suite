! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : Data pointer assingment
!*  SECONDARY FUNCTIONS TESTED : The module will be compiled with XLF 13.1
!*
!*  DESCRIPTION                : - Target is a contiguous array section
!*
!*  A nonzero-sized array section is contiguous provided that
!*     (a) its base object is contiguous,
!*     (b) it does not have a vector subscript,
!*     (c) the elements of the section, in array element order, are a subset of the base object elements
!*         that are consecutive in array element order
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
MODULE Mod
      IMPLICIT NONE

      INTEGER, TARGET               :: I0D(0)
      INTEGER, TARGET               :: I1D(10)
      INTEGER, TARGET               :: I5D(2,2,2,2,2)

      INTEGER, POINTER              :: ptr(:)
      INTEGER, POINTER              :: ptr5D(:,:,:,:,:)

END MODULE
