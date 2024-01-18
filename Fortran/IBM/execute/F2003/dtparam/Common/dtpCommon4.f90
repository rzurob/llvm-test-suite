!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 13, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration and specification
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  -- The common statement
!*
!*     A storage sequence is formed consisting of the sequence of storage units in the storage
!*     sequences (16.4.3.1) of all data objects in the common block object lists for the common
!*     block. The order of the storage sequences is the same as the order of the appearance of the
!*     common block object lists in the scoping unit.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT_I(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    CHARACTER(L)  :: C1
    INTEGER(K)    :: I(L)!=K
    CHARACTER(L)  :: C2
  END TYPE

  END MODULE

  PROGRAM dtpCommon4
  USE M
  IMPLICIT NONE

  TYPE(DT_I(2,7))  :: T1, T2, T3, T4, T5, T6, T7, T8, T9, T10
  COMMON /BLK/T1, T2, T3, T4, T5, T6, T7, T8, T9, T10

  T1%C1=CHAR(0)
  T1%C2=CHAR(0)
  T2%C1=CHAR(0)
  T2%C2=CHAR(0)
  T3%C1=CHAR(0)
  T3%C2=CHAR(0)
  T4%C1=CHAR(0)
  T4%C2=CHAR(0)
  T5%C1=CHAR(0)
  T5%C2=CHAR(0)
  T6%C1=CHAR(0)
  T6%C2=CHAR(0)
  T7%C1=CHAR(0)
  T7%C2=CHAR(0)
  T8%C1=CHAR(0)
  T8%C2=CHAR(0)
  T9%C1=CHAR(0)
  T9%C2=CHAR(0)
  T10%C1=CHAR(0)
  T10%C2=CHAR(0)

  T1%I  = 1
  T2%I  = 2
  T3%I  = 3
  T4%I  = 4
  T5%I  = 5
  T6%I  = 6
  T7%I  = 7
  T8%I  = 8
  T9%I  = 9
  T10%I = 10

  CALL ExtSub()

  END

  SUBROUTINE ExtSub()
  USE M

  TYPE(DT_I(2,7))  :: T
  COMMON /BLK/T(10)

  DO I=1, 10
    IF ( ANY( T(I)%I .NE. I ) ) ERROR STOP 11
  END DO

  END

