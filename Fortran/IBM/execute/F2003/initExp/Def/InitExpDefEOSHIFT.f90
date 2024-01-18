!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  a reference to an tranformational intrinsic
!*
!*  - EOSHIFT
!*  (318847/319511)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT
    CHARACTER  :: C=" "
    LOGICAL(2) :: L(3,3)=.FALSE.
    PROCEDURE(), POINTER, NOPASS :: ProcPtr=>NULL()
    CONTAINS
    PROCEDURE  :: Proc => ModSub
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg)
  CLASS(DT) :: Arg
  END SUBROUTINE

  END MODULE

  PROGRAM InitExpDefEOSHIFT
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(1),         PARAMETER :: V11(6)=(/(I, I=1,6)/)
  INTEGER(2),         PARAMETER :: V12(6)=(/(I, I=1,6)/)
  INTEGER(KIND(V11)), PARAMETER :: R11(6)=(/4,5,6,0,0,0/)
  INTEGER(KIND(R11)), PARAMETER :: R12(6)=(/99,99,1,2,3,4/)

  INTEGER(KIND(EOSHIFT(R12, -128))) :: T11(SIZE(V11))=EOSHIFT(V11, SHIFT=3 )
  INTEGER(KIND(EOSHIFT(V12, -128))) :: T12(SIZE(V12))=EOSHIFT(V12, SHIFT=-2, BOUNDARY=99_2)

  COMPLEX(8),         PARAMETER :: Z8(6)=(/((I, I), I=1,6)/)
  COMPLEX(16),        PARAMETER :: Z16(6)=(/((I,I), I=1,6)/)
  COMPLEX(KIND(Z8)),  PARAMETER :: RZ8(6)=(/(4,4),(5,5),(6,6),(0,0),(0,0),(0,0)/)
  COMPLEX(KIND(Z8)*2),PARAMETER :: RZ16(6)=(/(99,99),(99,99),(1,1),(2,2),(3,3),(4,4)/)

  COMPLEX(KIND(EOSHIFT(Z8, -128))) ::    &
      T21(SIZE(EOSHIFT(Z8, SHIFT=3 )))=EOSHIFT(Z8, SHIFT=3 )
  COMPLEX(KIND(EOSHIFT(Z16, -128))) ::    &
      T22(SIZE(EOSHIFT(Z16, SHIFT=-2, BOUNDARY=(99._16,99._16))))=EOSHIFT(Z16, SHIFT=-2, BOUNDARY=(99._16,99._16))

  CHARACTER,    PARAMETER :: C(3,3)=RESHAPE((/"A","D","G","B","E","H","C","F","I"/),(/3,3/))
  CHARACTER,    PARAMETER :: C1(3,3)=RESHAPE((/"*","*","*","A","D","G","B","E","H"/),(/3,3/))
  CHARACTER,    PARAMETER :: C2(3,3)=RESHAPE((/"*","E","G","A","F","H","B","/","I"/),(/3,3/))

  CHARACTER(KIND(EOSHIFT(C, -127))) ::                                 &
        T31(SIZE(EOSHIFT(C, SHIFT=-1, BOUNDARY="*", DIM=2 ), DIM=1),3)   &
        =EOSHIFT(C, SHIFT=-1, BOUNDARY="*", DIM=2 )
  CHARACTER(KIND(EOSHIFT(C, -129))) ::                                                  &
        T32(SIZE(EOSHIFT(C, SHIFT=(/-1,1,0/), BOUNDARY=(/"*","/","?"/), DIM=2), DIM=2),3) &
        =EOSHIFT(C, SHIFT=(/-1,1,0/), BOUNDARY=(/"*","/","?"/), DIM=2)

  TYPE(DT),    PARAMETER :: D(3,3)=RESHAPE((/      &
         DT("A"),DT("D"),DT("G"),DT("B"),DT("E"),DT("H"),DT("C"),DT("F"),DT("I")/),(/3,3/))
  TYPE(DT),    PARAMETER :: D1(3,3)=RESHAPE((/     &
         DT("*"),DT("*"),DT("*"),DT("A"),DT("D"),DT("G"),DT("B"),DT("E"),DT("H")/),(/3,3/))
  TYPE(DT),    PARAMETER :: D2(3,3)=RESHAPE((/     &
         DT("*"),DT("E"),DT("G"),DT("A"),DT("F"),DT("H"),DT("B"),DT("/"),DT("I")/),(/3,3/))

  TYPE(DT) :: T41(3,3)=EOSHIFT(D, SHIFT=-1, BOUNDARY=DT("*"), DIM=2 )
  TYPE(DT) :: T42(3,3)=EOSHIFT(D, SHIFT=(/-1,1,0/), BOUNDARY=(/DT("*"),DT("/"),DT("?")/), DIM=2)



  IF (KIND(T11)   .NE.   1 )        ERROR STOP 11
  IF (ANY( T11    .NE. R11 ))       ERROR STOP 12
  IF (KIND(T12)   .NE.   2 )        ERROR STOP 13
  IF (ANY( T12    .NE. R12 ))       ERROR STOP 14

  IF (KIND(T21)   .NE.   8 )        ERROR STOP 21
  IF (ANY( T21    .NE. RZ8 ))       ERROR STOP 22
  IF (KIND(T22)   .NE.  16 )        ERROR STOP 23
  IF (ANY( T22    .NE. RZ16))       ERROR STOP 24

  IF (KIND(T31)   .NE.   1 )        ERROR STOP 31
  IF (ANY( T31    .NE.  C1 ))       ERROR STOP 32
  IF (KIND(T32)   .NE.   1 )        ERROR STOP 33
  IF (ANY( T32    .NE.  C2 ))       ERROR STOP 34

  IF (ANY( T41%C  .NE.  D1%C ))     ERROR STOP 42
  IF (ANY( T42%C  .NE.  D2%C ))     ERROR STOP 44

  END



