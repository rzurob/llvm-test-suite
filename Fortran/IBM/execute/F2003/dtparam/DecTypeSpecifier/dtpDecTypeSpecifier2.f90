!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpDecTypeSpecifier2
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 16, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration 
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   
!*  The explicit type declaration may override or confirm the implicit type 
!*  that could otherwise be indicated by the first letter of an entity name
!*  
!*  - Override
!*
!*  (ICE) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  IMPLICIT TYPE(DT(1,2)) (A-B)
  IMPLICIT CLASS(DT(8,3))(C-D)
  IMPLICIT CLASS(*)      (E-F)


  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    INTEGER(K)    :: I=K
    CHARACTER(L)  :: C(L)
  END TYPE

  TYPE, EXTENDS(DT) :: DT1(K1,L1)
    INTEGER, KIND :: K1=K
    INTEGER, LEN  :: L1=K
  END TYPE

  SAVE :: B
  TYPE(DT(2,1)) :: B = DT(2,1)(C="X", I=-1)

  TYPE(DT1(4,2, K1=2, L1=2)), TARGET :: D = DT1(4,2, K1=2, L1=2)(-1, "XY")
  POINTER C
  CLASS(DT(4,2)) :: C

  ALLOCATABLE E
  class(DT(4,2)) :: E 

  END MODULE

  PROGRAM dtpDecTypeSpecifier2
  USE M


  IF ( B%K               .NE.   2          ) STOP 11
  IF ( B%L               .NE.   1          ) STOP 12
  IF ( B%I%KIND          .NE.   2          ) STOP 13
  IF ( B%I               .NE.  -1          ) STOP 14
  IF ( B%C%LEN           .NE.   1          ) STOP 15
  IF ( SIZE(B%C)         .NE.   1          ) STOP 16
  IF ( ANY(B%C           .NE.   "X"      ) ) STOP 17

  C => D

  SELECT TYPE (C)
  TYPE IS (DT1(4,*, K1=2, L1=*))

    IF ( C%K             .NE.   4          ) STOP 21
    IF ( C%L             .NE.   2          ) STOP 22
    IF ( C%K1            .NE.   2          ) STOP 23
    IF ( C%L1            .NE.   2          ) STOP 24
    IF ( C%I%KIND        .NE.   4          ) STOP 25
    IF ( C%I             .NE.  -1          ) STOP 26
    IF ( C%C%LEN         .NE.   2          ) STOP 27
    IF ( SIZE(C%C)       .NE.   2          ) STOP 28
    IF ( ANY(C%C         .NE.   "XY"     ) ) STOP 29

  CLASS DEFAULT
    STOP 20
  END SELECT

  ALLOCATE(E, SOURCE=DT1(4,2, K1=2, L1=1)(-1, "XY"))

  SELECT TYPE (E)
  TYPE IS (DT1(4,*, 2, *))

    IF ( E%K             .NE.   4          ) STOP 31
    IF ( E%L             .NE.   2          ) STOP 32
    IF ( E%K1            .NE.   2          ) STOP 33
    IF ( E%L1            .NE.   1          ) STOP 34
    IF ( E%I%KIND        .NE.   4          ) STOP 35
    IF ( E%I             .NE.  -1          ) STOP 36
    IF ( E%C%LEN         .NE.   2          ) STOP 37
    IF ( SIZE(E%C)       .NE.   2          ) STOP 38
    IF ( ANY(E%C         .NE.   "XY"     ) ) STOP 39

  CLASS DEFAULT
    STOP 30
  END SELECT

  END

