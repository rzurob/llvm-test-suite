! GB DTP extension using:
! ftcx_dtp /tstdev/OO_poly/selectType/CrossFeatures/IO2.f
! opt variations: -qck

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 02, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!* IO
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM IO2
  IMPLICIT CLASS(DT(4,63))(U)
  IMPLICIT INTEGER(8)(V)

  TYPE :: DT(K1,N1)    ! (4,63)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: Int
    CHARACTER(N1) :: C
  END TYPE

  INTEGER(8) :: i
  TYPE(DT(4,63)) :: V(16) = (/(DT(4,63)(Int=6, C="123"), i=1,16)/)

  CALL Sub(V, i)

  CONTAINS

  SUBROUTINE Sub(U, V)
  DIMENSION :: U(:)
  CLASS(*) :: V

  SELECT TYPE (U)
  CLASS IS (DT(4,*))

    IF (ANY(U%Int   .NE. 6))       STOP 20
    IF (ANY(U%C     .NE. "123"))   STOP 21
    IF (ANY(SHAPE(U).NE. (/16/)))  STOP 22

    READ(U(1)%C, FMT=*, IOSTAT=U(1)%Int, IOMSG= U(2)%C) U(2)%Int
    IF (U(2)%Int .NE. 123 )     STOP 20
    IF (U(1)%Int .NE. 0 )       STOP 21
    IF (U(2)%C   .NE. "123" )   STOP 21

  CLASS DEFAULT
    STOP 40
  END SELECT

  SELECT TYPE (V)
  CLASS DEFAULT
    STOP 40
  TYPE IS (INTEGER(8))

    READ(U(1)%C, FMT=*, IOSTAT=V) U(2)%Int
    IF (U(2)%Int .NE. 123 ) STOP 30
    IF (V        .NE. 0 )   STOP 31

  END SELECT

  END SUBROUTINE

  END



