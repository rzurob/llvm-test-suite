! GB DTP extension using:
! ftcx_dtp /tstdev/OO_poly/associate/CrossFeatures/IoMsg.f
! opt variations: -qck

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 10, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The IO
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(N1,K1,N2)    ! (1,4,513)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1,N2
      CHARACTER(N1) :: C      = "1"
      INTEGER(K1)   :: IOSTAT = 1
      CHARACTER(N2) :: IOMSG  = ""
    END TYPE

  END MODULE

  PROGRAM IoMsg

  USE M
  IMPLICIT TYPE(DT(1,4,513))(A)
  DIMENSION :: Arr(2:130)
  INTEGER :: i
  CHARACTER(3) :: C


  ASSOCIATE ( As => Arr )
    DO i=2, 129
      WRITE(As(i)%C, FMT=*, IOSTAT=As(i)%IOSTAT, IOMSG=As(i)%IOMSG) "!"
      IF ( As(i)%C           .NE. " " ) ERROR STOP 20
      IF ( As(i)%IOSTAT      .EQ. 0   ) ERROR STOP 21
      IF ( TRIM(As(i)%IOMSG) .EQ. ""  ) ERROR STOP 22
    END DO
  END ASSOCIATE

  END
