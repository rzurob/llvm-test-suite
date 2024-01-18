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
    TYPE :: DT
      CHARACTER      :: C      = "1"
      INTEGER        :: IOSTAT = 1
      CHARACTER(513) :: IOMSG  = ""
    END TYPE

  END MODULE

  PROGRAM IoMsg

  USE M
  IMPLICIT TYPE(DT)(A)
  DIMENSION :: Arr(2:130)
  INTEGER :: i
  CHARACTER(3) :: C


  ASSOCIATE ( As => Arr )
    DO i=2, 129
      WRITE(As(i)%C, FMT=*, IOSTAT=As(i)%IOSTAT, IOMSG=As(i)%IOMSG) "!"
      IF ( As(i)%C           .NE. " " ) STOP 20
      IF ( As(i)%IOSTAT      .EQ. 0   ) STOP 21
      IF ( TRIM(As(i)%IOMSG) .EQ. ""  ) STOP 22
    END DO
  END ASSOCIATE

  END

