! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/selectType/Quotes/TypeMatch6.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 24, 2005
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
!*  No matching clauses
!*
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id=1
    END TYPE

    TYPE, EXTENDS(Zero)  :: One    ! (20,4)
    END TYPE

    TYPE, EXTENDS(One) :: Two    ! (20,4)
    END TYPE

    TYPE, EXTENDS(Two) :: Three    ! (20,4)
    END TYPE

  END MODULE

  PROGRAM TypeMatch4
  USE M,  Two=>One , DT=>Two
  IMPLICIT NONE
  CLASS(*), ALLOCATABLE :: U(:,:)

    ALLOCATE(DT(20,4) :: U(2:3,3:4) )

    SELECT TYPE (One=>U)
    TYPE IS (Zero(*,4))
      STOP 40
    TYPE IS (Three(*,4))
      STOP 41
    TYpe IS (Two(*,4))
      STOP 43
    CLASS IS (Two(*,4))
!     Print*, "ok!"
      STOP 0
    CLASS DEFAULT
      STOP 44
    END SELECT

  END


