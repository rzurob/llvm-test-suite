! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all /tstdev/OO_poly/associate/Misc/Misc12.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 02, 2004
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
!*  Label on named select type  construct
!* ( ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc12
  IMPLICIT NONE

  TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
  END TYPE

  CLASS(Base(4,20)), ALLOCATABLE :: V

  ALLOCATE(V)

8   A:SELECT TYPE ( V )
11     TYPE IS ( Base(4,*) )
12     CLASS DEFAULT
10       STOP 32
9   END SELECT A

  DEALLOCATE(V)

  END
