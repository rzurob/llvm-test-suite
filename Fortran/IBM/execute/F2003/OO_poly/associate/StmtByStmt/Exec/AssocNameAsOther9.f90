! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 01, 2005
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
!*    The associate construct name is the same as a namelist name
!*   (Complain: i is poly and need dtio)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM AssocNameAsOther9
  CLASS(*), POINTER :: i, j, k
  NAMELIST /NL/i,j,k

  ALLOCATE(i, SOURCE=-1)
  ALLOCATE(j, SOURCE=-2)

  ASSOCIATE ( NL => (/i,j/) )
    SELECT TYPE ( NL)
    TYPE IS (INTEGER)
      SELECT TYPE (i)
      TYPE IS ( INTEGER )
        i = 1
      END SELECT
      SELECT TYPE (j)
      TYPE IS ( INTEGER )
        j = 1
      END SELECT
      PRINT *,  NL(1),  NL(2)
      IF ( NL(1) .NE. -1 ) STOP 11
      IF ( NL(2) .NE. -2 ) STOP 12
    END SELECT
  END ASSOCIATE


! SELECT TYPE (i)
! TYPE IS ( INTEGER )
!   SELECT TYPE (j)
!   TYPE IS ( INTEGER )
!      WRITE(*, NL)
!   END SELECT
! END SELECT

  END

