!*******************************************************************************
!*
!============================================================================
!*
!============================================================================
!*
!*  DATE                       : 2015-03-20
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION                :
!*   - do concurrent loop inside a forall statement should be invalid
!*   - verify message consistency with having a do loop inside a forall
!*     statement
!*
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

      integer i, j

      FORALL(integer :: k=1:10:1)
        DO i=1,100
        END DO
      END FORALL

      FORALL(integer :: k=1:10:1)
        DO CONCURRENT (j = 0:100, i = 0:100)
        END DO
      END FORALL

      END
