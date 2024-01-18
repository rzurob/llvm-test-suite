!*******************************************************************************
!*
!============================================================================
!*  XL Fortran Test Case                                IBM INTERNAL USE ONLY
!*
!============================================================================
!*
!*  TEST CASE NAME             : do_concurrent_d010.f
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : 2015-03-20
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION                : 
!*   - An exit-statemt appears in a do concurrent construct
!*
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

      integer i, j

      DO CONCURRENT (j = 0:100, i = 0:100)
          exit
      END DO

      DO CONCURRENT (j = 0:100)
        DO CONCURRENT (i = 0:100)
          exit
        END DO
      END DO

      Mothership: DO CONCURRENT (j = 0:100)
        DO CONCURRENT (i = 0:100)
          exit
        END DO
      END DO Mothership

      Mothership2: DO CONCURRENT (j = 0:100)
        DO CONCURRENT (i = 0:100)
          exit mothership2
        END DO
      END DO Mothership2

      DO CONCURRENT (j = 0:100)
        DO CONCURRENT (i = 0:100)
        END DO
        exit
      END DO

      Mothership3: DO CONCURRENT (j = 0:100)
        DO i = 0,100
          exit mothership3
        END DO
      END DO Mothership3

      Mothership4: DO CONCURRENT (j = 0:100)
        FORALL (i = 0:100)
        END FORALL
        exit
      END DO Mothership4

      DO CONCURRENT (j = 0:100)
        DO i = 0,100
          exit !no message for this one
        END DO
        exit
      END DO


end
