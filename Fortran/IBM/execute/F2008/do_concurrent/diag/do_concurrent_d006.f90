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
!*   - a RETURN statement shall not appear in a DO CONCURRENT construct
!*
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

      integer lxmax, lymax; parameter (lxmax = 22222, lymax = 22222)

      Mothership: DO CONCURRENT (j = 0:ly, i = 0:lx/8-1)
          RETURN
      END DO Mothership

      DO CONCURRENT (j = 0:ly, i = 0:lx/8-1)
          RETURN
      END DO

      Mothership2: DO CONCURRENT (j = 0:ly)
        abc: DO CONCURRENT (i = 0:lx/8-1)
          RETURN
        END DO abc
      END DO Mothership2

      Mothership3: DO CONCURRENT (j = 0:ly)
        abcd: DO CONCURRENT (i = 0:lx/8-1)
        END DO abcd
        RETURN
      END DO Mothership3

      DO CONCURRENT (j = 0:ly)
        DO CONCURRENT (i = 0:lx/8-1)
          RETURN
        END DO
      END DO

      DO CONCURRENT (j = 0:ly)
        DO CONCURRENT (i = 0:lx/8-1)
        END DO
        RETURN
      END DO

end
