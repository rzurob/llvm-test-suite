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
!*   - CYCLE appears inside a do concurrent construct belonging to an outer construct
!*
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

      integer lxmax, lymax; parameter (lxmax = 22222, lymax = 22222)
      dimension co(0:lxmax, 0:lymax), o(0:lxmax, 0:lymax)
      integer*4 total_step, it, ioutpt, iout, total_co_number, rc
      common co; common /z_ks/ k1, k2, r

      Mothership: DO CONCURRENT (j = 0:ly, i = 0:lx/8-1)
          co(i, j) = B'00111111'; o(i, j) = 0

          DO CONCURRENT (j2 = 0:ly, i2 = 0:lx/8-1)
              co(i, j) = B'00111111'; o(i, j) = 0
              CYCLE Mothership
          END DO
      END DO Mothership

      Mothership2: DO CONCURRENT (j = 0:ly, i = 0:lx/8-1)
          co(i, j) = B'00111111'; o(i, j) = 0

          DO CONCURRENT (j2 = 0:ly)
            DO CONCURRENT (i2 = 0:lx/8-1)
              co(i, j) = B'00111111'; o(i, j) = 0
              CYCLE Mothership2
            END DO
          END DO
      END DO Mothership2

      Mothership3: DO CONCURRENT (j = 0:ly, i = 0:lx/8-1)
          co(i, j) = B'00111111'; o(i, j) = 0

          DO CONCURRENT (j2 = 0:ly)
            DO CONCURRENT (i2 = 0:lx/8-1)
              co(i, j) = B'00111111'; o(i, j) = 0
            END DO
            CYCLE Mothership3
          END DO
      END DO Mothership3

      DO CONCURRENT (j = 0:ly, i = 0:lx/8-1)
          co(i, j) = B'00111111'; o(i, j) = 0

          insideLoop: DO CONCURRENT (j2 = 0:ly)
            DO CONCURRENT (i2 = 0:lx/8-1)
              co(i, j) = B'00111111'; o(i, j) = 0
              CYCLE insideLoop
            END DO
          END DO insideLoop
      END DO

      DO CONCURRENT (j2 = 0:ly, i2 = 0:lx/8-1)
          co(i, j) = B'00111111'; o(i, j) = 0
          CYCLE
      END DO

      end
