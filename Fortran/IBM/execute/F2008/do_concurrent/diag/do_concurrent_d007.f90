!*******************************************************************************
!*
!============================================================================
!*  XL Fortran Test Case                                IBM INTERNAL USE ONLY
!*
!============================================================================
!*
!*  TEST CASE NAME             : do_concurrent_d007.f
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
!*   - A branch in a do_concurrent construct branches outside the construct 
!*
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

      integer lxmax, lymax; parameter (lxmax = 22222, lymax = 22222)
      dimension co(0:lxmax, 0:lymax), o(0:lxmax, 0:lymax)
      integer i, j

      DO CONCURRENT (j = 0:100, i = 0:100)
          co(i, j) = B'00111111'; o(i, j) = 0
          go to 1004
1004      print *, "inside the loop still"
      END DO

      DO CONCURRENT (j = 0:100, i = 0:100)
          co(i, j) = B'00111111'; o(i, j) = 0
          go to 1005
      END DO

1005  print *, "outside the loop"

      ! nested case - branch from scope of inner loop
      DO CONCURRENT (j = 0:100)
        DO CONCURRENT (i = 0:100)
          co(i, j) = B'00111111'; o(i, j) = 0
          go to 1006
        END DO
      END DO

      ! nested case - branch from scope of outer loop
      DO CONCURRENT (j = 0:100)
        DO CONCURRENT (i = 0:100)
          co(i, j) = B'00111111'; o(i, j) = 0
        END DO
        go to 1006
      END DO

1006    print *, "outside of outer loop"

      ! nested case - branch from scope of inner loop to scope of outer loop RTC 118109
      DO CONCURRENT (j = 0:100)
        DO CONCURRENT (i = 0:100)
          co(i, j) = B'00111111'; o(i, j) = 0
          go to 1007
        END DO
1007    print *, "outside of inner loop"
      END DO

end
