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
!*    - concurrent_limit or concurrent_step is a reference to an index name
!*
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

  ! declarations
      integer j1, i1

  ! implicit typing
      ! concurrent_limit is a reference to an index name
      DO CONCURRENT (j = 0:1, i = 0:j)
      END DO

      ! concurrent_step is a reference to an index name
      DO CONCURRENT (j = 0:1, i = 0:2:j)
      END DO

      ! nested case - concurrent_limit is a reference to an index name
      DO CONCURRENT (j = 0:3)
        DO CONCURRENT (i = 0:2:z)
        END DO
      END DO

      ! nested case - concurrent_step is a reference to an index name
      DO CONCURRENT (j = 0:3)
        DO CONCURRENT (i = 0:z:1)
        END DO
      END DO

  ! explicit typing - declared in host scope
      ! concurrent_limit is a reference to an index name
      DO CONCURRENT (j1 = 0:1, i1 = 0:j1)
      END DO

      ! concurrent_step is a reference to an index name
      DO CONCURRENT (j1 = 0:1, i1 = 0:2:j1)
      END DO

      ! nested case - concurrent_limit is a reference to an index name
      DO CONCURRENT (j1 = 0:3)
        DO CONCURRENT (i1 = 0:2:j1)
        END DO
      END DO

      ! nested case - concurrent_step is a reference to an index name
      DO CONCURRENT (j1 = 0:3)
        DO CONCURRENT (i1 = 0:j1:1)
        END DO
      END DO

  ! explicit typing - declared do concurrent construct
      ! concurrent_limit is a reference to an index name
      DO CONCURRENT (integer :: j = 0:1, i = 0:j)
      END DO

      ! concurrent_step is a reference to an index name
      DO CONCURRENT (integer :: j = 0:1, i = 0:2:j)
      END DO

      ! nested case - concurrent_step is a reference to an index name
      DO CONCURRENT (integer :: j = 0:3)
        DO CONCURRENT (integer :: i = 0:2:j)
        END DO
      END DO

      ! nested case - concurrent_limit is a reference to an index name
      DO CONCURRENT (integer :: j = 0:3)
        DO CONCURRENT (integer :: i = 0:j:1)
        END DO
      END DO

end
