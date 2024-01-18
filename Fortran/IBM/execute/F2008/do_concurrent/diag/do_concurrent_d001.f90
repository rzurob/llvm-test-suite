!*******************************************************************************
!*
!============================================================================
!*
!============================================================================
!*
!*  TEST CASE NAME             : do_concurrent_d001.f
!*
!*  DATE                       : 2015-03-20
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION                : Pass an impure procedure as scalar_mask_expr
!*                               - defined as impure without keyword
!*                               - defined as impure with keyword
!*
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

      integer lxmax, lymax; parameter (lxmax = 2222, lymax = 2222)
      dimension co(0:lxmax, 0:lymax), o(0:lxmax, 0:lymax)
      integer*4 total_step, it, ioutpt, iout, total_co_number, rc
      common co, o; common /z_ks/ k1, k2, r
      integer j1, i1

  ! implicit typing for index variables
      DO CONCURRENT (j = 0:ly, i = 0:lx/8-1, p(j,i))
          co(i, j) = B'00111111'; o(i, j) = 0
      END DO

      DO CONCURRENT (j = 0:ly, i = 0:lx/8-1, q(j,i))
          co(i, j) = B'00111111'; o(i, j) = 0
      END DO

    ! nested case
      DO CONCURRENT (j = 0:ly)
        DO CONCURRENT (i = 0:lx/8-1, p(j,i))
          co(i, j) = B'00111111'; o(i, j) = 0
        END DO
      END DO

      DO CONCURRENT (j = 0:ly)
        DO CONCURRENT (i = 0:lx/8-1, q(j,i))
          co(i, j) = B'00111111'; o(i, j) = 0
        END DO
      END DO

  ! explicit typing for index variables from host scope
      DO CONCURRENT (j1 = 0:ly, i1 = 0:lx/8-1, p(j1,i1))
          co(i1, j1) = B'00111111'; o(i1, j1) = 0
      END DO

      DO CONCURRENT (j1 = 0:ly, i1 = 0:lx/8-1, q(j1,i1))
          co(i1, j1) = B'00111111'; o(i1, j1) = 0
      END DO

    ! nested case
      DO CONCURRENT (j1 = 0:ly)
        DO CONCURRENT (i1 = 0:lx/8-1, p(j1,i1))
          co(i1, j1) = B'00111111'; o(i1, j1) = 0
        END DO
      END DO

      DO CONCURRENT (j1 = 0:ly)
        DO CONCURRENT (i1 = 0:lx/8-1, q(j1,i1))
          co(i1, j1) = B'00111111'; o(i1, j1) = 0
        END DO
      END DO

    ! nested case with implicit index variable
      DO CONCURRENT (j1 = 0:ly)
        DO CONCURRENT (i = 0:lx/8-1, p(j1,i))
          co(i, j1) = B'00111111'; o(i, j1) = 0
        END DO
      END DO

      DO CONCURRENT (j1 = 0:ly)
        DO CONCURRENT (i = 0:lx/8-1, q(j1,i))
          co(i, j1) = B'00111111'; o(i, j1) = 0
        END DO
      END DO

  ! explicit typing for index variables from do concurrent construct
      DO CONCURRENT (integer :: j = 0:ly, k = 0:lx/8-1, p(k,i))
          co(i, k) = B'00111111'; o(i, k) = 0
      END DO

      DO CONCURRENT (integer :: j = 0:ly, k = 0:lx/8-1, q(k,i))
          co(i, k) = B'00111111'; o(i, k) = 0
      END DO

    ! nested case
      DO CONCURRENT (integer :: j = 0:ly)
        DO CONCURRENT (integer :: i = 0:lx/8-1, p(j,i))
          co(i, j) = B'00111111'; o(i, j) = 0
        END DO
      END DO

      DO CONCURRENT (integer :: j = 0:ly)
        DO CONCURRENT (integer :: i = 0:lx/8-1, q(j,i))
          co(i, j) = B'00111111'; o(i, j) = 0
        END DO
      END DO

    ! nested case with implicit index var
      DO CONCURRENT (integer :: j = 0:ly)
        DO CONCURRENT (i = 0:lx/8-1, p(j,i))
          co(i, j) = B'00111111'; o(i, j) = 0
        END DO
      END DO

      DO CONCURRENT (integer :: j = 0:ly)
        DO CONCURRENT (i = 0:lx/8-1, q(j,i))
          co(i, j) = B'00111111'; o(i, j) = 0
        END DO
      END DO

    end

  ! impure functions
      integer function p(i, j)
        integer, intent(in) :: i, j
        print *, "in procedure p"
        if (i+j .gt. 0) then
          p = true
        end if
      end

      integer impure function q(i, j)
        integer, intent(in) :: i, j
        print *, "in procedure q"
        if (i+j .gt. 0) then
          q = true
        end if
      end

