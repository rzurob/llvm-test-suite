! *********************************************************************
!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD:  EXEC_REP=10
! %COMPOPTS: -qfixed -qsmp -F:xlf90_r
! %GROUP: fxass123.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Feb. 13, 2004
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    : -qsmp
!*
!* DESCRIPTION                  : Test: ASSOCIATE with Parallel
!*                                Do LastPrivate OMP
!*                                clauses in a External Fortran
!*                                Function using interface and intent(in)
!*                                containing real, integer, character
!*                                and allocatable data types.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/10/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass123
      implicit none

      interface
        real  function fun_associate(a, d)
        integer, intent(in) :: a
        real  :: d(:,:)
        end function fun_associate
      end interface

      integer  i, j, k
      real   x, d(10,10)
      logical precision_r4

      d = 0.0
      do i=1,10
        do j=1,10
           d(i,j) = d(i,j) + i
        enddo
      enddo

      x = 0.0

!$OMP parallel do lastprivate(i)

      do i=1, 10
!$omp critical
        associate ( arg => x )
        arg = arg + fun_associate(i, d)
        if (.not. precision_r4(arg,x)) error stop 1
        end associate
!$omp end critical
        associate ( arg1 => i + 4 )
        if (arg1 .ne. (i + 4)) error stop 2
        end associate
      enddo

        if (.not. precision_r4(x,1725.0)) error stop 3

      end program

      real  function fun_associate(n, d)
      implicit none
      integer, intent(in) :: n
      integer  i, j
      real , allocatable :: p(:,:)
      real  :: d(:,:), tmp1, tmp2
      allocate(p(n+1,n-1))

      p = 0.0
      tmp2 = 0.0
      tmp1 = 0.0
      do i=1, n-1
        do j=1, n-1
          tmp1 = tmp1 + d(i,j) + 1
          tmp2 = tmp2 + p(i,j) + 1
        enddo
      enddo
      fun_associate = tmp2 + tmp1

            deallocate(p)
      end function fun_associate


