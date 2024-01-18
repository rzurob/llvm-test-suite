! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qsmp -F:xlf90_r -qfree=f90
! %GROUP: fxass124.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
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
!* DESCRIPTION                  : Test: ASSOCIATE with Parrallel
!*                                FIRSTPRIVATE OMP clauses with real
!*                                and integer
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/10/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass124
      implicit none

      integer :: num
      integer :: int
      real :: rel
      logical :: precision_r4

      int = 5
      rel = 5.0
      num = 1

      call omp_set_num_threads(num)

!$omp parallel firstprivate(int)

      associate ( arg => int )
      if (arg .ne. int) error stop 1
      end associate

      associate ( arg1 => int + 1 )
      if (arg1 .ne. (int + 1)) error stop 2
      end associate

      associate ( arg3 => int )
      arg3 = arg3 + 1
      if (arg3 .ne. int) error stop 3
      end associate

!$omp end parallel

      if (int .ne. 5) error stop 4

!$omp parallel firstprivate(rel)

      associate ( arg => rel )
      if (.not.precision_r4(arg,rel)) error stop 5
      end associate

      associate ( arg1 => rel + 1.0 )
      if (.not.precision_r4(arg1,(rel+1.0))) error stop 6
      end associate

      associate ( arg3 => rel )
      arg3 = arg3 + 1.0
      if (.not.precision_r4(arg3,(rel))) error stop 7
      end associate

!$omp end parallel

      if (.not. precision_r4(rel,5.0)) error stop 8

      end program
