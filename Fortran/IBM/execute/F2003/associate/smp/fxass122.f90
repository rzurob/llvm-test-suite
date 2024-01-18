! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qsmp -F:xlf90_r -qfree=f90
! %GROUP: fxass122.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!* ===================================================================
!*
!* DATE                         : Feb. 13, 2003
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    : -qsmp
!*
!* DESCRIPTION                  : Test: ASSOCIATE with Parallel
!*                                Do Private OMP clauses in a
!*                                Internal Fortran Subroutine. Using
!*                                Derived type containing
!*                                integer, character data types.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/10/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

   program fxass122

    implicit none
    integer :: io2 = 3
    type d_type
       sequence
       integer*8 int
       character*1 ch
    end type
    type(d_type) dt_der

    dt_der%int = 2
    dt_der%ch = 'a'

    call sub_associate_type(dt_der)

    call sub_associate_private(io2)

      contains

         subroutine sub_associate_type(dt_der)
           implicit none
           integer i
         type d_type
           sequence
           integer*8 int
           character*1 ch
         end type
           type(d_type) dt_der

!$omp parallel do private(dt_der)
          do i=1,5
               associate ( arg => dt_der%int )
               if (arg .ne. dt_der%int) error stop 1
               end associate

               associate ( arg1 => dt_der%int )
               arg1 = i
               if (arg1 .ne. dt_der%int) error stop 2
               end associate
          enddo
!$omp end parallel do

          dt_der%ch = 'b'
          if (dt_der%int /= 2) error stop 3
         end subroutine

         subroutine sub_associate_private(x)
          implicit none
          integer :: x,i,j

!$omp parallel do private(x)

          do i=1,2
             do j=1,2
               associate ( arg => x )
               if (arg .ne. x) error stop 4
               end associate

               associate ( arg1 => x)
               arg1 = i
               if (arg1 .ne. x) error stop 5
               end associate
             enddo
          enddo
!$omp end parallel do
         if (x /= 3) error stop 26
         end subroutine

    end program
