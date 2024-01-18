! *********************************************************************
!* ===================================================================
!*
!* DATE                         : Feb. 13, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    : -qsmp
!*
!* DESCRIPTION                  : Test: ASSOCIATE with Parrallel Do
!*                                Do Reduction OMP clauses in a
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

   program fxass096

    implicit none

    integer :: io1 = 3
    type d_type
       sequence
       integer int
       character*1 ch
    end type
    type(d_type) dt_der

    dt_der%int = 1
    dt_der%ch = 'a'

    call sub_associate_type(dt_der)

    call sub_associate_reduction(io1)

      contains

!----------------------- sub_associate_type -------------

         subroutine sub_associate_type(dt_der)
           implicit none
           integer i,x

         type d_type
           sequence
           integer int
           character*1 ch
         end type
           type(d_type) dt_der

           x = dt_der%int

!---------------------------------------------------------------
!testing associate construct with parallel do using derive types
!---------------------------------------------------------------

!$omp parallel do
          do i=1,5
               associate ( arg1 => x )
                  if(arg1 .ne. x) error stop 1_4
               end associate

!$omp critical
               associate ( arg2 => x*2 )
                  if(arg2 .ne. (x*2)) error stop 2_4
               end associate
!$omp end critical

               associate ( arg3 => x )
!$omp critical
                   arg3 = arg3 * i
                  if(arg3 .ne. x) error stop 3_4
!$omp end critical
               end associate
           enddo
!$omp end parallel do
          if (x .ne. 120) error stop 4_4
         end subroutine

!------------------------- subroutine sub_associate_reduction  -------------

         subroutine sub_associate_reduction(x)
         implicit none

          integer i,j
          integer x

!-------------------------------------------------------------------------
! testing  associate construct in paralled do
!-------------------------------------------------------------------------

!$omp parallel do
          do i=1, 4

!$omp critical
           associate( arg1 => x + i )
            if (arg1 .ne. (x+i)) error stop 5_4
           end associate
!$omp end critical

           associate ( arg2 => x )
!$omp critical
            arg2 = arg2 + i
            if (arg2 .ne. x ) error stop 6_4
!$omp end critical
           end associate
          enddo

!$omp end parallel do
          if (x .ne. 13) error stop 7_4
         end subroutine

    end program
