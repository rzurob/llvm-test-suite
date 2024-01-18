!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxstio114.sh 
! %COMPOPTS: 
! %GROUP:  redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : I/O Stream Access Mode
!*
!*  PROGRAMMER                 : Bahram Chehrazy
!*  DATE                       : March 2003
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*
!*  PRIMARY FUNCTIONS TESTED   : OPEN, WRITE, READ
!*
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test formatted Synchronous Stream I/O in
!*                               OpenMP parallel region.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments: 
!*  03/16/03   BC     Initial version 
!* 
!234567890123456789012345678901234567890123456789012345678901234567890 

  include 'check_array.inc'

  program fxstio114 

     implicit none
     integer    i, j, k, l, ios
     integer, parameter :: N = 10
     integer :: idnum1(N) /N*0/
     integer :: idnum2(N) /N*0/
     integer :: idnum3(N) /N*0/
     integer*4 	i4_in(N,N), i4_out(N,N)
     real*4    	r4_in(N,N), r4_out(N,N)
     complex*8 	x8_in(N,N), x8_out(N,N)

     include 'check_interface.inc'
     
!********************************************************** 
!        Initialization of arrays                         *
!********************************************************** 

     do i = 1, N
        do j = 1, N
           i4_in(i,j) = i+j
           r4_in(i,j) = SIN((i+j)*0.1)
           x8_in(i,j) = (SIN((i+j)*0.1), COS((i+j)*0.1))
        enddo 
     enddo 


!********************************************************** 
!       Writing and Reading the file                      *
!********************************************************** 

     do i=1,N
        OPEN(i+10, FORM='FORMATTED', ACCESS='STREAM', &
       &     STATUS='REPLACE', IOSTAT=ios, ERR=90)
     enddo

!$OMP  PARALLEL private(i, ios) 
!$OMP  DO
     do 20 i = 1,N
         WRITE(i+10, FMT='(10I10)', IOSTAT=ios, ERR=91) i4_in(i,:)
         WRITE(i+10, FMT='(10F9.6)', IOSTAT=ios, ERR=91) r4_in(i,:)
         WRITE(i+10, FMT='(20E15.7)', IOSTAT=ios, ERR=91) x8_in(i,:)
         goto 20
91       print *, "Error while writing to the file: IOSTAT = ", ios
         error stop 91 
20     enddo
!$OMP  END DO
!$OMP  END PARALLEL 

      do i=1,10
          REWIND(i+10, IOSTAT=ios, ERR=93)
      enddo 

!$OMP  PARALLEL DO private(i, ios) 
     do 30 i = 1,N
         READ(i+10, FMT='(10I10)', IOSTAT=ios, ERR=92) i4_out(i,:)
         READ(i+10, FMT='(10F9.6)', IOSTAT=ios, ERR=92) r4_out(i,:)
         READ(i+10, FMT='(20E15.7)', IOSTAT=ios, ERR=92) x8_out(i,:)
         goto 30
92       print *, "Error while reading from the file: IOSTAT = ", ios
         error stop 92 
30   enddo
!$OMP  END PARALLEL DO 


!********************************************************** 
!        Checking the Results                             *
!********************************************************** 

     if ( .not. Array_Check (i4_in, i4_out)  ) error stop 10
     if ( .not. Array_Check (r4_in, r4_out)  ) error stop 11
     if ( .not. Array_Check (x8_in, x8_out)  ) error stop 13

     do i=1,10
        CLOSE(i+10, STATUS='DELETE')
     enddo 

!**********************************************************
!  Several threads write into a file at the same time     *
!********************************************************** 

     call write_parallel(i4_in, r4_in, x8_in, N)

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90 
93   print *, "Error while rewinding the file: IOSTAT = ", ios
     error stop 93 

     contains
        subroutine write_parallel(I4, R4, X8, N)
           integer N
           integer, intent(in)    :: I4(N,N)
           real, intent(in)       :: R4(N,N)
           complex, intent(inout) :: X8(N,N)

           OPEN(1, FILE='fxstio114.dat',FORM='FORMATTED',ACCESS='STREAM', &
    &           STATUS='REPLACE', IOSTAT=ios, ERR=95, ACTION='WRITE')
         
!$OMP  PARALLEL DO private(i, ios)
           do 40 i = 1,N
              WRITE(1, FMT='(10I10)', IOSTAT=ios, ERR=96, POS=101*(i-1)+1) I4(i,:)
              WRITE(1, FMT='(10F9.6)', IOSTAT=ios, ERR=96, POS=91*(i-1)+1011) R4(i,:)
              WRITE(1, FMT='(20E15.7)', IOSTAT=ios, ERR=96, POS=301*(i-1)+1921) X8(i,:)

              goto 40
96            print *, "Error while writing to the file: IOSTAT = ", ios
              error stop 96
40          enddo
!$OMP  END PARALLEL DO

           CLOSE(1)

           return  
95         print *, "Error while openning the file: IOSTAT = ", ios
           error stop 95 
        end subroutine
   end program

