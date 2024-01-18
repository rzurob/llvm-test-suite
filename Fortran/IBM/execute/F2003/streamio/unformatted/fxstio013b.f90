!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 2003
!*
!*  PRIMARY FUNCTIONS TESTED   : OPEN, WRITE, READ
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test unformatted Asynchronous stream I/O.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  04/04/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  include 'check_array.inc'

  program fxstio013b

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
!   Writing Asynchronously and and Reading Synchronously  *
!**********************************************************

     OPEN(1, FORM='UNFORMATTED', ACCESS='STREAM', &
   &     ASYNCH='YES', STATUS='REPLACE', IOSTAT=ios, ERR=90)

     do i = 1,N
         WRITE(1, ID=idnum1(i), IOSTAT=ios, ERR=91, POS=300*(i-1)+1) i4_in(i,:)
         WRITE(1, ID=idnum2(i), IOSTAT=ios, ERR=91, POS=300*(i-1)+101) r4_in(i,:)
         WRITE(1, ID=idnum3(i), IOSTAT=ios, ERR=91, POS=300*(i-1)+201) x8_in(i,:)
     enddo

      do i=1,10
         WAIT(ID=idnum1(i))
         WAIT(ID=idnum2(i))
         WAIT(ID=idnum3(i))
      enddo

     CLOSE(1)

     OPEN(1, FORM='UNFORMATTED', ACCESS='STREAM', &
   &     ASYNCH='NO', STATUS='OLD', IOSTAT=ios, ERR=90)

     do i = 1,N
         READ(1, IOSTAT=ios, ERR=92, POS=300*(i-1)+1) i4_out(i,:)
         READ(1, IOSTAT=ios, ERR=92, POS=300*(i-1)+101) r4_out(i,:)
         READ(1, IOSTAT=ios, ERR=92, POS=300*(i-1)+201) x8_out(i,:)
     enddo

     if ( .not. Array_Check (i4_in, i4_out)  ) error stop 20
     if ( .not. Array_Check (r4_in, r4_out)  ) error stop 21
     if ( .not. Array_Check (x8_in, x8_out)  ) error stop 23

     CLOSE(1, STATUS='DELETE')

!**********************************************************
!   Writing Synchronously and and Reading Asynchronously  *
!**********************************************************

     OPEN(1, FORM='UNFORMATTED', ACCESS='STREAM', &
   &     ASYNCH='NO', STATUS='REPLACE', IOSTAT=ios, ERR=90)

     do i = 1,N
         WRITE(1, IOSTAT=ios, ERR=91, POS=300*(i-1)+1) i4_in(i,:)
         WRITE(1, IOSTAT=ios, ERR=91, POS=300*(i-1)+101) r4_in(i,:)
         WRITE(1, IOSTAT=ios, ERR=91, POS=300*(i-1)+201) x8_in(i,:)
     enddo

     CLOSE(1)

     OPEN(1, FORM='UNFORMATTED', ACCESS='STREAM', &
   &     ASYNCH='yes', STATUS='OLD', IOSTAT=ios, ERR=90)

     do i = 1,N
         READ(1, ID=idnum1(i), IOSTAT=ios, ERR=92, POS=300*(i-1)+1) i4_out(i,:)
         READ(1, ID=idnum2(i), IOSTAT=ios, ERR=92, POS=300*(i-1)+101) r4_out(i,:)
         READ(1, ID=idnum3(i), IOSTAT=ios, ERR=92, POS=300*(i-1)+201) x8_out(i,:)
     enddo

      do i=1,10
         WAIT(ID=idnum1(i))
         WAIT(ID=idnum2(i))
         WAIT(ID=idnum3(i))
      enddo

     if ( .not. Array_Check (i4_in, i4_out)  ) error stop 30
     if ( .not. Array_Check (r4_in, r4_out)  ) error stop 31
     if ( .not. Array_Check (x8_in, x8_out)  ) error stop 33

     CLOSE(1, STATUS='DELETE')

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91
92   print *, "Error while reading from the file: IOSTAT = ", ios
     error stop 92

   end program

