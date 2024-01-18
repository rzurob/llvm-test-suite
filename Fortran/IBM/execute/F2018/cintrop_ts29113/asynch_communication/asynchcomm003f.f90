
!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             :/cintrop_ts29113/asynch_communication/asynchcomm003f.f
!* FEATURE NAME                : C_Interop_Asynch_Communication 
!*  PROGRAMMER                 : Tapti Vaid
!*  DATE                       : 2013-10-07
!*
!*  DESCRIPTION
!*
!* Checks if the value of the asynchronous variable is not overwritten after the I/O with a stale value due to optimization. 
!* This will be done by introducing a delay in one of the tasks after it receives an asynch variable and before it modifies it. 
!* The 2 values will be printed at a later time.
!* 
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module mymod

implicit none
include 'mpif.h'
real :: a0, result1
real :: b0
 
end module mymod

program asynch
use mymod
USE, INTRINSIC :: ISO_FORTRAN_ENV
implicit none
LOGICAL precision_r4

integer :: nt, rank, len, mpierror, rc, i
integer :: status(MPI_STATUS_SIZE)
integer, parameter :: TAG_SEND_ARR = 10, TAG_RES_READY = 11
real :: temp = 0
integer :: reqs(2) 


! initialization
       call MPI_INIT(mpierror)
       if (mpierror .ne. MPI_SUCCESS) then
          print *,'Error starting MPI program. Terminating.'
          call MPI_ABORT(MPI_COMM_WORLD, rc, mpierror)
       end if

       call MPI_COMM_RANK(MPI_COMM_WORLD, rank, mpierror)
       call MPI_COMM_SIZE(MPI_COMM_WORLD, nt, mpierror)

       if (nt .ne. 2) then
          print *, "This program requires exactly 2 tasks"
          call MPI_ABORT(MPI_COMM_WORLD, rc, mpierror)
       end if


 
if (rank .eq. 0) then
b0=400
a0 = 100
	block 
	
	asynchronous :: b0
	call MPI_ISEND(b0, 1 , MPI_REAL, 1, TAG_SEND_ARR, MPI_COMM_WORLD, reqs(1), mpierror)
	! While waiting for the data to be sent, do some calculations:
	result1 = sqrt(a0)
    call MPI_WAIT(reqs(1), status, mpierror)
	b0 = 100	
	temp = b0+ result1 !! should be 110
	end block
	
IF (.not. precision_r4 (temp, 110.0000000)) error STOP 1



call MPI_IRECV(b0, 1, MPI_REAL, 1, TAG_RES_READY, MPI_COMM_WORLD, reqs(1), mpierror)
call MPI_WAIT(reqs(1), status, mpierror)
temp = temp+b0 !! should be 110 + 20 = 130

IF (.not. precision_r4 (temp, 130.0000000))error STOP 2 
IF (.not. precision_r4 (b0, 20.0000000))error STOP 3 



else !(if task# =1)
	block
	
	real, asynchronous :: b1
	call MPI_IRECV(b1, 1 , MPI_REAL, 0, TAG_SEND_ARR, MPI_COMM_WORLD, reqs(2), mpierror)
    call MPI_WAIT(reqs(2), status, mpierror)
	IF (.not. precision_r4 (b1, 400.00000000))error STOP 4
    ! Now that we received our data, calculate the sum and send the result to task 0.
	call sleep_(5)


	b1  = sqrt(b1) 
    call MPI_SEND(b1, 1, MPI_REAL, 0, TAG_RES_READY, MPI_COMM_WORLD, mpierror)

    end block

end if
call MPI_FINALIZE(mpierror)
end
