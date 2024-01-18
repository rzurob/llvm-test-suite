

!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             :/cintrop_ts29113/asynch_communication/asynchcomm009f.f
!* FEATURE NAME                : C_Interop_Asynch_Communication 
!*  PROGRAMMER                 : Tapti Vaid
!*  DATE                       : 2013-10-07
!*
!*  DESCRIPTION
!*
!* Checks the functionalilty of C_interop ASYNCHRONOUS Communication when a pointer to an asynchronous variable is sent over the MPI communication.
!* 
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


program asynch

USE, INTRINSIC :: ISO_FORTRAN_ENV
implicit none
LOGICAL precision_r4
include 'mpif.h'
real, pointer :: y
ASYNCHRONOUS :: y
real :: a0, b1,  res1=0, result1=0
real, target ::b0 
integer :: nt, rank, len, mpierror, rc, i
integer :: status(MPI_STATUS_SIZE)
integer, parameter :: TAG_SEND_ARR = 10, TAG_RES_READY = 11
integer :: reqs(2) 
y => b0


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


b0 = 0

if (rank .eq. 0) then
b0 = 50
a0 = 100
	block 

	ASYNCHRONOUS :: b0
	call MPI_ISEND(y , 1 , MPI_REAL, 1, TAG_SEND_ARR, MPI_COMM_WORLD, reqs(1), mpierror)
	! While waiting for the data to be sent, do some calculations:
	result1 = sqrt(a0)
    call MPI_WAIT(reqs(1), status, mpierror)
	b0 = a0*2 !Now that a0 has been sent we can alter its value
	
end block
!get the result from task 1

call MPI_RECV(b0, 1, MPI_REAL, 1, TAG_RES_READY, MPI_COMM_WORLD, status, mpierror)
if (.not. precision_r4 (result1, 10.00000000)) error STOP 1
if (.not. precision_r4 (b0 , 2500.000000)) error STOP 2

else !(if task# =1)
	block

	ASYNCHRONOUS :: b1
	call MPI_IRECV(b1, 1 , MPI_REAL, 0, TAG_SEND_ARR, MPI_COMM_WORLD, reqs(2), mpierror)
	call MPI_WAIT(reqs(2), status, mpierror)
	if (.not. precision_r4 (b1 ,50.00000000)) error STOP 3
	! Now that we received our data, calculate the sum and send the result to task 0.
	res1 = b1*b1
    call MPI_SEND(res1, 1, MPI_REAL, 0, TAG_RES_READY, MPI_COMM_WORLD, mpierror)

	end block
end if

call MPI_FINALIZE(mpierror)
end