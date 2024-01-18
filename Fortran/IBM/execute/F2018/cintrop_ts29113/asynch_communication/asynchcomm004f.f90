
!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             :/cintrop_ts29113/asynch_communication/asynchcomm004f.f
!* FEATURE NAME                : C_Interop_Asynch_Communication 
!*  PROGRAMMER                 : Tapti Vaid
!*  DATE                       : 2013-10-07
!*
!*  DESCRIPTION
!*
!* Checks the functionalilty of C_interop ASYNCHRONOUS Communication with an asynchronous variable of the attribute SAVE
!* 
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



module mymod

implicit none
include 'mpif.h'
real :: a0, res0=0, res1=0, result1=0
real, SAVE :: b0
 end module mymod

program asynch
use mymod
USE, INTRINSIC :: ISO_FORTRAN_ENV
implicit none
LOGICAL precision_r4

integer :: nt, rank, len, mpierror, rc, i
integer :: status(MPI_STATUS_SIZE)
integer, parameter :: TAG_SEND_ARR = 10, TAG_RES_READY = 11

integer :: reqs(2) 
!!real :: !! other varaibles that won't be used asynchronously but will be used somewhere else

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

! assuming there are 2 tasks in the MPI_COMM_WORLD; task 0 sends b to task 1 and calcuates the square root of a (after which it alters b)
! task 1 calculates the square of b and sends the result to task 0 
if (rank .eq. 0) then
b0 = 10
b0 = b0+1
a0 = 100
	block 
	
	asynchronous :: b0
	call MPI_ISEND(b0, 1 , MPI_REAL, 1, TAG_SEND_ARR, MPI_COMM_WORLD, reqs(1), mpierror)
    ! While waiting for the data to be sent, do some calculations:
    result1 = sqrt(a0)                  
    call MPI_WAIT(reqs(1), status, mpierror)
		  
	end block
!get the result from task 1

call MPI_RECV(res0, 1, MPI_REAL, 1, TAG_RES_READY, MPI_COMM_WORLD, status, mpierror)
IF (.not. precision_r4 (result1, 10.00000000))error STOP 1
IF (.not. precision_r4 (res0, 121.0000000)) error STOP 2
IF (.not. precision_r4 (b0 , 11.00000000)) error STOP 3

else !(if task# =1)
	block

	real, asynchronous :: b1
	call MPI_IRECV(b1, 1 , MPI_REAL, 0, TAG_SEND_ARR, MPI_COMM_WORLD, reqs(2), mpierror)
    call MPI_WAIT(reqs(2), status, mpierror)
    ! Now that we received our data, calculate the sum and send the result to task 0.
	res1 = b1*b1        
	call MPI_SEND(res1, 1, MPI_REAL, 0, TAG_RES_READY, MPI_COMM_WORLD, mpierror)

	end block
end if

call MPI_FINALIZE(mpierror)

end