
!*******************************************************************************
!*  ============================================================================
!*
!* FEATURE NAME                : C_Interop_Asynch_Communication
!*  DATE                       : 2013-10-07
!*
!*  DESCRIPTION
!*
!* Checks the functionalilty of C_interop ASYNCHRONOUS Communication under HOST association with the following scenario:
!*
!*Defined in: main
!*Made asynchronous in: main
!*Used for asynch communication in: internal subroutine
!*
!*The TC also checks when:
!*Defined in: main
!*Made asynchronous in: submodule (contained in main)
!*Used for asynch communication in: internal subroutine
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


program asynch
implicit none

include 'mpif.h'
real :: a0, res0=0, res1=0, result1=0
real :: b0, b1
asynchronous :: b0

call mpi_communication()

contains
subroutine mpi_communication()
integer :: nt, rank, len, mpierror, rc, i
integer :: status(MPI_STATUS_SIZE)
integer, parameter :: TAG_SEND_ARR = 10, TAG_RES_READY = 11

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

! assuming there are 2 tasks in the MPI_COMM_WORLD; task 0 sends b to task 1 and calcuates the square root of a (after which it alters b)
! task 1 calculates the square of b and sends the result to task 0
if (rank .eq. 0) then

a0 = 100

	block

	b0 = 50
	call MPI_ISEND(b0, 1 , MPI_REAL, 1, TAG_SEND_ARR, MPI_COMM_WORLD, reqs(1), mpierror)
	! While waiting for the data to be sent, do some calculations:
	result1 = sqrt(a0)
	call MPI_WAIT(reqs(1), status, mpierror)
	b0 = a0*2 !Now that a0 has been sent we can alter its value

	end block

!get the result from task 1
call MPI_RECV(res0, 1, MPI_REAL, 1, TAG_RES_READY, MPI_COMM_WORLD, status, mpierror)
if (result1 .ne. 10.00000000) error STOP 1
if (res0 .ne. 2500.000000) error STOP 2

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
end subroutine mpi_communication
end program