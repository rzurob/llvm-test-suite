
!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             :/cintrop_ts29113/asynch_communication/asynchcomm007f.f
!* FEATURE NAME                : C_Interop_Asynch_Communication
!*  DATE                       : 2013-10-07
!*
!*  DESCRIPTION
!*
!* Checks if ASYNCHRONOUS communication works correctly when only a specific section of the array is being sent over the MPI communication
!* (i.e the compiler should not overwrite that section with some other section of the array during optimization.)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



program sum2arrays
        USE, INTRINSIC :: ISO_FORTRAN_ENV
		implicit none
		LOGICAL precision_r4
       include 'mpif.h'

       integer :: nt, rank, len, mpierr, rc, i
       integer :: status(MPI_STATUS_SIZE)
       integer, parameter :: DIM = 100
       integer, parameter :: TAG_SEND_ARR = 10, TAG_RES_READY = 11
       real :: arraya(DIM)

       real::   buffa(10)
       real :: arrayb(DIM)
       integer :: reqs(2)
       real :: partialSum1 = 0, partialSum2 = 0

       ! initialization
       call MPI_INIT(mpierr)
       if (mpierr .ne. MPI_SUCCESS) then
          print *,'Error starting MPI program. Terminating.'
          call MPI_ABORT(MPI_COMM_WORLD, rc, mpierr)
       end if

       call MPI_COMM_RANK(MPI_COMM_WORLD, rank, mpierr)
       call MPI_COMM_SIZE(MPI_COMM_WORLD, nt, mpierr)

       if (nt .ne. 2) then
          print *, "This program requires exactly 2 tasks"
          call MPI_ABORT(MPI_COMM_WORLD, rc, mpierr)
       end if





if (rank .eq. 0) then
	arraya = [(real(i), i=1, DIM, 1)]
    block


	ASYNCHRONOUS :: arraya

	call MPI_ISEND(arraya(1:50:5), 10, MPI_REAL, 1, TAG_SEND_ARR, MPI_COMM_WORLD, reqs(1), mpierr)
	! While waiting for the data to be sent, do some calculations:
	arrayb = [(real(i+1*2), i=1, DIM, 1)]
	print *, "array b before sending is:", arrayb
 	partialSum1 = sum(arrayb)
	print *, "partial sum 1 =", partialSum1
	call MPI_WAIT(reqs(1), status, mpierr)

   	arraya = arrayb**2
	if (.not. precision_r4(sum(arraya) , 358950.0000)) error STOP 1

	end block


call MPI_RECV(partialSum2, 1, MPI_REAL, 1, TAG_RES_READY, MPI_COMM_WORLD, status, mpierr)

print *, "partial sum 1 =", partialSum1
print *, "partial sum 2 =", partialSum2
print *, "partialSum1 + partialSum2", partialSum1 + partialSum2

if (.not. precision_r4 ((partialSum1 + partialSum2) , 5485.000000)) error STOP 2

else
	block

	REAL ::reS
	ASYNCHRONOUS :: buffa
   	call MPI_IRECV(buffa, 10, MPI_REAL, 0, TAG_SEND_ARR, MPI_COMM_WORLD, reqs(2), mpierr)
	print *, "buffa", buffa
	call MPI_WAIT(reqs(2), status, mpierr)
	res = sum(buffa)
	print *, "res =", res
	call MPI_SEND(res, 1, MPI_REAL, 0, TAG_RES_READY, MPI_COMM_WORLD, mpierr)

    end block
end if

call MPI_FINALIZE(mpierr)
end
