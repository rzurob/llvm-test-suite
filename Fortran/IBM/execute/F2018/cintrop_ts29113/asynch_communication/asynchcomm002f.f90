
!*******************************************************************************
!*  ============================================================================
!*
!* FEATURE NAME                : C_Interop_Asynch_Communication
!*  DATE                       : 2013-10-07
!*
!*  DESCRIPTION
!*
!* Checks if ASYNCHRONOUS communication works correctly when 4 tasks run in parallel participating in asynchronous communication.
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


       integer, parameter :: DIM = 4000
       integer, parameter :: TAG_SEND_ARR = 10, TAG_RES_READY = 11
       real :: arraya(DIM)

       real::   buffa(DIM)
       real :: buffa1(DIM), buffa2(DIM), buffa3(DIM)
       real :: arrayb(DIM)
       integer :: reqs(4)

       real :: partialSum1 = 0,partialSum2=0, finalSum = 0

       ! initialization
       call MPI_INIT(mpierr)
       if (mpierr .ne. MPI_SUCCESS) then
          print *,'Error starting MPI program. Terminating.'
          call MPI_ABORT(MPI_COMM_WORLD, rc, mpierr)
       end if

       call MPI_COMM_RANK(MPI_COMM_WORLD, rank, mpierr)
       call MPI_COMM_SIZE(MPI_COMM_WORLD, nt, mpierr)

       if (nt .ne. 4) then
          print *, "This program requires exactly 4 tasks"
          call MPI_ABORT(MPI_COMM_WORLD, rc, mpierr)
       end if

       ! Assume there are 4 tasks in the world communicator:


arraya = [(real(i), i=1, DIM, 1)]
if (rank .eq. 0) then

block

	ASYNCHRONOUS :: arraya
	call MPI_ISEND(arraya(1:4000:1), 4000, MPI_REAL, 1, TAG_SEND_ARR, MPI_COMM_WORLD, reqs(1), mpierr)

   	 arrayb = [(real(i+1*2), i=1, DIM, 1)]
   	 partialSum1 = sum(arrayb)
	call MPI_WAIT(reqs(1), status, mpierr)
	call MPI_RECV(finalSum, 1, MPI_REAL, 3, TAG_RES_READY, MPI_COMM_WORLD, status, mpierr)

	print *, "partial sum 1 =", partialSum1
	print *, "finalSum =",finalSum
	print *, "partialSum1 + finalSum", partialSum1 + finalSum
	if (.not. precision_r4((partialSum1 + finalSum), 13344666.00)) error STOP 4

end block

else if (rank .eq.1) then
	block


	REAL :: result1
	ASYNCHRONOUS :: buffa1
   	 call MPI_IRECV(buffa1, 4000, MPI_REAL, 0, TAG_SEND_ARR, MPI_COMM_WORLD, reqs(2), mpierr)
	call MPI_WAIT(reqs(2), status, mpierr)
	call sleep_(5)
	call MPI_SEND(buffa1*2, 4000, MPI_REAL, 2, TAG_RES_READY, MPI_COMM_WORLD, mpierr)
	result1=sum(buffa1)
	if (.not. precision_r4(result1 , 8002000.000)) error STOP 1

 end block




else if (rank .eq.2) then

 block

        REAL ::result2
        ASYNCHRONOUS :: buffa2
	call MPI_RECV(buffa2, 4000, MPI_REAL, 1, TAG_RES_READY, MPI_COMM_WORLD, status, mpierr)

	call sleep_(5)
	call MPI_SEND(buffa2/3, 4000, MPI_REAL, 3, TAG_RES_READY, MPI_COMM_WORLD, mpierr)
	result2 = sum(buffa2)
        print *, "result2 =", result2
        if (.not. precision_r4(result2 , 16004000.00)) error STOP 2

 end block


else if (rank .eq.3) then

 block


        REAL ::result3
        ASYNCHRONOUS :: buffa3
	call MPI_RECV(buffa3, 4000, MPI_REAL, 2, TAG_RES_READY, MPI_COMM_WORLD, status, mpierr)

	call MPI_SEND(sum(buffa3), 1, MPI_REAL, 0, TAG_RES_READY, MPI_COMM_WORLD,mpierr)
	result3 = sum (buffa3)
	print *, "result3 =", result3
	if (.not. precision_r4(result3 , 5334666.500)) error STOP 3

  end block


end if

call MPI_FINALIZE(mpierr)
end
