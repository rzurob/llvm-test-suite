


!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             :/cintrop_ts29113/asynch_communication/asynchcomm013f.f
!* FEATURE NAME                : C_Interop_Asynch_Communication 
!*  PROGRAMMER                 : Tapti Vaid
!*  DATE                       : 2013-10-07
!*
!*  DESCRIPTION
!*
!* Checks the functionalilty of C_interop ASYNCHRONOUS Communication with a COMPLEX data type.
!* 
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module mymod

implicit none
include 'mpif.h'


complex :: a0, b0, b1, res0, res1, result1
end module mymod



program asynch


use mymod
implicit none

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


if (rank .eq. 0) then


b0 =(2.0,3.0) 
a0 =(3.0,2.0) 

block 
ASYNCHRONOUS :: b0

 call MPI_ISEND(b0, 1 , MPI_COMPLEX , 1, TAG_SEND_ARR, MPI_COMM_WORLD, reqs(1), mpierror)
 result1 =  a0
 call MPI_WAIT(reqs(1), status, mpierror)
		   
end block


call MPI_RECV(res0, 1, MPI_COMPLEX, 1, TAG_RES_READY, MPI_COMM_WORLD, status, mpierror)
print *, "Output from task 0 =", result1
print *, "Output from task 1 =", res0

else !(if task# =1)

block
	ASYNCHRONOUS :: b1

	call MPI_IRECV(b1, 1 , MPI_COMPLEX, 0, TAG_SEND_ARR, MPI_COMM_WORLD, reqs(2), mpierror)
	call MPI_WAIT(reqs(2), status, mpierror)

	b1=(5.0,5.0) 
	res1 = b1
   	call MPI_SEND(res1, 1, MPI_COMPLEX, 0, TAG_RES_READY, MPI_COMM_WORLD, mpierror)

end block
end if
call MPI_FINALIZE(mpierror)


       end



