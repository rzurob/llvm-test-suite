!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrSMP.f
!*
!*  PROGRAMMER                 : Michelle Zhang
!*  DATE                       : Aug 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION
!*
!* - data-target is specified in shared clause
!* - data-ptr is specified in lastprivate clause
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
    implicit none
    integer i
    integer, pointer :: p(:)
#ifdef bg4thd
	integer, parameter :: NTHR=3
    integer, target :: tar(NTHR) = (/1,2,3/)
#else
	integer, parameter :: NTHR=10
    integer, target :: tar(NTHR) = (/1,2,3,4,5,6,7,8,9,10/)
#endif

    call omp_set_num_threads(NTHR)

    !$omp parallel do lastprivate(p)
    do  i = 1, NTHR
        tar(i) = NTHR+1 - i 
        p(i:) => tar
    enddo

    if ( .not. associated(p, tar)) stop 1
    if ( lbound(p, 1) /= NTHR) stop 2 
    if ( ubound(p, 1) /= (NTHR*2)-1) stop 3 
#ifdef bg4thd
    if ( any (tar .ne. (/(i,i=3,1,-1)/)))  stop 4 
    if ( any (p .ne. (/(i,i=3,1,-1)/)))  stop 5 
#else
    if ( any (tar .ne. (/(i,i=10,1,-1)/)))  stop 4 
    if ( any (p .ne. (/(i,i=10,1,-1)/)))  stop 5 
#endif
end program
