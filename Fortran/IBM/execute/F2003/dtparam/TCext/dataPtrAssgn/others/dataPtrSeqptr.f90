! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/dataPtrAssgn/others/dataPtrSeqptr.f
! opt variations: -ql

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrSeqptr.f 
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
!* - data-target is of sequence type, data-ponter is of class(*)
!* - data-pointer is of sequence type, data-target is of class(*)
!* - sequence type data-pointer as arg of eoshift
!*  
!234567890123456789012345678901234567890123456789012345678901234567890
 
 program main

        type seqDT(k1)    ! (4)
	    integer, kind :: k1
            sequence
	    logical(k1)   :: ip 
        end type

        type(seqDT(4)), pointer :: sP(:)
        class(*), pointer :: ulmtP(:)
        type(seqDT(4)),allocatable, target :: tar(:)
 
        allocate(tar(10), source = (/ ( seqDT(4)(mod(i,2) == 0), i=11,20) /) )

        ulmtP(3:) => tar

	if ( .not. associated(ulmtP, tar) ) stop 21
	print *, lbound(ulmtP,1), ubound(ulmtP,1) 

        sp(2:6) => ulmtP(6:ubound(tar,1))

	if ( .not. associated(sp) ) stop 21
	print *, lbound(sp,1), ubound(sP,1) 

	tar = (/ ( seqDT(4)(mod(i,2) /= 0), i=11,20) /) 

	tar =  eoshift(sp, 2, sp(3)) 

	print *, tar%ip
 End program

