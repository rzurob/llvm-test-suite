!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-target is of sequence type, data-ponter is of class(*)
!* - data-pointer is of sequence type, data-target is of class(*)
!* - sequence type data-pointer as arg of eoshift
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 program main

        type seqDT
            sequence
	    logical :: ip
        end type

        type(seqDT), pointer :: sP(:)
        class(*), pointer :: ulmtP(:)
        type(seqDT),allocatable, target :: tar(:)

        allocate(tar(10), source = (/ ( seqDT(mod(i,2) == 0), i=11,20) /) )

        ulmtP(3:) => tar

	if ( .not. associated(ulmtP, tar) ) stop 21
	print *, lbound(ulmtP,1), ubound(ulmtP,1)

        sp(2:6) => ulmtP(6:ubound(tar,1))

	if ( .not. associated(sp) ) stop 21
	print *, lbound(sp,1), ubound(sP,1)

	tar = (/ ( seqDT(mod(i,2) /= 0), i=11,20) /)

	tar =  eoshift(sp, 2, sp(3))

	print *, tar%ip
 End program

