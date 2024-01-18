!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrFinal1.f 
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
!* - data-pointer/target are rank-1 array 
!* - data-pointer is self referenced w bounds-spec/remapping-lst 
!* - check if target is finalized
!*
!234567890123456789012345678901234567890123456789012345678901234567890
 

 module m

    integer :: countA = 0

    type base
	contains
	    final :: finalA
    end type

    contains
	subroutine finalA(a)
	    type(base), intent(inout) :: a(:)
          
	    countA = countA + 1 
	    print *, "final subroutine :: ", lbound(a,1), ubound(a,1)
        end subroutine

 end module

 program main
    use m
	
    type(base), pointer :: p(:)

    allocate(P(1024))
    p(3:) => p

    if ( .not. associated(p)) stop 12
    deallocate(p)

    if ( associated(p) ) stop 41 
    if ( countA /= 1 ) stop 43 

    countA = 0
 
    allocate(P(512))
    p(7:518) => p(512:1:-1)
    if ( .not. associated(p)) stop  45
    deallocate(p)

    if ( associated(p) ) stop 41 
    if ( countA /= 1 ) stop 43 

 End program
