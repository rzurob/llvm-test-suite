! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/dataPtrAssgn/others/dataPtrFinal.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=base

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrFinal.f
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
!* - data-target is rank-1 array, pointer is rank-2 w bounds-remapping-lst
!* - test if target is finalized, each finalizable component of each element 
!*     of data-tar is finalized 
!*  
!234567890123456789012345678901234567890123456789012345678901234567890
 

 module m

    integer :: counta2 =0, countA1 = 0, countB = 0, countA = 0

    type A(k1,n1)    ! (4,20) 
        integer, kind :: k1
        integer, len  :: n1
	contains
	    final :: fA_rk_2, fA_rk_1, fA_sc
    end type

    type, extends(A) :: B(k2,n2)    ! (4,20,4,20) 
        integer, kind  :: k2
        integer, len   :: n2
        type(A(k2,n2)) :: p(10) 
   	contains
	    final :: fB_rk_1
    end type 

    contains
	subroutine fa_rk_2(a)
	    type(A(4,*)), intent(inout) :: a(:,:)
	    counta2 = counta2 + 1
        end subroutine

	subroutine fa_rk_1(a)
	    type(A(4,*)), intent(inout) :: a(:)
	    countA1 = countA1 + 1
        end subroutine

	subroutine fa_sc(a)
	    type(A(4,*)), intent(inout) :: a
	    countA = countA + 1
        end subroutine

	subroutine fB_rk_1(a)
	    type(B(4,*,4,*)), intent(inout) :: a(:)
	    countB = countB + 1
        end subroutine

 end module

 program main
    use m
	
    type(A(4,:)), pointer :: p(:,:)
    class(A(4,:)), pointer :: p1(:)

    allocate(B(4,20,4,20) :: P1(20))
    p(3:6, 5:9) => p1

    if ( .not. associated(p)) stop 3
    deallocate(p)

    if ( associated(p) ) stop 11 

    if ( countB /= 0 ) stop 21	
    if ( counta2 /= 1 ) stop 31	
    if ( counta1 /= 0 ) stop 41	
    if ( counta /= 0 ) stop 51	


 End program
