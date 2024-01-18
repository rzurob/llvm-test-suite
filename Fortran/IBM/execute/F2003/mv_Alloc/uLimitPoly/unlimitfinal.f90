! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : unlimitfinal.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : 1.FROM and TO are unlimited polymorphic,
!*                               2. 3-rank
!*                               3.TO is finalized , FROM is not finalized
!*                        
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type fun
 	integer id
        contains
            final :: final1
    end type 

    type A 
         logical :: l(2)  
	 class(*), allocatable :: aTO (:,:,:)
         contains
             final :: final2
    end type

    class(*), allocatable :: FROM(:,:,:)

    integer :: iF = 0
    integer :: iA = 0
 
    contains
        subroutine final1(arg)
            type(fun), intent(inout) :: arg(:,:,:)
            iF = iF + 1
        end subroutine
        subroutine final2(arg)
            type (A), intent( inout) :: arg(:,:,:) 
            iA = iA + 1
        end subroutine
end module


program main
use m

    type(A), allocatable :: aA

    allocate(aA)

    allocate( A :: FROM(1,2,1))

    select type ( FROM )
        type is (A)
	    FROM(1,2,1) = A ((/ .true., .false. /), null() )
	    FROM(1,1,1) = A ((/ .false., .true. /), null() )
	class default
            stop 9
    end select

    allocate(aA%aTO(2,1,2), source=reshape( (/ (fun(i), i = 1,4 ) /),&
                         (/ 2,1,2 /) ))

    iA = 0
    iF = 0

    call move_alloc(FROM, aA%aTO )
 
    if ( iF /= 1 ) stop 21
    if ( iA /= 0 ) stop 23
    if ( allocated(FROM)) stop 25
    if ( .not. allocated(aA%aTO) ) stop 27 

    select type ( x => aA%aTo)
 	 type is (A)
             print *, shape(x), x(1,1,1)%l, x(1,2,1)%l
	 class default
	     stop 31
    end select
   end

