! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/mv_Alloc/uLimitPoly/unlimitfinal.f
! opt variations: -ql

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
    type fun(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id
        contains
            final :: final1
    end type 

    type A(k2)    ! (4) 
         integer, kind :: k2
         logical(k2)   :: l(2)  
         class(*), allocatable :: aTO (:,:,:)
         contains
             final :: final2
    end type

    class(*), allocatable :: FROM(:,:,:)

    integer :: iF = 0
    integer :: iA = 0
 
    contains
        subroutine final1(arg)
            type(fun(4)), intent(inout) :: arg(:,:,:)
            iF = iF + 1
        end subroutine
        subroutine final2(arg)
            type (A(4)), intent( inout) :: arg(:,:,:) 
            iA = iA + 1
        end subroutine
end module


program main
use m

    type(A(4)), allocatable :: aA

    allocate(aA)

    allocate( A(4) :: FROM(1,2,1))

    select type ( FROM )
        type is (A(4))
            FROM(1,2,1) = A(4) ((/ .true., .false. /), null() )
            FROM(1,1,1) = A(4) ((/ .false., .true. /), null() )
        class default
            stop 9
    end select

    allocate(aA%aTO(2,1,2), source=reshape( (/ (fun(4)(i), i = 1,4 ) /),&
                         (/ 2,1,2 /) ))

    iA = 0
    iF = 0

    call move_alloc(FROM, aA%aTO )
 
    if ( iF /= 1 ) stop 21
    if ( iA /= 0 ) stop 23
    if ( allocated(FROM)) stop 25
    if ( .not. allocated(aA%aTO) ) stop 27 

    select type ( x => aA%aTo)
        type is (A(4))
            print *, shape(x), x(1,1,1)%l, x(1,2,1)%l
        class default
            stop 31
    end select
    end

