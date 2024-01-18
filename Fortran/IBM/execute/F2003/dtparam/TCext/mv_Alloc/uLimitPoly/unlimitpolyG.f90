! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/mv_Alloc/uLimitPoly/unlimitpolyG.f
! opt variations: -qck -qnok -qnol

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : unlimitpolyG.f
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : 1.FROM and TO are unlimited polymorphic,
!*                               FROM and TO are rank two component of the
!*                               2.same derived-type
!*                               3.TO is finalized , FROM is not finalized
!*                               4.check dynamic type, size, value
!*                               5.FROM and TO are accesed by associate name of
!*                                      select type
!*                        
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type parent(k1,n1)    ! (4,8)
         integer, kind :: k1
         integer, len  :: n1
         character(n1) :: name 
         contains  
            final :: final1
    end type

    integer, save :: countP = 0 
    integer :: countC = 0 

    type, extends(parent) :: child    ! (4,8) 
	contains 
	    final :: final2 
    end type

    contains
        subroutine final1(arg)
            type(parent(4,*)), intent(in)  :: arg(:,:)
            countP = countp + 1
        end subroutine
        subroutine final2(arg)
            type(child(4,*)), intent(inout)  :: arg(:,:)
            countC = countC + 1
        end subroutine
end module


program main
use m

    integer i

    type level1(k2,n2)    ! (4,20) 
        integer, kind :: k2
        integer, len  :: n2
        class (*), allocatable :: from(:,:)
        class (*), allocatable :: to(:,:)
    end type

    type level2(k3,n3)    ! (4,20)
        integer, kind :: k3
        integer, len  :: n3
        class(*), allocatable ::  l2
    end type

    type(level2(4,20)) :: ll 

    allocate( level1(4,20)::ll%l2 )
    if ( .not. allocated(ll%l2) ) stop 21

    select type ( x => ll%l2 )
        class is (level1(4,*))
            allocate( x%from(1,1), source = child(4,8)("FORTRAN"))
            if ( .not. allocated(x%from) ) stop 31 

            allocate( x%to(2,2), source = reshape( (/ (parent(4,8)("COMPILER"), i = 1,4) /), (/2, 2/) ) )
            if ( .not. allocated(x%to) ) stop 32 

            countP = 0
            countC = 0

            call move_alloc(x%from, x%to)

            if ( .not. allocated(x%to) ) stop 35
            if ( allocated(x%from) ) stop 37
  
            if ( size(x%to, 1) /= 1) stop 41
            if ( size(x%to, 2) /= 1) stop 42
            if ( countp /= 1) stop 43
	    if ( countC /= 0) stop 44 

            select type ( y => x%to)
                type is (child(4,*))
                   if ( y(1,1)%name /= "FORTRAN" ) stop 51
                class default
                   print *, "wrong type"
                   stop 53 
            end select
 
        class default
            print *, "wrong type"
            stop 41 
    end select


   end

