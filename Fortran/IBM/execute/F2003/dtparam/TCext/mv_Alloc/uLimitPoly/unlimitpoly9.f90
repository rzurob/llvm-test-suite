! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/mv_Alloc/uLimitPoly/unlimitpoly9.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : unlimitpoly9.f
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 05/30/2006
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
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic,
!*                               TO and FROM are component of a derived-type 
!*                               which is a allocatable subobject of another
!*                               derived-type
!*                               dynamic type is a derived-type 
!*                               FROM is zero-size array 
!*				 TO is finalized, rank one
!23456789012345678901234567890123456789012345678901234567890123456789012


module m
   integer :: bNum = 0
   integer :: cNumrZero = 0
   integer :: cNumrOne = 0

   type base(k1,n1)    ! (4,7)
       integer, kind :: k1
       integer, len  :: n1
       character(n1)    name
       contains
         final:: final1
   end type

   type, extends(base) :: child    ! (4,7)
       contains
         final :: final2
	 final :: final3
   end type 

   type A(k2,n2)    ! (4,20)
       integer, kind :: k2
       integer, len  :: n2
       class(*), allocatable ::  l1(:)
   end type

   type B(k3,n3)    ! (4,20)
       integer, kind              :: k3
       integer, len               :: n3
       type(A(k3,:)), allocatable :: l2(:,:)
   end type

   contains
       subroutine final1(arg)
          type(base(4,*)), intent(in) :: arg
          print *, "finalization for base"
	  bNum = bNum + 1
       end subroutine
       subroutine final2(arg)
          type(child(4,*)), intent(in) :: arg
          print *, "finalization for child scalar"
	  cNumrZero = cNumrZero + 1 
       end subroutine
       subroutine final3(arg)
          type(child(4,*)), intent(in) :: arg(:)
          print *, "finalization for child rank one"
	  cNumrOne = cNumrOne + 1 
       end subroutine

end module

program main
use m
   type(B(4,20)) b1
   class(base(4,:)), allocatable :: bs

   allocate(A(4,20) :: b1%l2(2,1))
   if ( .not. allocated(b1%l2) ) stop 20

   ! allocate zero size array
   allocate( base(4,7) :: b1%l2(1,1)%l1(6:5))
   if ( .not. allocated(b1%l2(1,1)%l1) ) stop 30

   
   allocate( b1%l2(2,1)%l1(2:5), source = (/ (child(4,7)('FORTRAN'), i = 1,4) /) )
   if ( .not. allocated(b1%l2(2,1)%l1) ) stop 40 

   cNumrZero = 0
   cNumrOne = 0
   bNum = 0
       
   call move_alloc( b1%l2(1,1)%l1, b1%l2(2,1)%l1 )

   if ( cNumrOne /= 1 ) stop 41
   if ( cNumrZero /= 0 ) stop 43
   if ( bNum /= 4 ) stop 45

   if ( allocated(b1%l2(1,1)%l1) ) stop 50
   if ( .not. allocated(b1%l2(2,1)%l1) ) stop 60 

   if ( same_type_as(bs, b1%l2(2,1)%l1) .neqv. .true. ) stop 69

   if ( size(b1%l2(2,1)%l1) /= 0 ) stop 70

end

