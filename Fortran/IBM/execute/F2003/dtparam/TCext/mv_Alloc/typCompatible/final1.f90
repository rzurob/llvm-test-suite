! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/F2003/mv_Alloc/typCompatible/final1.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : final1.f 
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
!*  DESCRIPTION                : FROM and TO are polymorphic,
!*                               TO is of type parent, FROM of type child
!*                               FROM and TO are components of type 
!*                               Check if final subroutine is called 
!*                        
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type ::  base(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
       contains
           final  :: final1
   end type

   type, extends(base) :: A    ! (4,20)
       class(base(k1,:)), allocatable ::  l1(:)
       contains
           final  :: final2
   end type

   type, extends(A) ::  B    ! (4,20)
       class(A(k1,:)), allocatable :: l2(:)
   end type

   contains
       subroutine final1(arg)
           type(base(4,*)), intent(in) :: arg(:)
           print *, "This is final subroutine for type base"
       end subroutine

       subroutine final2(arg)
           type(A(4,*)), intent(in) :: arg(:)
           print *, "This is final subroutine for type A"
       end subroutine
end module

program main
use m
   type(B(4,:)), allocatable :: b1, b2(:)

   allocate(B(4,20) :: b1)
   allocate(A(4,20) :: b1%l2(2))
   allocate(A(4,20) :: b1%l2(2)%l1(50))

   allocate(B(4,20) :: b2(2))
   allocate(A(4,20) :: b2(1)%l2(3) )

   call move_alloc( b2(1)%l2, b1%l2(2)%l1 )

   if ( .not. allocated(b1%l2(2)%l1) ) stop 31
   if ( allocated(b2(1)%l2)) stop 32

   select type (x => b1%l2(2)%l1)

       type is (A(4,*))
           if ( size(x) /= 3 ) stop 41
       class default
           stop 51
   end select

end
