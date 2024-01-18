! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qnodeferredlp -qreuse=base /tstdev/F2003/mv_Alloc/typCompatible/mixpolyunpoly1.f
! opt variations: -qnol -qdefaultpv -qdeferredlp -qreuse=none

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : mixpolyunpoly1.f 
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
!*  DESCRIPTION                : FROM is polymorphic,
!8				 TO is unlimited polymorphic
!*                               FROM is component of child 
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
   type ::  base(n1,k1)    ! (20,4)
       integer, kind :: k1
       integer, len  :: n1

       integer(k1)      id
       contains
           final  :: final1
   end type

   type, extends(base) :: A    ! (20,4)
       class(base(n1,k1)), allocatable ::  l1(:)
       contains
           final  :: final2
   end type

   type, extends(A) ::  B    ! (20,4)
       class(A(n1,k1)), allocatable :: l2
   end type

   contains
       subroutine final1(arg)
           type(base(*,4)), intent(in) :: arg
           print *, "This is final subroutine for type base"
       end subroutine

       subroutine final2(arg)
           type(A(*,4)), intent(in) :: arg
           print *, "This is final subroutine for type A"
       end subroutine
end module

program main
use m
   type(B(20,4)), allocatable :: b1
   class(*), allocatable :: b2(:)
   integer i

   allocate(b1)
   allocate(b1%l2)
   allocate(b1%l2%l1(2:7), source =(/ (base(20,4)(i), i = 101, 106) /) )

   allocate(b2(2), source = (/ (1.0d2,1.4d1), (1.0d2, 1.4d1) /))

   call move_alloc( b1%l2%l1, b2 )

   if ( allocated(b1%l2%l1) ) stop 31
   if ( .not. allocated(b2)) stop 32

   select type (b2)

       type is (base(*,4))
           if ( size(b2) /= 6 ) stop 41
           if ( lbound(b2,1) /= 2 ) stop 42
           if ( ubound(b2,1) /= 7 ) stop 45
           print *, b2%id
       class default
           stop 51
   end select

end

