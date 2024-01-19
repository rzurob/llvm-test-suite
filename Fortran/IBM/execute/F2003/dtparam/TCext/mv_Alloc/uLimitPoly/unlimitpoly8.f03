! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/mv_Alloc/uLimitPoly/unlimitpoly8.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/30/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic, scalar
!*                               TO and FROM are component of a derived-type
!*                               which is a allocatable subobject of another
!*                               derived-type
!*                               dynamic type is a derived-type
!*                               when TO is deallocated, final subroutines for
!*                               its declared type and its parent type should
!*                               be called.
!*
!*				 TO is finalized; FROM is not finalized
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m

   integer ::  bNum = 0
   integer :: cNum = 0

   type base(k1,n1)    ! (4,7)
       integer, kind :: k1
       integer, len  :: n1
       character(n1)    name

       contains
         final:: final1
   end type

   type, extends(base) :: child(k2,n2)    ! (4,7,4,20)
       integer, kind :: k2
       integer, len  :: n2
       contains
         final :: final2
   end type

   type A(k3,n3)    ! (4,20)
       integer, kind :: k3
       integer, len  :: n3
       class(*), allocatable ::  l1
   end type

   type B(k4,n4)    ! (4,20)
       integer, kind              :: k4
       integer, len               :: n4
       type(A(k4,:)), allocatable :: l2(:)
   end type

   contains
       subroutine final1(arg)
          type(base(4,*)), intent(in) :: arg
          print *, "finalization for base"

          bNum = bNum + 1
       end subroutine
       subroutine final2(arg)
          type(child(4,*,4,*)), intent(in) :: arg
          print *, "finalization for child"

          cNum = cNum + 1
       end subroutine

end module

program main
use m

   type(B(4,20)) b1

   allocate(A(4,20) :: b1%l2(2))
   if ( .not. allocated(b1%l2) ) error stop 20

   allocate( b1%l2(1)%l1, source = base(4,7)('Fortran'))
   if ( .not. allocated(b1%l2(1)%l1) ) error stop 30

   allocate( b1%l2(2)%l1, source = child(4,7,4,20)('FORTRAN'))
   if ( .not. allocated(b1%l2(2)%l1) ) error stop 40

   bNum = 0
   cNum = 0

   call move_alloc( b1%l2(1)%l1, b1%l2(2)%l1 )

   if ( bNum /= 1) error stop 41
   if ( CNum /= 1) error stop 43

   if ( allocated(b1%l2(1)%l1) ) error stop 50
   if ( .not. allocated(b1%l2(2)%l1) ) error stop 60

   select type ( a => b1%l2(2)%l1 )
       type is (base(4,*))
            if ( a%name /= 'Fortran' ) error stop 70
       class default
           stop 90
   end select

end

