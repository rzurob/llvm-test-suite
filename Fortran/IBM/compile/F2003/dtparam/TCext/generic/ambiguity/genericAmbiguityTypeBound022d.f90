! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound022d.f
! opt variations: -qnok -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : With Class Hierarchy
!*
!*                                      ( Base )
!*                                      /        \
!*                                 (Child1)   (Child2)
!*                                     |         |
!*                                 (Gen31)    (Gen32)
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

module genericName

   type b1(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure, nopass :: twoargs1
         generic :: twoarg => twoargs1

         procedure, nopass :: twoargsa1
         generic :: twoarga => twoargsa1
   end type

   type, extends(b1) :: c1(k2,n2)    ! (20,4,4,20)
       integer, kind :: k2
       integer, len  :: n2
   end type

   type, extends(b1) :: c2(k3,n3)    ! (20,4,4,20)
       integer, kind :: k3
       integer, len  :: n3
   end type

   type, extends(c1) :: g1(k4,n4)    ! (20,4,4,20,4,20)
       integer, kind :: k4
       integer, len  :: n4
      contains
         procedure, nopass :: twoargs2
         generic :: twoarg => twoargs2
   end type

   type, extends(c2) :: g2(k5,n5)    ! (20,4,4,20,4,20)
       integer, kind :: k5
       integer, len  :: n5
      contains
         procedure, pass :: twoargsa2
         generic :: twoarga => twoargsa2
   end type

   contains

      subroutine twoargs1(a, b)
         class(b1(*,4)), intent(in) :: a
         class(b1(*,4)), intent(in) :: b

      end subroutine

      subroutine twoargs2(a, b)
         class(c1(*,4,4,*)), intent(in) :: a
         class(g2(*,4,4,*,4,*)), intent(in) :: b

      end subroutine

      subroutine twoargsa1(a, b)
         class(b1(*,4)), intent(in) :: a
         class(c2(*,4,4,*)), intent(in) :: b

      end subroutine

      subroutine twoargsa2(e, f, g)
         class(g2(*,4,4,*,4,*)), intent(in) :: e
         class(g1(*,4,4,*,4,*)), intent(in) :: f
         class(g2(*,4,4,*,4,*)), intent(in) :: g

      end subroutine

end module

module binoperator


   type b11(n6,k6)    ! (20,4)
      integer, kind :: k6
      integer, len  :: n6
      integer(k6)   :: i
   end type

   type, extends(b11) :: c11(k7,n7)    ! (20,4,4,20)
       integer, kind :: k7
       integer, len  :: n7

   end type


   type, extends(b11) :: c12(k8,n8)    ! (20,4,4,20)
       integer, kind :: k8
       integer, len  :: n8
      contains
         procedure, pass :: addc
         generic :: operator(+) => addc
   end type

   type, extends(c11) :: g11(k9,n9)    ! (20,4,4,20,4,20)
       integer, kind :: k9
       integer, len  :: n9
   end type

   type, extends(c12) :: g12(k10,n10)    ! (20,4,4,20,4,20)
       integer, kind :: k10
       integer, len  :: n10
      contains
         procedure, pass :: addb
         generic :: operator(+) => addb
   end type

   contains

      type(b11(20,4)) function addb(a, b)
         class(g12(*,4,4,*,4,*)), intent(in) :: a
         class(b11(*,4)), intent(in) :: b

         addb = b11(20,4)(10)

      end function

      type(b11(20,4)) function addc(a, b)
         class(c12(*,4,4,*)), intent(in) :: a
         class(c12(*,4,4,*)), intent(in) :: b

         addc = b11(20,4)(10)

      end function

end module

module assignment

   type b21(n11,k11)    ! (20,4)
      integer, kind :: k11
      integer, len  :: n11
      integer(k11)  :: i
   end type

   type, extends(b21) :: c21(k12,n12)    ! (20,4,4,20)
       integer, kind :: k12
       integer, len  :: n12
      contains
         procedure, pass(b) :: assgn1
         generic :: assignment(=) => assgn1
   end type

   type, extends(b21) :: c22(k13,n13)    ! (20,4,4,20)
       integer, kind :: k13
       integer, len  :: n13
      contains
         procedure, pass :: assgn2
         generic :: assignment(=) => assgn2
   end type

   type, extends(c21) :: g21(k14,n14)    ! (20,4,4,20,4,20)
       integer, kind :: k14
       integer, len  :: n14

   end type

   type, extends(c22) :: g22(k15,n15)    ! (20,4,4,20,4,20)
       integer, kind :: k15
       integer, len  :: n15
   end type


   contains

      subroutine assgn1(a, b)
         class(g22(*,4,4,*,4,*)), intent(out) :: a
         class(c21(*,4,4,*)), intent(in)  :: b

      end subroutine

      subroutine assgn2(a, b)
         class(c22(*,4,4,*)), intent(out) :: a
         class(b21(*,4)), intent(in)  :: b

      end subroutine

end module

program genericAmbiguityTypeBound022d
end program
