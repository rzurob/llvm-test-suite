! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=none /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound010d.f
! opt variations: -qnok -qnol -qreuse=base

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : two argument with pass-arg to be first arg specified (for generic-name, operator, and assignment tb)
!*                                  - one arg polymorphic, the other intrinsic types of different kind
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure, pass :: printa
         generic :: print => printa
   end type

   type, extends(base) :: child(n2,k2)    ! (20,4,20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: j
      contains
         procedure, pass :: printb
         generic :: print => printb
   end type

   contains

      subroutine printa(a, b)
         class(base(*,4)), intent(in) :: a
         integer(8), intent(in)  :: b

      end subroutine

      subroutine printb(a, b)
         class(child(*,4,*,4)), intent(in) :: a
         integer, intent(in)  :: b

      end subroutine

end module

module binoperator

   type base1(n3,k3)    ! (20,4)
      integer, kind :: k3
      integer, len  :: n3
      integer(k3)   :: i
      contains
         procedure, pass :: adda
         generic :: operator(+) => adda
   end type

   type, extends(base1) :: child1(n4,k4)    ! (20,4,20,4)
      integer, kind :: k4
      integer, len  :: n4
      integer(k4)   :: j
      contains
         procedure, pass :: addb
         generic :: operator(+) => addb
   end type

   contains

      type(base1(20,4)) function adda(a, b)
         class(base1(*,4)), intent(in) :: a
         complex, intent(in)  :: b

         adda = base1(20,4)(10)

      end function

      type(base1(20,4)) function addb(a, b)
         class(child1(*,4,*,4)), intent(in) :: a
         complex(8), intent(in)  :: b

         addb = base1(20,4)(20)

      end function

end module

module assignment

   type base2(n5,k5)    ! (20,4)
     integer, kind :: k5
     integer, len  :: n5
     integer(k5)   :: i, j
      contains
         procedure, pass :: assgn1
         generic :: assignment(=) => assgn1
   end type

   type, extends(base2) :: child2(k6,n6)    ! (20,4,4,20)
       integer, kind :: k6
       integer, len  :: n6
      contains
         procedure, pass :: assgn2
         generic :: assignment(=) => assgn2
   end type

   contains

      subroutine assgn1(a, b)
         class(base2(*,4)), intent(out) :: a
         character(1), intent(in)  :: b

      end subroutine

      subroutine assgn2(a, b)
         class(child2(*,4,4,*)), intent(out) :: a
         character(4), intent(in)  :: b !<- different len but same kind

      end subroutine

end module

program genericAmbiguityTypeBound010d
end program
