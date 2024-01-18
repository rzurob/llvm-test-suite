! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound009d.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : two argument with pass-arg to be first arg specified (for generic-name, operator, and assignment tb)
!*                                  - one arg polymorphic, the other polymorphic extended type with pointer/allocatable attribute
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

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j
      contains
         procedure, pass :: printb
         generic :: print => printb
   end type

   contains

      subroutine printa(a, b)
         class(base(*,4)), intent(in) :: a
         class(base(*,4)), allocatable, intent(in) :: b

      end subroutine

      subroutine printb(a, b)
         class(child(*,4)), intent(in) :: a
         class(child(*,4)), allocatable, intent(in) :: b

      end subroutine

end module

module binoperator

   type base1(n2,k2)    ! (20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: i
      contains
         procedure, pass :: adda
         generic :: operator(+) => adda
   end type

   type, extends(base1) :: child1    ! (20,4)
      integer(k2) :: j
      contains
         procedure, pass :: addb
         generic :: operator(+) => addb
   end type

   contains

      type(base1(20,4)) function adda(a, b)
         class(base1(*,4)), intent(in) :: a
         class(base1(*,4)), allocatable, intent(in)  :: b

         adda = base1(20,4)(10)

      end function

      type(base1(20,4)) function addb(a, b)
         class(child1(*,4)), intent(in) :: a
         class(child1(*,4)), pointer, intent(in)  :: b

         addb = base1(20,4)(20)

      end function

end module

module assignment

   type base2(n3,k3)    ! (20,4)
     integer, kind :: k3
     integer, len  :: n3
     integer(k3)   :: i, j
      contains
         procedure, pass :: assgn1
         generic :: assignment(=) => assgn1
   end type

   type, extends(base2) :: child2    ! (20,4)
      contains
         procedure, pass :: assgn2
         generic :: assignment(=) => assgn2
   end type

   contains

      subroutine assgn1(a, b)
         class(base2(*,4)), intent(out) :: a
         class(base2(*,4)), intent(in)  :: b

      end subroutine

      subroutine assgn2(a, b)
         class(child2(*,4)), intent(out) :: a
         type(child2(*,4)), intent(in)  :: b

      end subroutine

end module

program genericAmbiguityTypeBound009d
end program
