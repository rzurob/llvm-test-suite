! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/ambiguity/genericAmbiguityInterface002d.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : ambiguous but between interface and type bounds
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


module assignment

   type base2(k1)    ! (4)
     integer, kind :: k1
     integer(k1)   :: i, j
      contains
         procedure, pass :: assgn1
         generic :: assignment(=) => assgn1
   end type

   type, extends(base2) :: child2    ! (4)
   end type

   contains

      subroutine assgn1(a, b)
         class(base2(4)), intent(out) :: a
         character(1), intent(in)  :: b

      end subroutine

      subroutine assgn2(a, b)
         class(child2(4)), intent(out) :: a
         character(4), intent(in)  :: b   !<- different len but same kind

      end subroutine

end module

module binoperator

   type base1(k2)    ! (4)
      integer, kind :: k2
      integer(k2)   :: i
      contains
         procedure, pass :: adda
         generic :: operator(*) => adda
   end type

   type, extends(base1) :: child1    ! (4)
      integer(k2) :: j
   end type

   interface operator(*)
      procedure addb
   end interface

   contains

      type(base1(4)) function adda(a, b)
         class(base1(4)), intent(in) :: a
         complex, intent(in)  :: b

         adda = base1(4)(10)

      end function

      type(base1(4)) function addb(a, b)
         class(child1(4)), intent(in) :: a
         complex, intent(in)  :: b

         addb = base1(4)(10)

      end function

end module

program genericAmbiguityInterface002d
   use assignment

   interface assignment(=)
      procedure assgn2
   end interface

end program
