!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DESCRIPTION                : assignment: pass-obj specified with elemental subroutines but with array sections (ICE in another place currently)
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

   type base
      character(3) :: c = 'xxx'
      contains
         procedure, pass(b) :: ab
         generic :: assignment(=) => ab
   end type

   type, extends(base) :: child
      character(3) :: d = 'xxx'
      contains
         procedure, pass(b) :: ab => cb
         generic :: assignment(=) => ab
   end type

   contains

   elemental subroutine ab ( a, b )
      character(*), intent(out) :: a
      class(base), intent(in)   :: b

      a = b%c(1:2) // 'X'

   end subroutine

   elemental subroutine cb ( a, b )
      character(*), intent(out)  :: a
      class(child), intent(in)   :: b

      a(1:3) = b%c(1:2) // 'Z'
      a(4:6) = b%d(1:2) // 'Z'

   end subroutine

end module

program genericAssignmentPass009
   use m

   class(base), allocatable :: b1(:)
   class(base), allocatable :: b2(:)
   character(3) :: c(4)
   character(6) :: c1(4)


   allocate ( b1(4), source = (/ base('abc'), base('def'), base('ghi'), base('jkl') /) )
   allocate ( b2(4), source = reshape ( source = (/ base('ABC'), base('DEF'), base('GHI'), base('JKL') /), shape = (/4/) ) )

   c = b1(1:4)
   print *, c

   c = b2((/4,2,3,1/))
   print *, c

   deallocate ( b1, b2 )

   allocate ( b1(4), source = (/ child('abc','ABC'), child('def','DEF'), child('ghi','GHI'), child('jkl','JKL') /) )
   allocate ( b2(4), source = (/ child('ABC','abc'), child('DEF','def'), child('GHI','ghi'), child('JKL','jkl') /) )

   c1 = (/ b1(4:3:-1), b2(1), b2(4) /)
   print *, c1

   c1 = (/ b1(1:2), b2(1:2) /)
   print *, c1

   c1 = (/ b1((/4,1/)), b2(1:3:2) /)
   print *,c1

end program
