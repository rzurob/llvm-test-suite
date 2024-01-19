! GB DTP extension using:
! ftcx_dtp -qk -qdeferredlp -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentPass008.f
! opt variations: -qck -qnok -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DESCRIPTION                : assignment: pass-obj specified with elemental subroutines
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

   type base(k1,n1)    ! (4,3)
      integer, kind :: k1
      integer, len  :: n1
      character(n1) :: c = 'xxx'
      contains
         procedure, pass(b) :: ab
         generic :: assignment(=) => ab
   end type

   type, extends(base) :: child    ! (4,3)
      character(n1) :: d = 'xxx'
      contains
         procedure, pass(b) :: ab => cb
         generic :: assignment(=) => ab
   end type

   contains

   elemental subroutine ab ( a, b )
      character(*), intent(out) :: a
      class(base(4,*)), intent(in)   :: b

      a = b%c(1:2) // 'X'

   end subroutine

   elemental subroutine cb ( a, b )
      character(*), intent(out)  :: a
      class(child(4,*)), intent(in)   :: b

      a(1:3) = b%c(1:2) // 'Z'
      a(4:6) = b%d(1:2) // 'Z'

   end subroutine

end module

program genericAssignmentPass008
   use m

   class(base(4,:)), allocatable :: b1(:)
   class(base(4,:)), allocatable :: b2(:,:)
   character(3) :: c(4)
   character(6) :: c1(4)


   allocate ( b1(4), source = (/ base(4,3)('abc'), base(4,3)('def'), base(4,3)('ghi'), base(4,3)('jkl') /) )
   allocate ( b2(2,2), source = reshape ( source = (/ base(4,3)('ABC'), base(4,3)('DEF'), base(4,3)('GHI'), base(4,3)('JKL') /), shape = (/2,2/) ) )

   c = b1
   print *, c

   c = reshape ( source = b2, shape= (/4/) )
   print *, c

   deallocate ( b1, b2 )

   allocate ( b1(4), source = (/ child(4,3)('abc','ABC'), child(4,3)('def','DEF'), child(4,3)('ghi','GHI'), child(4,3)('jkl','JKL') /) )

   c1 = b1
   print *, c1

   allocate ( b2(2,2), source = reshape ( source = (/ child(4,3)('ABC','abc'), child(4,3)('DEF','def'), child(4,3)('GHI','ghi'), child(4,3)('JKL','jkl') /), shape = (/2,2/) ) )

   c1 = reshape ( source = b2, shape= (/4/) )
   print *, c1

   c = (/ base(4,3)('aaa'), base(4,3)('bbb'), base(4,3)('ccc'), base(4,3)('ddd') /)
   print *,c

   c1 = (/ (child(4,3)('ibm','ftn'), i = 1, 4 ) /)
   print *,c1

end program
