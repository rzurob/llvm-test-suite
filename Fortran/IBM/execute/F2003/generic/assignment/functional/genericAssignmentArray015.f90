!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : assignment: with array derived type component that has elemental generic assignment
!*                                           non-polymorphic operands
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

   type inner
      integer :: i= 0
      contains
         generic, private :: operator(+) => inneradd
         procedure, pass, private :: inneradd
         generic, private :: assignment(=) => innerassgn
         procedure, pass, private :: innerassgn
   end type

   type base
      character(3) :: c ='xxx'
      type(inner), allocatable  :: in(:)
      contains
         generic :: assignment(=) => bassgn1d
        procedure, pass :: bassgn1d
   end type

   contains

      type(inner) elemental function inneradd ( a, b )
         class(inner), intent(in) :: a
         type(inner), intent(in)  :: b

         inneradd%i = a%i + b%i

      end function

      subroutine bassgn1d ( a, b )
         class(base), intent(out) :: a
         type(base), intent(in) :: b(:)

         a%c = b(1)%c

         if ( .not. allocated (a%in) ) allocate ( a%in(3) )

         a%in = b(1)%in
         do i = 2,size(b)
            if ( size (a%in) /= size(b(1)%in) ) then
                 print *, 'non comforming array components'
               error stop 1_4
            else
               a%in = a%in + b(i)%in
            end if

         end do

         print *, 'bassgn1d'

      end subroutine

      elemental subroutine innerassgn ( a, b )
         class(inner), intent(out) :: a
         type(inner), intent(in) :: b

         a%i = a%i + b%i

      end subroutine

end module

program genericAssignmentArray015
   use m

   type(base) :: b1
   type(base), pointer :: b2(:)
   type(base), allocatable :: b3(:)

   b1 = base ( 'xxx', (/ inner(-99), inner(-99), inner(-99)  /) )

   allocate ( b2(3), source = (/ base ( 'ibm', (/ inner(1), inner(2), inner(3)  /) ), &
                                 base ( 'IBM', (/ inner(2), inner(3), inner(4)  /) ), &
                                 base ( 'ftn', (/ inner(3), inner(4), inner(5)  /) ) /) )

   allocate ( b3(4), source = (/ base ( 'IBM', (/ inner(11), inner(12), inner(13)  /) ), &
                                 base ( 'FTN', (/ inner(12), inner(13), inner(14)  /) ), &
                                 base ( 'XLF', (/ inner(13), inner(14), inner(15)  /) ), &
                                 base ( 'xlf', (/ inner(14), inner(15), inner(16)  /) ) /) )

   b1 = b2
   print *, b1%c,  b1%in%i

   b1 = b3
   print *, b1%c,  b1%in%i

end program
