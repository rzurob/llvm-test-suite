! GB DTP extension using:
! ftcx_dtp -qck -ql -qnodefaultpv -qdeferredlp /tstdev/F2003/generic/assignment/functional/genericAssignmentArray015.f
! opt variations: -qnock -qnol -qdefaultpv -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
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

   type inner(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i= 0
      contains
         generic, private :: operator(+) => inneradd
         procedure, pass, private :: inneradd
         generic, private :: assignment(=) => innerassgn
         procedure, pass, private :: innerassgn
   end type

   type base(k2,n2,k3)    ! (1,3,4)
      integer, kind                   :: k2,k3
      integer, len                    :: n2
      character(kind=k2,len=n2)       :: c ='xxx'
      type(inner(:,k3)), allocatable  :: in(:)
      contains
         generic :: assignment(=) => bassgn1d
        procedure, pass :: bassgn1d
   end type

   contains

      type(inner(20,4)) elemental function inneradd ( a, b )
         class(inner(*,4)), intent(in) :: a
         type(inner(*,4)), intent(in)  :: b

         inneradd%i = a%i + b%i

      end function

      subroutine bassgn1d ( a, b )
         class(base(1,*,4)), intent(out) :: a
         type(base(1,*,4)), intent(in) :: b(:)

         a%c = b(1)%c

         if ( .not. allocated (a%in) ) allocate (inner(20,4) :: a%in(3) )

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
         class(inner(*,4)), intent(out) :: a
         type(inner(*,4)), intent(in) :: b

         a%i = a%i + b%i

      end subroutine

end module

program genericAssignmentArray015
   use m

   type(base(1,3,4)) :: b1
   type(base(1,:,4)), pointer :: b2(:)
   type(base(1,:,4)), allocatable :: b3(:)

   b1 = base(1,3,4) ( 'xxx', (/ inner(20,4)(-99), inner(20,4)(-99), inner(20,4)(-99)  /) )

   allocate ( b2(3), source = (/ base(1,3,4) ( 'ibm', (/ inner(20,4)(1), inner(20,4)(2), inner(20,4)(3)  /) ), &
                                 base(1,3,4) ( 'IBM', (/ inner(20,4)(2), inner(20,4)(3), inner(20,4)(4)  /) ), &
                                 base(1,3,4) ( 'ftn', (/ inner(20,4)(3), inner(20,4)(4), inner(20,4)(5)  /) ) /) )

   allocate ( b3(4), source = (/ base(1,3,4) ( 'IBM', (/ inner(20,4)(11), inner(20,4)(12), inner(20,4)(13)  /) ), &
                                 base(1,3,4) ( 'FTN', (/ inner(20,4)(12), inner(20,4)(13), inner(20,4)(14)  /) ), &
                                 base(1,3,4) ( 'XLF', (/ inner(20,4)(13), inner(20,4)(14), inner(20,4)(15)  /) ), &
                                 base(1,3,4) ( 'xlf', (/ inner(20,4)(14), inner(20,4)(15), inner(20,4)(16)  /) ) /) )

   b1 = b2
   print *, b1%c,  b1%in%i

   b1 = b3
   print *, b1%c,  b1%in%i

end program
