! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/mix/genericMix001.f
! opt variations: -qnol -qnodeferredlp

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
!*  SECONDARY FUNCTIONS TESTED : Mix generic type bounds
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : mix add, subtract, and assignment in a type
!*
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

module myint

   type int(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains

         procedure, pass :: add
         procedure, pass :: subtract
         procedure, pass :: assignment

         generic :: operator(+) => add
         generic :: operator(-) => subtract
         generic :: assignment(=) => assignment

   end type

   contains

      subroutine assignment(a,b)
         class(int(*,4)), intent(out) :: a
         class(int(*,4)), intent(in)  :: b

         print *, 'assignment'
         a%i = b%i

      end subroutine

      type(int(20,4)) function add ( a, b)
         class(int(*,4)), intent(in) :: a
         class(int(*,4)), intent(in) :: b

         print *, 'add'
         add%i = a%i + b%i

      end function

      type(int(20,4)) function subtract ( a, b)
         class(int(*,4)), intent(in) :: a
         class(int(*,4)), intent(in) :: b

         print *, 'sub'
         subtract = a + int(20,4)(-1*b%i)

      end function

end module

program genericMix001
   use myint

   type(int(20,4)) :: i
   type(int(:,4)), allocatable :: j
   class(int(:,4)), pointer :: k

   allocate ( int(20,4) :: j, k )

   i = int(20,4)(10) + int(20,4)(5)
   j = i + int(20,4)(15) - int(20,4)(10)
   k = i + j - i - j
   
   print *, i%i
   print *, j%i
   print *, k%i

end program
