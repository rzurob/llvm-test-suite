! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorArray003.f
! opt variations: -qnock -qnol -qnodeferredlp

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
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Binary Operator: Scalar to Array (**,//)
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

   type base(n1,k1)    ! (20,8)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i = -999
      contains
         procedure, pass :: pow
         generic :: operator ( ** ) => pow
   end type

   type, extends(base) :: child(k2,n2)    ! (20,8,1,3)
      integer, kind             :: k2
      integer, len              :: n2
      character(kind=k2,len=n2) :: c = 'xxx'
      contains
         generic :: operator ( // ) => concat
         procedure, pass :: concat
   end type

   interface operator( ** )
      class(base(:,8)) function atoapow (a, b)
         import base
         class(base(*,8)), intent(in) :: a(:), b(:)
         allocatable :: atoapow(:)
      end function
   end interface operator ( ** )

   interface operator( // )
      module procedure atoaconcat
   end interface operator ( // )

   contains

   function pow (a, b)
      class(base(*,8)), intent(in) :: a, b(:)
      class(base(:,8)), allocatable :: pow(:)

      allocate ( pow(size(b)), source = b )

      do i=1, size(b)
         pow(i)%i = a%i ** b(i)%i
      end do

   end function

   function concat (a, b)
      class(child(*,8,1,*)), intent(in) :: a, b(:)
      class(child(:,8,1,:)), allocatable :: concat(:)

      allocate ( concat(size(b)), source = b )

      do i=1, size(b)
         concat(i)%c = a%c(1:1) // b(i)%c(2:3)
      end do

   end function

   function atoaconcat (a, b)
      class(child(*,8,1,*)), intent(in) :: a(:), b(:)
      class(child(:,8,1,:)), allocatable :: atoaconcat(:)

      allocate ( atoaconcat(size(b)), source = b )
      if ( size ( a ) /= size ( b ) ) error stop 1_4

      do i=1, size(b)
         atoaconcat(i)%c = a(i)%c(1:1) // b(i)%c(2:3)
      end do

   end function

end module

function atoapow (a, b)
   use m, only: base
   class(base(*,8)), intent(in) :: a(:), b(:)
   class(base(:,8)), allocatable :: atoapow(:)

   allocate ( base(20,8):: atoapow(size(b)) )

   do i=1, size(b)
      atoapow(i)%i = b(i)%i ** a(i)%i
   end do

end function

program genericOperatorScalar003
   use m

   class(base(:,8)), allocatable :: b1, b2(:), b3(:), b4(:,:)
   class(child(:,8,1,:)), pointer :: c1, c2(:), c3(:)
   type(child(20,8,1,3)) c4(4)

   allocate ( b1, source = base(20,8) (2) )
   allocate ( b2(4), source = b1 **  (/ ( base(20,8) (i), i = 1, 4 ) /) )
   allocate ( b3(size(b2)), source = b1 ** b2 )

   print *, b1%i
   print *, b2%i
   print *, b3%i
   
   b2(4)%i = 4

   allocate ( b4(2,2), source = reshape ( source = ( b2 ** b2 ) , shape = (/2,2/) ) )
   print *, b4%i

   allocate ( c1, source = child(20,8,1,3) ( 2, 'abc' ) )
   allocate ( c2(4), source = child(20,8,1,3) ( 2, 'abc' ) // (/ child(20,8,1,3) ( 2, 'ABC' ), child(20,8,1,3) ( 2, 'DEF' ), child(20,8,1,3) ( 2, 'GHI' ), child(20,8,1,3) ( 2, 'JKL' ) /) )
   allocate ( c3(4), source = c2 // c2 )

   print *, c2%c
   print *, c3%c
   
   c4 = c2 // c3
   
   print *, c4%c
   
   deallocate ( b3 )
   allocate ( b3(4), source = c2 ** c3 )
   print *, b3%i

   
end program
